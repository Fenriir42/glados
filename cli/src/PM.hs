module PM
  ( runInit,
    runBuild,
    runRun,
    runTest,
    runLint,
    runFmt,
    runDoc,
    runClean,
  )
where

import AST.Types.Common (FuncName (..))
import Compile (compileSource, compileSourceWith, execute, executeFunctionLineCov, resolveStdlib)
import qualified Compiler (Bytecode)
import Compiler.Bytecode (Instruction (ICovBranch, ICovMark), bytecodeFunction, bytecodeInstructions)
import Compiler.Serialize (encodeBytecodes)
import Control.Exception (try)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Display (bold, dim, green, printColored, printOk, printStep, red, reset, yellow)
import Manifest (Manifest (..), loadManifest)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem)

-- ---------------------------------------------------------------------------
-- init

runInit :: String -> IO ()
runInit name = do
  exists <- doesDirectoryExist name
  if exists
    then die ("directory `" ++ name ++ "` already exists")
    else do
      createDirectoryIfMissing True (name </> "src")
      createDirectoryIfMissing True (name </> "tests")
      writeFile (name </> "quant.toml") (tomlTemplate name)
      writeFile (name </> "src" </> "main.qa") (mainTemplate name)
      writeFile (name </> "tests" </> "main_test.qa") (mainTestTemplate name)
      printStep "created" (name </> "quant.toml")
      printStep "created" (name </> "src" </> "main.qa")
      printStep "created" (name </> "tests" </> "main_test.qa")
      printOk ("project `" ++ name ++ "` ready -- `cd " ++ name ++ " && glados test`")

tomlTemplate :: String -> String
tomlTemplate name =
  unlines
    [ "[project]",
      "name    = \"" ++ name ++ "\"",
      "version = \"0.1.0\"",
      "entry   = \"src/main.qa\""
    ]

mainTemplate :: String -> String
mainTemplate name =
  unlines
    [ "import io",
      "",
      "fn main() -> void {",
      "    io.print(\"Hello, " ++ name ++ "!\\n\");",
      "}"
    ]

mainTestTemplate :: String -> String
mainTestTemplate name =
  unlines
    [ "// Tests for " ++ name,
      "",
      "fn test_example() -> void {",
      "    assert(1 + 1 == 2, \"basic arithmetic\");",
      "}"
    ]

-- ---------------------------------------------------------------------------
-- build

runBuild :: Bool -> IO ()
runBuild _release = do
  m <- loadManifest
  stdlib <- resolveStdlib (mStdlib m)
  createDirectoryIfMissing True ".build"
  printStep "compiling" (mEntry m)
  bc <- compileSource stdlib (mEntry m)
  let out = ".build" </> "main.qbc"
  BSL.writeFile out (encodeBytecodes bc)
  printStep "written" out
  printOk "build successful"

-- ---------------------------------------------------------------------------
-- run

runRun :: IO ()
runRun = do
  m <- loadManifest
  stdlib <- resolveStdlib (mStdlib m)
  printStep "compiling" (mEntry m)
  bc <- compileSource stdlib (mEntry m)
  printStep "running" (mEntry m)
  execute bc

-- ---------------------------------------------------------------------------
-- test

runTest :: Maybe FilePath -> Bool -> Maybe Int -> Maybe FilePath -> IO ()
runTest mFile showCov covMin covOut = do
  m <- loadManifest
  stdlib <- resolveStdlib (mStdlib m)
  let srcDir = takeDirectory (mEntry m)
  files <- case mFile of
    Just f -> return [f]
    Nothing -> findTestFiles (mTestDir m)
  if null files
    then putStrLn ("no test files found in `" ++ mTestDir m ++ "`") >> exitSuccess
    else do
      covRef <- newIORef Set.empty
      lineCovRef <- newIORef Map.empty
      branchCovRef <- newIORef Map.empty
      bcMapRef <- newIORef Map.empty
      pairs <- mapM (runTestFile srcDir stdlib covRef lineCovRef branchCovRef bcMapRef) files
      let passed = sum (map fst pairs)
          failed = sum (map snd pairs)
      putStrLn ""
      let doCov = showCov || isJust covMin || isJust covOut
      when doCov $ do
        covered <- readIORef covRef
        lineCovHits <- readIORef lineCovRef
        branchCovHits <- readIORef branchCovRef
        bcMap <- readIORef bcMapRef
        printCoverageReport (mCovIgnore m) covMin covOut covered lineCovHits branchCovHits bcMap
      if failed == 0
        then printOk ("all " ++ show passed ++ " test(s) passed")
        else do
          printColored $
            bold
              ++ red
              ++ "FAILED"
              ++ reset
              ++ ": "
              ++ show failed
              ++ " failed, "
              ++ show passed
              ++ " passed\n"
          exitFailure

-- | Compile a test file once, then run each @test_*@ function individually.
-- Falls back to file-level execution when the file has its own @main@.
runTestFile ::
  FilePath ->
  FilePath ->
  IORef (Set.Set FuncName) ->
  IORef (Map.Map FuncName (Set.Set Int)) ->
  IORef (Map.Map (FuncName, Int) (Set.Set Bool)) ->
  IORef (Map.Map FuncName Compiler.Bytecode) ->
  FilePath ->
  IO (Int, Int)
runTestFile srcDir stdlib covRef lineCovRef branchCovRef bcMapRef fp = do
  src <- readFile fp
  printColored $ dim ++ "testing " ++ reset ++ fp ++ "\n"
  bc <- compileSourceWith [srcDir, stdlib] fp
  modifyIORef bcMapRef (\m -> foldr (\b acc -> Map.insert (bytecodeFunction b) b acc) m bc)
  let fns = scanTestFunctions src
  if null fns || hasMain src
    then do
      result <- try (execute bc) :: IO (Either ExitCode ())
      case result of
        Right () -> printOk "  (file)" >> return (1, 0)
        Left ExitSuccess -> printOk "  (file)" >> return (1, 0)
        Left (ExitFailure _) ->
          printColored (bold ++ red ++ "  FAIL" ++ reset ++ " (file)\n") >> return (0, 1)
    else do
      results <- mapM (runOneFn bc covRef lineCovRef branchCovRef) fns
      let p = length (filter id results)
          f = length (filter not results)
      return (p, f)

runOneFn ::
  [Compiler.Bytecode] ->
  IORef (Set.Set FuncName) ->
  IORef (Map.Map FuncName (Set.Set Int)) ->
  IORef (Map.Map (FuncName, Int) (Set.Set Bool)) ->
  String ->
  IO Bool
runOneFn bc covRef lineCovRef branchCovRef fname = do
  putStr (dotLine fname)
  result <- executeFunctionLineCov covRef lineCovRef branchCovRef (FuncName (T.pack fname)) bc
  case result of
    Right () ->
      printColored (bold ++ green ++ "ok" ++ reset ++ "\n") >> return True
    Left errMsg -> do
      printColored (bold ++ red ++ "FAIL" ++ reset ++ "\n")
      mapM_ (\l -> putStrLn ("    " ++ l)) (filter (not . null) (lines errMsg))
      return False

-- ---------------------------------------------------------------------------
-- Coverage report

data FileCovData = FileCovData
  { fcPath :: FilePath,
    fcFunctions :: [(String, Bool)],
    fcLineHit :: Int,
    fcLineTotal :: Int,
    -- | Branch coverage: (both-outcomes-seen, total-branch-points)
    fcBranchHit :: Int,
    fcBranchTotal :: Int
  }

-- | True when @fp@ matches any pattern in the ignore list.
-- Patterns are matched as suffixes, so @"main.qa"@ matches @"src/main.qa"@.
isIgnored :: [FilePath] -> FilePath -> Bool
isIgnored patterns fp = any (`isSuffixOf` fp) patterns

printCoverageReport ::
  [FilePath] ->
  Maybe Int ->
  Maybe FilePath ->
  Set.Set FuncName ->
  Map.Map FuncName (Set.Set Int) ->
  Map.Map (FuncName, Int) (Set.Set Bool) ->
  Map.Map FuncName Compiler.Bytecode ->
  IO ()
printCoverageReport ignoreList covMin covOut covered lineCovHits branchCovHits bcMap = do
  allFiles <- findQaWith (const True) "src"
  let srcFiles = filter (not . isIgnored ignoreList) allFiles
  when (null srcFiles) $ return ()
  fileData <- mapM (collectFileCov covered lineCovHits branchCovHits bcMap) srcFiles
  printColored $ "\n" ++ bold ++ "Coverage:" ++ reset ++ "\n"
  mapM_ renderFileCov fileData
  let totalFns = sum (map (length . fcFunctions) fileData)
      totalCov = sum (map (length . filter snd . fcFunctions) fileData)
      totalLines = sum (map fcLineTotal fileData)
      totalLinesHit = sum (map fcLineHit fileData)
      totalBranches = sum (map fcBranchTotal fileData)
      totalBranchHit = sum (map fcBranchHit fileData)
      totalPct = if totalFns == 0 then 100 else (totalCov * 100) `div` totalFns
  printColored $
    "  "
      ++ bold
      ++ "total"
      ++ reset
      ++ "  fn "
      ++ covBar totalCov totalFns
      ++ " "
      ++ show totalCov
      ++ "/"
      ++ show totalFns
      ++ " ("
      ++ pct totalCov totalFns
      ++ ")  ln "
      ++ covBar totalLinesHit totalLines
      ++ " "
      ++ show totalLinesHit
      ++ "/"
      ++ show totalLines
      ++ " ("
      ++ pct totalLinesHit totalLines
      ++ ")  br "
      ++ covBar totalBranchHit totalBranches
      ++ " "
      ++ show totalBranchHit
      ++ "/"
      ++ show totalBranches
      ++ " ("
      ++ pct totalBranchHit totalBranches
      ++ ")\n"
  case covOut of
    Just path -> writeCovJson path fileData >> printStep "written" path
    Nothing -> return ()
  case covMin of
    Just threshold ->
      when (totalPct < threshold) $ do
        printColored $
          bold
            ++ red
            ++ "coverage below threshold"
            ++ reset
            ++ ": "
            ++ show totalPct
            ++ "% < "
            ++ show threshold
            ++ "%\n"
        exitFailure
    Nothing -> return ()

collectFileCov ::
  Set.Set FuncName ->
  Map.Map FuncName (Set.Set Int) ->
  Map.Map (FuncName, Int) (Set.Set Bool) ->
  Map.Map FuncName Compiler.Bytecode ->
  FilePath ->
  IO FileCovData
collectFileCov covered lineCovHits branchCovHits bcMap fp = do
  src <- readFile fp
  let fns = scanSourceFunctions src
      results = [(name, FuncName (T.pack name) `Set.member` covered) | name <- fns]
      (lineHit, lineTotal, branchHit, branchTotal) = foldr countAll (0, 0, 0, 0) fns
  return (FileCovData fp results lineHit lineTotal branchHit branchTotal)
  where
    countAll name (lh, lt, bh, bt) =
      let fname = FuncName (T.pack name)
          instrs = maybe [] bytecodeInstructions (Map.lookup fname bcMap)
          execLines = Set.fromList [n | ICovMark n <- instrs]
          hitLines = Map.findWithDefault Set.empty fname lineCovHits
          branchLines = [n | ICovBranch n <- instrs]
          -- A branch point is "fully covered" when both True and False were seen.
          (bHit, bTotal) = foldr (countBranch fname) (0, 0) branchLines
       in ( lh + Set.size (Set.intersection execLines hitLines),
            lt + Set.size execLines,
            bh + bHit,
            bt + bTotal
          )
    countBranch fname lineNo (bh, bt) =
      let outcomes = Map.findWithDefault Set.empty (fname, lineNo) branchCovHits
          fullyHit = Set.size outcomes >= 2
       in (bh + if fullyHit then 1 else 0, bt + 1)

renderFileCov :: FileCovData -> IO ()
renderFileCov fd = do
  let fp = fcPath fd
      fns = fcFunctions fd
      lineHit = fcLineHit fd
      lineTotal = fcLineTotal fd
      branchHit = fcBranchHit fd
      branchTotal = fcBranchTotal fd
  let covCount = length (filter snd fns)
      total = length fns
  printColored $
    "  "
      ++ dim
      ++ fp
      ++ reset
      ++ "  fn "
      ++ covBar covCount total
      ++ " "
      ++ show covCount
      ++ "/"
      ++ show total
      ++ " ("
      ++ pct covCount total
      ++ ")  ln "
      ++ covBar lineHit lineTotal
      ++ " "
      ++ show lineHit
      ++ "/"
      ++ show lineTotal
      ++ " ("
      ++ pct lineHit lineTotal
      ++ ")  br "
      ++ covBar branchHit branchTotal
      ++ " "
      ++ show branchHit
      ++ "/"
      ++ show branchTotal
      ++ " ("
      ++ pct branchHit branchTotal
      ++ ")\n"
  mapM_ printCovLine fns

printCovLine :: (String, Bool) -> IO ()
printCovLine (name, True) =
  printColored $ "    " ++ green ++ "+" ++ reset ++ " " ++ name ++ "\n"
printCovLine (name, False) =
  printColored $ "    " ++ red ++ "-" ++ reset ++ " " ++ name ++ "\n"

covBar :: Int -> Int -> String
covBar covered total =
  let width = 10
      filled = if total == 0 then width else (covered * width) `div` total
   in "[" ++ replicate filled '#' ++ replicate (width - filled) '.' ++ "]"

pct :: Int -> Int -> String
pct _ 0 = "n/a"
pct n d = show ((n * 100) `div` d) ++ "%"

writeCovJson :: FilePath -> [FileCovData] -> IO ()
writeCovJson outPath fileData = do
  let totalFns = sum (map (length . fcFunctions) fileData)
      totalCov = sum (map (length . filter snd . fcFunctions) fileData)
      totalLines = sum (map fcLineTotal fileData)
      totalLinesHit = sum (map fcLineHit fileData)
      totalBranches = sum (map fcBranchTotal fileData)
      totalBranchHit = sum (map fcBranchHit fileData)
      totalPct = if totalFns == 0 then 100 else (totalCov * 100) `div` totalFns
      totalLinePct = if totalLines == 0 then 100 else (totalLinesHit * 100) `div` totalLines
      totalBranchPct = if totalBranches == 0 then 100 else (totalBranchHit * 100) `div` totalBranches
      fileEntries = intercalate ",\n    " (map renderFileEntry fileData)
      json =
        "{\n"
          ++ "  \"total\": {\"covered\": "
          ++ show totalCov
          ++ ", \"total\": "
          ++ show totalFns
          ++ ", \"pct\": "
          ++ show totalPct
          ++ ", \"lines_hit\": "
          ++ show totalLinesHit
          ++ ", \"lines_total\": "
          ++ show totalLines
          ++ ", \"lines_pct\": "
          ++ show totalLinePct
          ++ ", \"branches_hit\": "
          ++ show totalBranchHit
          ++ ", \"branches_total\": "
          ++ show totalBranches
          ++ ", \"branches_pct\": "
          ++ show totalBranchPct
          ++ "},\n"
          ++ "  \"files\": [\n    "
          ++ fileEntries
          ++ "\n  ]\n}\n"
  writeFile outPath json

renderFileEntry :: FileCovData -> String
renderFileEntry fd =
  let fp = fcPath fd
      fns = fcFunctions fd
      lineHit = fcLineHit fd
      lineTotal = fcLineTotal fd
      branchHit = fcBranchHit fd
      branchTotal = fcBranchTotal fd
      cov = length (filter snd fns)
      total = length fns
      pctVal = if total == 0 then 100 else (cov * 100) `div` total
      linePct = if lineTotal == 0 then 100 else (lineHit * 100) `div` lineTotal
      branchPct = if branchTotal == 0 then 100 else (branchHit * 100) `div` branchTotal
      fnParts = intercalate ", " (map renderFnEntry fns)
   in "{\"path\": \""
        ++ fp
        ++ "\", \"covered\": "
        ++ show cov
        ++ ", \"total\": "
        ++ show total
        ++ ", \"pct\": "
        ++ show pctVal
        ++ ", \"lines_hit\": "
        ++ show lineHit
        ++ ", \"lines_total\": "
        ++ show lineTotal
        ++ ", \"lines_pct\": "
        ++ show linePct
        ++ ", \"branches_hit\": "
        ++ show branchHit
        ++ ", \"branches_total\": "
        ++ show branchTotal
        ++ ", \"branches_pct\": "
        ++ show branchPct
        ++ ", \"functions\": ["
        ++ fnParts
        ++ "]}"

renderFnEntry :: (String, Bool) -> String
renderFnEntry (name, isCov) =
  "{\"name\": \""
    ++ name
    ++ "\", \"covered\": "
    ++ (if isCov then "true" else "false")
    ++ "}"

-- | Scan source text for all @fn@ declarations; return their names.
scanSourceFunctions :: String -> [String]
scanSourceFunctions src =
  [ name
    | l <- lines src,
      let s = dropWhile (== ' ') l,
      "fn " `isPrefixOf` s || "static fn " `isPrefixOf` s,
      let after = if "static fn " `isPrefixOf` s then drop 10 s else drop 3 s,
      let name = takeWhile (\c -> isAlphaNum c || c == '_') after,
      not (null name)
  ]

dotLine :: String -> String
dotLine name =
  let col = 38
      padded = "  " ++ name ++ " "
      dots = replicate (max 3 (col - length padded)) '.'
   in padded ++ dots ++ " "

-- | Scan source text for @fn test_*@ declarations; return their names.
scanTestFunctions :: String -> [String]
scanTestFunctions src =
  [ name
    | l <- lines src,
      let s = dropWhile (== ' ') l,
      "fn test_" `isPrefixOf` s,
      let name = takeWhile (\c -> isAlphaNum c || c == '_') (drop 3 s),
      "test_" `isPrefixOf` name
  ]

hasMain :: String -> Bool
hasMain src =
  any (\l -> "fn main(" `isPrefixOf` dropWhile (== ' ') l) (lines src)

findTestFiles :: FilePath -> IO [FilePath]
findTestFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else findQaWith ("_test.qa" `isSuffixOf`) dir

-- ---------------------------------------------------------------------------
-- lint

runLint :: IO ()
runLint = do
  m <- loadManifest
  files <- findQaWith (const True) "src"
  tests <- findQaWith (const True) (mTestDir m)
  let all' = files ++ tests
  if null all'
    then putStrLn "no source files found" >> exitSuccess
    else do
      printStep "linting" (show (length all') ++ " files")
      ec <- rawSystem "wheatley" all'
      exitWith ec

-- ---------------------------------------------------------------------------
-- fmt

runFmt :: Bool -> IO ()
runFmt check = do
  m <- loadManifest
  files <- findQaWith (const True) "src"
  tests <- findQaWith (const True) (mTestDir m)
  let all' = files ++ tests
      flags = ["--check" | check]
  if null all'
    then putStrLn "no source files found" >> exitSuccess
    else do
      printStep "formatting" (show (length all') ++ " files")
      ec <- rawSystem "quant-fmt" (flags ++ all')
      exitWith ec

-- ---------------------------------------------------------------------------
-- doc

runDoc :: String -> FilePath -> IO ()
runDoc _format _out = do
  putStrLn $
    bold
      ++ yellow
      ++ "warning"
      ++ reset
      ++ ": doc generation is not yet implemented"
  exitSuccess

-- ---------------------------------------------------------------------------
-- clean

runClean :: IO ()
runClean = do
  exists <- doesDirectoryExist ".build"
  if exists
    then do
      removeDirectoryRecursive ".build"
      printStep "removed" ".build/"
      printOk "clean"
    else printOk "nothing to clean"

-- ---------------------------------------------------------------------------
-- File discovery

findQaWith :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findQaWith predicate dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else go dir
  where
    go d = do
      entries <- listDirectory d
      let paths = map (d </>) entries
      results <- mapM classify paths
      return (concatMap snd results)
      where
        classify p = do
          isDir <- doesDirectoryExist p
          isFile <- doesFileExist p
          if isDir
            then do sub <- go p; return (False, sub)
            else
              return
                ( isFile,
                  [p | isFile && takeExtension p == ".qa" && predicate p]
                )

-- ---------------------------------------------------------------------------
-- Helpers

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
