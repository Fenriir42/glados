module PM
  ( runInit,
    runBuild,
    runRun,
    runTest,
    runLint,
    runFmt,
    runDoc,
    runClean,
    runWatch,
    runBench,
  )
where

import AST.Types.Common (FuncName (..))
import Compile (buildNative, compileSource, compileSourceWith, execute, executeFunction, executeFunctionLineCov, resolveStdlib)
import qualified Compiler (Bytecode)
import Compiler.Bytecode (Instruction (ICovBranch, ICovMark), bytecodeFunction, bytecodeInstructions)
import Compiler.Serialize (encodeBytecodes)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Display (bold, dim, green, printColored, printOk, printStep, red, reset)
import Manifest (Manifest (..), loadManifest)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (BufferMode (LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
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

runBuild :: Bool -> Maybe String -> IO ()
runBuild _release target = do
  m <- loadManifest
  stdlib <- resolveStdlib (mStdlib m)
  createDirectoryIfMissing True ".build"
  printStep "compiling" (mEntry m)
  bc <- compileSource stdlib (mEntry m)
  case target of
    Just "c" -> do
      let out = ".build" </> "main"
      printStep "native" (out ++ ".c")
      buildNative bc out
      printStep "written" out
      printOk "build successful"
    Just other -> die ("unknown build target `" ++ other ++ "` (supported: c)")
    Nothing -> do
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

data DocEntry = DocEntry
  { deName :: String,
    deSig :: String,
    deDoc :: [String]
  }

data DocPage = DocPage
  { dpModule :: String,
    dpEntries :: [DocEntry]
  }

runDoc :: String -> FilePath -> IO ()
runDoc format outDir = do
  m <- loadManifest
  let srcDir = takeDirectory (mEntry m)
  files <- findQaWith (const True) srcDir
  if null files
    then putStrLn "no source files found" >> exitSuccess
    else do
      createDirectoryIfMissing True outDir
      pages <- mapM buildDocPage files
      let withEntries = filter (not . null . dpEntries) pages
      case format of
        "md" -> do
          mapM_ (writeDocPageMd outDir) withEntries
          writeDocIndexMd outDir (mName m) withEntries
        _ -> do
          mapM_ (writeDocPageHtml outDir) withEntries
          writeDocIndexHtml outDir (mName m) withEntries
      printOk ("docs written to " ++ outDir)

buildDocPage :: FilePath -> IO DocPage
buildDocPage fp = do
  src <- readFile fp
  return DocPage {dpModule = takeBaseName fp, dpEntries = scanDocEntries (lines src)}

scanDocEntries :: [String] -> [DocEntry]
scanDocEntries = go []
  where
    go _ [] = []
    go buf (l : ls)
      | isTopLevelDecl l =
          let (sig, rest) = collectSig (l : ls)
              entry = DocEntry {deName = extractDeclName l, deSig = sig, deDoc = reverse buf}
           in entry : go [] rest
      | isDocComment l = go (stripDocPrefix l : buf) ls
      | otherwise = go [] ls

isTopLevelDecl :: String -> Bool
isTopLevelDecl l =
  not (null l)
    && head l /= ' '
    && any (`isPrefixOf` l) ["fn ", "struct ", "error "]

extractDeclName :: String -> String
extractDeclName l
  | "fn " `isPrefixOf` l = ident (drop 3 l)
  | "struct " `isPrefixOf` l = ident (drop 7 l)
  | "error " `isPrefixOf` l = ident (drop 6 l)
  | otherwise = ""
  where
    ident = takeWhile (\c -> isAlphaNum c || c == '_')

collectSig :: [String] -> (String, [String])
collectSig ls =
  let chunk = take 10 ls
      (before, withBrace) = break (elem '{') chunk
      sigLines = before ++ take 1 withBrace
      after = drop 1 withBrace ++ drop 10 ls
      rawSig = unwords (map (dropWhile (== ' ')) sigLines)
      clean = reverse . dropWhile (\c -> c == ' ' || c == '{') . reverse $ rawSig
   in (clean, after)

isDocComment :: String -> Bool
isDocComment l =
  not (null l)
    && head l /= ' '
    && ("// " `isPrefixOf` l || l == "//")

stripDocPrefix :: String -> String
stripDocPrefix "//" = ""
stripDocPrefix l = drop 3 l

docParas :: [String] -> [[String]]
docParas ls = go ls []
  where
    go [] acc = [reverse acc | not (null acc)]
    go (x : xs) acc
      | null x = [reverse acc | not (null acc)] ++ go xs []
      | otherwise = go xs (x : acc)

-- ---------------------------------------------------------------------------
-- HTML rendering

escHtml :: String -> String
escHtml = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc c = [c]

kindFromSig :: String -> String
kindFromSig s
  | "fn " `isPrefixOf` s = "fn"
  | "struct " `isPrefixOf` s = "struct"
  | "error " `isPrefixOf` s = "error"
  | otherwise = ""

moduleCss :: String
moduleCss =
  intercalate
    "\n"
    [ "*{box-sizing:border-box;margin:0;padding:0}",
      "body{font-family:system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#f8f7ff;color:#111827;line-height:1.6}",
      ".layout{display:flex;min-height:100vh}",
      ".sidebar{width:220px;min-width:220px;background:#1e1b2e;color:#c4b5fd;display:flex;flex-direction:column;position:sticky;top:0;height:100vh;overflow-y:auto}",
      ".sidebar-header{padding:1.2rem 1rem;border-bottom:1px solid rgba(255,255,255,.08)}",
      ".back{color:#a78bfa;text-decoration:none;font-size:.82rem;display:block;margin-bottom:.5rem}",
      ".back:hover{color:#c4b5fd}",
      ".mod-title{font-weight:700;font-size:.98rem;color:#ede9fe;letter-spacing:.02em;display:block}",
      ".sidebar-nav{padding:.8rem 0;flex:1}",
      ".sidebar-nav a{display:block;padding:.3rem 1rem;color:#a78bfa;text-decoration:none;font-size:.84rem;font-family:'JetBrains Mono','Fira Code',ui-monospace,monospace;border-left:2px solid transparent;transition:all .12s}",
      ".sidebar-nav a:hover,.sidebar-nav a.active{color:#ede9fe;background:rgba(167,139,250,.12);border-left-color:#7c3aed}",
      ".content{flex:1;padding:2.5rem 3rem;min-width:0}",
      ".page-title{font-size:1.75rem;font-weight:800;color:#1e1b2e;border-bottom:2px solid #7c3aed;padding-bottom:.4rem;margin-bottom:2rem;letter-spacing:-.01em}",
      ".entry{margin-bottom:1.8rem;background:#fff;border-radius:8px;box-shadow:0 1px 3px rgba(0,0,0,.06),0 1px 2px rgba(0,0,0,.04);overflow:hidden;border:1px solid #ede9fe}",
      ".entry-header{display:flex;align-items:center;justify-content:space-between;padding:.65rem 1.2rem;background:#f5f3ff;border-bottom:1px solid #ede9fe}",
      ".entry-header h2{font-size:.95rem;font-weight:700;color:#5b21b6;font-family:'JetBrains Mono','Fira Code',ui-monospace,monospace}",
      ".anchor{color:#c4b5fd;text-decoration:none;font-size:.85rem;transition:color .12s}",
      ".anchor:hover{color:#7c3aed}",
      "pre.sig{background:#0d1117;color:#e6edf3;padding:.85rem 1.2rem;font-size:.85rem;overflow-x:auto;font-family:'JetBrains Mono','Fira Code',ui-monospace,monospace;line-height:1.55;margin:0}",
      ".doc{padding:.85rem 1.2rem;color:#374151}",
      ".doc p{margin-bottom:.45rem;line-height:1.65}",
      ".doc p:last-child{margin-bottom:0}",
      ".no-doc{padding:.6rem 1.2rem;font-size:.83rem;color:#9ca3af;font-style:italic}",
      "@media(max-width:640px){.sidebar{display:none}.content{padding:1.2rem}}"
    ]

indexCss :: String
indexCss =
  intercalate
    "\n"
    [ "*{box-sizing:border-box;margin:0;padding:0}",
      "body{font-family:system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#f8f7ff;color:#111827;line-height:1.6}",
      ".hero{background:linear-gradient(135deg,#1e1b2e 0%,#4c1d95 100%);color:#fff;padding:3.5rem 2rem}",
      ".hero-inner{max-width:920px;margin:0 auto}",
      ".hero h1{font-size:2.2rem;font-weight:800;margin-bottom:.5rem;letter-spacing:-.02em}",
      ".hero p{color:#c4b5fd;font-size:1rem}",
      ".index-content{max-width:920px;margin:2.5rem auto;padding:0 1.5rem 3rem}",
      ".section-label{font-size:.75rem;font-weight:700;color:#9ca3af;text-transform:uppercase;letter-spacing:.1em;margin-bottom:1rem}",
      ".module-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(230px,1fr));gap:1.1rem}",
      ".module-card{background:#fff;border-radius:10px;padding:1.2rem 1.4rem;box-shadow:0 1px 3px rgba(0,0,0,.06);border:1px solid #ede9fe;text-decoration:none;color:inherit;display:block;transition:box-shadow .18s,transform .14s}",
      ".module-card:hover{box-shadow:0 6px 22px rgba(124,58,237,.14);transform:translateY(-2px)}",
      ".card-title{font-size:1rem;font-weight:700;color:#5b21b6;font-family:'JetBrains Mono','Fira Code',ui-monospace,monospace;margin-bottom:.2rem}",
      ".card-count{font-size:.75rem;color:#9ca3af;margin-bottom:.85rem}",
      ".card-entries{list-style:none;font-size:.82rem;font-family:'JetBrains Mono','Fira Code',ui-monospace,monospace}",
      ".card-entries li{padding:.1rem 0;display:flex;align-items:center;gap:.35rem}",
      ".k{font-size:.68rem;padding:.1rem .28rem;border-radius:3px;font-weight:700;letter-spacing:.02em;flex-shrink:0}",
      ".k-fn{background:#ede9fe;color:#7c3aed}",
      ".k-struct{background:#ecfdf5;color:#065f46}",
      ".k-error{background:#fef2f2;color:#991b1b}",
      ".entry-name{color:#374151;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}"
    ]

scrollSpyJs :: String
scrollSpyJs =
  "(function(){"
    ++ "var L=document.querySelectorAll('.sidebar-nav a');"
    ++ "var S=document.querySelectorAll('.entry');"
    ++ "function spy(){var c='';"
    ++ "S.forEach(function(s){if(s.getBoundingClientRect().top<=80)c=s.id;});"
    ++ "L.forEach(function(a){a.classList.toggle('active',a.getAttribute('href')==='#'+c);});"
    ++ "}window.addEventListener('scroll',spy,{passive:true});spy();"
    ++ "})();"

writeDocPageHtml :: FilePath -> DocPage -> IO ()
writeDocPageHtml outDir page = do
  let fname = outDir </> dpModule page ++ ".html"
  writeFile fname (renderModuleHtml page)
  printStep "wrote" fname

writeDocIndexHtml :: FilePath -> String -> [DocPage] -> IO ()
writeDocIndexHtml outDir projName pages = do
  let fname = outDir </> "index.html"
  writeFile fname (renderIndexHtml projName pages)
  printStep "wrote" fname

renderModuleHtml :: DocPage -> String
renderModuleHtml page =
  unlines
    [ "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"UTF-8\">",
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
      "<title>" ++ escHtml (dpModule page) ++ " - Quant Docs</title>",
      "<style>" ++ moduleCss ++ "</style>",
      "</head>",
      "<body>",
      "<div class=\"layout\">",
      "<aside class=\"sidebar\">",
      "<div class=\"sidebar-header\">",
      "<a class=\"back\" href=\"index.html\">&#8592; Index</a>",
      "<span class=\"mod-title\">" ++ escHtml (dpModule page) ++ "</span>",
      "</div>",
      "<nav class=\"sidebar-nav\">"
        ++ concatMap
          (\e -> "<a href=\"#" ++ escHtml (deName e) ++ "\">" ++ escHtml (deName e) ++ "</a>")
          (dpEntries page),
      "</nav>",
      "</aside>",
      "<main class=\"content\">",
      "<h1 class=\"page-title\">" ++ escHtml (dpModule page) ++ "</h1>",
      concatMap renderEntryHtml (dpEntries page),
      "</main>",
      "</div>",
      "<script>" ++ scrollSpyJs ++ "</script>",
      "</body>",
      "</html>"
    ]

renderEntryHtml :: DocEntry -> String
renderEntryHtml e =
  "<section class=\"entry\" id=\""
    ++ escHtml (deName e)
    ++ "\">"
    ++ "<div class=\"entry-header\">"
    ++ "<h2>"
    ++ escHtml (deName e)
    ++ "</h2>"
    ++ "<a class=\"anchor\" href=\"#"
    ++ escHtml (deName e)
    ++ "\">#</a>"
    ++ "</div>"
    ++ "<pre class=\"sig\">"
    ++ escHtml (deSig e)
    ++ "</pre>"
    ++ ( if null (deDoc e)
           then "<div class=\"no-doc\">No documentation.</div>"
           else "<div class=\"doc\">" ++ concatMap (\ps -> "<p>" ++ escHtml (unwords ps) ++ "</p>") (docParas (deDoc e)) ++ "</div>"
       )
    ++ "</section>\n"

renderIndexHtml :: String -> [DocPage] -> String
renderIndexHtml projName pages =
  unlines
    [ "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"UTF-8\">",
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
      "<title>" ++ escHtml projName ++ " - Quant Docs</title>",
      "<style>" ++ indexCss ++ "</style>",
      "</head>",
      "<body>",
      "<header class=\"hero\">",
      "<div class=\"hero-inner\">",
      "<h1>" ++ escHtml projName ++ "</h1>",
      "<p>Generated from Quant source documentation.</p>",
      "</div>",
      "</header>",
      "<main class=\"index-content\">",
      "<p class=\"section-label\">Modules</p>",
      "<div class=\"module-grid\">",
      concatMap renderModuleCardHtml pages,
      "</div>",
      "</main>",
      "</body>",
      "</html>"
    ]

renderModuleCardHtml :: DocPage -> String
renderModuleCardHtml page =
  let n = length (dpEntries page)
      countStr = show n ++ " " ++ if n == 1 then "entry" else "entries"
      visible = take 6 (dpEntries page)
      overflow = n - length visible
   in "<a class=\"module-card\" href=\""
        ++ escHtml (dpModule page)
        ++ ".html\">"
        ++ "<div class=\"card-title\">"
        ++ escHtml (dpModule page)
        ++ "</div>"
        ++ "<div class=\"card-count\">"
        ++ countStr
        ++ "</div>"
        ++ "<ul class=\"card-entries\">"
        ++ concatMap renderCardEntryHtml visible
        ++ ( if overflow > 0
               then "<li><span class=\"entry-name\">+" ++ show overflow ++ " more\8230</span></li>"
               else ""
           )
        ++ "</ul>"
        ++ "</a>\n"

renderCardEntryHtml :: DocEntry -> String
renderCardEntryHtml e =
  let k = kindFromSig (deSig e)
      klass = case k of
        "struct" -> "k-struct"
        "error" -> "k-error"
        _ -> "k-fn"
   in "<li><span class=\"k "
        ++ klass
        ++ "\">"
        ++ k
        ++ "</span><span class=\"entry-name\">"
        ++ escHtml (deName e)
        ++ "</span></li>"

-- ---------------------------------------------------------------------------
-- Markdown rendering

writeDocPageMd :: FilePath -> DocPage -> IO ()
writeDocPageMd outDir page = do
  let fname = outDir </> dpModule page ++ ".md"
  writeFile fname (renderModuleMd page)
  printStep "wrote" fname

writeDocIndexMd :: FilePath -> String -> [DocPage] -> IO ()
writeDocIndexMd outDir projName pages = do
  let fname = outDir </> "index.md"
  writeFile fname (renderIndexMd projName pages)
  printStep "wrote" fname

renderModuleMd :: DocPage -> String
renderModuleMd page =
  "# "
    ++ dpModule page
    ++ "\n\n"
    ++ concatMap renderEntryMd (dpEntries page)

renderEntryMd :: DocEntry -> String
renderEntryMd e =
  "## `"
    ++ deName e
    ++ "`\n\n```quant\n"
    ++ deSig e
    ++ "\n```\n\n"
    ++ ( if null (deDoc e)
           then ""
           else intercalate "\n\n" (map unwords (docParas (deDoc e))) ++ "\n\n"
       )

renderIndexMd :: String -> [DocPage] -> String
renderIndexMd projName pages =
  "# "
    ++ projName
    ++ " API\n\nGenerated from Quant source documentation.\n\n## Modules\n\n"
    ++ concatMap
      ( \page ->
          "- [**"
            ++ dpModule page
            ++ "**]("
            ++ dpModule page
            ++ ".md), "
            ++ show (length (dpEntries page))
            ++ " entries\n"
      )
      pages

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
-- watch

-- | Re-run a glados subcommand whenever a source `.qa` file changes.  Polls
-- modification times of every `.qa` file under `src/` and the test directory
-- (~400 ms); portable and dependency-free.  @rawArgs@ is the subcommand to
-- run (default: @build@); Ctrl-C stops the loop.
runWatch :: [String] -> IO ()
runWatch rawArgs = do
  hSetBuffering stdout LineBuffering
  m <- loadManifest
  self <- getExecutablePath
  let cmd = if null rawArgs then ["build"] else rawArgs
      roots = ["src", mTestDir m]
      runOnce = do
        printColored (bold ++ green ++ "  run     " ++ reset ++ "glados " ++ unwords cmd ++ "\n")
        ec <- rawSystem self cmd
        case ec of
          ExitSuccess -> printOk "up to date"
          ExitFailure c ->
            printColored (bold ++ red ++ "  FAILED  " ++ reset ++ "exit " ++ show c ++ "\n")
      snapshot = do
        files <- concat <$> mapM (findQaWith (const True)) roots
        mapM (\f -> (,) f <$> safeMtime f) (sort files)
      loop prev = do
        threadDelay 400000
        cur <- snapshot
        if cur /= prev
          then printStep "watch" "change detected" >> runOnce >> loop cur
          else loop prev
  printStep "watch" ("watching " ++ intercalate " and " roots ++ " for .qa changes (Ctrl-C to stop)")
  runOnce
  snapshot >>= loop

-- | Modification time of a file, or 'Nothing' if it vanished mid-scan (which
-- itself counts as a change).
safeMtime :: FilePath -> IO (Maybe UTCTime)
safeMtime f = do
  r <- try (getModificationTime f) :: IO (Either SomeException UTCTime)
  return (either (const Nothing) Just r)

-- ---------------------------------------------------------------------------
-- bench

-- | Discover and run microbenchmarks.  With a FILE argument, run just that
-- file's @bench_*@ functions; otherwise scan @benchmarks/@ and the test
-- directory for @*_bench.qa@ files.  Each @bench_*@ function is expected to
-- call @bench.bench_fn@, which prints its own timing line.
runBench :: Maybe FilePath -> IO ()
runBench (Just f) = do
  stdlib <- resolveStdlib Nothing
  n <- runBenchFile (takeDirectory f) stdlib f
  putStrLn ""
  printOk (show n ++ " benchmark(s) run")
runBench Nothing = do
  m <- loadManifest
  stdlib <- resolveStdlib (mStdlib m)
  let srcDir = takeDirectory (mEntry m)
  files <- findBenchFiles ["benchmarks", mTestDir m]
  if null files
    then putStrLn "no benchmark files found (looked in benchmarks/ and the test dir)" >> exitSuccess
    else do
      counts <- mapM (runBenchFile srcDir stdlib) files
      putStrLn ""
      printOk (show (sum counts) ++ " benchmark(s) run")

runBenchFile :: FilePath -> FilePath -> FilePath -> IO Int
runBenchFile srcDir stdlib fp = do
  src <- readFile fp
  printColored $ dim ++ "benchmarking " ++ reset ++ fp ++ "\n"
  bc <- compileSourceWith [srcDir, stdlib] fp
  let fns = scanBenchFunctions src
  mapM_ (runOneBench bc) fns
  return (length fns)

runOneBench :: [Compiler.Bytecode] -> String -> IO ()
runOneBench bc fname = do
  r <- executeFunction (FuncName (T.pack fname)) bc
  case r of
    Right () -> return ()
    Left err -> printColored (bold ++ red ++ "  FAIL " ++ reset ++ fname ++ ": " ++ err ++ "\n")

-- | Scan source text for @fn bench_*@ declarations; return their names.
scanBenchFunctions :: String -> [String]
scanBenchFunctions src =
  [ name
    | l <- lines src,
      let s = dropWhile (== ' ') l,
      "fn bench_" `isPrefixOf` s,
      let name = takeWhile (\c -> isAlphaNum c || c == '_') (drop 3 s),
      "bench_" `isPrefixOf` name
  ]

-- | Collect @*_bench.qa@ files under the given roots.
findBenchFiles :: [FilePath] -> IO [FilePath]
findBenchFiles roots = concat <$> mapM (findQaWith ("_bench.qa" `isSuffixOf`)) roots

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
