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
import Compile (compileSource, execute, executeFunction, resolveStdlib)
import qualified Compiler (Bytecode)
import Compiler.Serialize (encodeBytecodes)
import Control.Exception (try)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, isSuffixOf)
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
import System.FilePath (takeExtension, (</>))
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

runTest :: Maybe FilePath -> IO ()
runTest mFile = do
  m <- loadManifest
  stdlib <- resolveStdlib (mStdlib m)
  files <- case mFile of
    Just f -> return [f]
    Nothing -> findTestFiles (mTestDir m)
  if null files
    then putStrLn ("no test files found in `" ++ mTestDir m ++ "`") >> exitSuccess
    else do
      pairs <- mapM (runTestFile stdlib) files
      let passed = sum (map fst pairs)
          failed = sum (map snd pairs)
      putStrLn ""
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
runTestFile :: FilePath -> FilePath -> IO (Int, Int)
runTestFile stdlib fp = do
  src <- readFile fp
  printColored $ dim ++ "testing " ++ reset ++ fp ++ "\n"
  bc <- compileSource stdlib fp
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
      results <- mapM (runOneFn bc) fns
      let p = length (filter id results)
          f = length (filter not results)
      return (p, f)

runOneFn :: [Compiler.Bytecode] -> String -> IO Bool
runOneFn bc fname = do
  putStr (dotLine fname)
  result <- executeFunction (FuncName (T.pack fname)) bc
  case result of
    Right () ->
      printColored (bold ++ green ++ "ok" ++ reset ++ "\n") >> return True
    Left errMsg -> do
      printColored (bold ++ red ++ "FAIL" ++ reset ++ "\n")
      mapM_ (\l -> putStrLn ("    " ++ l)) (filter (not . null) (lines errMsg))
      return False

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
