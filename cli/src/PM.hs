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

import Compile (compileSource, execute, resolveStdlib)
import Compiler.Serialize (encodeBytecodes)
import Control.Exception (try)
import qualified Data.ByteString.Lazy as BSL
import Data.List (isSuffixOf)
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
      writeFile (name </> "tests" </> ".gitkeep") ""
      printStep "created" (name </> "quant.toml")
      printStep "created" (name </> "src" </> "main.qa")
      printOk ("project `" ++ name ++ "` ready -- `cd " ++ name ++ " && glados run`")

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
    then do
      putStrLn $ "no test files found in `" ++ mTestDir m ++ "`"
      exitSuccess
    else do
      results <- mapM (runOneTest stdlib) files
      let passed = length (filter id results)
          failed = length (filter not results)
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

runOneTest :: FilePath -> FilePath -> IO Bool
runOneTest stdlib fp = do
  putStr $ dim ++ "  test" ++ reset ++ "  " ++ fp ++ " ... "
  result <- try (compileSource stdlib fp >>= execute) :: IO (Either ExitCode ())
  case result of
    Right () -> putStrLn (bold ++ green ++ "ok" ++ reset) >> return True
    Left ExitSuccess -> putStrLn (bold ++ green ++ "ok" ++ reset) >> return True
    Left (ExitFailure _) ->
      putStrLn (bold ++ red ++ "FAIL" ++ reset) >> return False

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
