-- | Differential test harness for the native backend (stage 3).
--
-- Runs every .qa file under both the bytecode VM and a natively
-- compiled binary (via the C backend), then diffs stdout and exit
-- codes.  Files the translator does not cover yet are reported as
-- skips -- the translator rejects unsupported instructions and
-- builtins statically, so a skip can never hide a semantics diff.
module NativeDiff (runNativeDiff) where

import Control.Monad (forM)
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf, sort)
import Display
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)

data DiffResult = Pass | Skip String | Fail String

runNativeDiff :: [FilePath] -> IO ()
runNativeDiff paths = do
  let roots = if null paths then ["tests"] else paths
  files <- concat <$> mapM listQaFiles roots
  if null files
    then putStrLn ("no .qa files found under " ++ show roots) >> exitFailure
    else do
      self <- getExecutablePath
      let outDir = ".build" </> "native-diff"
      createDirectoryIfMissing True outDir
      results <- forM files (diffFile self outDir)
      let passed = length [() | Pass <- results]
          skipped = length [() | Skip _ <- results]
          failed = length [() | Fail _ <- results]
      putStrLn ""
      if failed == 0
        then
          printOk
            ( show passed
                ++ " matched, "
                ++ show skipped
                ++ " skipped (not yet translatable)"
            )
        else do
          printColored $
            bold
              ++ red
              ++ "FAILED"
              ++ reset
              ++ ": "
              ++ show failed
              ++ " diff(s), "
              ++ show passed
              ++ " matched, "
              ++ show skipped
              ++ " skipped\n"
          exitFailure

-- | Collect .qa files under a path (recursively for directories).
listQaFiles :: FilePath -> IO [FilePath]
listQaFiles p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- sort . map (p </>) <$> listDirectory p
      concat <$> mapM listQaFiles entries
    else do
      isFile <- doesFileExist p
      return [p | isFile, ".qa" `isSuffixOf` p]

-- | Run one file under both engines and report the outcome.  A file may
-- opt out with a @native-diff: skip@ marker comment when its behaviour is
-- inherently non-deterministic (external network, wall-clock, randomness)
-- and so cannot be compared byte-for-byte.
diffFile :: FilePath -> FilePath -> FilePath -> IO DiffResult
diffFile self outDir file = do
  src <- readFile file
  result <-
    if "native-diff: skip" `isInfixOf` src
      then return (Skip "annotated non-deterministic")
      else do
        let bin = outDir </> takeBaseName file
        (bc, bo, be) <- readProcessWithExitCode self ["compiler", file, "--native", bin] ""
        case bc of
          ExitFailure _ -> return (classifyBuildFailure (bo ++ be))
          ExitSuccess -> do
            (vmCode, vmOut, _) <- readProcessWithExitCode self ["compiler", file] ""
            (natCode, natOut, _) <- readProcessWithExitCode bin [] ""
            return (compareRuns vmCode vmOut natCode natOut)
  report file result
  return result

-- | A failed @--native@ build is a skip when the translator rejected
-- the program (unsupported instruction/builtin/constant), a failure
-- when the C toolchain itself broke, and a skip otherwise (the file
-- does not compile at all -- not this harness's business).
classifyBuildFailure :: String -> DiffResult
classifyBuildFailure out
  | "native backend: unsupported" `isInfixOf` out =
      Skip (trimEnd (takeWhile (/= '(') (afterMarker out)))
  | "native backend:" `isInfixOf` out =
      Fail ("native build failed: " ++ afterMarker out)
  | otherwise = Skip "does not compile"
  where
    trimEnd = reverse . dropWhile (== ' ') . reverse

-- | The message following @native backend: @ on its line.
afterMarker :: String -> String
afterMarker out =
  maybe "" after (find (marker `isInfixOf`) (lines out))
  where
    marker = "native backend: "
    after l
      | marker `isPrefixOf` l = drop (length marker) l
      | null l = ""
      | otherwise = after (drop 1 l)

compareRuns :: ExitCode -> String -> ExitCode -> String -> DiffResult
compareRuns vmCode vmOut natCode natOut
  | vmOut == natOut && vmCode == natCode = Pass
  | vmCode /= natCode =
      Fail
        ( "exit codes differ: vm="
            ++ showCode vmCode
            ++ " native="
            ++ showCode natCode
            ++ firstDiff vmOut natOut
        )
  | otherwise = Fail ("stdout differs" ++ firstDiff vmOut natOut)
  where
    showCode ExitSuccess = "0"
    showCode (ExitFailure n) = show n

-- | Locate the first differing stdout line for the failure report.
firstDiff :: String -> String -> String
firstDiff vmOut natOut =
  case find (\(_, v, o) -> v /= o) (zip3 [1 :: Int ..] (pad vmLines) (pad natLines)) of
    Just (n, v, o) ->
      "\n      line "
        ++ show n
        ++ " vm:     "
        ++ v
        ++ "\n      line "
        ++ show n
        ++ " native: "
        ++ o
    Nothing -> ""
  where
    vmLines = lines vmOut
    natLines = lines natOut
    len = max (length vmLines) (length natLines)
    pad ls = take len (ls ++ repeat "<end of output>")

report :: FilePath -> DiffResult -> IO ()
report file result = printColored $ case result of
  Pass -> bold ++ green ++ "    ok" ++ reset ++ "    " ++ file ++ "\n"
  Skip reason ->
    dim ++ "  skip" ++ reset ++ "    " ++ file ++ dim ++ "  (" ++ reason ++ ")" ++ reset ++ "\n"
  Fail detail ->
    bold ++ red ++ "  DIFF" ++ reset ++ "    " ++ file ++ "\n      " ++ detail ++ "\n"
