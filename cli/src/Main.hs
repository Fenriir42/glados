module Main where

import Compile (buildNative, compileSource, emitCFile, execute, resolveStdlib)
import qualified Compiler (Options (..), options, prologue)
import Compiler.Disasm (disassemble)
import Compiler.Serialize (decodeBytecodes, encodeBytecodes)
import qualified Data.ByteString.Lazy as BSL
import NativeDiff (runNativeDiff)
import Options.Applicative
import PM
  ( runBuild,
    runClean,
    runDoc,
    runFmt,
    runInit,
    runLint,
    runRun,
    runTest,
  )
import System.Exit (exitFailure)

-- ---------------------------------------------------------------------------
-- Command ADT

data Command
  = CmdCompiler Compiler.Options
  | CmdInit String
  | CmdBuild Bool (Maybe String)
  | CmdRun
  | CmdTest (Maybe FilePath) Bool (Maybe Int) (Maybe FilePath)
  | CmdNativeDiff [FilePath]
  | CmdLint
  | CmdFmt Bool
  | CmdDoc String FilePath
  | CmdClean

-- ---------------------------------------------------------------------------
-- Parsers

commandParser :: Parser Command
commandParser =
  hsubparser $
    cmd
      "compiler"
      (CmdCompiler <$> Compiler.options)
      (Compiler.prologue ++ " (direct)")
      <> cmd
        "init"
        initParser
        "scaffold a new project in directory NAME"
      <> cmd
        "build"
        buildParser
        "compile the project entry point to .build/main.qbc"
      <> cmd
        "run"
        (pure CmdRun)
        "compile and immediately run the project"
      <> cmd
        "test"
        testParser
        "discover and run all *_test.qa files"
      <> cmd
        "native-diff"
        nativeDiffParser
        "run .qa files under both VM and native binary, diff outputs"
      <> cmd
        "lint"
        (pure CmdLint)
        "run wheatley over all project source files"
      <> cmd
        "fmt"
        fmtParser
        "format all project source files with quant-fmt"
      <> cmd
        "doc"
        docParser
        "generate API documentation from doc comments"
      <> cmd
        "clean"
        (pure CmdClean)
        "remove build artefacts (.build/)"
  where
    cmd name parser desc =
      command name (info parser (progDesc desc <> fullDesc))

initParser :: Parser Command
initParser =
  CmdInit <$> argument str (metavar "NAME" <> help "project name and directory")

buildParser :: Parser Command
buildParser =
  CmdBuild
    <$> switch (long "release" <> help "enable release optimisations")
    <*> optional (strOption (long "target" <> metavar "TARGET" <> help "build target: c (native binary via the C backend)"))

testParser :: Parser Command
testParser =
  CmdTest
    <$> optional (argument str (metavar "FILE" <> help "run only this test file"))
    <*> switch (long "cov" <> help "print function coverage report after tests")
    <*> optional (option auto (long "cov-min" <> metavar "PCT" <> help "fail if coverage is below PCT%"))
    <*> optional (strOption (long "cov-out" <> metavar "FILE" <> help "write JSON coverage report to FILE"))

nativeDiffParser :: Parser Command
nativeDiffParser =
  CmdNativeDiff
    <$> many (argument str (metavar "PATH..." <> help "files or directories to diff (default: tests/)"))

fmtParser :: Parser Command
fmtParser =
  CmdFmt
    <$> switch (long "check" <> help "exit 1 if any file would be changed")

docParser :: Parser Command
docParser =
  CmdDoc
    <$> strOption
      ( long "format"
          <> metavar "FMT"
          <> value "html"
          <> showDefault
          <> help "output format: html or md"
      )
    <*> strOption
      ( long "out"
          <> metavar "DIR"
          <> value "docs/api/"
          <> showDefault
          <> help "output directory"
      )

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Quant language toolchain"
        <> header "glados -- compiler, runner, and project manager for Quant"
    )

-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = execParser opts >>= dispatch

dispatch :: Command -> IO ()
dispatch (CmdCompiler o) = runCompiler o
dispatch (CmdInit name) = runInit name
dispatch (CmdBuild rel target) = runBuild rel target
dispatch CmdRun = runRun
dispatch (CmdTest mf cov covMin covOut) = runTest mf cov covMin covOut
dispatch (CmdNativeDiff paths) = runNativeDiff paths
dispatch CmdLint = runLint
dispatch (CmdFmt check) = runFmt check
dispatch (CmdDoc fmt out) = runDoc fmt out
dispatch CmdClean = runClean

-- ---------------------------------------------------------------------------
-- Direct compiler subcommand

runCompiler :: Compiler.Options -> IO ()
runCompiler (Compiler.Options mFile dump mOut mLoad mStdlib mNative mEmitC) =
  case mLoad of
    Just bcFile -> do
      bs <- BSL.readFile bcFile
      bytecodes <- orDie "load error" (decodeBytecodes bs)
      execute bytecodes
    Nothing -> do
      filePath <- maybe (die "specify a source file or --load FILE") return mFile
      stdlibDir <- resolveStdlib mStdlib
      bytecodes <- compileSource stdlibDir filePath
      case (mNative, mEmitC) of
        (Just bin, _) -> buildNative bytecodes bin
        (Nothing, Just cFile) -> emitCFile bytecodes cFile
        (Nothing, Nothing)
          | dump -> putStr (disassemble bytecodes)
          | otherwise -> case mOut of
              Just outFile -> BSL.writeFile outFile (encodeBytecodes bytecodes)
              Nothing -> execute bytecodes

-- ---------------------------------------------------------------------------
-- Helpers

orDie :: String -> Either String a -> IO a
orDie _ (Right v) = return v
orDie prefix (Left e) = die (prefix ++ ": " ++ e)

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
