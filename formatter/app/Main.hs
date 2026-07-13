module Main (main) where

import AST.Types.AST (Program (..))
import Config (FormatOptions (..), defaultOptions, loadConfig)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Formatter (formatProgram)
import Lib (lexString)
import Options.Applicative
import Parser.Decl (parseDecl)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Text.Megaparsec as MP

-- ---------------------------------------------------------------------------
-- CLI arguments

data Emit = EmitStdout | EmitInPlace
  deriving (Show, Eq)

data Args = Args
  { argFiles :: [FilePath],
    argCheck :: Bool,
    argEmit :: Emit,
    argConfigPath :: Maybe FilePath,
    argVerbose :: Bool
  }
  deriving (Show)

argsParser :: Parser Args
argsParser =
  Args
    <$> many (argument str (metavar "FILE..."))
    <*> switch
      ( long "check"
          <> short 'c'
          <> help "Exit non-zero if any file would be reformatted (do not write)"
      )
    <*> flag
      EmitInPlace
      EmitStdout
      ( long "emit"
          <> short 'e'
          <> help "Write formatted output to stdout instead of editing in-place"
      )
    <*> optional
      ( strOption
          ( long "config-path"
              <> short 'C'
              <> metavar "FILE"
              <> help "Path to quant-fmt.toml (default: auto-detect)"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print file names as they are processed"
      )

-- ---------------------------------------------------------------------------
-- Config

findConfig :: FilePath -> IO (Maybe FilePath)
findConfig fp = do
  let candidates =
        [ takeDirectory fp </> "quant-fmt.toml",
          takeDirectory fp </> ".quant-fmt.toml"
        ]
  go candidates
  where
    go [] = return Nothing
    go (c : cs) = do
      exists <- doesFileExist c
      if exists then return (Just c) else go cs

resolveOpts :: Maybe FilePath -> FilePath -> IO FormatOptions
resolveOpts (Just cfgFp) _ = loadConfigSafe cfgFp
resolveOpts Nothing srcFp = do
  mCfg <- findConfig srcFp
  case mCfg of
    Just cfgFp -> loadConfigSafe cfgFp
    Nothing -> return defaultOptions

loadConfigSafe :: FilePath -> IO FormatOptions
loadConfigSafe fp =
  loadConfig fp
    `catch` handler
  where
    handler :: SomeException -> IO FormatOptions
    handler e = do
      hPutStrLn stderr ("Warning: could not read config " ++ fp ++ ": " ++ show e)
      return defaultOptions

-- ---------------------------------------------------------------------------
-- Parsing

parseSource :: FilePath -> Text -> Either String (Program ())
parseSource label src =
  case lexString (T.unpack src) of
    Left err -> Left ("Lex error: " ++ err)
    Right tokens ->
      case runParser (MP.many parseDecl) label tokens of
        Left err -> Left ("Parse error: " ++ errorBundlePretty err)
        Right decls -> Right (Program decls)

-- ---------------------------------------------------------------------------
-- Processing

processFile :: Args -> FilePath -> IO Bool
processFile args fp = do
  opts <- resolveOpts (argConfigPath args) fp
  src <- TIO.readFile fp
  whenVerbose args $ hPutStrLn stderr ("Formatting: " ++ fp)
  runFormat args opts fp src (argEmit args)

processStdin :: Args -> IO Bool
processStdin args = do
  src <- TIO.getContents
  runFormat args defaultOptions "<stdin>" src EmitStdout

runFormat :: Args -> FormatOptions -> FilePath -> Text -> Emit -> IO Bool
runFormat args opts label src emit =
  case parseSource label src of
    Left err -> do
      hPutStrLn stderr (label ++ ": " ++ err)
      return False
    Right prog -> do
      let formatted = formatProgram opts src prog
      if argCheck args
        then
          if formatted == src
            then return True
            else do
              hPutStrLn stderr (label ++ ": would reformat")
              return False
        else do
          case emit of
            EmitStdout -> TIO.putStr formatted
            EmitInPlace -> TIO.writeFile label formatted
          return True

whenVerbose :: Args -> IO () -> IO ()
whenVerbose args = when (argVerbose args)

-- ---------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  args <-
    execParser $
      info
        (argsParser <**> helper)
        (fullDesc <> progDesc "Format Quant (.qa) source files")
  case argFiles args of
    [] -> do
      ok <- processStdin args
      if ok then exitSuccess else exitFailure
    files -> do
      results <- mapM (processFile args) files
      if and results then exitSuccess else exitFailure
