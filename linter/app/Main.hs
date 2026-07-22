module Main (main) where

import AST.Types.AST (Program (..))
import Control.Monad (when)
import Data.Either (lefts, rights)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib (lexString)
import Lint
  ( LintDiag (..),
    LintOpts (..),
    RuleName,
    Severity (..),
    allRules,
    defaultLintOpts,
    formatDiag,
    lintProgram,
    ruleDesc,
    ruleId,
    ruleSev,
  )
import Options.Applicative
import Parser.Decl (parseDecl)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Text.Megaparsec as MP

-- ---------------------------------------------------------------------------
-- CLI

data Args = Args
  { argFiles :: [FilePath],
    argDeny :: [String],
    argAllow :: [String],
    argPrintRules :: Bool
  }

argsParser :: Parser Args
argsParser =
  Args
    <$> many (argument str (metavar "FILE..."))
    <*> many
      ( strOption
          ( long "deny"
              <> short 'd'
              <> metavar "RULE"
              <> help "Treat RULE as an error instead of a warning"
          )
      )
    <*> many
      ( strOption
          ( long "allow"
              <> short 'a'
              <> metavar "RULE"
              <> help "Suppress RULE entirely"
          )
      )
    <*> switch
      ( long "rules"
          <> help "List all available rules and exit"
      )

-- ---------------------------------------------------------------------------
-- Rule resolution

parseRuleId :: String -> Either String RuleName
parseRuleId s =
  case filter (\r -> ruleId r == s) allRules of
    [r] -> Right r
    _ ->
      Left
        ( "unknown rule `"
            ++ s
            ++ "`. Use --rules to list available rules."
        )

resolveRules :: [String] -> IO (Set RuleName)
resolveRules ids = do
  let results = map parseRuleId ids
      errors = lefts results
      rules = rights results
  mapM_ (hPutStrLn stderr . ("wheatley: " ++)) errors
  if null errors
    then return (Set.fromList rules)
    else exitFailure

-- ---------------------------------------------------------------------------
-- Parsing

parseSource :: FilePath -> String -> Either String (Program ())
parseSource label src =
  case lexString src of
    Left err -> Left ("lex error: " ++ err)
    Right tokens ->
      case runParser (MP.many parseDecl) label tokens of
        Left err -> Left (errorBundlePretty err)
        Right decls -> Right (Program decls)

-- ---------------------------------------------------------------------------
-- Linting

lintFile :: LintOpts -> FilePath -> IO (Bool, [LintDiag])
lintFile opts fp = do
  src <- TIO.readFile fp
  case parseSource fp (T.unpack src) of
    Left err -> do
      hPutStrLn stderr (fp ++ ": " ++ err)
      return (False, [])
    Right prog ->
      return (True, lintProgram opts fp prog)

-- ---------------------------------------------------------------------------
-- Output

printRules :: IO ()
printRules = do
  putStrLn "Available rules:"
  mapM_ printRule allRules
  where
    printRule r =
      putStrLn
        ( "  "
            ++ ruleId r
            ++ " ("
            ++ (if ruleSev r == SevError then "error" else "warning")
            ++ "): "
            ++ ruleDesc r
        )

-- ---------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  args <-
    execParser $
      info
        (argsParser <**> helper)
        ( fullDesc
            <> progDesc "Static linter for Quant (.qa) source files"
            <> header "wheatley - Quant language linter"
        )

  when (argPrintRules args) $ printRules >> exitSuccess

  deny <- resolveRules (argDeny args)
  allow <- resolveRules (argAllow args)
  let opts = defaultLintOpts {lintDeny = deny, lintAllow = allow}

  case argFiles args of
    [] -> do
      hPutStrLn stderr "wheatley: no input files"
      exitFailure
    files -> do
      results <- mapM (lintFile opts) files
      let parseOk = all fst results
          diags =
            sortBy (comparing diagLine)
              . concatMap snd
              $ results
      mapM_ (putStrLn . formatDiag) diags
      let hasErrors = any (\d -> diagSev d == SevError) diags
      if parseOk && not hasErrors
        then exitSuccess
        else exitFailure
