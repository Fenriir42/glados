module Repl
  ( runRepl,
    ReplConfig (..),
    defaultReplConfig,
  )
where

import AST.Types.AST (Program (..))
import AST.Types.Common (FuncName (..), VarName (..), displaySpan)
import Compiler.Bytecode (Bytecode, bytecodeFunction)
import Compiler.Codegen (compileProgram)
import Compiler.Error (displayError)
import Compiler.Import (resolveImports)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Lib (lexString)
import Parser.Decl (parseDecl)
import System.Console.Haskeline
  ( CompletionFunc,
    InputT,
    completeWord,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
    setComplete,
    simpleCompletion,
  )
import Text.Megaparsec (errorBundlePretty, many, runParser)
import TypeChecker (TypeCheckResult (..), tcAllCallMap, typeCheck)
import TypeChecker.Error (TypeCheckError, tcErrMessage, tcErrSpan)
import VM (runProgram)
import VM.Interpreter (VMError (..))

-- ---------------------------------------------------------------------------
-- Config

data ReplConfig = ReplConfig
  { replPrompt :: String,
    replWelcome :: String,
    replGoodbye :: String,
    replStdlib :: FilePath
  }

defaultReplConfig :: ReplConfig
defaultReplConfig =
  ReplConfig
    { replPrompt = "quant> ",
      replWelcome =
        unlines
          [ bold ++ cyan ++ "Quant REPL" ++ reset ++ ", type " ++ bold ++ ":help" ++ reset ++ " for commands",
            "Define functions with "
              ++ bold
              ++ "fn name(...) -> T { ... }"
              ++ reset
              ++ ", then "
              ++ bold
              ++ ":run"
              ++ reset
              ++ " to execute main()."
          ],
      replGoodbye = "Goodbye!",
      replStdlib = "./std"
    }

-- ---------------------------------------------------------------------------
-- REPL state

-- | Accumulated compiled functions across REPL inputs.
type Env = Map FuncName Bytecode

-- ---------------------------------------------------------------------------
-- Entry point

runRepl :: ReplConfig -> IO ()
runRepl config = do
  putStr (replWelcome config)
  runInputT (setComplete replCompletion defaultSettings) (loop config Map.empty)
  putStrLn (replGoodbye config)

-- ---------------------------------------------------------------------------
-- Input loop

loop :: ReplConfig -> Env -> InputT IO ()
loop config env = do
  mInput <- getMultilineInput (replPrompt config)
  case mInput of
    Nothing -> return ()
    Just input ->
      let trimmed = dropWhile (== ' ') input
       in if null trimmed
            then loop config env
            else do
              mEnv' <- handleInput config env trimmed
              mapM_ (loop config) mEnv'

-- | Collect a complete input, prompting for continuation when braces are unbalanced.
getMultilineInput :: String -> InputT IO (Maybe String)
getMultilineInput prompt = do
  mLine <- getInputLine prompt
  case mLine of
    Nothing -> return Nothing
    Just line -> accumulate (braceBalance line) [line]
  where
    accumulate balance acc
      | balance <= 0 = return $ Just (unlines (reverse acc))
      | otherwise = do
          mLine <- getInputLine "  ...> "
          case mLine of
            Nothing -> return $ Just (unlines (reverse acc))
            Just line -> accumulate (balance + braceBalance line) (line : acc)

-- | Count unclosed '{' minus closed '}', used to detect multi-line blocks.
braceBalance :: String -> Int
braceBalance = foldl (\n c -> if c == '{' then n + 1 else if c == '}' then n - 1 else n) 0

-- ---------------------------------------------------------------------------
-- Command dispatch

-- | Returns Nothing to quit, Just newEnv to continue.
handleInput :: ReplConfig -> Env -> String -> InputT IO (Maybe Env)
handleInput config env input
  | input `elem` [":quit", ":q"] = return Nothing
  | input `elem` [":help", ":h", ":?"] = showHelp >> return (Just env)
  | input `elem` [":reset", ":clear", ":c"] = resetEnv >> return (Just Map.empty)
  | input `elem` [":env", ":e"] = showEnv env >> return (Just env)
  | ":run" `isPrefixOf` input = runMain env >> return (Just env)
  | ":load " `isPrefixOf` input = do
      let path = drop 6 input
      env' <- loadFile config env path
      return (Just env')
  | ":load" == input = do
      outputStrLn "Usage: :load <file>"
      return (Just env)
  | otherwise = do
      env' <- evalInput config env input
      return (Just env')

-- ---------------------------------------------------------------------------
-- Eval

-- | Try to parse as function declaration(s); if that fails, try as a
-- statement wrapped in fn __repl__() -> void { ... }.
evalInput :: ReplConfig -> Env -> String -> InputT IO Env
evalInput config env src = do
  result <- liftIO $ tryCompile config src (Map.elems env)
  case result of
    Left errMsg -> outputStrLn errMsg >> return env
    Right newBcs ->
      let newEnv = foldl (\e bc -> Map.insert (bytecodeFunction bc) bc e) env newBcs
          newNames = [T.unpack (unFuncName (bytecodeFunction bc)) | bc <- newBcs]
       in do
            outputStrLn $ dim ++ "defined: " ++ unwords newNames ++ reset
            -- Auto-run if this defines (or re-defines) main
            when ("main" `elem` map (T.unpack . unFuncName . bytecodeFunction) newBcs) $
              runMain newEnv
            return newEnv

-- | Compile @src@ together with already-compiled @existing@ functions.
tryCompile :: ReplConfig -> String -> [Bytecode] -> IO (Either String [Bytecode])
tryCompile config src existing = do
  case lexString src of
    Left lexErr -> return (Left lexErr)
    Right tokens ->
      case runParser (many parseDecl) "<repl>" tokens of
        Left err -> return (Left (errorBundlePretty err))
        Right rawDecls -> do
          declsOrErr <- resolveImports [replStdlib config] rawDecls
          case declsOrErr of
            Left importErr -> return (Left importErr)
            Right decls -> do
              let tcResult = typeCheck (Program decls)
                  typeErrs = tcErrors tcResult
              case typeErrs of
                errs@(_ : _) -> return (Left (concatMap formatTypeErr errs))
                [] ->
                  return $ case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
                    Left err -> Left (displayError err (lines src))
                    Right bcs -> Right (existing ++ bcs)

-- ---------------------------------------------------------------------------
-- :run

runMain :: Env -> InputT IO ()
runMain env =
  case Map.lookup (FuncName "main") env of
    Nothing -> outputStrLn $ warn "no main() defined, use :load or define fn main() -> void { ... }"
    Just _ -> do
      result <- liftIO $ runProgram (Map.elems env)
      case result of
        Left err -> outputStrLn (prettyVMError err)
        Right _ -> return ()

-- ---------------------------------------------------------------------------
-- :load

loadFile :: ReplConfig -> Env -> FilePath -> InputT IO Env
loadFile config env path = do
  srcOrErr <-
    liftIO $
      (Right <$> readFile path) `catch` \e ->
        return $ Left (show (e :: SomeException))
  case srcOrErr of
    Left err -> outputStrLn (warn ("cannot open " ++ path ++ ": " ++ err)) >> return env
    Right src -> do
      result <- liftIO $ tryCompile config src []
      case result of
        Left errMsg -> outputStrLn errMsg >> return env
        Right bcs -> do
          let newEnv = foldl (\e bc -> Map.insert (bytecodeFunction bc) bc e) env bcs
              newNames = map (T.unpack . unFuncName . bytecodeFunction) bcs
          outputStrLn $
            dim
              ++ "loaded "
              ++ show (length bcs)
              ++ " function(s): "
              ++ unwords newNames
              ++ reset
          return newEnv

-- ---------------------------------------------------------------------------
-- UI helpers

showHelp :: InputT IO ()
showHelp =
  mapM_
    outputStrLn
    [ "",
      bold ++ "Commands:" ++ reset,
      "  " ++ bold ++ ":help" ++ reset ++ "  (:h :?)         show this help",
      "  " ++ bold ++ ":quit" ++ reset ++ "  (:q)            exit the REPL",
      "  " ++ bold ++ ":run" ++ reset ++ "                   execute main()",
      "  " ++ bold ++ ":load" ++ reset ++ " <file>           load and compile a .qa file",
      "  " ++ bold ++ ":reset" ++ reset ++ " (:clear :c)     clear all defined functions",
      "  " ++ bold ++ ":env" ++ reset ++ "   (:e)            show defined functions",
      "",
      bold ++ "Usage:" ++ reset,
      "  Type a complete function definition (multi-line OK).",
      "  If you define main(), it runs automatically.",
      "  Use :run to re-run main() after updating other functions.",
      ""
    ]

resetEnv :: InputT IO ()
resetEnv = outputStrLn $ dim ++ "environment cleared" ++ reset

showEnv :: Env -> InputT IO ()
showEnv env
  | Map.null env = outputStrLn $ dim ++ "(empty)" ++ reset
  | otherwise = do
      outputStrLn $ bold ++ "Defined functions:" ++ reset
      mapM_ (\n -> outputStrLn ("  " ++ T.unpack (unFuncName n))) (Map.keys env)

-- ---------------------------------------------------------------------------
-- VM error display

prettyVMError :: VMError -> String
prettyVMError err =
  bold
    ++ red
    ++ "runtime error"
    ++ reset
    ++ ": "
    ++ vmErrorMsg root
    ++ context
  where
    (root, context) = unwrapContext err

unwrapContext :: VMError -> (VMError, String)
unwrapContext (VMInContext func ip inner) =
  let (root, _) = unwrapContext inner
      ctx =
        "\n  "
          ++ bold
          ++ cyan
          ++ "in"
          ++ reset
          ++ " `"
          ++ T.unpack (unFuncName func)
          ++ "` at instruction "
          ++ show ip
   in (root, ctx)
unwrapContext e = (e, "")

vmErrorMsg :: VMError -> String
vmErrorMsg (VMRuntimeError msg) = msg
vmErrorMsg (VMUndefinedVar v) = "undefined variable `" ++ T.unpack (unVarName v) ++ "`"
vmErrorMsg (VMUndefinedFunction f) = "undefined function `" ++ T.unpack (unFuncName f) ++ "`"
vmErrorMsg (VMStackUnderflow ctx) = "stack underflow in " ++ ctx
vmErrorMsg (VMOutOfBounds i len) = "index " ++ show i ++ " out of bounds (length " ++ show len ++ ")"
vmErrorMsg (VMTypeMismatch msg) = "type mismatch: " ++ msg
vmErrorMsg (VMInContext _ _ inner) = vmErrorMsg inner

-- ---------------------------------------------------------------------------
-- ANSI

esc :: String -> String
esc code = "\ESC[" ++ code ++ "m"

reset, bold, dim, red, cyan :: String
reset = esc "0"
bold = esc "1"
dim = esc "2"
red = esc "31"
cyan = esc "36"

warn :: String -> String
warn msg = bold ++ red ++ "warning" ++ reset ++ ": " ++ msg

-- ---------------------------------------------------------------------------
-- Type error formatting (compact, no source-line context needed in REPL)

formatTypeErr :: TypeCheckError -> String
formatTypeErr e =
  bold
    ++ red
    ++ "error[type]"
    ++ reset
    ++ ": "
    ++ tcErrMessage e
    ++ "\n"
    ++ "  "
    ++ bold
    ++ cyan
    ++ "-->"
    ++ reset
    ++ " "
    ++ T.unpack (displaySpan (tcErrSpan e))
    ++ "\n"

-- ---------------------------------------------------------------------------
-- Tab completion

replCompletion :: CompletionFunc IO
replCompletion = completeWord Nothing " \t" $ \word -> do
  let candidates =
        [ ":help",
          ":quit",
          ":load",
          ":run",
          ":reset",
          ":env",
          ":h",
          ":q",
          ":c",
          ":e",
          "fn",
          "pub",
          "if",
          "else",
          "while",
          "for",
          "return",
          "int",
          "float",
          "bool",
          "str",
          "void",
          "println",
          "print",
          "math.sqrt",
          "math.abs",
          "True",
          "False"
        ]
  return $ map simpleCompletion $ filter (word `isPrefixOf`) candidates
