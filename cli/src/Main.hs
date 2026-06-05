module Main where

import AST.Types.AST (Program (..))
import AST.Types.Common (FuncName (..), VarName (..))
import qualified Compiler (Bytecode, Options (..), options, prologue)
import Compiler.Codegen (compileProgram)
import Compiler.Disasm (disassemble)
import Compiler.Error (displayError)
import Compiler.Serialize (decodeBytecodes, encodeBytecodes)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Lib (lexFile)
import Options.Applicative
import Parser.Decl (parseDecl)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, runParser)
import VM (runProgram)
import VM.Interpreter (VMError (..))

-- ---------------------------------------------------------------------------
-- CLI wiring

commandParser :: Parser Compiler.Options
commandParser =
  hsubparser
    ( command
        "compiler"
        ( info
            Compiler.options
            (progDesc Compiler.prologue <> fullDesc)
        )
    )

opts :: ParserInfo Compiler.Options
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "GLaDOS - Generic Language and Data Operand Syntax"
        <> header "glados cli"
    )

-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = execParser opts >>= runCompiler

runCompiler :: Compiler.Options -> IO ()
runCompiler (Compiler.Options mFile dump mOut mLoad) =
  case mLoad of
    Just bcFile -> do
      bs <- BSL.readFile bcFile
      bytecodes <- orDie "Load error" (decodeBytecodes bs)
      execute bytecodes
    Nothing -> do
      filePath <- maybe (die "Specify a source file or --load FILE") return mFile
      bytecodes <- compileSource filePath
      if dump
        then putStr (disassemble bytecodes)
        else case mOut of
          Just outFile -> BSL.writeFile outFile (encodeBytecodes bytecodes)
          Nothing -> execute bytecodes

-- ---------------------------------------------------------------------------
-- Compilation pipeline

compileSource :: FilePath -> IO [Compiler.Bytecode]
compileSource filePath = do
  src <- readFile filePath
  tokens <- lexFile filePath >>= orDie "Lex error"
  decls <- orDie "Parse error" $
    case runParser (many parseDecl) filePath tokens of
      Left err -> Left (errorBundlePretty err)
      Right ds -> Right ds
  case compileProgram (Program decls) of
    Left err -> do
      putStr (displayError err (lines src))
      exitFailure
    Right bc -> return bc

-- ---------------------------------------------------------------------------
-- Execution

execute :: [Compiler.Bytecode] -> IO ()
execute bytecodes = do
  result <- runProgram bytecodes
  case result of
    Left err -> putStr (prettyVMError err) >> exitFailure
    Right _ -> return ()

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
    ++ "\n"
    ++ context
  where
    (root, context) = unwrapContext err

unwrapContext :: VMError -> (VMError, String)
unwrapContext (VMInContext func ip inner) =
  let (root, _) = unwrapContext inner
      funcStr = T.unpack (unFuncName func)
      ctxLine =
        "  "
          ++ bold
          ++ cyan
          ++ "in"
          ++ reset
          ++ " function `"
          ++ funcStr
          ++ "` at instruction "
          ++ show ip
          ++ "\n"
   in (root, ctxLine)
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

reset, bold, red, cyan :: String
reset = esc "0"
bold = esc "1"
red = esc "31"
cyan = esc "36"

-- ---------------------------------------------------------------------------
-- Helpers

orDie :: String -> Either String a -> IO a
orDie _ (Right v) = return v
orDie prefix (Left err) = die (prefix ++ ": " ++ err)

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
