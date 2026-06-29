module Main where

import AST.Types.AST (Program (..))
import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
    VarName (..),
    displaySpan,
  )
import qualified Compiler (Bytecode, Options (..), options, prologue)
import Compiler.Codegen (compileProgram)
import Compiler.Disasm (disassemble)
import Compiler.Error (displayError)
import Compiler.Import (resolveImports)
import Compiler.Serialize (decodeBytecodes, encodeBytecodes)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Lib (lexFile)
import Options.Applicative
import Parser.Decl (parseDecl)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hIsTerminalDevice, stdout)
import Text.Megaparsec (errorBundlePretty, runParser)
import TypeChecker (TypeCheckResult (..), tcErrors, typeCheck)
import TypeChecker.Error (TypeCheckError, tcErrMessage, tcErrSpan)
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
runCompiler (Compiler.Options mFile dump mOut mLoad stdlibDir) =
  case mLoad of
    Just bcFile -> do
      bs <- BSL.readFile bcFile
      bytecodes <- orDie "Load error" (decodeBytecodes bs)
      execute bytecodes
    Nothing -> do
      filePath <- maybe (die "Specify a source file or --load FILE") return mFile
      bytecodes <- compileSource stdlibDir filePath
      if dump
        then putStr (disassemble bytecodes)
        else case mOut of
          Just outFile -> BSL.writeFile outFile (encodeBytecodes bytecodes)
          Nothing -> execute bytecodes

-- ---------------------------------------------------------------------------
-- Compilation pipeline

compileSource :: FilePath -> FilePath -> IO [Compiler.Bytecode]
compileSource stdlibDir filePath = do
  src <- readFile filePath
  tokens <- lexFile filePath >>= orDie "Lex error"
  rawDecls <- orDie "Parse error" $
    case runParser (many parseDecl) filePath tokens of
      Left err -> Left (errorBundlePretty err)
      Right ds -> Right ds
  decls <- resolveImports [takeDirectory filePath, stdlibDir] rawDecls >>= orDie "Import error"
  let typeErrs = tcErrors (typeCheck (Program decls))
  case typeErrs of
    [] -> return ()
    errs -> do
      mapM_ (\e -> printErr (displayTypeError e (lines src))) errs
      exitFailure
  case compileProgram (Program decls) of
    Left err -> do
      printErr (displayError err (lines src))
      exitFailure
    Right bc -> return bc

-- ---------------------------------------------------------------------------
-- Execution

execute :: [Compiler.Bytecode] -> IO ()
execute bytecodes = do
  result <- runProgram bytecodes
  case result of
    Left err -> printErr (prettyVMError err) >> exitFailure
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

-- | Print an error string, stripping ANSI escape codes when stdout is not a TTY.
printErr :: String -> IO ()
printErr s = do
  isTTY <- hIsTerminalDevice stdout
  putStr (if isTTY then s else stripAnsi s)

-- | Remove ANSI SGR escape sequences from a string.
stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) = stripAnsi (drop 1 (dropWhile (/= 'm') rest))
stripAnsi (c : cs) = c : stripAnsi cs

esc :: String -> String
esc code = "\ESC[" ++ code ++ "m"

reset, bold, red, cyan :: String
reset = esc "0"
bold = esc "1"
red = esc "31"
cyan = esc "36"

-- ---------------------------------------------------------------------------
-- Type error display (mirrors Compiler.Error.displayError style)

displayTypeError :: TypeCheckError -> [String] -> String
displayTypeError err sourceLines =
  let span' = tcErrSpan err
      startPos = spanStart span'
      lineNo = unLine (posLine startPos)
      colStart = unColumn (posColumn startPos)
      colEnd = unColumn (posColumn (spanEnd span'))
      lineStr = show lineNo
      pad = replicate (length lineStr) ' '
      srcLine =
        if lineNo >= 1 && lineNo <= length sourceLines
          then sourceLines !! (lineNo - 1)
          else ""
      caretLen = max 1 (if colEnd > colStart then colEnd - colStart else 1)
      caret =
        replicate (colStart - 1) ' '
          ++ bold
          ++ red
          ++ replicate caretLen '^'
          ++ reset
      loc = T.unpack (displaySpan span')
      msg = tcErrMessage err
   in unlines
        [ bold ++ red ++ "error[type]" ++ reset ++ ": " ++ bold ++ msg ++ reset,
          " " ++ bold ++ cyan ++ pad ++ " --> " ++ reset ++ loc,
          " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset,
          " " ++ bold ++ cyan ++ lineStr ++ "  |" ++ reset ++ " " ++ srcLine,
          " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset ++ " " ++ caret
        ]

-- ---------------------------------------------------------------------------
-- Helpers

orDie :: String -> Either String a -> IO a
orDie _ (Right v) = return v
orDie prefix (Left err) = die (prefix ++ ": " ++ err)

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
