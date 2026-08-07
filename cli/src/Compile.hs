module Compile
  ( resolveStdlib,
    compileSource,
    compileSourceWith,
    execute,
    executeFunction,
    executeFunctionCov,
    executeFunctionLineCov,
    collectStdlibFuncNames,
    displayTypeError,
    displayTypeWarning,
    prettyVMError,
    die,
    orDie,
  )
where

import AST.Types.AST (Program (..))
import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
    VarName (..),
    displaySpan,
    unFuncName,
  )
import qualified Compiler (Bytecode)
import Compiler.Codegen (compileProgram)
import Compiler.Error (displayError)
import Compiler.Import (resolveImports)
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Char (isAlphaNum)
import Data.IORef (IORef)
import Data.List (isPrefixOf, isSuffixOf, partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Display
import Lib (lexFile)
import Parser.Decl (parseDecl)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty, many, runParser)
import TypeChecker (TypeCheckResult (..), tcAllCallMap, tcErrors, typeCheck)
import TypeChecker.Error (TypeCheckError (..), tcErrMessage, tcErrSpan)
import VM (runFunction, runFunctionCov, runFunctionLineCov, runProgram)
import VM.Interpreter (VMError (..))

-- ---------------------------------------------------------------------------
-- Stdlib resolution

resolveStdlib :: Maybe FilePath -> IO FilePath
resolveStdlib (Just dir) = return dir
resolveStdlib Nothing = do
  env <- lookupEnv "QUANT_STDLIB"
  case env of
    Just dir -> return dir
    Nothing -> do
      let systemPath = "/usr/local/share/quant/lib"
      exists <- doesDirectoryExist systemPath
      return (if exists then systemPath else "./std")

-- ---------------------------------------------------------------------------
-- Compilation pipeline

compileSource :: FilePath -> FilePath -> IO [Compiler.Bytecode]
compileSource stdlibDir filePath = do
  tokens <- lexFile filePath >>= orDie "lex error"
  src <- readFile filePath
  rawDecls <- orDie "parse error" $
    case runParser (many parseDecl) filePath tokens of
      Left err -> Left (errorBundlePretty err)
      Right ds -> Right ds
  decls <-
    resolveImports [takeDirectory filePath, stdlibDir] rawDecls
      >>= orDie "import error"
  let tcResult = typeCheck (Program decls)
      typeErrs = tcErrors tcResult
  unless (null typeErrs) $ do
    stdFuncs <- collectStdlibFuncNames stdlibDir
    let (implicitWarns, realErrors) = partition (isImplicitStdlib stdFuncs) typeErrs
    mapM_ (\e -> printWarn (displayTypeWarning stdFuncs e (lines src))) implicitWarns
    unless (null realErrors) $ do
      mapM_ (\e -> printErr (displayTypeError e (lines src))) realErrors
      exitFailure
  case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
    Left err -> printErr (displayError err (lines src)) >> exitFailure
    Right bc -> return bc

execute :: [Compiler.Bytecode] -> IO ()
execute bytecodes = do
  result <- runProgram bytecodes
  case result of
    Left err -> printErr (prettyVMError err) >> exitFailure
    Right _ -> return ()

-- | Like 'compileSource' but accepts extra import-search directories in
-- addition to the file's own directory.  The file's directory is always first.
compileSourceWith :: [FilePath] -> FilePath -> IO [Compiler.Bytecode]
compileSourceWith extraDirs filePath = do
  tokens <- lexFile filePath >>= orDie "lex error"
  src <- readFile filePath
  rawDecls <- orDie "parse error" $
    case runParser (many parseDecl) filePath tokens of
      Left err -> Left (errorBundlePretty err)
      Right ds -> Right ds
  decls <-
    resolveImports (takeDirectory filePath : extraDirs) rawDecls
      >>= orDie "import error"
  let tcResult = typeCheck (Program decls)
      typeErrs = tcErrors tcResult
  unless (null typeErrs) $ do
    stdFuncs <- case extraDirs of
      (stdlib : _) -> collectStdlibFuncNames stdlib
      [] -> return Map.empty
    let (implicitWarns, realErrors) = partition (isImplicitStdlib stdFuncs) typeErrs
    mapM_ (\e -> printWarn (displayTypeWarning stdFuncs e (lines src))) implicitWarns
    unless (null realErrors) $ do
      mapM_ (\e -> printErr (displayTypeError e (lines src))) realErrors
      exitFailure
  case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
    Left err -> printErr (displayError err (lines src)) >> exitFailure
    Right bc -> return bc

-- | Run a named function from compiled bytecode, returning the error message
-- as a string rather than printing and exiting.  Used by the test runner.
executeFunction :: FuncName -> [Compiler.Bytecode] -> IO (Either String ())
executeFunction fname bytecodes = do
  result <- runFunction fname bytecodes
  return $ case result of
    Right _ -> Right ()
    Left err -> Left (prettyVMError err)

-- | Like 'executeFunction' but records called user-functions into @covRef@.
executeFunctionCov :: IORef (Set.Set FuncName) -> FuncName -> [Compiler.Bytecode] -> IO (Either String ())
executeFunctionCov covRef fname bytecodes = do
  result <- runFunctionCov covRef fname bytecodes
  return $ case result of
    Right _ -> Right ()
    Left err -> Left (prettyVMError err)

-- | Like 'executeFunctionCov' but also records per-function hit line numbers
-- via 'ICovMark' instructions and per-branch outcomes via 'ICovBranch'.
executeFunctionLineCov ::
  IORef (Set.Set FuncName) ->
  IORef (Map.Map FuncName (Set.Set Int)) ->
  IORef (Map.Map (FuncName, Int) (Set.Set Bool)) ->
  FuncName ->
  [Compiler.Bytecode] ->
  IO (Either String ())
executeFunctionLineCov covRef lineCovRef branchCovRef fname bytecodes = do
  result <- runFunctionLineCov covRef lineCovRef branchCovRef fname bytecodes
  return $ case result of
    Right _ -> Right ()
    Left err -> Left (prettyVMError err)

-- ---------------------------------------------------------------------------
-- VM error display

prettyVMError :: VMError -> String
prettyVMError err =
  bold ++ red ++ "runtime error" ++ reset ++ ": " ++ vmErrorMsg root ++ "\n" ++ ctx
  where
    (root, ctx) = unwrapContext err

unwrapContext :: VMError -> (VMError, String)
unwrapContext (VMInContext func ip inner) =
  let (root, _) = unwrapContext inner
      ctxLine =
        "  "
          ++ bold
          ++ cyan
          ++ "in"
          ++ reset
          ++ " function `"
          ++ T.unpack (unFuncName func)
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
-- Type error display

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
      caret = replicate (colStart - 1) ' ' ++ bold ++ red ++ replicate caretLen '^' ++ reset
      loc = T.unpack (displaySpan span')
      msg = tcErrMessage err
   in unlines
        [ bold ++ red ++ "error[type]" ++ reset ++ ": " ++ bold ++ msg ++ reset,
          " " ++ bold ++ cyan ++ pad ++ " --> " ++ reset ++ loc,
          " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset,
          " " ++ bold ++ cyan ++ lineStr ++ "  |" ++ reset ++ " " ++ srcLine,
          " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset ++ " " ++ caret
        ]

displayTypeWarning :: Map.Map FuncName String -> TypeCheckError -> [String] -> String
displayTypeWarning stdFuncs err@(TCUndefinedFunc sp fname) sourceLines =
  case Map.lookup fname stdFuncs of
    Nothing -> displayTypeError err sourceLines
    Just modName ->
      let startPos = spanStart sp
          lineNo = unLine (posLine startPos)
          colStart = unColumn (posColumn startPos)
          colEnd = unColumn (posColumn (spanEnd sp))
          lineStr = show lineNo
          pad = replicate (length lineStr) ' '
          srcLine =
            if lineNo >= 1 && lineNo <= length sourceLines
              then sourceLines !! (lineNo - 1)
              else ""
          caretLen = max 1 (if colEnd > colStart then colEnd - colStart else 1)
          caret = replicate (colStart - 1) ' ' ++ bold ++ yellow ++ replicate caretLen '^' ++ reset
          loc = T.unpack (displaySpan sp)
          msg =
            "function `"
              ++ T.unpack (unFuncName fname)
              ++ "` is not explicitly imported from `"
              ++ modName
              ++ "`; add `from "
              ++ modName
              ++ " import "
              ++ T.unpack (unFuncName fname)
              ++ "`"
       in unlines
            [ bold ++ yellow ++ "warning[import]" ++ reset ++ ": " ++ bold ++ msg ++ reset,
              " " ++ bold ++ cyan ++ pad ++ " --> " ++ reset ++ loc,
              " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset,
              " " ++ bold ++ cyan ++ lineStr ++ "  |" ++ reset ++ " " ++ srcLine,
              " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset ++ " " ++ caret
            ]
displayTypeWarning _ err sourceLines = displayTypeError err sourceLines

-- ---------------------------------------------------------------------------
-- Stdlib implicit-import detection

collectStdlibFuncNames :: FilePath -> IO (Map.Map FuncName String)
collectStdlibFuncNames dir = do
  files <- listDirectory dir `catch` ignoreErr []
  let qaFiles =
        [ (dir </> f, dropExtension (takeBaseName f))
          | f <- files,
            ".qa" `isSuffixOf` f
        ]
  entries <- mapM scanFile qaFiles
  return (Map.fromList (concat entries))
  where
    ignoreErr :: a -> SomeException -> IO a
    ignoreErr v _ = return v
    scanFile (fp, modName) = do
      content <- readFile fp `catch` ignoreErr ""
      let funcs =
            [ FuncName (T.pack name)
              | l <- lines content,
                let stripped = dropWhile (== ' ') l,
                "fn " `isPrefixOf` stripped || "static fn " `isPrefixOf` stripped,
                let after =
                      if "static fn " `isPrefixOf` stripped
                        then drop 10 stripped
                        else drop 3 stripped,
                let name = takeWhile (\c -> c == '_' || isAlphaNum c) after,
                not (null name)
            ]
      return [(f, modName) | f <- funcs]

isImplicitStdlib :: Map.Map FuncName String -> TypeCheckError -> Bool
isImplicitStdlib stdFuncs (TCUndefinedFunc _ fname) = Map.member fname stdFuncs
isImplicitStdlib _ _ = False

-- ---------------------------------------------------------------------------
-- Helpers

orDie :: String -> Either String a -> IO a
orDie _ (Right v) = return v
orDie prefix (Left e) = die (prefix ++ ": " ++ e)

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
