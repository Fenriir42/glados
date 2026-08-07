module Main (main) where

import AST.Types.AST (Program (..))
import Compiler.Codegen (compileProgram)
import Compiler.Import (resolveImports)
import Data.List (isInfixOf, isPrefixOf)
import Lib (lexString)
import Parser.Decl (parseDecl)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, many, runParser)
import TypeChecker (TypeCheckResult (..), tcAllCallMap, tcErrors, typeCheck)
import TypeChecker.Error (TypeCheckError (..))
import VM (runProgram)

-- ---------------------------------------------------------------------------
-- Pipeline helpers

data PipelineResult
  = LexError String
  | ParseError String
  | ImportError String
  | TypeErrors [TypeCheckError]
  | CompileError String
  | RunOk
  | RunError String

runPipeline :: String -> IO PipelineResult
runPipeline src =
  case lexString src of
    Left err -> return $ LexError err
    Right tokens ->
      case runParser (many parseDecl) "<test>" tokens of
        Left bundle -> return $ ParseError (errorBundlePretty bundle)
        Right decls -> do
          let tcResult = typeCheck (Program decls)
              typeErrs = tcErrors tcResult
          if not (null typeErrs)
            then return $ TypeErrors typeErrs
            else case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
              Left err -> return $ CompileError (show err)
              Right bcs -> do
                result <- runProgram bcs
                return $ case result of
                  Left err -> RunError (show err)
                  Right _ -> RunOk

-- | Path to stdlib relative to the cli/ package root (where cabal runs tests).
stdlibPath :: FilePath
stdlibPath = "../std"

-- | Like 'runPipeline' but resolves imports from the standard library first.
runPipelineWithImports :: String -> IO PipelineResult
runPipelineWithImports src =
  case lexString src of
    Left err -> return $ LexError err
    Right tokens ->
      case runParser (many parseDecl) "<test>" tokens of
        Left bundle -> return $ ParseError (errorBundlePretty bundle)
        Right rawDecls -> do
          declsOrErr <- resolveImports [stdlibPath] rawDecls
          case declsOrErr of
            Left importErr -> return $ ImportError importErr
            Right decls -> do
              let tcResult = typeCheck (Program decls)
                  typeErrs = tcErrors tcResult
              if not (null typeErrs)
                then return $ TypeErrors typeErrs
                else case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
                  Left err -> return $ CompileError (show err)
                  Right bcs -> do
                    result <- runProgram bcs
                    return $ case result of
                      Left err -> RunError (show err)
                      Right _ -> RunOk

-- | Run pipeline with a custom in-memory module named @modName@.
-- The module source is written to a temp dir used as the stdlib path.
runPipelineWithModule :: String -> String -> String -> IO PipelineResult
runPipelineWithModule modName modSrc mainSrc =
  withSystemTempDirectory "quant-test" $ \tmpDir -> do
    writeFile (tmpDir ++ "/" ++ modName ++ ".qa") modSrc
    case lexString mainSrc of
      Left err -> return $ LexError err
      Right tokens ->
        case runParser (many parseDecl) "<test>" tokens of
          Left bundle -> return $ ParseError (errorBundlePretty bundle)
          Right rawDecls -> do
            declsOrErr <- resolveImports [tmpDir] rawDecls
            case declsOrErr of
              Left importErr -> return $ ImportError importErr
              Right decls -> do
                let tcResult = typeCheck (Program decls)
                    typeErrs = tcErrors tcResult
                if not (null typeErrs)
                  then return $ TypeErrors typeErrs
                  else case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
                    Left err -> return $ CompileError (show err)
                    Right bcs -> do
                      result <- runProgram bcs
                      return $ case result of
                        Left err -> RunError (show err)
                        Right _ -> RunOk

-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = hspec $ do
  describe "Pipeline , valid programs" $ do
    it "runs a simple main returning void" $ do
      let src = "fn main() -> void { }"
      runPipeline src >>= \r -> r `shouldBe` RunOk

    it "runs integer arithmetic without type errors" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  x: int = 2 + 3;",
                "  println(x);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "runs float arithmetic without type errors" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  f: float = 1.5 + 2.5;",
                "  println(f);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "runs a recursive function" $ do
      let src =
            unlines
              [ "fn fact(n: int) -> int {",
                "  if (n <= 1) { return 1; };",
                "  return n * fact(n - 1);",
                "}",
                "fn main() -> void {",
                "  println(fact(5));",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "passes a well-typed function call" $ do
      let src =
            unlines
              [ "fn add(a: int, b: int) -> int { return a + b; }",
                "fn main() -> void { println(add(3, 4)); }"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

  describe "Pipeline , type errors caught before codegen" $ do
    it "rejects float literal assigned to int variable" $ do
      let src = "fn f() -> void { x: int = 3.14; }"
      result <- runPipeline src
      case result of
        TypeErrors errs -> length errs `shouldBe` 1
        other -> expectationFailure $ "Expected TypeErrors, got: " ++ show other

    it "rejects wrong argument count" $ do
      let src =
            unlines
              [ "fn add(a: int, b: int) -> int { return a + b; }",
                "fn main() -> void { add(1); }"
              ]
      result <- runPipeline src
      case result of
        TypeErrors _ -> return ()
        other -> expectationFailure $ "Expected TypeErrors, got: " ++ show other

    it "rejects return type mismatch" $ do
      let src = "fn f() -> int { return 3.14; }"
      result <- runPipeline src
      case result of
        TypeErrors _ -> return ()
        other -> expectationFailure $ "Expected TypeErrors, got: " ++ show other

    it "rejects undefined variable" $ do
      let src = "fn f() -> int { return undeclared; }"
      result <- runPipeline src
      case result of
        TypeErrors (TCUndefinedVar {} : _) -> return ()
        other -> expectationFailure $ "Expected TCUndefinedVar, got: " ++ show other

    it "rejects non-bool condition in if" $ do
      let src = "fn f() -> void { if (42) { }; }"
      result <- runPipeline src
      case result of
        TypeErrors _ -> return ()
        other -> expectationFailure $ "Expected TypeErrors, got: " ++ show other

  describe "Pipeline , parse errors" $ do
    it "reports parse error for unbalanced braces" $ do
      let src = "fn f() -> void { "
      result <- runPipeline src
      case result of
        ParseError msg -> "unexpected" `isPrefixOf` msg || not (null msg) `shouldBe` True
        other -> expectationFailure $ "Expected ParseError, got: " ++ show other

  describe "Pipeline , type errors shadow codegen" $ do
    it "stops at type errors, never reaches codegen" $ do
      let src =
            unlines
              [ "fn bad() -> void { x: int = 3.14; }",
                "fn main() -> void { bad(); }"
              ]
      result <- runPipeline src
      case result of
        TypeErrors _ -> return ()
        RunOk -> expectationFailure "Should have been rejected by type checker"
        other -> expectationFailure $ "Expected TypeErrors, got: " ++ show other

  describe "Pipeline , imports" $ do
    it "resolves 'import math' and calls math.sqrt" $ do
      let src =
            unlines
              [ "import math",
                "fn main() -> void {",
                "  x: float = math.sqrt(16.0);",
                "  println(x);",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "resolves 'from math import sqrt' as bare name" $ do
      let src =
            unlines
              [ "from math import sqrt",
                "fn main() -> void {",
                "  x: float = sqrt(9.0);",
                "  println(x);",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "resolves 'from math import *' and calls multiple bare names" $ do
      let src =
            unlines
              [ "from math import *",
                "fn main() -> void {",
                "  x: float = sqrt(4.0);",
                "  n: int = abs(-7);",
                "  println(x);",
                "  println(n);",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "resolves 'from math import sqrt, pow' , multiple selective names" $ do
      let src =
            unlines
              [ "from math import sqrt, pow",
                "fn main() -> void {",
                "  x: float = sqrt(25.0);",
                "  y: float = pow(2.0, 8.0);",
                "  println(x);",
                "  println(y);",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "resolves 'from string import len' overriding array builtin" $ do
      let src =
            unlines
              [ "from string import len",
                "fn main() -> void {",
                "  n: int = len(\"hello\");",
                "  println(n);",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "resolves 'from string import to_upper, concat'" $ do
      let src =
            unlines
              [ "from string import to_upper, concat",
                "fn main() -> void {",
                "  s: str = to_upper(\"hello\");",
                "  t: str = concat(s, \"!\");",
                "  println(t);",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "reports ImportError for an unknown module" $ do
      let src = "import nonexistent\nfn main() -> void { }"
      result <- runPipelineWithImports src
      case result of
        ImportError msg -> msg `shouldSatisfy` ("nonexistent" `isInfixOf`)
        other -> expectationFailure $ "Expected ImportError, got: " ++ show other

    it "reports ImportError for a direct import cycle" $ do
      withSystemTempDirectory "quant-cycle" $ \tmpDir -> do
        writeFile (tmpDir ++ "/a.qa") "import b\nfn fa() -> void { }"
        writeFile (tmpDir ++ "/b.qa") "import a\nfn fb() -> void { }"
        let src = "import a\nfn main() -> void { }"
        case lexString src of
          Left err -> expectationFailure $ "LexError: " ++ err
          Right tokens ->
            case runParser (many parseDecl) "<test>" tokens of
              Left bundle -> expectationFailure $ "ParseError: " ++ errorBundlePretty bundle
              Right rawDecls -> do
                result <- resolveImports [tmpDir] rawDecls
                case result of
                  Left msg -> msg `shouldSatisfy` ("cycle" `isInfixOf`)
                  Right _ -> expectationFailure "Expected cycle error, got success"

  describe "Pipeline - string interpolation" $ do
    it "compiles and runs a plain backtick string" $ do
      let src = unlines ["fn main() -> void { println(`hello`); }"]
      runPipeline src >>= (`shouldBe` RunOk)

    it "compiles and runs interpolation with an int variable" $ do
      let src = unlines ["fn main() -> void { n: int = 42; println(`n={n}`); }"]
      runPipeline src >>= (`shouldBe` RunOk)

    it "compiles and runs interpolation with arithmetic" $ do
      let src = unlines ["fn main() -> void { println(`{1 + 2}`); }"]
      runPipeline src >>= (`shouldBe` RunOk)

    it "compiles interpolation returned from a user function" $ do
      let src =
            unlines
              [ "fn label(name: str, val: int) -> str { return `{name}={val}`; }",
                "fn main() -> void { println(label(\"x\", 7)); }"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

  describe "Pipeline - match statement" $ do
    it "match on integer literal" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  n: int = 0;",
                "  match n {",
                "    0 => println(42);",
                "    _ => println(99);",
                "  };",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "match on integer range" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  n: int = 5;",
                "  match n {",
                "    0 => println(0);",
                "    1..9 => println(1);",
                "    _ => println(2);",
                "  };",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "match wildcard catches all" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  n: int = 100;",
                "  match n {",
                "    _ => println(7);",
                "  };",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "match ok branch on orerror success" $ do
      let src =
            unlines
              [ "error Fail { };",
                "fn try_it(x: int) -> orerror(int, Fail) {",
                "  return x * 2;",
                "}",
                "fn main() -> void {",
                "  result: orerror(int, Fail) = try_it(3);",
                "  match result {",
                "    ok(n) => println(n);",
                "    err(Fail e) => println(0);",
                "  };",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "match err branch on orerror error" $ do
      let src =
            unlines
              [ "error Bad { msg: str };",
                "fn fail_me() -> orerror(int, Bad) {",
                "  return error Bad { msg: \"oops\" };",
                "}",
                "fn main() -> void {",
                "  result: orerror(int, Bad) = fail_me();",
                "  match result {",
                "    ok(n) => println(n);",
                "    err(Bad e) => println(e.msg);",
                "  };",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

  describe "Pipeline - first-class functions" $ do
    it "function reference stored in variable and called indirectly" $ do
      let src =
            unlines
              [ "from math import abs",
                "fn main() -> void {",
                "  f: (int) -> int = abs;",
                "  println(f(-7));",
                "}"
              ]
      runPipelineWithImports src >>= (`shouldBe` RunOk)

    it "function passed as argument and called inside callee" $ do
      let src =
            unlines
              [ "fn apply(f: (int) -> int, x: int) -> int { return f(x); }",
                "fn double(n: int) -> int { return n * 2; }",
                "fn main() -> void {",
                "  println(apply(double, 5));",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "lambda expression stored and called" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  triple: (int) -> int = fn(x: int) -> int { return x * 3; };",
                "  println(triple(4));",
                "}",
                ""
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "lambda passed directly as argument" $ do
      let src =
            unlines
              [ "fn apply(f: (int) -> int, x: int) -> int { return f(x); }",
                "fn main() -> void {",
                "  println(apply(fn(x: int) -> int { return x + 10; }, 5));",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

  describe "Pipeline - generics" $ do
    it "identity function works for int" $ do
      let src =
            unlines
              [ "fn identity[T](x: T) -> T { return x; }",
                "fn main() -> void {",
                "  n: int = identity(42);",
                "  println(n);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "identity function works for str" $ do
      let src =
            unlines
              [ "fn identity[T](x: T) -> T { return x; }",
                "fn main() -> void {",
                "  s: str = identity(\"hello\");",
                "  println(s);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "generic pair swap returns correct types" $ do
      let src =
            unlines
              [ "fn first[T, U](a: T, b: U) -> T { return a; }",
                "fn second[T, U](a: T, b: U) -> U { return b; }",
                "fn main() -> void {",
                "  x: int = first(7, \"x\");",
                "  s: str = second(7, \"y\");",
                "  println(x);",
                "  println(s);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "generic higher-order map over array" $ do
      let src =
            unlines
              [ "fn map_arr[T, U](arr: [T], f: (T) -> U, n: int) -> [U] {",
                "  result: [U] = [];",
                "  i: int = 0;",
                "  while (i < n) {",
                "    push(result, f(arr[i]));",
                "    i++;",
                "  };",
                "  return result;",
                "}",
                "fn main() -> void {",
                "  nums: [int] = [1, 2, 3];",
                "  doubled: [int] = map_arr(nums, fn(x: int) -> int { return x * 2; }, 3);",
                "  println(doubled[0]);",
                "  println(doubled[2]);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

    it "type checker accepts generic called with different concrete types" $ do
      let src =
            unlines
              [ "fn wrap[T](x: T) -> T { return x; }",
                "fn main() -> void {",
                "  a: int = wrap(1);",
                "  b: float = wrap(2.0);",
                "  c: bool = wrap(True);",
                "  println(a);",
                "}"
              ]
      runPipeline src >>= (`shouldBe` RunOk)

  describe "Pipeline - pub/static visibility" $ do
    let modSrc =
          unlines
            [ "static fn helper(x: int) -> int { return x * 2; }",
              "fn double(x: int) -> int { return helper(x); }"
            ]

    it "wildcard import does not expose static functions" $ do
      let src = unlines ["from mymod import *", "fn main() -> void { helper(1); }"]
      result <- runPipelineWithModule "mymod" modSrc src
      case result of
        TypeErrors _ -> return ()
        other -> expectationFailure $ "Expected TypeErrors (static hidden), got: " ++ show other

    it "explicit named import of static function is blocked" $ do
      let src = unlines ["from mymod import helper", "fn main() -> void { helper(1); }"]
      result <- runPipelineWithModule "mymod" modSrc src
      case result of
        TypeErrors _ -> return ()
        other -> expectationFailure $ "Expected TypeErrors (static blocked), got: " ++ show other

    it "wildcard import exposes public functions" $ do
      let src = unlines ["from mymod import *", "fn main() -> void { double(3); }"]
      runPipelineWithModule "mymod" modSrc src >>= (`shouldBe` RunOk)

    it "qualified import allows calling public functions" $ do
      let src = unlines ["import mymod", "fn main() -> void { mymod.double(3); }"]
      runPipelineWithModule "mymod" modSrc src >>= (`shouldBe` RunOk)

instance Show PipelineResult where
  show RunOk = "RunOk"
  show (LexError e) = "LexError: " ++ e
  show (ParseError e) = "ParseError: " ++ e
  show (ImportError e) = "ImportError: " ++ e
  show (TypeErrors es) = "TypeErrors[" ++ show (length es) ++ "]"
  show (CompileError e) = "CompileError: " ++ e
  show (RunError e) = "RunError: " ++ e

instance Eq PipelineResult where
  RunOk == RunOk = True
  LexError a == LexError b = a == b
  ParseError a == ParseError b = a == b
  ImportError a == ImportError b = a == b
  CompileError a == CompileError b = a == b
  RunError a == RunError b = a == b
  _ == _ = False
