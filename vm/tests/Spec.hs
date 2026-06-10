{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST.Types.AST (Program (..))
import AST.Types.Common (FuncName (..))
import Compiler.Bytecode (Value (..))
import Compiler.Codegen (compileProgram)
import Lib (lexString)
import Parser.Decl (parseDecl)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, many, runParser)
import VM (runProgram)
import VM.Interpreter (VMError (..))

-- ---------------------------------------------------------------------------
-- Helpers

-- | Compile and run a Quant program, returning main's return value.
run :: String -> IO (Either VMError Value)
run src =
  case lexString src of
    Left err -> return $ Left $ VMRuntimeError ("lex: " ++ err)
    Right tokens ->
      case runParser (many parseDecl) "<test>" tokens of
        Left bundle -> return $ Left $ VMRuntimeError ("parse: " ++ errorBundlePretty bundle)
        Right decls ->
          case compileProgram (Program decls) of
            Left err -> return $ Left $ VMRuntimeError ("compile: " ++ show err)
            Right bcs -> runProgram bcs

-- ---------------------------------------------------------------------------
-- Main

main :: IO ()
main = hspec $ do
  describe "Integer arithmetic" $ do
    it "adds two integers" $
      run "fn main() -> int { return 2 + 3; }" >>= (`shouldBe` Right (VInt 5))

    it "subtracts integers" $
      run "fn main() -> int { return 10 - 4; }" >>= (`shouldBe` Right (VInt 6))

    it "multiplies integers" $
      run "fn main() -> int { return 3 * 7; }" >>= (`shouldBe` Right (VInt 21))

    it "divides integers (truncating)" $
      run "fn main() -> int { return 10 / 3; }" >>= (`shouldBe` Right (VInt 3))

    it "computes modulo" $
      run "fn main() -> int { return 10 % 3; }" >>= (`shouldBe` Right (VInt 1))

    it "negates an integer" $
      run "fn main() -> int { return -(5); }" >>= (`shouldBe` Right (VInt (-5)))

    it "evaluates compound expression" $
      run "fn main() -> int { return 2 + 3 * 4; }" >>= (`shouldBe` Right (VInt 14))

  describe "Float arithmetic" $ do
    it "adds two floats" $
      run "fn main() -> float { return 1.5 + 2.5; }" >>= (`shouldBe` Right (VFloat 4.0))

    it "divides floats" $
      run "fn main() -> float { return 10.0 / 4.0; }" >>= (`shouldBe` Right (VFloat 2.5))

    it "multiplies floats" $
      run "fn main() -> float { return 2.0 * 3.5; }" >>= (`shouldBe` Right (VFloat 7.0))

  describe "Boolean and comparison" $ do
    it "3 > 2 is True" $
      run "fn main() -> bool { return 3 > 2; }" >>= (`shouldBe` Right (VBool True))

    it "2 > 3 is False" $
      run "fn main() -> bool { return 2 > 3; }" >>= (`shouldBe` Right (VBool False))

    it "3 == 3 is True" $
      run "fn main() -> bool { return 3 == 3; }" >>= (`shouldBe` Right (VBool True))

    it "3 != 4 is True" $
      run "fn main() -> bool { return 3 != 4; }" >>= (`shouldBe` Right (VBool True))

    it "3 <= 3 is True" $
      run "fn main() -> bool { return 3 <= 3; }" >>= (`shouldBe` Right (VBool True))

    it "True && False is False" $
      run "fn main() -> bool { return True && False; }" >>= (`shouldBe` Right (VBool False))

    it "True || False is True" $
      run "fn main() -> bool { return True || False; }" >>= (`shouldBe` Right (VBool True))

    it "!True is False" $
      run "fn main() -> bool { return !True; }" >>= (`shouldBe` Right (VBool False))

  describe "Variables" $ do
    it "reads an integer variable" $
      run "fn main() -> int { x: int = 42; return x; }" >>= (`shouldBe` Right (VInt 42))

    it "reassigns a variable" $
      run "fn main() -> int { x: int = 10; x = x + 5; return x; }" >>= (`shouldBe` Right (VInt 15))

    it "chains multiple variable assignments" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  a: int = 3;",
                "  b: int = 4;",
                "  c: int = a * b;",
                "  return c;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 12))

  describe "Control flow — if/else" $ do
    it "takes true branch" $
      run "fn main() -> int { if (True) { return 1; } else { return 2; }; return 0; }"
        >>= (`shouldBe` Right (VInt 1))

    it "takes false branch" $
      run "fn main() -> int { if (False) { return 1; } else { return 2; }; return 0; }"
        >>= (`shouldBe` Right (VInt 2))

    it "evaluates condition expression" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  x: int = 5;",
                "  if (x > 3) { return 1; } else { return 0; };",
                "  return -1;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 1))

  describe "Control flow — while loop" $ do
    it "counts to 5" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  x: int = 0;",
                "  while (x < 5) { x = x + 1; };",
                "  return x;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 5))

    it "skips loop body when condition is false initially" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  x: int = 10;",
                "  while (x < 5) { x = x + 1; };",
                "  return x;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 10))

    it "accumulates a sum" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  i: int = 1;",
                "  s: int = 0;",
                "  while (i <= 10) { s = s + i; i = i + 1; };",
                "  return s;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 55))

  describe "Functions" $ do
    it "calls a simple helper" $ do
      let src =
            unlines
              [ "fn double(n: int) -> int { return n * 2; }",
                "fn main() -> int { return double(7); }"
              ]
      run src >>= (`shouldBe` Right (VInt 14))

    it "calls a function with two arguments" $ do
      let src =
            unlines
              [ "fn add(a: int, b: int) -> int { return a + b; }",
                "fn main() -> int { return add(3, 4); }"
              ]
      run src >>= (`shouldBe` Right (VInt 7))

    it "computes factorial recursively" $ do
      let src =
            unlines
              [ "fn fact(n: int) -> int {",
                "  if (n <= 1) { return 1; };",
                "  return n * fact(n - 1);",
                "}",
                "fn main() -> int { return fact(10); }"
              ]
      run src >>= (`shouldBe` Right (VInt 3628800))

    it "computes fibonacci recursively" $ do
      let src =
            unlines
              [ "fn fib(n: int) -> int {",
                "  if (n <= 1) { return n; };",
                "  return fib(n - 1) + fib(n - 2);",
                "}",
                "fn main() -> int { return fib(10); }"
              ]
      run src >>= (`shouldBe` Right (VInt 55))

    it "returns void" $
      run "fn main() -> void { }" >>= (`shouldBe` Right VUnit)

  describe "Casts" $ do
    it "casts float to int (truncates)" $
      run "fn main() -> int { return int(3.7); }" >>= (`shouldBe` Right (VInt 3))

    it "casts int to float" $
      run "fn main() -> float { return float(5); }" >>= (`shouldBe` Right (VFloat 5.0))

    it "casts bool to int" $
      run "fn main() -> int { return int(True); }" >>= (`shouldBe` Right (VInt 1))

  describe "For loops" $ do
    it "counts to 5 with C-style for" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  s: int = 0;",
                "  for (i: int = 1; i <= 5; i = i + 1) { s = s + i; };",
                "  return s;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 15))

    it "for loop with break exits early" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  steps: int = 0;",
                "  for (i: int = 0; i < 10; i = i + 1) {",
                "    if (i == 5) { break; };",
                "    steps = steps + 1;",
                "  };",
                "  return steps;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 5))

    it "for loop with continue skips iterations" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  s: int = 0;",
                "  for (i: int = 0; i < 10; i = i + 1) {",
                "    if (i % 2 == 0) { continue; };",
                "    s = s + i;",
                "  };",
                "  return s;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 25))

    it "while loop with break" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  x: int = 0;",
                "  while (True) { x = x + 1; if (x == 7) { break; }; };",
                "  return x;",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 7))

  describe "Compound assignment" $ do
    it "+= on a variable" $
      run "fn main() -> int { x: int = 10; x += 5; return x; }"
        >>= (`shouldBe` Right (VInt 15))

    it "-= on a variable" $
      run "fn main() -> int { x: int = 10; x -= 3; return x; }"
        >>= (`shouldBe` Right (VInt 7))

    it "*= on a variable" $
      run "fn main() -> int { x: int = 4; x *= 3; return x; }"
        >>= (`shouldBe` Right (VInt 12))

    it "/= on a variable" $
      run "fn main() -> int { x: int = 20; x /= 4; return x; }"
        >>= (`shouldBe` Right (VInt 5))

    it "+= on an array element" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  arr: [int] = [1, 2, 3];",
                "  arr[1] += 10;",
                "  return arr[1];",
                "}"
              ]
      run src >>= (`shouldBe` Right (VInt 12))

  describe "Error paths" $ do
    it "reports VMUndefinedFunction when main is missing" $ do
      result <- run "fn other() -> int { return 1; }"
      result `shouldBe` Left (VMUndefinedFunction (FuncName "main"))

    it "reports VMOutOfBounds for out-of-range array access" $ do
      let src =
            unlines
              [ "fn main() -> int {",
                "  arr: [int] = [1, 2, 3];",
                "  return arr[5];",
                "}"
              ]
      result <- run src
      case result of
        Left err | isOutOfBounds err -> return ()
        other -> expectationFailure $ "Expected VMOutOfBounds, got: " ++ show other
  where
    isOutOfBounds (VMOutOfBounds _ _) = True
    isOutOfBounds (VMInContext _ _ inner) = isOutOfBounds inner
    isOutOfBounds _ = False
