module Main (main) where

import AST.Types.AST (Program (..))
import AST.Types.Common (unVarName)
import AST.Types.Type
  ( PrimitiveType (..),
    Type (..),
    defaultFloatType,
    defaultIntType,
  )
import Lib (lexString)
import Parser.Decl (parseDecl)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, many, runParser)
import TypeChecker (TypeCheckResult (..), typeCheck)
import TypeChecker.Error (TypeCheckError (..))

check :: String -> TypeCheckResult
check src =
  let prog = case lexString src of
        Left err -> error ("Lex error: " ++ err)
        Right ts -> case runParser (many parseDecl) "<test>" ts of
          Left bundle -> error ("Parse error: " ++ errorBundlePretty bundle)
          Right decls -> Program decls
   in typeCheck prog

main :: IO ()
main = hspec $ do
  describe "TypeChecker - valid programs" $ do
    it "accepts an empty program" $
      tcErrors (check "") `shouldBe` []

    it "accepts a well-typed binary function" $ do
      let src =
            unlines
              [ "fn add(a: int, b: int) -> int {",
                "  return a + b;",
                "}"
              ]
      tcErrors (check src) `shouldBe` []

    it "accepts void function with println" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "  x: int = 42;",
                "  println(x);",
                "}"
              ]
      tcErrors (check src) `shouldBe` []

    it "accepts a for loop with correct types" $ do
      let src =
            unlines
              [ "fn sum(n: int) -> int {",
                "  total: int = 0;",
                "  for (i: int = 0; i < n; i = i + 1) {",
                "    total = total + i;",
                "  };",
                "  return total;",
                "}"
              ]
      tcErrors (check src) `shouldBe` []

    it "accepts float arithmetic and return" $ do
      let src =
            unlines
              [ "fn avg(a: float, b: float) -> float {",
                "  return (a + b) / 2.0;",
                "}"
              ]
      tcErrors (check src) `shouldBe` []

    it "populates tcTypes (at least one expression annotated)" $ do
      let res = check "fn f() -> int { return 42; }"
      null (tcTypes res) `shouldBe` False

  describe "TypeChecker - type errors" $ do
    it "reports undefined variable" $ do
      let src = unlines ["fn f() -> int {", "  return x;", "}"]
      let errs = tcErrors (check src)
      length errs `shouldBe` 1
      case errs of
        [TCUndefinedVar _ v] -> unVarName v `shouldBe` "x"
        _ -> expectationFailure ("Expected [TCUndefinedVar], got: " ++ show errs)

    it "reports float literal assigned to int variable" $ do
      let src = unlines ["fn f() -> void {", "  x: int = 3.14;", "}"]
      let errs = tcErrors (check src)
      length errs `shouldBe` 1
      case errs of
        [TCTypeMismatch _ expected actual] -> do
          expected `shouldBe` TypePrimitive (PrimInt defaultIntType)
          actual `shouldBe` TypePrimitive (PrimFloat defaultFloatType)
        _ -> expectationFailure ("Expected [TCTypeMismatch], got: " ++ show errs)

    it "reports wrong argument count" $ do
      let src =
            unlines
              [ "fn add(a: int, b: int) -> int { return a + b; }",
                "fn main() -> void { add(1); }"
              ]
      any isWrongArgCount (tcErrors (check src)) `shouldBe` True

    it "reports return type mismatch (float for int)" $ do
      let src = unlines ["fn f() -> int {", "  return 3.14;", "}"]
      any isReturnMismatch (tcErrors (check src)) `shouldBe` True

    it "reports non-bool condition in if" $ do
      let src = unlines ["fn f() -> void {", "  if (42) { };", "}"]
      any isConditionNotBool (tcErrors (check src)) `shouldBe` True

isWrongArgCount :: TypeCheckError -> Bool
isWrongArgCount (TCWrongArgCount {}) = True
isWrongArgCount _ = False

isReturnMismatch :: TypeCheckError -> Bool
isReturnMismatch (TCReturnMismatch {}) = True
isReturnMismatch _ = False

isConditionNotBool :: TypeCheckError -> Bool
isConditionNotBool (TCConditionNotBool {}) = True
isConditionNotBool _ = False
