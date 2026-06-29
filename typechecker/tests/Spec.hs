module Main (main) where

import AST.Types.AST (Program (..))
import AST.Types.Common (SourceSpan (..), VarName (..), unVarName)
import AST.Types.Type
  ( PrimitiveType (..),
    Type (..),
    defaultFloatType,
    defaultIntType,
  )
import Data.Map (Map)
import qualified Data.Map as Map
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

    it "populates tcVarUseSites for local variables" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "    x: int = 42;",
                "    y: int = x + 1;",
                "    println(x);",
                "}"
              ]
          res = check src
          vus = tcVarUseSites res
          -- All use sites keyed by variable name
          xSites = [sp | (sp, (VarName n, _)) <- Map.toList vus, n == "x"]
          ySites = [sp | (sp, (VarName n, _)) <- Map.toList vus, n == "y"]
      -- x: declaration + 2 uses = 3 entries
      length xSites `shouldBe` 3
      -- y: declaration only = 1 entry
      length ySites `shouldBe` 1

    it "all x use sites point to the same declaration span" $ do
      let src =
            unlines
              [ "fn main() -> void {",
                "    x: int = 10;",
                "    y: int = x + x;",
                "}"
              ]
          res = check src
          vus = tcVarUseSites res
          xDefSpans = [defSp | (_, (VarName n, defSp)) <- Map.toList vus, n == "x"]
      -- All entries for x must share the same declaration span
      length (dedup xDefSpans) `shouldBe` 1

    it "populates tcVarUseSites for function parameters" $ do
      let src = "fn add(a: int, b: int) -> int { return a + b; }"
          res = check src
          vus = tcVarUseSites res
          aCount = length [() | (_, (VarName n, _)) <- Map.toList vus, n == "a"]
          bCount = length [() | (_, (VarName n, _)) <- Map.toList vus, n == "b"]
      -- Each param: 1 declaration + 1 use in body = 2
      aCount `shouldBe` 2
      bCount `shouldBe` 2

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

dedup :: (Eq a) => [a] -> [a]
dedup [] = []
dedup (x : xs) = x : dedup (filter (/= x) xs)
