module TypeChecker.Builtins
  ( builtinReturnType,
    isKnownBuiltin,
  )
where

import AST.Types.Common (FuncName (..))
import AST.Types.Type
  ( PrimitiveType (..),
    Type (..),
    defaultFloatType,
    defaultIntType,
  )
import qualified Data.Text as T

-- | Return type of a known builtin, if statically determinable.
-- Returns Nothing for builtins whose return type depends on their argument
-- (e.g. array.pop returns the element type, which requires generics).
builtinReturnType :: FuncName -> Maybe Type
builtinReturnType (FuncName n)
  | n `elem` printFuncs = voidT
  | n `elem` ["len", "array.len"] = intT
  | n `elem` ["push", "array.push", "sys.exit"] = voidT
  | "math." `T.isPrefixOf` n = floatT
  | "string." `T.isPrefixOf` n = stringT
  | "io." `T.isPrefixOf` n = voidT
  | "sys." `T.isPrefixOf` n = voidT
  | otherwise = Nothing
  where
    printFuncs = ["print", "println", "io.print", "io.println"]
    voidT = Just (TypePrimitive PrimNone)
    intT = Just (TypePrimitive (PrimInt defaultIntType))
    floatT = Just (TypePrimitive (PrimFloat defaultFloatType))
    stringT = Just (TypePrimitive PrimString)

-- | True for any function name that the VM/runtime knows about, including
-- the std-module prefix convention (math.*, string.*, io.*, sys.*, array.*).
isKnownBuiltin :: FuncName -> Bool
isKnownBuiltin (FuncName n) =
  n `elem` standaloneBuiltins
    || any (`T.isPrefixOf` n) modulePrefixes
  where
    standaloneBuiltins = ["print", "println", "len", "push", "pop"]
    modulePrefixes = ["math.", "string.", "io.", "sys.", "array."]
