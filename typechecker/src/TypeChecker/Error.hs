module TypeChecker.Error
  ( TypeCheckError (..),
    tcErrSpan,
    tcErrMessage,
  )
where

import AST.Types.Common (ErrorName (..), FuncName (..), SourceSpan, VarName (..))
import AST.Types.Operator (BinaryOp, UnaryOp)
import AST.Types.Type (Type)
import qualified Data.Text as T

data TypeCheckError
  = TCUndefinedVar SourceSpan VarName
  | TCUndefinedFunc SourceSpan FuncName
  | -- | Expected type, actual type
    TCTypeMismatch SourceSpan Type Type
  | TCBinaryOpMismatch SourceSpan BinaryOp Type Type
  | TCUnaryOpMismatch SourceSpan UnaryOp Type
  | -- | Expected count, actual count
    TCWrongArgCount SourceSpan FuncName Int Int
  | -- | Declared return type, actual type
    TCReturnMismatch SourceSpan Type Type
  | TCIndexNonArray SourceSpan Type
  | TCInvalidCast SourceSpan Type Type
  | TCConditionNotBool SourceSpan Type
  | TCUnknownError SourceSpan ErrorName
  deriving stock (Show, Eq)

tcErrSpan :: TypeCheckError -> SourceSpan
tcErrSpan (TCUndefinedVar s _) = s
tcErrSpan (TCUndefinedFunc s _) = s
tcErrSpan (TCTypeMismatch s _ _) = s
tcErrSpan (TCBinaryOpMismatch s _ _ _) = s
tcErrSpan (TCUnaryOpMismatch s _ _) = s
tcErrSpan (TCWrongArgCount s _ _ _) = s
tcErrSpan (TCReturnMismatch s _ _) = s
tcErrSpan (TCIndexNonArray s _) = s
tcErrSpan (TCInvalidCast s _ _) = s
tcErrSpan (TCConditionNotBool s _) = s
tcErrSpan (TCUnknownError s _) = s

tcErrMessage :: TypeCheckError -> String
tcErrMessage (TCUndefinedVar _ v) =
  "undefined variable `" ++ T.unpack (unVarName v) ++ "`"
tcErrMessage (TCUndefinedFunc _ f) =
  "undefined function `" ++ T.unpack (unFuncName f) ++ "`"
tcErrMessage (TCTypeMismatch _ expected actual) =
  "expected `" ++ show expected ++ "`, got `" ++ show actual ++ "`"
tcErrMessage (TCBinaryOpMismatch _ op l r) =
  "operator `" ++ show op ++ "` cannot be applied to `" ++ show l ++ "` and `" ++ show r ++ "`"
tcErrMessage (TCUnaryOpMismatch _ op t) =
  "operator `" ++ show op ++ "` cannot be applied to `" ++ show t ++ "`"
tcErrMessage (TCWrongArgCount _ f expected actual) =
  "function `"
    ++ T.unpack (unFuncName f)
    ++ "` expects "
    ++ show expected
    ++ " argument(s), got "
    ++ show actual
tcErrMessage (TCReturnMismatch _ declared actual) =
  "return type mismatch: declared `" ++ show declared ++ "`, got `" ++ show actual ++ "`"
tcErrMessage (TCIndexNonArray _ t) =
  "cannot index into non-array type `" ++ show t ++ "`"
tcErrMessage (TCInvalidCast _ from to) =
  "cannot cast `" ++ show from ++ "` to `" ++ show to ++ "`"
tcErrMessage (TCConditionNotBool _ t) =
  "condition must be `bool`, got `" ++ show t ++ "`"
tcErrMessage (TCUnknownError _ e) =
  "unknown error type `" ++ T.unpack (unErrorName e) ++ "`"
