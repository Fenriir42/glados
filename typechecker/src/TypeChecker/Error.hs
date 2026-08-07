module TypeChecker.Error
  ( TypeCheckError (..),
    tcErrSpan,
    tcErrMessage,
  )
where

import AST.Types.Common (ErrorName (..), FieldName (..), FuncName (..), SourceSpan, TypeName (..), VarName (..), unFieldName, unTypeName)
import AST.Types.Operator (BinaryOp, UnaryOp)
import AST.Types.Type (Type)
import Data.List (intercalate)
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
  | TCMissingStructFields SourceSpan TypeName [FieldName]
  | TCUndefinedInterface SourceSpan TypeName
  | TCMissingInterfaceMethod SourceSpan TypeName TypeName FuncName
  | -- | Enum type name used but not declared
    TCUndefinedEnum SourceSpan TypeName
  | -- | Variant not present in the enum
    TCUnknownEnumVariant SourceSpan TypeName TypeName
  | -- | Concrete type bound to a type parameter does not implement the required interface
    -- | TCBoundViolation span typeParamName concreteType interfaceName
    TCBoundViolation SourceSpan TypeName Type TypeName
  | -- | Data-carrying variant used with bare ExprField instead of ExprEnumVariantInit
    TCEnumVariantRequiresFields SourceSpan TypeName TypeName
  | -- | Unknown field in an enum variant init expression
    TCEnumVariantUnknownField SourceSpan TypeName TypeName FieldName
  | -- | Field referenced in match binding not present in the variant
    TCEnumBindingUnknownField SourceSpan TypeName TypeName FieldName
  | -- | @await@ applied to an expression that is not a @task(T)@
    TCAwaitNonTask SourceSpan Type
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
tcErrSpan (TCMissingStructFields s _ _) = s
tcErrSpan (TCUndefinedInterface s _) = s
tcErrSpan (TCMissingInterfaceMethod s _ _ _) = s
tcErrSpan (TCUndefinedEnum s _) = s
tcErrSpan (TCUnknownEnumVariant s _ _) = s
tcErrSpan (TCBoundViolation s _ _ _) = s
tcErrSpan (TCEnumVariantRequiresFields s _ _) = s
tcErrSpan (TCEnumVariantUnknownField s _ _ _) = s
tcErrSpan (TCEnumBindingUnknownField s _ _ _) = s
tcErrSpan (TCAwaitNonTask s _) = s

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
tcErrMessage (TCMissingStructFields _ tname missing) =
  "struct `"
    ++ T.unpack (unTypeName tname)
    ++ "` init missing fields: "
    ++ intercalate ", " (map (T.unpack . unFieldName) missing)
tcErrMessage (TCUndefinedInterface _ iname) =
  "undefined interface `" ++ T.unpack (unTypeName iname) ++ "`"
tcErrMessage (TCMissingInterfaceMethod _ iname tname mname) =
  "impl of `"
    ++ T.unpack (unTypeName iname)
    ++ "` for `"
    ++ T.unpack (unTypeName tname)
    ++ "` is missing method `"
    ++ T.unpack (unFuncName mname)
    ++ "`"
tcErrMessage (TCUndefinedEnum _ ename) =
  "undefined enum `" ++ T.unpack (unTypeName ename) ++ "`"
tcErrMessage (TCUnknownEnumVariant _ ename vname) =
  "enum `"
    ++ T.unpack (unTypeName ename)
    ++ "` has no variant `"
    ++ T.unpack (unTypeName vname)
    ++ "`"
tcErrMessage (TCBoundViolation _ tv concreteType iface) =
  "type parameter `"
    ++ T.unpack (unTypeName tv)
    ++ "` requires `"
    ++ T.unpack (unTypeName iface)
    ++ "`, but `"
    ++ show concreteType
    ++ "` does not implement it"
tcErrMessage (TCEnumVariantRequiresFields _ ename vname) =
  "enum variant `"
    ++ T.unpack (unTypeName ename)
    ++ "."
    ++ T.unpack (unTypeName vname)
    ++ "` carries fields -- use `{ field: expr }` initialiser syntax"
tcErrMessage (TCEnumVariantUnknownField _ ename vname fname) =
  "enum variant `"
    ++ T.unpack (unTypeName ename)
    ++ "."
    ++ T.unpack (unTypeName vname)
    ++ "` has no field `"
    ++ T.unpack (unFieldName fname)
    ++ "`"
tcErrMessage (TCEnumBindingUnknownField _ ename vname fname) =
  "enum variant `"
    ++ T.unpack (unTypeName ename)
    ++ "."
    ++ T.unpack (unTypeName vname)
    ++ "` has no field `"
    ++ T.unpack (unFieldName fname)
    ++ "` to bind"
tcErrMessage (TCAwaitNonTask _ t) =
  "`await` expects a `task(T)` value, got `" ++ show t ++ "`"
