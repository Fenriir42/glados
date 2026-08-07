module TypeChecker.Env
  ( Env (..),
    emptyEnv,
    lookupVar,
    lookupVarDef,
    lookupFunc,
    lookupStruct,
    lookupError,
    lookupEnum,
    lookupInterface,
    lookupGenericParams,
    lookupGenericBounds,
    insertVar,
    insertVarWithSpan,
    insertFunc,
    insertStruct,
    insertError,
    insertEnum,
    insertInterface,
    insertGenericParams,
    insertGenericBounds,
    withVars,
    withTypeVars,
    withGenericBounds,
    setReturnType,
  )
where

import AST.Types.AST (InterfaceMethodSig (..))
import AST.Types.Common (ErrorName, FuncName, SourceSpan, TypeName, VarName)
import AST.Types.Type (EnumType, ErrorType, FunctionType, QualifiedType, StructType)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Env = Env
  { envVars :: Map VarName QualifiedType,
    envVarDefs :: Map VarName SourceSpan,
    envFuncs :: Map FuncName FunctionType,
    envStructs :: Map TypeName StructType,
    envErrors :: Map ErrorName ErrorType,
    -- | Enum name -> descriptor (variant names)
    envEnums :: Map TypeName EnumType,
    envReturnType :: Maybe QualifiedType,
    -- | Type variable names in scope inside a generic function body
    envTypeVars :: Set TypeName,
    -- | Type parameters per generic function (for call-site inference)
    envGenericParams :: Map FuncName [TypeName],
    -- | Interface bounds per generic function: fname -> [(TypeParam, [Interface])]
    envGenericBounds :: Map FuncName [(TypeName, [TypeName])],
    -- | Bounds in scope in the current generic function body
    envCurrentBounds :: Map TypeName [TypeName],
    -- | Interface name -> full method signatures
    envInterfaces :: Map TypeName [InterfaceMethodSig]
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Nothing Set.empty Map.empty Map.empty Map.empty Map.empty

lookupVar :: VarName -> Env -> Maybe QualifiedType
lookupVar v = Map.lookup v . envVars

lookupVarDef :: VarName -> Env -> Maybe SourceSpan
lookupVarDef v = Map.lookup v . envVarDefs

lookupFunc :: FuncName -> Env -> Maybe FunctionType
lookupFunc f = Map.lookup f . envFuncs

lookupStruct :: TypeName -> Env -> Maybe StructType
lookupStruct t = Map.lookup t . envStructs

lookupError :: ErrorName -> Env -> Maybe ErrorType
lookupError e = Map.lookup e . envErrors

lookupEnum :: TypeName -> Env -> Maybe EnumType
lookupEnum t = Map.lookup t . envEnums

insertVar :: VarName -> QualifiedType -> Env -> Env
insertVar v qt env = env {envVars = Map.insert v qt (envVars env)}

insertVarWithSpan :: VarName -> QualifiedType -> SourceSpan -> Env -> Env
insertVarWithSpan v qt sp env =
  env
    { envVars = Map.insert v qt (envVars env),
      envVarDefs = Map.insert v sp (envVarDefs env)
    }

insertFunc :: FuncName -> FunctionType -> Env -> Env
insertFunc f ft env = env {envFuncs = Map.insert f ft (envFuncs env)}

insertStruct :: TypeName -> StructType -> Env -> Env
insertStruct t st env = env {envStructs = Map.insert t st (envStructs env)}

insertError :: ErrorName -> ErrorType -> Env -> Env
insertError e et env = env {envErrors = Map.insert e et (envErrors env)}

insertEnum :: TypeName -> EnumType -> Env -> Env
insertEnum t et env = env {envEnums = Map.insert t et (envEnums env)}

withVars :: [(VarName, QualifiedType)] -> Env -> Env
withVars pairs env = foldr (\(v, qt) e -> insertVar v qt e) env pairs

withTypeVars :: [TypeName] -> Env -> Env
withTypeVars tvs env = env {envTypeVars = Set.fromList tvs}

insertGenericParams :: FuncName -> [TypeName] -> Env -> Env
insertGenericParams f tvs env =
  env {envGenericParams = Map.insert f tvs (envGenericParams env)}

lookupGenericParams :: FuncName -> Env -> [TypeName]
lookupGenericParams f env =
  Map.findWithDefault [] f (envGenericParams env)

insertGenericBounds :: FuncName -> [(TypeName, [TypeName])] -> Env -> Env
insertGenericBounds f bs env =
  env {envGenericBounds = Map.insert f bs (envGenericBounds env)}

lookupGenericBounds :: FuncName -> Env -> [(TypeName, [TypeName])]
lookupGenericBounds f env =
  Map.findWithDefault [] f (envGenericBounds env)

withGenericBounds :: [(TypeName, [TypeName])] -> Env -> Env
withGenericBounds bs env = env {envCurrentBounds = Map.fromList bs}

lookupInterface :: TypeName -> Env -> Maybe [InterfaceMethodSig]
lookupInterface t = Map.lookup t . envInterfaces

insertInterface :: TypeName -> [InterfaceMethodSig] -> Env -> Env
insertInterface t ms env = env {envInterfaces = Map.insert t ms (envInterfaces env)}

setReturnType :: QualifiedType -> Env -> Env
setReturnType qt env = env {envReturnType = Just qt}
