module TypeChecker.Env
  ( Env (..),
    emptyEnv,
    lookupVar,
    lookupVarDef,
    lookupFunc,
    lookupStruct,
    lookupError,
    insertVar,
    insertVarWithSpan,
    insertFunc,
    insertStruct,
    insertError,
    withVars,
    setReturnType,
  )
where

import AST.Types.Common (ErrorName, FuncName, SourceSpan, TypeName, VarName)
import AST.Types.Type (ErrorType, FunctionType, QualifiedType, StructType)
import Data.Map (Map)
import qualified Data.Map as Map

data Env = Env
  { envVars :: Map VarName QualifiedType,
    envVarDefs :: Map VarName SourceSpan,
    envFuncs :: Map FuncName FunctionType,
    envStructs :: Map TypeName StructType,
    envErrors :: Map ErrorName ErrorType,
    envReturnType :: Maybe QualifiedType
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Map.empty Map.empty Nothing

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

withVars :: [(VarName, QualifiedType)] -> Env -> Env
withVars pairs env = foldr (\(v, qt) e -> insertVar v qt e) env pairs

setReturnType :: QualifiedType -> Env -> Env
setReturnType qt env = env {envReturnType = Just qt}
