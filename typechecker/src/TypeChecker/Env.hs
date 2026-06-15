module TypeChecker.Env
  ( Env (..),
    emptyEnv,
    lookupVar,
    lookupVarDef,
    lookupFunc,
    insertVar,
    insertVarWithSpan,
    insertFunc,
    withVars,
    setReturnType,
  )
where

import AST.Types.Common (FuncName, SourceSpan, VarName)
import AST.Types.Type (FunctionType, QualifiedType)
import Data.Map (Map)
import qualified Data.Map as Map

data Env = Env
  { envVars :: Map VarName QualifiedType,
    envVarDefs :: Map VarName SourceSpan,
    envFuncs :: Map FuncName FunctionType,
    envReturnType :: Maybe QualifiedType
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Nothing

lookupVar :: VarName -> Env -> Maybe QualifiedType
lookupVar v = Map.lookup v . envVars

lookupVarDef :: VarName -> Env -> Maybe SourceSpan
lookupVarDef v = Map.lookup v . envVarDefs

lookupFunc :: FuncName -> Env -> Maybe FunctionType
lookupFunc f = Map.lookup f . envFuncs

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

withVars :: [(VarName, QualifiedType)] -> Env -> Env
withVars pairs env = foldr (\(v, qt) e -> insertVar v qt e) env pairs

setReturnType :: QualifiedType -> Env -> Env
setReturnType qt env = env {envReturnType = Just qt}
