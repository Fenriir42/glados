module TypeChecker.Env
  ( Env (..),
    emptyEnv,
    lookupVar,
    lookupFunc,
    insertVar,
    insertFunc,
    withVars,
    setReturnType,
  )
where

import AST.Types.Common (FuncName, VarName)
import AST.Types.Type (FunctionType, QualifiedType)
import Data.Map (Map)
import qualified Data.Map as Map

data Env = Env
  { envVars :: Map VarName QualifiedType,
    envFuncs :: Map FuncName FunctionType,
    envReturnType :: Maybe QualifiedType
  }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Nothing

lookupVar :: VarName -> Env -> Maybe QualifiedType
lookupVar v = Map.lookup v . envVars

lookupFunc :: FuncName -> Env -> Maybe FunctionType
lookupFunc f = Map.lookup f . envFuncs

insertVar :: VarName -> QualifiedType -> Env -> Env
insertVar v qt env = env {envVars = Map.insert v qt (envVars env)}

insertFunc :: FuncName -> FunctionType -> Env -> Env
insertFunc f ft env = env {envFuncs = Map.insert f ft (envFuncs env)}

withVars :: [(VarName, QualifiedType)] -> Env -> Env
withVars pairs env = foldr (\(v, qt) e -> insertVar v qt e) env pairs

setReturnType :: QualifiedType -> Env -> Env
setReturnType qt env = env {envReturnType = Just qt}
