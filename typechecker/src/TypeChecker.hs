module TypeChecker
  ( TypeCheckResult (..),
    typeCheck,
    module TypeChecker.Error,
  )
where

import AST.Types.AST (Decl (..), FunctionDecl (..), Program (..), programDecls)
import AST.Types.Common (FuncName, Located (..), SourceSpan, VarName, locSpan, unLocated)
import AST.Types.Type (FunctionType (..), Type)
import Control.Monad.State (execState)
import Data.Map (Map)
import qualified Data.Map as Map
import TypeChecker.Env (Env, emptyEnv, envFuncs, insertFunc)
import TypeChecker.Error
import TypeChecker.Infer (TCState (..), checkDecl, initialTCState)

data TypeCheckResult = TypeCheckResult
  { tcErrors :: [TypeCheckError],
    tcTypes :: Map SourceSpan Type,
    tcCallSites :: Map SourceSpan (FuncName, FunctionType),
    tcBuiltinCallSites :: Map SourceSpan FuncName,
    tcFuncEnv :: Map FuncName FunctionType,
    tcFuncDefSites :: Map FuncName SourceSpan,
    tcCallWithArgs :: Map SourceSpan (FuncName, FunctionType, [SourceSpan]),
    tcVarUseSites :: Map SourceSpan (VarName, SourceSpan)
  }

-- | Type-check a parsed program.  Returns all diagnostics, a map from
-- every expression span to its inferred type, and a map from every
-- function-call name span to its resolved (FuncName, FunctionType).
typeCheck :: Program () -> TypeCheckResult
typeCheck prog =
  let decls = programDecls prog
      funcEnv = foldr collectFunc emptyEnv decls
      finalState = execState (mapM_ (checkDecl funcEnv) decls) initialTCState
      defSites =
        Map.fromList
          [ (unLocated (funcDeclName fd), locSpan (funcDeclName fd))
            | Located _ (DeclFunction _ fd) <- decls
          ]
   in TypeCheckResult
        (tcsErrors finalState)
        (tcsTypes finalState)
        (tcsCallSites finalState)
        (tcsBuiltinCallSites finalState)
        (envFuncs funcEnv)
        defSites
        (tcsCallWithArgs finalState)
        (tcsVarUseSites finalState)
  where
    collectFunc :: Located (Decl ()) -> Env -> Env
    collectFunc (Located _ (DeclFunction _ fd)) env =
      insertFunc (unLocated (funcDeclName fd)) (mkFuncType fd) env
    collectFunc _ env = env

    mkFuncType :: FunctionDecl () -> FunctionType
    mkFuncType fd = FunctionType (funcDeclParams fd) (funcDeclReturnType fd)
