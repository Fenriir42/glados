module TypeChecker
  ( TypeCheckResult (..),
    typeCheck,
    module TypeChecker.Error,
  )
where

import AST.Types.AST (Decl (..), FunctionDecl (..), Program (..), programDecls)
import AST.Types.Common (Located (..), SourceSpan, unLocated)
import AST.Types.Type (FunctionType (..), Type)
import Control.Monad.State (execState)
import Data.Map (Map)
import TypeChecker.Env (Env, emptyEnv, insertFunc)
import TypeChecker.Error
import TypeChecker.Infer (TCState (..), checkDecl, initialTCState)

data TypeCheckResult = TypeCheckResult
  { tcErrors :: [TypeCheckError],
    tcTypes :: Map SourceSpan Type
  }

-- | Type-check a parsed program.  Returns all diagnostics and a map from
-- every expression span to its inferred type (for hover support).
typeCheck :: Program () -> TypeCheckResult
typeCheck prog =
  let decls = programDecls prog
      funcEnv = foldr collectFunc emptyEnv decls
      finalState = execState (mapM_ (checkDecl funcEnv) decls) initialTCState
   in TypeCheckResult (tcsErrors finalState) (tcsTypes finalState)
  where
    collectFunc :: Located (Decl ()) -> Env -> Env
    collectFunc (Located _ (DeclFunction _ fd)) env =
      insertFunc (unLocated (funcDeclName fd)) (mkFuncType fd) env
    collectFunc _ env = env

    mkFuncType :: FunctionDecl () -> FunctionType
    mkFuncType fd = FunctionType (funcDeclParams fd) (funcDeclReturnType fd)
