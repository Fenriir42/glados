module TypeChecker
  ( TypeCheckResult (..),
    typeCheck,
    module TypeChecker.Error,
  )
where

import AST.Types.AST (Decl (..), ErrorDecl (..), FFIDecl (..), FFIFuncDecl (..), FunctionDecl (..), Program (..), StructDecl (..), programDecls)
import AST.Types.Common (FuncName, Located (..), SourceSpan, TypeName, VarName, locSpan, unLocated)
import AST.Types.Type (ErrorType (..), FunctionType (..), StructType (..), Type)
import Control.Monad.State (execState)
import Data.Map (Map)
import qualified Data.Map as Map
import TypeChecker.Env (Env, emptyEnv, envFuncs, insertError, insertFunc, insertGenericParams, insertStruct)
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
    tcVarUseSites :: Map SourceSpan (VarName, SourceSpan),
    tcVarDeclSites :: Map SourceSpan (VarName, SourceSpan)
  }

-- | Type-check a parsed program.  Returns all diagnostics, a map from
-- every expression span to its inferred type, and a map from every
-- function-call name span to its resolved (FuncName, FunctionType).
typeCheck :: Program () -> TypeCheckResult
typeCheck prog =
  let decls = programDecls prog
      funcEnv = foldr collectFunc emptyEnv decls
      ffiEnv = foldr collectFFI funcEnv decls
      structEnv = foldr collectStruct ffiEnv decls
      fullEnv = foldr collectError structEnv decls
      finalState = execState (mapM_ (checkDecl fullEnv) decls) initialTCState
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
        (envFuncs fullEnv)
        defSites
        (tcsCallWithArgs finalState)
        (tcsVarUseSites finalState)
        (tcsVarDeclSites finalState)
  where
    collectFunc :: Located (Decl ()) -> Env -> Env
    collectFunc (Located _ (DeclFunction _ fd)) env =
      let fname = unLocated (funcDeclName fd)
          tvs = map unLocated (funcDeclTypeParams fd) :: [TypeName]
          env' = insertFunc fname (mkFuncType fd) env
       in if null tvs then env' else insertGenericParams fname tvs env'
    collectFunc _ env = env

    collectFFI :: Located (Decl ()) -> Env -> Env
    collectFFI (Located _ (DeclFFI fd)) env =
      foldr (\ffd e -> insertFunc (unLocated (ffiFuncName ffd)) (mkFFIFuncType ffd) e) env (ffiFuncs fd)
    collectFFI _ env = env

    mkFFIFuncType :: FFIFuncDecl -> FunctionType
    mkFFIFuncType ffd = FunctionType (ffiFuncParams ffd) (ffiFuncReturnType ffd)

    collectStruct :: Located (Decl ()) -> Env -> Env
    collectStruct (Located _ (DeclStruct _ sd)) env =
      let tname = unLocated (structDeclName sd)
          tvs = map unLocated (structDeclTypeParams sd)
          fields = map unLocated (structDeclFields sd)
       in insertStruct tname (StructType tname tvs fields) env
    collectStruct _ env = env

    collectError :: Located (Decl ()) -> Env -> Env
    collectError (Located _ (DeclError _ ed)) env =
      let ename = unLocated (errorDeclName ed)
          fields = map unLocated (errorDeclFields ed)
       in insertError ename (ErrorType ename fields) env
    collectError _ env = env

    mkFuncType :: FunctionDecl () -> FunctionType
    mkFuncType fd = FunctionType (funcDeclParams fd) (funcDeclReturnType fd)
