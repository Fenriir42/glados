module TypeChecker
  ( TypeCheckResult (..),
    typeCheck,
    tcAllCallMap,
    module TypeChecker.Error,
  )
where

import AST.Types.AST (Decl (..), EnumDecl (..), EnumVariant (..), ErrorDecl (..), FFIDecl (..), FFIFuncDecl (..), FunctionDecl (..), ImplDecl (..), ImplForDecl (..), InterfaceDecl (..), InterfaceMethodSig (..), Program (..), StructDecl (..), programDecls)
import AST.Types.Common (FuncName (..), Located (..), SourceSpan, TypeName (..), VarName (..), initialPos, locSpan, spanSingle, unLocated)
import AST.Types.Type (Constness (..), EnumType (..), ErrorType (..), FunctionType (..), Parameter (..), PrimitiveType (..), QualifiedType (..), StructType (..), Type (..), paramName, paramType, qualType)
import Control.Monad.State.Strict (execState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import TypeChecker.Env (Env, emptyEnv, envFuncs, envInterfaces, insertEnum, insertError, insertFunc, insertGenericBounds, insertGenericParams, insertIfaceAssocTypes, insertInterface, insertStruct, lookupInterface)
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
    tcVarDeclSites :: Map SourceSpan (VarName, SourceSpan),
    -- | Maps each ExprMethodCall span to the resolved function name (e.g. Vec2.len).
    -- Module-style calls (math.sqrt) are absent, only real method calls appear.
    tcMethodCallMap :: Map SourceSpan FuncName,
    -- | Maps LHS span of an overloaded binary/unary op to the resolved method name.
    tcOpOverloadMap :: Map SourceSpan FuncName,
    -- | Receiver spans of enum-variant field-access expressions (Direction.North).
    -- Codegen checks this set to emit INewError instead of IFieldGet.
    tcEnumVariantSpans :: Set SourceSpan,
    -- | Method calls on bounded type vars that need runtime dispatch (IDynMethodCall).
    -- Maps the method-name span to the unqualified method name.
    tcDynMethodCalls :: Map SourceSpan T.Text
  }

-- | Combined map of all dispatch-through-method spans: explicit method calls
-- and operator overloads.  Pass this to 'compileProgram'.
tcAllCallMap :: TypeCheckResult -> Map SourceSpan FuncName
tcAllCallMap r = Map.union (tcMethodCallMap r) (tcOpOverloadMap r)

-- | Pre-defined operator trait interfaces injected into every program's env.
builtinInterfaces :: Map TypeName [InterfaceMethodSig]
builtinInterfaces = Map.fromList (map mkEntry opTraits)
  where
    opTraits =
      [ (TypeName "Add", FuncName "add"),
        (TypeName "Sub", FuncName "sub"),
        (TypeName "Mul", FuncName "mul"),
        (TypeName "Div", FuncName "div"),
        (TypeName "Rem", FuncName "rem"),
        (TypeName "Eq", FuncName "eq"),
        (TypeName "Ne", FuncName "ne"),
        (TypeName "Lt", FuncName "lt"),
        (TypeName "Gt", FuncName "gt"),
        (TypeName "Le", FuncName "le"),
        (TypeName "Ge", FuncName "ge"),
        (TypeName "Neg", FuncName "neg")
      ]
    mkEntry (tname, fname) = (tname, [dummyMethodSig fname])
    dummyMethodSig fname =
      InterfaceMethodSig
        { ifaceMethodName = Located builtinSpan fname,
          ifaceMethodParams = [],
          ifaceMethodReturnType = Located builtinSpan (QualifiedType Mutable (TypePrimitive PrimNone)),
          ifaceMethodDefault = Nothing
        }
    builtinSpan = spanSingle (initialPos "/builtin")

-- | Type-check a parsed program.  Returns all diagnostics, a map from
-- every expression span to its inferred type, and a map from every
-- function-call name span to its resolved (FuncName, FunctionType).
typeCheck :: Program () -> TypeCheckResult
typeCheck prog =
  let decls = programDecls prog
      baseEnv = emptyEnv {envInterfaces = builtinInterfaces}
      funcEnv = foldr collectFunc baseEnv decls
      ffiEnv = foldr collectFFI funcEnv decls
      structEnv = foldr collectStruct ffiEnv decls
      implEnv = foldr collectImpl structEnv decls
      implForEnv = foldr collectImplFor implEnv decls
      ifaceEnv = collectInterfaces implForEnv decls
      enumEnv = foldr collectEnum ifaceEnv decls
      errorEnv = foldr collectError enumEnv decls
      fullEnv = registerDefaultMethods errorEnv decls
      finalState = execState (mapM_ (checkDecl fullEnv) decls) initialTCState
      defSites =
        Map.fromList $
          [ (unLocated (funcDeclName fd), locSpan (funcDeclName fd))
            | Located _ (DeclFunction _ fd) <- decls
          ]
            ++ [ (unLocated (funcDeclName fd), locSpan (funcDeclName fd))
                 | Located _ (DeclImpl _ idecl) <- decls,
                   Located _ fd <- implMethods idecl
               ]
            ++ [ (unLocated (funcDeclName fd), locSpan (funcDeclName fd))
                 | Located _ (DeclImplFor _ ifdecl) <- decls,
                   Located _ fd <- implForMethods ifdecl
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
        (tcsMethodCallMap finalState)
        (tcsOpOverloadMap finalState)
        (tcsEnumVariantSpans finalState)
        (tcsDynMethodCalls finalState)
  where
    collectFunc :: Located (Decl ()) -> Env -> Env
    collectFunc (Located _ (DeclFunction _ fd)) env =
      let fname = unLocated (funcDeclName fd)
          tvs = map unLocated (funcDeclTypeParams fd) :: [TypeName]
          bounds = funcDeclTypeBounds fd
          env' = insertFunc fname (mkFuncType fd) env
          env'' = if null tvs then env' else insertGenericParams fname tvs env'
       in if null bounds then env'' else insertGenericBounds fname bounds env''
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

    collectImpl :: Located (Decl ()) -> Env -> Env
    collectImpl (Located _ (DeclImpl _ idecl)) env =
      foldr
        ( \(Located _ fd) e ->
            let fname = unLocated (funcDeclName fd)
                tvs = map unLocated (funcDeclTypeParams fd)
                bounds = funcDeclTypeBounds fd
                e' = insertFunc fname (mkFuncType fd) e
                e'' = if null tvs then e' else insertGenericParams fname tvs e'
             in if null bounds then e'' else insertGenericBounds fname bounds e''
        )
        env
        (implMethods idecl)
    collectImpl _ env = env

    collectImplFor :: Located (Decl ()) -> Env -> Env
    collectImplFor (Located _ (DeclImplFor _ ifdecl)) env =
      foldr
        ( \(Located _ fd) e ->
            let fname = unLocated (funcDeclName fd)
                tvs = map unLocated (funcDeclTypeParams fd)
                bounds = funcDeclTypeBounds fd
                e' = insertFunc fname (mkFuncType fd) e
                e'' = if null tvs then e' else insertGenericParams fname tvs e'
             in if null bounds then e'' else insertGenericBounds fname bounds e''
        )
        env
        (implForMethods ifdecl)
    collectImplFor _ env = env

    -- For every `impl Iface for T` that omits a default-bodied method, register
    -- the synthesized `T.method` so completeness checks, call-site inference,
    -- and structural bound checks treat it as provided.  The compiler emits the
    -- matching bytecode from the same default body.
    registerDefaultMethods :: Env -> [Located (Decl ())] -> Env
    registerDefaultMethods = foldr addImpl
      where
        addImpl (Located _ (DeclImplFor _ ifdecl)) e =
          let tname = unLocated (implForTypeName ifdecl)
              sigs = concat (lookupInterface (unLocated (implForIfaceName ifdecl)) e)
           in foldr (addDefault tname) e sigs
        addImpl _ e = e
        addDefault tname sig e =
          let qualName = FuncName (unTypeName tname <> "." <> unFuncName (unLocated (ifaceMethodName sig)))
           in case (ifaceMethodDefault sig, Map.lookup qualName (envFuncs e)) of
                (Just _, Nothing) ->
                  insertFunc qualName (FunctionType (map (fmap (bindSelf tname)) (ifaceMethodParams sig)) (ifaceMethodReturnType sig)) e
                _ -> e
        -- The parser leaves a bare @self@ parameter as an anonymous param of
        -- type @TypeStruct "self"@; bind it to the implementing type.
        bindSelf tname p
          | qualType (paramType p) == TypeStruct (TypeName "self") =
              p {paramName = VarName "self", paramType = QualifiedType Mutable (TypeStruct tname)}
          | otherwise = p

    -- Interfaces are stored with their method lists already flattened through
    -- `extends` chains, so impl-for checking, bound satisfaction, and dynamic
    -- dispatch all see inherited methods without knowing about inheritance.
    collectInterfaces :: Env -> [Located (Decl ())] -> Env
    collectInterfaces env0 ds =
      let raw =
            Map.fromList
              [ ( unLocated (ifaceDeclName idecl),
                  ( map unLocated (ifaceDeclExtends idecl),
                    ifaceDeclMethods idecl,
                    map unLocated (ifaceDeclAssocTypes idecl)
                  )
                )
                | Located _ (DeclInterface _ idecl) <- ds
              ]
          -- Own methods first (a child re-declaration shadows the inherited
          -- one); the visited set breaks extends cycles; unknown parents are
          -- resolved against env0 so builtin operator traits can be extended.
          flatten visited n
            | n `Set.member` visited = []
            | otherwise = case Map.lookup n raw of
                Just (parents, own, _) ->
                  own ++ concatMap (flatten (Set.insert n visited)) parents
                Nothing -> concat (lookupInterface n env0)
          -- Associated types flatten the same way (child + inherited).
          flattenAssoc visited n
            | n `Set.member` visited = []
            | otherwise = case Map.lookup n raw of
                Just (parents, _, assocs) ->
                  assocs ++ concatMap (flattenAssoc (Set.insert n visited)) parents
                Nothing -> []
          dedupByName = go Set.empty
            where
              go _ [] = []
              go seen (sig : rest)
                | nm `Set.member` seen = go seen rest
                | otherwise = sig : go (Set.insert nm seen) rest
                where
                  nm = unLocated (ifaceMethodName sig)
       in Map.foldrWithKey
            ( \n _ e ->
                insertIfaceAssocTypes n (nubOrd (flattenAssoc Set.empty n)) $
                  insertInterface n (dedupByName (flatten Set.empty n)) e
            )
            env0
            raw

    nubOrd :: [TypeName] -> [TypeName]
    nubOrd = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | x `Set.member` seen = go seen xs
          | otherwise = x : go (Set.insert x seen) xs

    collectError :: Located (Decl ()) -> Env -> Env
    collectError (Located _ (DeclError _ ed)) env =
      let ename = unLocated (errorDeclName ed)
          fields = map unLocated (errorDeclFields ed)
       in insertError ename (ErrorType ename fields) env
    collectError _ env = env

    collectEnum :: Located (Decl ()) -> Env -> Env
    collectEnum (Located _ (DeclEnum _ ed)) env =
      let tname = unLocated (enumDeclName ed)
          evs = [unLocated v | v <- enumDeclVariants ed]
          variants = map enumVariantName evs
          variantFields =
            [ (enumVariantName ev, map unLocated (enumVariantFields ev))
              | ev <- evs
            ]
       in insertEnum tname (EnumType tname variants variantFields) env
    collectEnum _ env = env

    mkFuncType :: FunctionDecl () -> FunctionType
    mkFuncType fd
      | funcDeclAsync fd =
          FunctionType
            (funcDeclParams fd)
            (fmap (\qt -> qt {qualType = TypeTask (qualType qt)}) (funcDeclReturnType fd))
      | otherwise = FunctionType (funcDeclParams fd) (funcDeclReturnType fd)
