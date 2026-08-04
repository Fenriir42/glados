module TypeChecker.Infer
  ( TCState (..),
    initialTCState,
    inferExpr,
    checkBlock,
    checkDecl,
  )
where

import AST.Types.AST
  ( Block (..),
    Decl (..),
    Expr (..),
    ForInit (..),
    FunctionDecl (..),
    ImplDecl (..),
    ImplForDecl (..),
    LValue (..),
    MatchArm (..),
    MatchPattern (..),
    Stmt (..),
  )
import AST.Types.Common
  ( ErrorName (..),
    FieldName (..),
    FuncName (..),
    Located (..),
    SourceSpan,
    TypeName (..),
    VarName (..),
    locSpan,
    unErrorName,
    unFieldName,
    unFuncName,
    unLocated,
    unTypeName,
  )
import AST.Types.Literal
  ( ArrayLiteral (..),
    Literal (..),
  )
import AST.Types.Operator
  ( BinaryOp (..),
    UnaryOp (..),
    isArithmeticOp,
    isBitwiseOp,
    isComparisonOp,
    isLogicalOp,
  )
import AST.Types.Type
  ( ArrayType (..),
    Constness (..),
    EnumType (..),
    ErrorField (..),
    ErrorType (..),
    FunctionType (..),
    Parameter (..),
    PrimitiveType (..),
    QualifiedType (..),
    ResultType (..),
    StructField (..),
    StructType (structFields, structTypeParams),
    Type (..),
    defaultFloatType,
    defaultIntType,
    isIntegralType,
    isNumericType,
    paramName,
    paramType,
    paramVariadic,
    qualConstness,
    qualType,
  )
import Control.Monad (foldM, foldM_, forM_, unless, void, when)
import Control.Monad.State.Strict (State, modify)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import TypeChecker.Builtins (builtinReturnType, isKnownBuiltin)
import TypeChecker.Env
  ( Env (..),
    insertVar,
    insertVarWithSpan,
    lookupEnum,
    lookupError,
    lookupFunc,
    lookupGenericParams,
    lookupInterface,
    lookupStruct,
    lookupVar,
    lookupVarDef,
    setReturnType,
    withTypeVars,
  )
import TypeChecker.Error (TypeCheckError (..))

data TCState = TCState
  { tcsErrors :: [TypeCheckError],
    tcsTypes :: Map SourceSpan Type,
    tcsCallSites :: Map SourceSpan (FuncName, FunctionType),
    tcsBuiltinCallSites :: Map SourceSpan FuncName,
    tcsCallWithArgs :: Map SourceSpan (FuncName, FunctionType, [SourceSpan]),
    tcsVarUseSites :: Map SourceSpan (VarName, SourceSpan),
    tcsVarDeclSites :: Map SourceSpan (VarName, SourceSpan),
    tcsMethodCallMap :: Map SourceSpan FuncName,
    -- | Maps LHS-expression span of an overloaded binary/unary op to the
    -- resolved method name (e.g. Vec2.add for `a + b` where a: Vec2).
    tcsOpOverloadMap :: Map SourceSpan FuncName,
    -- | Receiver spans of enum-variant field-access expressions.
    -- Codegen checks this set to emit INewError instead of IFieldGet.
    tcsEnumVariantSpans :: Set SourceSpan
  }

initialTCState :: TCState
initialTCState = TCState [] Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Set.empty

type TC = State TCState

recordError :: TypeCheckError -> TC ()
recordError e = modify $ \s -> s {tcsErrors = tcsErrors s ++ [e]}

recordType :: SourceSpan -> Type -> TC ()
recordType sp t = modify $ \s -> s {tcsTypes = Map.insert sp t (tcsTypes s)}

recordCallSite :: SourceSpan -> FuncName -> FunctionType -> TC ()
recordCallSite sp fname ft =
  modify $ \s -> s {tcsCallSites = Map.insert sp (fname, ft) (tcsCallSites s)}

recordBuiltinCallSite :: SourceSpan -> FuncName -> TC ()
recordBuiltinCallSite sp fname =
  modify $ \s -> s {tcsBuiltinCallSites = Map.insert sp fname (tcsBuiltinCallSites s)}

recordCallWithArgs :: SourceSpan -> FuncName -> FunctionType -> [SourceSpan] -> TC ()
recordCallWithArgs sp fname ft argSpans =
  modify $ \s -> s {tcsCallWithArgs = Map.insert sp (fname, ft, argSpans) (tcsCallWithArgs s)}

recordVarUse :: SourceSpan -> VarName -> SourceSpan -> TC ()
recordVarUse useSp name defSp =
  modify $ \s -> s {tcsVarUseSites = Map.insert useSp (name, defSp) (tcsVarUseSites s)}

-- | Record that a variable was declared at NAME_SPAN with its full STMT_SPAN.
recordVarDecl :: SourceSpan -> VarName -> SourceSpan -> TC ()
recordVarDecl nameSp name stmtSp =
  modify $ \s -> s {tcsVarDeclSites = Map.insert nameSp (name, stmtSp) (tcsVarDeclSites s)}

recordMethodCall :: SourceSpan -> FuncName -> TC ()
recordMethodCall sp fname =
  modify $ \s -> s {tcsMethodCallMap = Map.insert sp fname (tcsMethodCallMap s)}

recordOpOverload :: SourceSpan -> FuncName -> TC ()
recordOpOverload sp fname =
  modify $ \s -> s {tcsOpOverloadMap = Map.insert sp fname (tcsOpOverloadMap s)}

recordEnumVariant :: SourceSpan -> TC ()
recordEnumVariant sp =
  modify $ \s -> s {tcsEnumVariantSpans = Set.insert sp (tcsEnumVariantSpans s)}

-- | Map a binary operator to its overload method name, if any.
opMethodName :: BinaryOp -> Maybe FuncName
opMethodName OpAdd = Just (FuncName "add")
opMethodName OpSub = Just (FuncName "sub")
opMethodName OpMul = Just (FuncName "mul")
opMethodName OpDiv = Just (FuncName "div")
opMethodName OpMod = Just (FuncName "rem")
opMethodName OpEq = Just (FuncName "eq")
opMethodName OpNeq = Just (FuncName "ne")
opMethodName OpLt = Just (FuncName "lt")
opMethodName OpGt = Just (FuncName "gt")
opMethodName OpLte = Just (FuncName "le")
opMethodName OpGte = Just (FuncName "ge")
opMethodName _ = Nothing

-- | Map a unary operator to its overload method name, if any.
unaryMethodName :: UnaryOp -> Maybe FuncName
unaryMethodName OpNeg = Just (FuncName "neg")
unaryMethodName _ = Nothing

-- | Extract the struct name from a type (for overload resolution).
structNameOf :: Type -> Maybe TypeName
structNameOf (TypeStruct n) = Just n
structNameOf (TypeGenericApp n _) = Just n
structNameOf _ = Nothing

-- ---------------------------------------------------------------------------
-- Expression inference

-- | Infer the type of an expression, recording all errors and type annotations
-- as side effects.  Returns Nothing only when the type genuinely cannot be
-- determined (an error has already been recorded).
inferExpr :: Env -> Located (Expr ()) -> TC (Maybe Type)
inferExpr env (Located sp expr) = do
  mType <- go expr
  forM_ mType (recordType sp)
  return mType
  where
    go :: Expr () -> TC (Maybe Type)
    go (ExprLiteral lit) = inferLit lit
    go (ExprVar (Located vspan name)) =
      case lookupVar name env of
        Just qt -> do
          forM_ (lookupVarDef name env) $ \defSp ->
            recordVarUse vspan name defSp
          return (Just (qualType qt))
        Nothing ->
          -- Fallback: a function name used as a first-class value
          case lookupFunc (FuncName (unVarName name)) env of
            Just ft -> return (Just (TypeFunction ft))
            Nothing ->
              -- Fallback: an enum type name used as a namespace (e.g. Direction.North)
              case lookupEnum (TypeName (unVarName name)) env of
                Just _ -> return (Just (TypeStruct (TypeName (unVarName name))))
                Nothing -> recordError (TCUndefinedVar vspan name) >> return Nothing
    go (ExprBinary op lhs rhs) = do
      mL <- inferExpr env lhs
      mR <- inferExpr env rhs
      case mL of
        Just lt
          | Just mname <- opMethodName op,
            Just tname <- structNameOf lt ->
              let qualFname = FuncName (unTypeName tname <> "." <> unFuncName mname)
               in case lookupFunc qualFname env of
                    Just ft -> do
                      recordOpOverload (locSpan lhs) qualFname
                      return (Just (qualType (unLocated (funcReturnType ft))))
                    Nothing -> case (mL, mR) of
                      (Just lt', Just rt) -> checkBinary sp op lt' rt
                      _ -> return Nothing
        _ -> case (mL, mR) of
          (Just lt, Just rt) -> checkBinary sp op lt rt
          _ -> return Nothing
    go (ExprUnary op operand) = do
      mT <- inferExpr env operand
      case mT of
        Just t
          | Just mname <- unaryMethodName op,
            Just tname <- structNameOf t ->
              let qualFname = FuncName (unTypeName tname <> "." <> unFuncName mname)
               in case lookupFunc qualFname env of
                    Just ft -> do
                      recordOpOverload (locSpan operand) qualFname
                      return (Just (qualType (unLocated (funcReturnType ft))))
                    Nothing -> checkUnary sp op t
        Just t -> checkUnary sp op t
        Nothing -> return Nothing
    go (ExprCall (Located nameSpan fname) args) = do
      -- Try indirect call: variable holding a function value
      let vname = VarName (unFuncName fname)
      case lookupVar vname env of
        Just qt | TypeFunction ft <- qualType qt -> do
          mapM_ (inferExpr env) args
          return (Just (qualType (unLocated (funcReturnType ft))))
        _ -> inferCall sp nameSpan fname args
    go (ExprIndex arrExpr idxExpr) = do
      mArrType <- inferExpr env arrExpr
      void $ inferExpr env idxExpr
      case mArrType of
        Just (TypeArray (ArrayType elemQt)) -> return (Just (qualType elemQt))
        Just (TypeDict _ valType) -> return (Just valType)
        Just t -> recordError (TCIndexNonArray (locSpan arrExpr) t) >> return Nothing
        Nothing -> return Nothing
    go (ExprField structExpr locField) = goFieldAccess
      where
        goFieldAccess = do
          mStructType <- inferExpr env structExpr
          case mStructType of
            Just (TypeStruct tname) ->
              case lookupEnum tname env of
                Just et ->
                  let vname = TypeName (unFieldName (unLocated locField))
                   in if vname `elem` enumTypeVariants et
                        then do
                          recordEnumVariant (locSpan structExpr)
                          return (Just (TypeStruct (enumTypeName et)))
                        else do
                          recordError (TCUnknownEnumVariant (locSpan locField) (enumTypeName et) vname)
                          return Nothing
                Nothing ->
                  case lookupStruct tname env of
                    Nothing -> return Nothing
                    Just st ->
                      let fname = unLocated locField
                          mSf = find (\f -> fieldName f == fname) (structFields st)
                       in return (fmap (qualType . fieldType) mSf)
            Just (TypeGenericApp tname typeArgs) ->
              case lookupStruct tname env of
                Nothing -> return Nothing
                Just st ->
                  let fname = unLocated locField
                      mSf = find (\f -> fieldName f == fname) (structFields st)
                      binding = Map.fromList (zip (structTypeParams st) (map qualType typeArgs))
                   in return (fmap (applyBindings binding . qualType . fieldType) mSf)
            Just (TypeNamed tname) ->
              case lookupError (ErrorName (unTypeName tname)) env of
                Just et ->
                  let fname = unLocated locField
                      mEf = find (\f -> errorFieldName f == fname) (errorTypeFields et)
                   in return (fmap errorFieldType mEf)
                Nothing -> return Nothing
            Just (TypeTuple elemTypes) ->
              let fname = unFieldName (unLocated locField)
               in case reads (drop 1 (T.unpack fname)) of
                    [(idx, "")]
                      | idx >= 0 && idx < length elemTypes ->
                          return (Just (qualType (elemTypes !! idx)))
                    _ -> return Nothing
            _ -> return Nothing
    go (ExprTupleInit elems) = do
      mTypes <- mapM (inferExpr env) elems
      let types = [QualifiedType Mutable t | Just t <- mTypes]
      if length types == length elems
        then return (Just (TypeTuple types))
        else return Nothing
    go (ExprMethodCall receiver (Located methodSp methodFuncName) args) = do
      let qualFname = FuncName (receiverPrefix (unLocated receiver) <> unFuncName methodFuncName)
      -- For a plain ExprVar receiver that doesn't name a known var/func/enum, treat
      -- it as a module-namespace prefix (e.g. sys.exit, math.pi) and skip receiver
      -- inference entirely to avoid a spurious "undefined variable" error.  The
      -- qualified call handles known builtins (isKnownBuiltin) and imported functions.
      mReceiverType <- case unLocated receiver of
        ExprVar (Located _ name)
          | Nothing <- lookupVar name env,
            Nothing <- lookupFunc (FuncName (unVarName name)) env,
            Nothing <- lookupEnum (TypeName (unVarName name)) env ->
              return Nothing
        _ -> inferExpr env receiver
      case mStructNameOf mReceiverType of
        Just tname -> do
          -- Real method call: Vec2.len(receiver, args)
          let qualFname' = FuncName (unTypeName tname <> "." <> unFuncName methodFuncName)
          recordMethodCall methodSp qualFname'
          inferCall sp methodSp qualFname' (receiver : args)
        Nothing ->
          -- Module/qualified call fallback: sys.exit, math.sqrt, imported funcs, etc.
          inferCall sp methodSp qualFname args
      where
        mStructNameOf (Just (TypeStruct n)) = Just n
        mStructNameOf (Just (TypeGenericApp n _)) = Just n
        mStructNameOf _ = Nothing
        receiverPrefix (ExprVar (Located _ v)) = unVarName v <> "."
        receiverPrefix (ExprField (Located _ e) (Located _ f)) =
          receiverPrefix e <> unFieldName f <> "."
        receiverPrefix _ = ""
    go (ExprStructInit (Located initSp tname) fieldExprs) = do
      mFieldTypes <- mapM (\(_, e) -> inferExpr env e) fieldExprs
      case lookupStruct tname env of
        Just st -> do
          let provided = [fname | (Located _ fname, _) <- fieldExprs]
              missing = [fieldName f | f <- structFields st, fieldName f `notElem` provided]
          unless (null missing) $ recordError (TCMissingStructFields initSp tname missing)
          let tvs = structTypeParams st
          if null tvs
            then return (Just (TypeStruct tname))
            else do
              let fieldTypePairs =
                    [ (qualType (fieldType sf), act)
                      | ((Located _ fn, _), mAct) <- zip fieldExprs mFieldTypes,
                        sf <- structFields st,
                        fieldName sf == fn,
                        Just act <- [mAct]
                    ]
                  binding = inferTypeVarBindings tvs (map fst fieldTypePairs) (map snd fieldTypePairs)
                  typeArgs =
                    [ QualifiedType Mutable (Map.findWithDefault (TypeVar tv) tv binding)
                      | tv <- tvs
                    ]
              return (Just (TypeGenericApp tname typeArgs))
        Nothing -> return (Just (TypeStruct tname))
    go (ExprArrayInit (Located _ elemType) elems) = do
      mapM_ (inferExpr env) elems
      return (Just (TypeArray (ArrayType (QualifiedType Mutable elemType))))
    go (ExprError (Located _ ename) fieldExprs) = do
      forM_ fieldExprs $ \(_, e) -> void (inferExpr env e)
      case lookupError ename env of
        Nothing -> recordError (TCUnknownError sp ename) >> return Nothing
        Just _ ->
          return (Just (TypeResult (ResultType (TypePrimitive PrimNone) ename)))
    go (ExprTry inner) = do
      mInner <- inferExpr env inner
      case mInner of
        Just (TypeResult (ResultType successType _)) -> return (Just successType)
        Just (TypeOption innerType) -> return (Just innerType)
        other -> return other
    go (ExprMust inner) = do
      mInner <- inferExpr env inner
      case mInner of
        Just (TypeResult (ResultType successType _)) -> return (Just successType)
        Just (TypeOption innerType) -> return (Just innerType)
        other -> return other
    go (ExprSome inner) = do
      mT <- inferExpr env inner
      return (fmap TypeOption mT)
    go ExprNone =
      return (Just (TypeOption (TypePrimitive PrimNone)))
    go (ExprDictLit pairs) = do
      case pairs of
        [] -> return (Just (TypeDict (TypePrimitive PrimNone) (TypePrimitive PrimNone)))
        ((k, v) : rest) -> do
          mKey <- inferExpr env k
          mVal <- inferExpr env v
          mapM_ (\(ke, ve) -> inferExpr env ke >> inferExpr env ve) rest
          case (mKey, mVal) of
            (Just kt, Just vt) -> return (Just (TypeDict kt vt))
            _ -> return Nothing
    go (ExprLambda params retQType body) = do
      let paramEnv =
            foldl
              (\e (Located _ p) -> insertVar (paramName p) (paramType p) e)
              (setReturnType (unLocated retQType) env)
              params
      checkBlock paramEnv body
      return (Just (TypeFunction (FunctionType params retQType)))
    go (ExprParen inner) = inferExpr env inner
    go (ExprCast inner (Located _ castTo)) = do
      mFrom <- inferExpr env inner
      case mFrom of
        Just fromType ->
          if isCastValid fromType castTo
            then return (Just castTo)
            else recordError (TCInvalidCast sp fromType castTo) >> return Nothing
        Nothing -> return (Just castTo)

    inferLit :: Literal (Located (Expr ())) -> TC (Maybe Type)
    inferLit (LitInt _) = return (Just (TypePrimitive (PrimInt defaultIntType)))
    inferLit (LitFloat _) = return (Just (TypePrimitive (PrimFloat defaultFloatType)))
    inferLit (LitString _) = return (Just (TypePrimitive PrimString))
    inferLit (LitBool _) = return (Just (TypePrimitive PrimBool))
    inferLit (LitArray (ArrayLiteral [])) = return Nothing
    inferLit (LitArray (ArrayLiteral (e : rest))) = do
      mT <- inferExpr env e
      mapM_ (inferExpr env) rest
      return $ fmap (TypeArray . ArrayType . QualifiedType Mutable) mT

    inferCall :: SourceSpan -> SourceSpan -> FuncName -> [Located (Expr ())] -> TC (Maybe Type)
    inferCall callSp nameSpan fname args = do
      let argSpans = map locSpan args
      argResults <- mapM (inferExpr env) args
      case lookupFunc fname env of
        Just ft -> do
          let params = funcParams ft
          let tvs = lookupGenericParams fname env
          -- For generic functions, infer type-variable bindings and apply to return type
          let binding =
                if null tvs
                  then Map.empty
                  else
                    inferTypeVarBindings
                      tvs
                      (map (qualType . paramType . unLocated) params)
                      (catMaybes argResults)
          let rawRet = qualType (unLocated (funcReturnType ft))
          let retType = if Map.null binding then rawRet else applyBindings binding rawRet
          recordCallSite nameSpan fname ft
          recordCallWithArgs callSp fname ft argSpans
          let regularParams = filter (not . paramVariadic . unLocated) params
              mVariadicParam = find (paramVariadic . unLocated) params
          case mVariadicParam of
            Nothing -> do
              when (length args /= length params) $
                recordError (TCWrongArgCount sp fname (length params) (length args))
              forM_ (zip3 args argResults (map unLocated params)) $ \(argExpr, mArgType, p) ->
                forM_ mArgType $ \argType ->
                  let expectedType = qualType (paramType p)
                   in unless (typesCompatible argType expectedType) $
                        recordError (TCTypeMismatch (locSpan argExpr) expectedType argType)
            Just (Located _ vp) -> do
              let nRegular = length regularParams
              when (length args < nRegular) $
                recordError (TCWrongArgCount sp fname nRegular (length args))
              forM_ (zip3 args argResults (map unLocated regularParams)) $ \(argExpr, mArgType, p) ->
                forM_ mArgType $ \argType ->
                  let expectedType = qualType (paramType p)
                   in unless (typesCompatible argType expectedType) $
                        recordError (TCTypeMismatch (locSpan argExpr) expectedType argType)
              let elemType = case qualType (paramType vp) of
                    TypeArray (ArrayType qt) -> qualType qt
                    t -> t
              forM_ (zip (drop nRegular args) (drop nRegular argResults)) $ \(argExpr, mArgType) ->
                forM_ mArgType $ \argType ->
                  unless (typesCompatible argType elemType) $
                    recordError (TCTypeMismatch (locSpan argExpr) elemType argType)
          return (Just retType)
        Nothing ->
          if isKnownBuiltin fname
            then do
              recordBuiltinCallSite nameSpan fname
              return (builtinReturnType fname)
            else do
              recordError (TCUndefinedFunc nameSpan fname)
              return Nothing

-- ---------------------------------------------------------------------------
-- Statement / block checking

checkBlock :: Env -> Block () -> TC ()
checkBlock env (Block _ stmts) = foldM_ checkStmt env stmts

-- | Type-check a statement and return the (possibly extended) environment.
-- New variables declared in this statement are visible in subsequent ones.
checkStmt :: Env -> Located (Stmt ()) -> TC Env
checkStmt env (Located stmtSpan stmt) = case stmt of
  StmtVarDecl (Located declSpan name) (Located _ qt) mInit -> do
    forM_ mInit $ \initExpr -> do
      mT <- inferExpr env initExpr
      forM_ mT $ \t ->
        unless (typesCompatible t (qualType qt)) $
          recordError (TCTypeMismatch (locSpan initExpr) (qualType qt) t)
    recordVarDecl declSpan name stmtSpan
    recordVarUse declSpan name declSpan
    return (insertVarWithSpan name qt declSpan env)
  StmtAssign lvalue _ rhs -> do
    let lvType = lvalueType env lvalue
    mRhsType <- inferExpr env rhs
    case (lvType, mRhsType) of
      (Just lt, Just rt) ->
        unless (typesCompatible lt rt) $
          recordError (TCTypeMismatch (locSpan rhs) lt rt)
      _ -> return ()
    return env
  StmtExpr exprStmt -> do
    void $ inferExpr env exprStmt
    return env
  StmtIf cond thenBlock mElse -> do
    mCondType <- inferExpr env cond
    forM_ mCondType $ \t ->
      unless (t == TypePrimitive PrimBool) $
        recordError (TCConditionNotBool (locSpan cond) t)
    checkBlock env thenBlock
    forM_ mElse (checkBlock env)
    return env
  StmtWhile cond body -> do
    mCondType <- inferExpr env cond
    forM_ mCondType $ \t ->
      unless (t == TypePrimitive PrimBool) $
        recordError (TCConditionNotBool (locSpan cond) t)
    checkBlock env body
    return env
  StmtFor mInit mCond mStep body -> do
    env' <- case mInit of
      Nothing -> return env
      Just (ForInitDecl (Located declSpan name) (Located _ qt) initExpr) -> do
        mT <- inferExpr env initExpr
        forM_ mT $ \t ->
          unless (typesCompatible t (qualType qt)) $
            recordError (TCTypeMismatch (locSpan initExpr) (qualType qt) t)
        recordVarUse declSpan name declSpan
        return (insertVarWithSpan name qt declSpan env)
      Just (ForInitExpr exprStmt) -> do
        void $ inferExpr env exprStmt
        return env
    forM_ mCond $ \cond -> do
      mT <- inferExpr env' cond
      forM_ mT $ \t ->
        unless (t == TypePrimitive PrimBool) $
          recordError (TCConditionNotBool (locSpan cond) t)
    forM_ mStep $ \step -> void $ checkStmt env' step
    checkBlock env' body
    return env
  StmtReturn mExpr -> do
    forM_ (envReturnType env) $ \retQt ->
      case mExpr of
        Nothing ->
          unless (qualType retQt == TypePrimitive PrimNone) $
            recordError (TCReturnMismatch stmtSpan (qualType retQt) (TypePrimitive PrimNone))
        Just expr -> do
          mT <- inferExpr env expr
          forM_ mT $ \t ->
            unless (typesCompatible t (qualType retQt)) $
              recordError (TCReturnMismatch (locSpan expr) (qualType retQt) t)
    return env
  StmtBreak -> return env
  StmtContinue -> return env
  StmtBlock block -> checkBlock env block >> return env
  StmtMatch subj arms -> do
    mSubjType <- inferExpr env subj
    mapM_ (checkMatchArm env mSubjType) arms
    return env
  StmtTupleDecl vars (Located _ tupleQt) initExpr -> do
    mT <- inferExpr env initExpr
    let elemTypes = case mT of
          Just (TypeTuple ts) -> ts
          _ -> case qualType tupleQt of
            TypeTuple ts -> ts
            _ -> []
    let pairs = zip vars (elemTypes ++ repeat (QualifiedType Mutable (TypePrimitive PrimNone)))
    foldM
      ( \e (Located vsp v, qt) -> do
          recordVarDecl vsp v stmtSpan
          recordVarUse vsp v vsp
          recordType vsp (qualType qt)
          return (insertVarWithSpan v qt vsp e)
      )
      env
      pairs
  StmtStructDecl fields (Located _ structQt) initExpr -> do
    mT <- inferExpr env initExpr
    let tname = case mT of
          Just (TypeStruct n) -> Just n
          Just (TypeGenericApp n _) -> Just n
          _ -> case qualType structQt of
            TypeStruct n -> Just n
            TypeGenericApp n _ -> Just n
            _ -> Nothing
    let fieldTypeOf fn = case tname >>= (`lookupStruct` env) of
          Just st ->
            case find (\f -> fieldName f == fn) (structFields st) of
              Just sf -> fieldType sf
              Nothing -> QualifiedType Mutable (TypePrimitive PrimNone)
          Nothing -> QualifiedType Mutable (TypePrimitive PrimNone)
    foldM
      ( \e (Located fsp fn) -> do
          let qt = fieldTypeOf fn
              v = VarName (unFieldName fn)
          recordVarDecl fsp v stmtSpan
          recordVarUse fsp v fsp
          recordType fsp (qualType qt)
          return (insertVarWithSpan v qt fsp e)
      )
      env
      fields

-- ---------------------------------------------------------------------------
-- Declaration checking

checkDecl :: Env -> Located (Decl ()) -> TC ()
checkDecl env (Located declSpan decl) = case decl of
  DeclFunction _ fd -> checkFunction env fd
  DeclImpl _ idecl -> mapM_ (checkFunction env . unLocated) (implMethods idecl)
  DeclImplFor _ ifdecl -> do
    let ifaceName = unLocated (implForIfaceName ifdecl)
        typeName = unLocated (implForTypeName ifdecl)
    case lookupInterface ifaceName env of
      Nothing -> recordError (TCUndefinedInterface declSpan ifaceName)
      Just requiredMethods ->
        forM_ requiredMethods $ \mname -> do
          let qualName = FuncName (unTypeName typeName <> "." <> unFuncName mname)
          case lookupFunc qualName env of
            Nothing -> recordError (TCMissingInterfaceMethod declSpan ifaceName typeName mname)
            Just _ -> return ()
    mapM_ (checkFunction env . unLocated) (implForMethods ifdecl)
  _ -> return ()

checkFunction :: Env -> FunctionDecl () -> TC ()
checkFunction baseEnv fd = do
  let retQt = unLocated (funcDeclReturnType fd)
  let params = funcDeclParams fd
  let tvs = map unLocated (funcDeclTypeParams fd)
  let env =
        foldr
          (\(Located psp p) e -> insertVarWithSpan (paramName p) (paramType p) psp e)
          (setReturnType retQt (withTypeVars tvs baseEnv))
          params
  mapM_ (\(Located psp p) -> recordVarUse psp (paramName p) psp) params
  checkBlock env (funcDeclBody fd)

-- ---------------------------------------------------------------------------
-- Match arm checking

checkMatchArm :: Env -> Maybe Type -> MatchArm () -> TC ()
checkMatchArm env mSubjType (MatchArm pat body) = do
  armEnv <- case pat of
    MatchOk (Located vsp v) -> do
      let innerQt = case mSubjType of
            Just (TypeResult (ResultType t _)) -> QualifiedType Mutable t
            Just t -> QualifiedType Mutable t
            Nothing -> QualifiedType Mutable (TypePrimitive PrimNone)
      recordVarDecl vsp v vsp
      recordVarUse vsp v vsp
      recordType vsp (qualType innerQt)
      return (insertVarWithSpan v innerQt vsp env)
    MatchErr (Located _ ename) (Located vsp v) -> do
      let errQt = QualifiedType Mutable (TypeNamed (TypeName (unErrorName ename)))
      recordVarDecl vsp v vsp
      recordVarUse vsp v vsp
      recordType vsp (qualType errQt)
      return (insertVarWithSpan v errQt vsp env)
    MatchSome (Located vsp v) -> do
      let innerQt = case mSubjType of
            Just (TypeOption t) -> QualifiedType Mutable t
            Just t -> QualifiedType Mutable t
            Nothing -> QualifiedType Mutable (TypePrimitive PrimNone)
      recordVarDecl vsp v vsp
      recordVarUse vsp v vsp
      recordType vsp (qualType innerQt)
      return (insertVarWithSpan v innerQt vsp env)
    MatchNone -> return env
    MatchLit e -> void (inferExpr env e) >> return env
    MatchRange lo hi -> do
      void (inferExpr env lo)
      void (inferExpr env hi)
      return env
    MatchWildcard -> return env
    MatchTuple vars -> do
      let elemTypes = case mSubjType of
            Just (TypeTuple ts) -> ts
            _ -> replicate (length vars) (QualifiedType Mutable (TypePrimitive PrimNone))
          pairs = zip vars (elemTypes ++ repeat (QualifiedType Mutable (TypePrimitive PrimNone)))
      foldM
        ( \e (Located vsp v, qt) -> do
            recordVarDecl vsp v vsp
            recordVarUse vsp v vsp
            recordType vsp (qualType qt)
            return (insertVarWithSpan v qt vsp e)
        )
        env
        pairs
    MatchEnumVariant (Located _ ename) (Located vsp _) -> do
      recordType vsp (TypeStruct ename)
      return env
    MatchStruct fields -> do
      let tname = case mSubjType of
            Just (TypeStruct n) -> Just n
            Just (TypeGenericApp n _) -> Just n
            _ -> Nothing
          fieldTypeOf fn = case tname >>= (`lookupStruct` env) of
            Just st ->
              case find (\f -> fieldName f == fn) (structFields st) of
                Just sf -> fieldType sf
                Nothing -> QualifiedType Mutable (TypePrimitive PrimNone)
            Nothing -> QualifiedType Mutable (TypePrimitive PrimNone)
      foldM
        ( \e (Located fsp fn) -> do
            let qt = fieldTypeOf fn
                v = VarName (unFieldName fn)
            recordVarDecl fsp v fsp
            recordVarUse fsp v fsp
            recordType fsp (qualType qt)
            return (insertVarWithSpan v qt fsp e)
        )
        env
        fields
  void (checkStmt armEnv body)

-- ---------------------------------------------------------------------------
-- Helpers

-- | Resolve the type of an lvalue by walking the environment.
lvalueType :: Env -> Located (LValue ()) -> Maybe Type
lvalueType env (Located _ lv) = case lv of
  LVarRef (Located _ name) -> fmap qualType (lookupVar name env)
  LArrayIndex inner _ ->
    case lvalueType env inner of
      Just (TypeArray (ArrayType elemQt)) -> Just (qualType elemQt)
      Just (TypeDict _ valType) -> Just valType
      _ -> Nothing
  LFieldAccess inner locField ->
    case lvalueType env inner of
      Just (TypeStruct tname) ->
        case lookupStruct tname env of
          Nothing -> Nothing
          Just st ->
            let fname = unLocated locField
                mSf = find (\f -> fieldName f == fname) (structFields st)
             in fmap (qualType . fieldType) mSf
      Just (TypeGenericApp tname typeArgs) ->
        case lookupStruct tname env of
          Nothing -> Nothing
          Just st ->
            let fname = unLocated locField
                mSf = find (\f -> fieldName f == fname) (structFields st)
                binding = Map.fromList (zip (structTypeParams st) (map qualType typeArgs))
             in fmap (applyBindings binding . qualType . fieldType) mSf
      _ -> Nothing

-- | Two types are compatible when they can be used interchangeably.
-- We allow any int width with any other int width, and similarly for float,
-- so that e.g. int<8> and int<32> don't generate spurious errors in practice.
-- TypeVar is compatible with any type (type erasure: checked at call site).
typesCompatible :: Type -> Type -> Bool
typesCompatible t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> True
  (TypeVar _, _) -> True
  (_, TypeVar _) -> True
  (TypePrimitive (PrimInt _), TypePrimitive (PrimInt _)) -> True
  (TypePrimitive (PrimFloat _), TypePrimitive (PrimFloat _)) -> True
  -- Arrays are compatible if element types are compatible
  (TypeArray (ArrayType qt1), TypeArray (ArrayType qt2)) ->
    typesCompatible (qualType qt1) (qualType qt2)
  -- Two function types are compatible if arity and types match (names and spans ignored)
  (TypeFunction ft1, TypeFunction ft2) ->
    let ps1 = map (paramType . unLocated) (funcParams ft1)
        ps2 = map (paramType . unLocated) (funcParams ft2)
        r1 = qualType (unLocated (funcReturnType ft1))
        r2 = qualType (unLocated (funcReturnType ft2))
     in length ps1 == length ps2
          && all (\(p1, p2) -> typesCompatible (qualType p1) (qualType p2)) (zip ps1 ps2)
          && typesCompatible r1 r2
  -- error value (ExprError) is compatible with any orerror(..., E) sharing the same error name
  (TypeResult (ResultType _ e1), TypeResult (ResultType _ e2)) -> e1 == e2
  -- returning a plain success value into an orerror return type
  (t, TypeResult (ResultType expected _)) -> typesCompatible t expected
  -- any two option types are compatible (none has inner type PrimNone)
  (TypeOption _, TypeOption _) -> True
  -- returning a plain value into an option type
  (t, TypeOption expected) -> typesCompatible t expected
  -- dict types: compatible if key and value types are compatible;
  -- PrimNone key/val means "empty literal" and is compatible with any dict
  (TypeDict (TypePrimitive PrimNone) (TypePrimitive PrimNone), TypeDict _ _) -> True
  (TypeDict k1 v1, TypeDict k2 v2) ->
    typesCompatible k1 k2 && typesCompatible v1 v2
  -- tuple types: compatible if same arity and element types are compatible
  (TypeTuple ts1, TypeTuple ts2) ->
    length ts1 == length ts2
      && all (\(q1, q2) -> typesCompatible (qualType q1) (qualType q2)) (zip ts1 ts2)
  -- generic struct instantiations: compatible if same struct name and type args match
  (TypeGenericApp n1 args1, TypeGenericApp n2 args2) ->
    n1 == n2
      && length args1 == length args2
      && all (\(a1, a2) -> typesCompatible (qualType a1) (qualType a2)) (zip args1 args2)
  _ -> False

-- ---------------------------------------------------------------------------
-- Generic type-variable inference helpers

-- | Build a binding map from type variable names to concrete types by
-- unifying formal parameter types (which may contain TypeVar) with the
-- actual argument types supplied at a call site.
inferTypeVarBindings :: [TypeName] -> [Type] -> [Type] -> Map TypeName Type
inferTypeVarBindings tvs formals actuals =
  foldl (\m (f, a) -> unifyOne tvs f a m) Map.empty (zip formals actuals)
  where
    unifyOne :: [TypeName] -> Type -> Type -> Map TypeName Type -> Map TypeName Type
    unifyOne tvs' (TypeVar n) actual m
      | n `elem` tvs' = Map.insertWith (\_ old -> old) n actual m
    unifyOne tvs' (TypeArray (ArrayType fqt)) (TypeArray (ArrayType aqt)) m =
      unifyOne tvs' (qualType fqt) (qualType aqt) m
    unifyOne tvs' (TypeOption ft) (TypeOption at) m =
      unifyOne tvs' ft at m
    unifyOne tvs' (TypeDict fk fv) (TypeDict ak av) m =
      unifyOne tvs' fk ak (unifyOne tvs' fv av m)
    unifyOne tvs' (TypeFunction ft1) (TypeFunction ft2) m =
      let ps1 = map (qualType . paramType . unLocated) (funcParams ft1)
          ps2 = map (qualType . paramType . unLocated) (funcParams ft2)
          r1 = qualType (unLocated (funcReturnType ft1))
          r2 = qualType (unLocated (funcReturnType ft2))
          m' = foldl (\acc (f, a) -> unifyOne tvs' f a acc) m (zip ps1 ps2)
       in unifyOne tvs' r1 r2 m'
    unifyOne tvs' (TypeTuple ts1) (TypeTuple ts2) m =
      foldl (\acc (q1, q2) -> unifyOne tvs' (qualType q1) (qualType q2) acc) m (zip ts1 ts2)
    unifyOne tvs' (TypeGenericApp n1 args1) (TypeGenericApp n2 args2) m
      | n1 == n2 =
          foldl (\acc (q1, q2) -> unifyOne tvs' (qualType q1) (qualType q2) acc) m (zip args1 args2)
    unifyOne _ _ _ m = m

-- | Apply a type-variable binding map to a type, substituting TypeVar nodes.
applyBindings :: Map TypeName Type -> Type -> Type
applyBindings m (TypeVar n) = Map.findWithDefault (TypeVar n) n m
applyBindings m (TypeArray (ArrayType qt)) =
  TypeArray (ArrayType (QualifiedType (qualConstness qt) (applyBindings m (qualType qt))))
applyBindings m (TypeOption t) = TypeOption (applyBindings m t)
applyBindings m (TypeDict k v) = TypeDict (applyBindings m k) (applyBindings m v)
applyBindings m (TypeTuple ts) =
  TypeTuple (map (\qt -> QualifiedType (qualConstness qt) (applyBindings m (qualType qt))) ts)
applyBindings m (TypeGenericApp n args) =
  TypeGenericApp n (map (\qt -> QualifiedType (qualConstness qt) (applyBindings m (qualType qt))) args)
applyBindings m (TypeFunction ft) =
  TypeFunction
    ft
      { funcParams = map (fmap applyInParam) (funcParams ft),
        funcReturnType = fmap applyInQType (funcReturnType ft)
      }
  where
    applyInParam p = p {paramType = applyInQType (paramType p)}
    applyInQType qt = QualifiedType (qualConstness qt) (applyBindings m (qualType qt))
applyBindings _ t = t

-- | All casts between primitive types are considered valid.
isCastValid :: Type -> Type -> Bool
isCastValid (TypePrimitive _) (TypePrimitive _) = True
isCastValid _ _ = False

checkBinary :: SourceSpan -> BinaryOp -> Type -> Type -> TC (Maybe Type)
checkBinary sp op lt rt
  | isArithmeticOp op =
      if isNumericType lt && typesCompatible lt rt
        then return (Just lt)
        else recordError (TCBinaryOpMismatch sp op lt rt) >> return Nothing
  | isComparisonOp op =
      if typesCompatible lt rt
        then return (Just (TypePrimitive PrimBool))
        else recordError (TCBinaryOpMismatch sp op lt rt) >> return Nothing
  | isLogicalOp op =
      if lt == TypePrimitive PrimBool && rt == TypePrimitive PrimBool
        then return (Just (TypePrimitive PrimBool))
        else recordError (TCBinaryOpMismatch sp op lt rt) >> return Nothing
  | isBitwiseOp op =
      if isIntegralType lt && typesCompatible lt rt
        then return (Just lt)
        else recordError (TCBinaryOpMismatch sp op lt rt) >> return Nothing
  | otherwise = return Nothing

checkUnary :: SourceSpan -> UnaryOp -> Type -> TC (Maybe Type)
checkUnary sp op t = case op of
  OpNeg | isNumericType t -> return (Just t)
  OpPos | isNumericType t -> return (Just t)
  OpNot | t == TypePrimitive PrimBool -> return (Just t)
  OpBitNot | isIntegralType t -> return (Just t)
  _ -> recordError (TCUnaryOpMismatch sp op t) >> return Nothing
