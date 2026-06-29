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
    LValue (..),
    MatchArm (..),
    MatchPattern (..),
    Stmt (..),
  )
import AST.Types.Common
  ( ErrorName (..),
    FuncName,
    Located (..),
    SourceSpan,
    TypeName (..),
    VarName,
    locSpan,
    unErrorName,
    unLocated,
    unTypeName,
  )
import AST.Types.Literal
  ( ArrayLiteral (..),
    Literal (..),
  )
import AST.Types.Operator
  ( BinaryOp,
    UnaryOp (..),
    isArithmeticOp,
    isBitwiseOp,
    isComparisonOp,
    isLogicalOp,
  )
import AST.Types.Type
  ( ArrayType (..),
    Constness (..),
    ErrorField (..),
    ErrorType (..),
    FunctionType (..),
    PrimitiveType (..),
    QualifiedType (..),
    ResultType (..),
    StructField (..),
    StructType (..),
    Type (..),
    defaultFloatType,
    defaultIntType,
    isIntegralType,
    isNumericType,
    paramName,
    paramType,
    paramVariadic,
    qualType,
  )
import Control.Monad (foldM_, forM_, unless, void, when)
import Control.Monad.State (State, modify)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import TypeChecker.Builtins (builtinReturnType, isKnownBuiltin)
import TypeChecker.Env
  ( Env (..),
    insertVarWithSpan,
    lookupError,
    lookupFunc,
    lookupStruct,
    lookupVar,
    lookupVarDef,
    setReturnType,
  )
import TypeChecker.Error (TypeCheckError (..))

data TCState = TCState
  { tcsErrors :: [TypeCheckError],
    tcsTypes :: Map SourceSpan Type,
    tcsCallSites :: Map SourceSpan (FuncName, FunctionType),
    tcsBuiltinCallSites :: Map SourceSpan FuncName,
    tcsCallWithArgs :: Map SourceSpan (FuncName, FunctionType, [SourceSpan]),
    tcsVarUseSites :: Map SourceSpan (VarName, SourceSpan)
  }

initialTCState :: TCState
initialTCState = TCState [] Map.empty Map.empty Map.empty Map.empty Map.empty

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
        Nothing -> recordError (TCUndefinedVar vspan name) >> return Nothing
    go (ExprBinary op lhs rhs) = do
      mL <- inferExpr env lhs
      mR <- inferExpr env rhs
      case (mL, mR) of
        (Just lt, Just rt) -> checkBinary sp op lt rt
        _ -> return Nothing
    go (ExprUnary op operand) = do
      mT <- inferExpr env operand
      case mT of
        Just t -> checkUnary sp op t
        Nothing -> return Nothing
    go (ExprCall (Located nameSpan fname) args) =
      inferCall sp nameSpan fname args
    go (ExprIndex arrExpr idxExpr) = do
      mArrType <- inferExpr env arrExpr
      void $ inferExpr env idxExpr
      case mArrType of
        Just (TypeArray (ArrayType elemQt)) -> return (Just (qualType elemQt))
        Just t -> recordError (TCIndexNonArray (locSpan arrExpr) t) >> return Nothing
        Nothing -> return Nothing
    go (ExprField structExpr locField) = do
      mStructType <- inferExpr env structExpr
      case mStructType of
        Just (TypeStruct tname) ->
          case lookupStruct tname env of
            Nothing -> return Nothing
            Just st ->
              let fname = unLocated locField
                  mSf = find (\f -> fieldName f == fname) (structFields st)
               in return (fmap (qualType . fieldType) mSf)
        -- TypeNamed is used for error-bound variables in err(E v) match arms
        Just (TypeNamed tname) ->
          case lookupError (ErrorName (unTypeName tname)) env of
            Just et ->
              let fname = unLocated locField
                  mEf = find (\f -> errorFieldName f == fname) (errorTypeFields et)
               in return (fmap errorFieldType mEf)
            Nothing -> return Nothing
        _ -> return Nothing
    go (ExprStructInit (Located _ tname) fieldExprs) = do
      forM_ fieldExprs $ \(_, e) -> inferExpr env e
      return (Just (TypeStruct tname))
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
        other -> return other
    go (ExprMust inner) = do
      mInner <- inferExpr env inner
      case mInner of
        Just (TypeResult (ResultType successType _)) -> return (Just successType)
        other -> return other
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
          let retType = qualType (unLocated (funcReturnType ft))
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

-- ---------------------------------------------------------------------------
-- Declaration checking

checkDecl :: Env -> Located (Decl ()) -> TC ()
checkDecl env (Located _ decl) = case decl of
  DeclFunction _ fd -> checkFunction env fd
  _ -> return ()

checkFunction :: Env -> FunctionDecl () -> TC ()
checkFunction baseEnv fd = do
  let retQt = unLocated (funcDeclReturnType fd)
  let params = funcDeclParams fd
  let env =
        foldr
          (\(Located psp p) e -> insertVarWithSpan (paramName p) (paramType p) psp e)
          (setReturnType retQt baseEnv)
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
      recordVarUse vsp v vsp
      return (insertVarWithSpan v innerQt vsp env)
    MatchErr (Located _ ename) (Located vsp v) -> do
      let errQt = QualifiedType Mutable (TypeNamed (TypeName (unErrorName ename)))
      recordVarUse vsp v vsp
      return (insertVarWithSpan v errQt vsp env)
    MatchLit e -> void (inferExpr env e) >> return env
    MatchRange lo hi -> do
      void (inferExpr env lo)
      void (inferExpr env hi)
      return env
    MatchWildcard -> return env
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
      _ -> Nothing

-- | Two types are compatible when they can be used interchangeably.
-- We allow any int width with any other int width, and similarly for float,
-- so that e.g. int<8> and int<32> don't generate spurious errors in practice.
typesCompatible :: Type -> Type -> Bool
typesCompatible t1 t2 = case (t1, t2) of
  _ | t1 == t2 -> True
  (TypePrimitive (PrimInt _), TypePrimitive (PrimInt _)) -> True
  (TypePrimitive (PrimFloat _), TypePrimitive (PrimFloat _)) -> True
  -- error value (ExprError) is compatible with any orerror(..., E) sharing the same error name
  (TypeResult (ResultType _ e1), TypeResult (ResultType _ e2)) -> e1 == e2
  -- returning a plain success value into an orerror return type
  (t, TypeResult (ResultType expected _)) -> typesCompatible t expected
  _ -> False

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
