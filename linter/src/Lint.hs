module Lint
  ( Severity (..),
    RuleName (..),
    LintDiag (..),
    LintOpts (..),
    defaultLintOpts,
    ruleId,
    ruleDesc,
    ruleSev,
    allRules,
    formatDiag,
    lintProgram,
  )
where

import AST.Types.AST
  ( Block (..),
    Decl (..),
    ErrorDecl (..),
    ErrorSetDecl (..),
    Expr (..),
    ForInit (..),
    FunctionDecl (..),
    LValue (..),
    MatchArm (..),
    Program (..),
    Stmt (..),
    StructDecl (..),
    programDecls,
  )
import AST.Types.Common
  ( Column (..),
    ErrorName (..),
    FuncName (..),
    Line (..),
    Located (..),
    SourcePos (..),
    SourceSpan (..),
    TypeName (..),
    VarName (..),
    locSpan,
    locValue,
    unErrorName,
    unFuncName,
    unTypeName,
    unVarName,
  )
import AST.Types.Operator (AssignOp (..))
import AST.Types.Type (Parameter (..), PrimitiveType (..), QualifiedType (..), Type (..), paramName)
import Data.Char (isAlphaNum, isDigit, isLower, isUpper)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Diagnostic types

data Severity = SevWarning | SevError
  deriving (Eq, Ord, Show)

data RuleName
  = RUnusedVar
  | RUnusedParam
  | RUnreachableCode
  | RMissingReturn
  | RFnNaming
  | RTypeNaming
  | REmptyBlock
  | RShadow
  deriving (Eq, Ord, Enum, Bounded, Show)

ruleId :: RuleName -> String
ruleId RUnusedVar = "unused-var"
ruleId RUnusedParam = "unused-param"
ruleId RUnreachableCode = "unreachable-code"
ruleId RMissingReturn = "missing-return"
ruleId RFnNaming = "fn-naming"
ruleId RTypeNaming = "type-naming"
ruleId REmptyBlock = "empty-block"
ruleId RShadow = "shadow"

ruleDesc :: RuleName -> String
ruleDesc RUnusedVar = "variable is declared but never read"
ruleDesc RUnusedParam = "function parameter is never used"
ruleDesc RUnreachableCode = "code is unreachable after return"
ruleDesc RMissingReturn = "non-void function may not return on all paths"
ruleDesc RFnNaming = "function name should be snake_case"
ruleDesc RTypeNaming = "type name should be PascalCase"
ruleDesc REmptyBlock = "block body is empty"
ruleDesc RShadow = "variable shadows an outer binding"

ruleSev :: RuleName -> Severity
ruleSev RMissingReturn = SevError
ruleSev _ = SevWarning

allRules :: [RuleName]
allRules = [minBound .. maxBound]

data LintDiag = LintDiag
  { diagFile :: FilePath,
    diagLine :: Int,
    diagCol :: Int,
    diagSev :: Severity,
    diagRule :: RuleName,
    diagMsg :: String
  }

formatDiag :: LintDiag -> String
formatDiag d =
  diagFile d
    ++ ":"
    ++ show (diagLine d)
    ++ ":"
    ++ show (diagCol d)
    ++ ": ["
    ++ (if diagSev d == SevError then "E" else "W")
    ++ "] "
    ++ diagMsg d
    ++ " ["
    ++ ruleId (diagRule d)
    ++ "]"

-- ---------------------------------------------------------------------------
-- Lint options

data LintOpts = LintOpts
  { lintDeny :: Set RuleName,
    lintAllow :: Set RuleName
  }

defaultLintOpts :: LintOpts
defaultLintOpts = LintOpts Set.empty Set.empty

-- ---------------------------------------------------------------------------
-- Entry point

lintProgram :: LintOpts -> FilePath -> Program () -> [LintDiag]
lintProgram opts fp prog =
  applyAllow opts
    . map (applySev opts)
    $ concatMap (lintDecl fp . locValue) (programDecls prog)

applyAllow :: LintOpts -> [LintDiag] -> [LintDiag]
applyAllow opts = filter (\d -> diagRule d `Set.notMember` lintAllow opts)

applySev :: LintOpts -> LintDiag -> LintDiag
applySev opts d
  | diagRule d `Set.member` lintDeny opts = d {diagSev = SevError}
  | otherwise = d

-- ---------------------------------------------------------------------------
-- Declaration-level rules

lintDecl :: FilePath -> Decl () -> [LintDiag]
lintDecl fp (DeclFunction _ fd) = lintFunction fp fd
lintDecl fp (DeclStruct _ sd) = lintStructName fp sd
lintDecl fp (DeclError _ ed) = lintErrorDeclName fp ed
lintDecl fp (DeclErrorSet _ esd) = lintErrorSetName fp esd
lintDecl _ _ = []

-- fn-naming + body rules
lintFunction :: FilePath -> FunctionDecl () -> [LintDiag]
lintFunction fp fd =
  nameDiags ++ paramDiags ++ unusedVarDiags ++ bodyDiags
  where
    Located nameSpan fname = funcDeclName fd
    name = T.unpack (unFuncName fname)
    nameDiags
      | isSnakeCase name = []
      | otherwise =
          [ mkDiag
              fp
              nameSpan
              SevWarning
              RFnNaming
              ("function `" ++ name ++ "` should be snake_case")
          ]
    params = funcDeclParams fd
    retT = locValue (funcDeclReturnType fd)
    body = funcDeclBody fd
    used = usedVarsBlock body
    paramDiags = concatMap (checkUnusedParam fp used) params
    unusedVarDiags = checkUnusedVars fp body used
    bodyDiags =
      lintBlock fp [Set.fromList (map (paramName . locValue) params)] body
        ++ checkMissingReturn fp fd retT body

-- type-naming rules

lintStructName :: FilePath -> StructDecl -> [LintDiag]
lintStructName fp sd =
  let Located sp tname = structDeclName sd
      name = T.unpack (unTypeName tname)
   in [mkDiag fp sp SevWarning RTypeNaming ("struct `" ++ name ++ "` should be PascalCase") | not (isPascalCase name)]

lintErrorDeclName :: FilePath -> ErrorDecl -> [LintDiag]
lintErrorDeclName fp ed =
  let Located sp ename = errorDeclName ed
      name = T.unpack (unErrorName ename)
   in [mkDiag fp sp SevWarning RTypeNaming ("error `" ++ name ++ "` should be PascalCase") | not (isPascalCase name)]

lintErrorSetName :: FilePath -> ErrorSetDecl -> [LintDiag]
lintErrorSetName fp esd =
  let Located sp ename = errorSetDeclName esd
      name = T.unpack (unErrorName ename)
   in [mkDiag fp sp SevWarning RTypeNaming ("error set `" ++ name ++ "` should be PascalCase") | not (isPascalCase name)]

-- ---------------------------------------------------------------------------
-- missing-return

checkMissingReturn :: FilePath -> FunctionDecl () -> QualifiedType -> Block () -> [LintDiag]
checkMissingReturn fp fd retT body
  | isVoidReturn retT = []
  | allPathsReturn body = []
  | otherwise =
      let Located nameSpan _ = funcDeclName fd
       in [mkDiag fp nameSpan SevError RMissingReturn "non-void function may not return on all paths"]

isVoidReturn :: QualifiedType -> Bool
isVoidReturn qt = qualType qt == TypePrimitive PrimNone

allPathsReturn :: Block () -> Bool
allPathsReturn (Block _ stmts) = any (pathReturns . locValue) stmts
  where
    pathReturns (StmtReturn _) = True
    pathReturns (StmtIf _ tb (Just eb)) = allPathsReturn tb && allPathsReturn eb
    pathReturns (StmtBlock b) = allPathsReturn b
    pathReturns (StmtMatch _ arms) = all (pathReturns . locValue . matchArmBody) arms
    pathReturns _ = False

-- ---------------------------------------------------------------------------
-- unused-var: whole-function scan (scope-naive, avoids false positives)

checkUnusedVars :: FilePath -> Block () -> Set VarName -> [LintDiag]
checkUnusedVars fp body used =
  [ mkDiag
      fp
      (locSpan lv)
      SevWarning
      RUnusedVar
      ("variable `" ++ T.unpack (unVarName (locValue lv)) ++ "` is declared but never read")
    | lv <- allDeclsBlock body,
      locValue lv `Set.notMember` used
  ]

allDeclsBlock :: Block () -> [Located VarName]
allDeclsBlock (Block _ stmts) = concatMap (allDeclsStmt . locValue) stmts

allDeclsStmt :: Stmt () -> [Located VarName]
allDeclsStmt (StmtVarDecl lv _ _) = [lv]
allDeclsStmt (StmtIf _ tb meb) = allDeclsBlock tb ++ maybe [] allDeclsBlock meb
allDeclsStmt (StmtWhile _ body) = allDeclsBlock body
allDeclsStmt (StmtFor mInit _ _ body) =
  (case mInit of Just (ForInitDecl lv _ _) -> [lv]; _ -> [])
    ++ allDeclsBlock body
allDeclsStmt (StmtBlock b) = allDeclsBlock b
allDeclsStmt (StmtMatch _ arms) = concatMap (allDeclsStmt . locValue . matchArmBody) arms
allDeclsStmt (StmtTupleDecl vars _ _) = vars
allDeclsStmt _ = []

-- ---------------------------------------------------------------------------
-- unused-param

checkUnusedParam :: FilePath -> Set VarName -> Located Parameter -> [LintDiag]
checkUnusedParam fp used (Located sp p)
  | paramName p `Set.member` used = []
  | otherwise =
      [ mkDiag
          fp
          sp
          SevWarning
          RUnusedParam
          ("parameter `" ++ T.unpack (unVarName (paramName p)) ++ "` is never used")
      ]

-- ---------------------------------------------------------------------------
-- Block linting: unreachable-code, empty-block, shadow

-- scopes: innermost first; each scope is the set of var names declared there
lintBlock :: FilePath -> [Set VarName] -> Block () -> [LintDiag]
lintBlock fp scopes (Block _ stmts) =
  unreachDiags ++ stmtDiags
  where
    -- unreachable-code: everything after the first StmtReturn at this level
    firstRetIdx = length (takeWhile (not . isReturnStmt . locValue) stmts)
    afterReturn = drop (firstRetIdx + 1) stmts
    unreachDiags =
      [ mkDiag fp (locSpan s) SevWarning RUnreachableCode "unreachable code after return"
        | s <- afterReturn
      ]
    -- walk stmts, threading the scope stack
    (_, stmtDiags) = foldl' step (scopes, []) (take (firstRetIdx + 1) stmts)
    step (scs, acc) (Located sp stmt) =
      let (scs', d) = lintStmt fp scs sp stmt
       in (scs', acc ++ d)

isReturnStmt :: Stmt () -> Bool
isReturnStmt (StmtReturn _) = True
isReturnStmt _ = False

lintStmt :: FilePath -> [Set VarName] -> SourceSpan -> Stmt () -> ([Set VarName], [LintDiag])
lintStmt fp scopes sp stmt = case stmt of
  StmtVarDecl (Located _ vname) _ mInit ->
    let initDiags = maybe [] (lintExprDiags fp . locValue) mInit
        shadowDiag
          | any (Set.member vname) scopes =
              [ mkDiag
                  fp
                  sp
                  SevWarning
                  RShadow
                  ("variable `" ++ T.unpack (unVarName vname) ++ "` shadows an outer binding")
              ]
          | otherwise = []
        scopes' = case scopes of
          [] -> [Set.singleton vname]
          (s : ss) -> Set.insert vname s : ss
     in (scopes', initDiags ++ shadowDiag)
  StmtAssign lv op expr ->
    let lvDiags = if op == AssignSimple then [] else lintLValueDiags fp (locValue lv)
        exprDiags = lintExprDiags fp (locValue expr)
     in (scopes, lvDiags ++ exprDiags)
  StmtExpr expr ->
    (scopes, lintExprDiags fp (locValue expr))
  StmtIf cond thenB maybeElse ->
    let condDiags = lintExprDiags fp (locValue cond)
        thenDiags =
          if null (blockStmts thenB)
            then [mkDiag fp (blockSpan thenB) SevWarning REmptyBlock "empty if-body"]
            else lintBlock fp (Set.empty : scopes) thenB
        elseDiags = case maybeElse of
          Nothing -> []
          Just eb ->
            if null (blockStmts eb)
              then [mkDiag fp (blockSpan eb) SevWarning REmptyBlock "empty else body"]
              else lintBlock fp (Set.empty : scopes) eb
     in (scopes, condDiags ++ thenDiags ++ elseDiags)
  StmtWhile cond body ->
    let condDiags = lintExprDiags fp (locValue cond)
        bodyDiags =
          if null (blockStmts body)
            then [mkDiag fp (blockSpan body) SevWarning REmptyBlock "empty while-body"]
            else lintBlock fp (Set.empty : scopes) body
     in (scopes, condDiags ++ bodyDiags)
  StmtFor mInit mCond mStep body ->
    let forScope = Set.empty : scopes
        (forScope', initDiags) = case mInit of
          Nothing -> (forScope, [])
          Just (ForInitDecl (Located _ vn) _ expr) ->
            let d = lintExprDiags fp (locValue expr)
                s = case forScope of (h : t) -> Set.insert vn h : t; [] -> [Set.singleton vn]
             in (s, d)
          Just (ForInitExpr expr) -> (forScope, lintExprDiags fp (locValue expr))
        condDiags = maybe [] (lintExprDiags fp . locValue) mCond
        stepDiags = maybe [] (\s -> snd (lintStmt fp forScope' (locSpan s) (locValue s))) mStep
        bodyDiags = lintBlock fp (Set.empty : forScope') body
     in (scopes, initDiags ++ condDiags ++ stepDiags ++ bodyDiags)
  StmtReturn mExpr ->
    (scopes, maybe [] (lintExprDiags fp . locValue) mExpr)
  StmtBlock b ->
    (scopes, lintBlock fp (Set.empty : scopes) b)
  StmtMatch expr arms ->
    let exprDiags = lintExprDiags fp (locValue expr)
        armDiags = concatMap (lintMatchArm fp scopes) arms
     in (scopes, exprDiags ++ armDiags)
  StmtTupleDecl _ _ e ->
    (scopes, lintExprDiags fp (locValue e))
  StmtBreak -> (scopes, [])
  StmtContinue -> (scopes, [])

lintMatchArm :: FilePath -> [Set VarName] -> MatchArm () -> [LintDiag]
lintMatchArm fp scopes arm =
  let s = matchArmBody arm
   in snd (lintStmt fp (Set.empty : scopes) (locSpan s) (locValue s))

lintLValueDiags :: FilePath -> LValue () -> [LintDiag]
lintLValueDiags fp (LArrayIndex lv idx) =
  lintLValueDiags fp (locValue lv) ++ lintExprDiags fp (locValue idx)
lintLValueDiags fp (LFieldAccess lv _) =
  lintLValueDiags fp (locValue lv)
lintLValueDiags _ _ = []

lintExprDiags :: FilePath -> Expr () -> [LintDiag]
lintExprDiags fp = \case
  ExprLambda params _ body ->
    let used = usedVarsBlock body
        paramDiags = concatMap (checkUnusedParam fp used) params
        bodyDiags =
          lintBlock fp [Set.fromList (map (paramName . locValue) params)] body
            ++ checkUnusedVars fp body used
     in paramDiags ++ bodyDiags
  ExprBinary _ l r -> go l ++ go r
  ExprUnary _ e -> go e
  ExprCall _ args -> concatMap go args
  ExprIndex arr idx -> go arr ++ go idx
  ExprField e _ -> go e
  ExprStructInit _ fields -> concatMap (go . snd) fields
  ExprArrayInit _ elems -> concatMap go elems
  ExprDictLit pairs -> concatMap (\(k, v) -> go k ++ go v) pairs
  ExprError _ fields -> concatMap (go . snd) fields
  ExprTry e -> go e
  ExprMust e -> go e
  ExprSome e -> go e
  ExprParen e -> go e
  ExprCast e _ -> go e
  ExprTupleInit elems -> concatMap go elems
  _ -> []
  where
    go = lintExprDiags fp . locValue

-- ---------------------------------------------------------------------------
-- Variable usage collection

usedVarsBlock :: Block () -> Set VarName
usedVarsBlock (Block _ stmts) = foldMap (usedVarsStmt . locValue) stmts

usedVarsStmt :: Stmt () -> Set VarName
usedVarsStmt = \case
  StmtVarDecl _ _ mInit -> maybe Set.empty (usedVarsExpr . locValue) mInit
  StmtAssign lv op expr ->
    -- for compound assignment, the lvalue is also read
    (if op == AssignSimple then Set.empty else usedVarsLValue (locValue lv))
      <> usedVarsExpr (locValue expr)
  StmtExpr expr -> usedVarsExpr (locValue expr)
  StmtIf cond tb meb ->
    usedVarsExpr (locValue cond)
      <> usedVarsBlock tb
      <> maybe Set.empty usedVarsBlock meb
  StmtWhile cond body -> usedVarsExpr (locValue cond) <> usedVarsBlock body
  StmtFor mInit mCond mStep body ->
    foldMap usedVarsForInit mInit
      <> maybe Set.empty (usedVarsExpr . locValue) mCond
      <> maybe Set.empty (usedVarsStmt . locValue) mStep
      <> usedVarsBlock body
  StmtReturn mExpr -> maybe Set.empty (usedVarsExpr . locValue) mExpr
  StmtBlock b -> usedVarsBlock b
  StmtMatch expr arms ->
    usedVarsExpr (locValue expr)
      <> foldMap (usedVarsStmt . locValue . matchArmBody) arms
  StmtTupleDecl _ _ e -> usedVarsExpr (locValue e)
  StmtBreak -> Set.empty
  StmtContinue -> Set.empty

usedVarsForInit :: ForInit () -> Set VarName
usedVarsForInit (ForInitDecl _ _ expr) = usedVarsExpr (locValue expr)
usedVarsForInit (ForInitExpr expr) = usedVarsExpr (locValue expr)

usedVarsLValue :: LValue () -> Set VarName
usedVarsLValue (LVarRef (Located _ v)) = Set.singleton v
usedVarsLValue (LArrayIndex lv idx) = usedVarsLValue (locValue lv) <> usedVarsExpr (locValue idx)
usedVarsLValue (LFieldAccess lv _) = usedVarsLValue (locValue lv)

usedVarsExpr :: Expr () -> Set VarName
usedVarsExpr = \case
  ExprLiteral lit -> foldMap (usedVarsExpr . locValue) lit
  ExprVar (Located _ v) -> Set.singleton v
  ExprBinary _ l r -> go l <> go r
  ExprUnary _ e -> go e
  ExprCall _ args -> foldMap go args
  ExprIndex arr idx -> go arr <> go idx
  ExprField e _ -> go e
  ExprStructInit _ fields -> foldMap (go . snd) fields
  ExprArrayInit _ elems -> foldMap go elems
  ExprDictLit pairs -> foldMap (\(k, v) -> go k <> go v) pairs
  ExprError _ fields -> foldMap (go . snd) fields
  ExprTry e -> go e
  ExprMust e -> go e
  ExprSome e -> go e
  ExprNone -> Set.empty
  ExprLambda _ _ body -> usedVarsBlock body
  ExprParen e -> go e
  ExprCast e _ -> go e
  ExprTupleInit elems -> foldMap go elems
  where
    go = usedVarsExpr . locValue

-- ---------------------------------------------------------------------------
-- Naming predicates

isSnakeCase :: String -> Bool
isSnakeCase [] = False
isSnakeCase s = all (\c -> isLower c || isDigit c || c == '_') s

isPascalCase :: String -> Bool
isPascalCase [] = False
isPascalCase (c : _) = isUpper c && isAlphaNum c

-- ---------------------------------------------------------------------------
-- Helpers

mkDiag :: FilePath -> SourceSpan -> Severity -> RuleName -> String -> LintDiag
mkDiag fp sp sev rule msg =
  LintDiag
    { diagFile = fp,
      diagLine = unLine (posLine (spanStart sp)),
      diagCol = unColumn (posColumn (spanStart sp)),
      diagSev = sev,
      diagRule = rule,
      diagMsg = msg
    }
