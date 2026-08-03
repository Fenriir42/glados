module Formatter (formatProgram) where

import AST.Types.AST
  ( Block (..),
    Decl (..),
    ErrorDecl (..),
    ErrorSetDecl (..),
    Expr (..),
    FFIDecl (..),
    FFIFuncDecl (..),
    ForInit (..),
    FunctionDecl (..),
    ImportDecl (..),
    ImportTarget (..),
    LValue (..),
    MatchArm (..),
    MatchPattern (..),
    ModulePath (..),
    Program (..),
    Stmt (..),
    StructDecl (..),
    Visibility (..),
    programDecls,
  )
import AST.Types.Common
  ( ErrorName (..),
    FieldName (..),
    FuncName (..),
    Line (..),
    Located (..),
    ModuleName (..),
    SourcePos (..),
    SourceSpan (..),
    TypeName (..),
    VarName (..),
    locSpan,
    unErrorName,
    unFieldName,
    unFuncName,
    unModuleName,
    unTypeName,
    unVarName,
  )
import AST.Types.Literal
  ( ArrayLiteral (..),
    BoolLiteral (..),
    FloatLiteral (..),
    IntBase (..),
    IntLiteral (..),
    Literal (..),
    StringLiteral (..),
  )
import AST.Types.Operator
  ( AssignOp (..),
    assignOpSymbol,
    binaryOpSymbol,
    unaryOpSymbol,
  )
import AST.Types.Type
  ( ArrayType (..),
    Constness (..),
    ErrorField (..),
    ErrorSetMember (..),
    FunctionType (..),
    Parameter (..),
    QualifiedType (..),
    ResultType (..),
    StructField (..),
    Type (..),
  )
import Config (FormatOptions (..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, partition, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex, showOct)

-- ---------------------------------------------------------------------------
-- Core helpers

type Lines = [Text]

-- | Produce the indent string for nesting level n.
ind :: FormatOptions -> Int -> Text
ind opts n
  | optHardTabs opts = T.replicate (fromIntegral n) "\t"
  | otherwise = T.replicate (fromIntegral (n * optIndentSize opts)) " "

-- | Append `;` to the last line (Quant uses endBy, so every statement
-- in a block is followed by a semicolon terminator).
appendSemi :: Lines -> Lines
appendSemi [] = [";"]
appendSemi ls = init ls ++ [last ls <> ";"]

-- | Remove trailing comma from the last line unless optTrailingComma.
dropTrailingComma :: FormatOptions -> Lines -> Lines
dropTrailingComma _ [] = []
dropTrailingComma opts ls
  | optTrailingComma opts = ls
  | otherwise = init ls ++ [T.dropEnd 1 (last ls)]

-- ---------------------------------------------------------------------------
-- Comment extraction

-- | Map from 0-based line numbers to standalone comment text.
-- Standalone = the line contains only whitespace and then // or #.
type CommentsMap = IntMap Text

-- | Extract all standalone comment lines from source text.
extractComments :: Text -> CommentsMap
extractComments src =
  IM.fromList
    [ (i, T.stripStart ln)
      | (i, ln) <- zip [0 ..] (T.lines src),
        let s = T.stripStart ln,
        T.isPrefixOf "//" s || (not (T.null s) && T.isPrefixOf "#" s)
    ]

-- | Collect all comments at 0-based lines in the inclusive range [lo, hi].
commentsInRange :: CommentsMap -> Int -> Int -> [Text]
commentsInRange cm lo hi
  | lo > hi = []
  | otherwise = [v | k <- [lo .. hi], Just v <- [IM.lookup k cm]]

-- | Convert a 1-based SourcePos line to a 0-based Int.
spanLine :: SourcePos -> Int
spanLine sp = unLine (posLine sp) - 1

-- | True end line (0-based) of a block (proxy for the closing }).
blockEndLine :: Block () -> Int -> Int
blockEndLine blk def = case blockStmts blk of
  [] -> def
  stmts -> stmtEndLine (last stmts)

-- | True end line (0-based) of a statement.
-- Compound stmts close with } whose spanEnd is corrupted; use last nested stmt instead.
stmtEndLine :: Located (Stmt ()) -> Int
stmtEndLine (Located _ (StmtIf _ thenBlk Nothing)) =
  blockEndLine thenBlk (spanLine (spanStart (blockSpan thenBlk)))
stmtEndLine (Located _ (StmtIf _ _ (Just elseBlk))) =
  blockEndLine elseBlk (spanLine (spanStart (blockSpan elseBlk)))
stmtEndLine (Located _ (StmtWhile _ body)) =
  blockEndLine body (spanLine (spanStart (blockSpan body)))
stmtEndLine (Located _ (StmtFor _ _ _ body)) =
  blockEndLine body (spanLine (spanStart (blockSpan body)))
stmtEndLine (Located _ (StmtBlock blk)) =
  blockEndLine blk (spanLine (spanStart (blockSpan blk)))
stmtEndLine (Located _ (StmtMatch _ arms)) = case reverse arms of
  [] -> 0
  (MatchArm _ lastBody : _) -> stmtEndLine lastBody
stmtEndLine (Located sp _) = spanLine (spanEnd sp)

-- | True end line (0-based) of a top-level decl.
-- DeclFunction's spanEnd covers the } token with a corrupted endPos; use last stmt instead.
declEndLine :: Located (Decl ()) -> Int
declEndLine (Located _ (DeclFunction _ fd)) =
  blockEndLine (funcDeclBody fd) (spanLine (spanStart (blockSpan (funcDeclBody fd))))
declEndLine (Located sp _) = spanLine (spanEnd sp)

-- | True start line (0-based) of a top-level decl.
-- The Located span of a DeclFunction has a corrupted spanStart because
-- parseFunctionType -> parseQualifiedType -> parseConstness returns
-- voidSpann (posFile="/dev/null"), which wins the min comparison when
-- merging spans. We bypass this by reading the function name span directly.
declTrueStartLine :: Located (Decl ()) -> Int
declTrueStartLine (Located _ (DeclFunction _ fd)) =
  spanLine (spanStart (locSpan (funcDeclName fd)))
declTrueStartLine (Located sp _) = spanLine (spanStart sp)

-- | True start line (0-based) of a statement.
-- StmtVarDecl spans are corrupted for the same reason (parseQualifiedType).
-- Use the variable name token's span as the authoritative start line.
stmtTrueStartLine :: Located (Stmt ()) -> Int
stmtTrueStartLine (Located _ (StmtVarDecl (Located nameSpan _) _ _)) =
  spanLine (spanStart nameSpan)
stmtTrueStartLine (Located sp _) = spanLine (spanStart sp)

-- ---------------------------------------------------------------------------
-- Format all stmts in a block, inserting comments between them.
--
-- `openLine`: 0-based line of the opening `{`; used to capture comments
-- that appear between `{` and the first statement.

blockBody :: CommentsMap -> FormatOptions -> Int -> Int -> Block () -> Lines
blockBody cm opts n openLine blk = go openLine (blockStmts blk)
  where
    go _ [] = []
    go prevEnd (ls : rest) =
      let startL = stmtTrueStartLine ls
          endL = stmtEndLine ls
          before = [ind opts (n + 1) <> c | c <- commentsInRange cm (prevEnd + 1) (startL - 1)]
       in before ++ appendSemi (fmtStmt cm opts (n + 1) ls) ++ go endL rest

-- ---------------------------------------------------------------------------
-- Program

-- | Format a program, preserving standalone comments from the original source.
formatProgram :: FormatOptions -> Text -> Program () -> Text
formatProgram opts src prog =
  let cm = extractComments src
      locDecls = programDecls prog
      (lImps, lOthers) = partition (isImport . locValue) locDecls
      sorted =
        if optReorderImports opts
          then sortBy (comparing (importKey . locValue)) lImps
          else lImps
      impLines = map (fmtImport . getImport . locValue) sorted
      otherBlocks = fmtTopDecls cm opts lOthers
      sections = filter (not . null) $ impLines : otherBlocks
   in T.unlines (intercalate [""] sections)

-- | Format top-level declarations, inserting comments that appear between them.
fmtTopDecls :: CommentsMap -> FormatOptions -> [Located (Decl ())] -> [Lines]
fmtTopDecls cm opts = go (-1)
  where
    go _ [] = []
    go prevEnd (ld : rest) =
      let startL = declTrueStartLine ld
          endL = declEndLine ld
          before = commentsInRange cm (prevEnd + 1) (startL - 1)
          body = fmtDecl cm opts 0 ld
       in (before ++ body) : go endL rest

isImport :: Decl () -> Bool
isImport (DeclImport _) = True
isImport _ = False

importKey :: Decl () -> Text
importKey (DeclImport d) = modPathText (importPath d)
importKey _ = ""

getImport :: Decl () -> ImportDecl
getImport (DeclImport d) = d
getImport _ = error "getImport: not an import"

-- ---------------------------------------------------------------------------
-- Imports

modPathText :: ModulePath -> Text
modPathText mp =
  T.intercalate "." [unModuleName n | Located _ n <- modulePathParts mp]

fmtImport :: ImportDecl -> Text
fmtImport (ImportDecl path ImportAll) =
  "import " <> modPathText path
fmtImport (ImportDecl path (ImportNames names)) =
  "from "
    <> modPathText path
    <> " import "
    <> T.intercalate ", " [unVarName (locValue n) | n <- names]
fmtImport (ImportDecl path ImportWildcard) =
  "from " <> modPathText path <> " import *"

-- ---------------------------------------------------------------------------
-- Declarations

fmtDecl :: CommentsMap -> FormatOptions -> Int -> Located (Decl ()) -> Lines
fmtDecl cm opts n (Located _ (DeclFunction vis fd)) =
  fmtFuncDecl cm opts n (spanLine (spanStart (blockSpan (funcDeclBody fd)))) vis fd
fmtDecl _ opts n (Located _ (DeclStruct vis sd)) = fmtStructDecl opts n vis sd
fmtDecl _ _ _ (Located _ (DeclImport _)) = []
fmtDecl _ opts n (Located _ (DeclError vis ed)) = fmtErrorDecl opts n vis ed
fmtDecl _ opts n (Located _ (DeclErrorSet vis esd)) = fmtErrorSetDecl opts n vis esd
fmtDecl _ opts n (Located _ (DeclFFI ffi)) = fmtFFIDecl opts n ffi

fmtVis :: Visibility -> Text
fmtVis Public = ""
fmtVis Static = "static "

fmtFuncDecl :: CommentsMap -> FormatOptions -> Int -> Int -> Visibility -> FunctionDecl () -> Lines
fmtFuncDecl cm opts n openLine vis fd =
  let tparams = case funcDeclTypeParams fd of
        [] -> ""
        ps -> "[" <> T.intercalate ", " [unTypeName (locValue p) | p <- ps] <> "]"
      params = T.intercalate ", " [fmtParam (locValue p) | p <- funcDeclParams fd]
      ret = fmtQType (locValue (funcDeclReturnType fd))
      sig =
        ind opts n
          <> fmtVis vis
          <> "fn "
          <> unFuncName (locValue (funcDeclName fd))
          <> tparams
          <> "("
          <> params
          <> ") -> "
          <> ret
          <> " {"
   in [sig] ++ blockBody cm opts n openLine (funcDeclBody fd) ++ [ind opts n <> "}"]

fmtParam :: Parameter -> Text
fmtParam p =
  unVarName (paramName p)
    <> ": "
    <> (if paramVariadic p then "..." else "")
    <> fmtQType (paramType p)

fmtStructDecl :: FormatOptions -> Int -> Visibility -> StructDecl -> Lines
fmtStructDecl opts n vis sd =
  let header =
        ind opts n
          <> fmtVis vis
          <> "struct "
          <> unTypeName (locValue (structDeclName sd))
          <> " {"
      rawFields =
        [ ind opts (n + 1)
            <> unFieldName (fieldName f)
            <> ": "
            <> fmtQType (fieldType f)
            <> ","
          | Located _ f <- structDeclFields sd
        ]
   in [header] ++ dropTrailingComma opts rawFields ++ [ind opts n <> "}"]

fmtErrorDecl :: FormatOptions -> Int -> Visibility -> ErrorDecl -> Lines
fmtErrorDecl opts n vis ed =
  let nameStr = unErrorName (locValue (errorDeclName ed))
      header = ind opts n <> fmtVis vis <> "error " <> nameStr
   in case errorDeclFields ed of
        [] -> [header <> ";"]
        fields ->
          let rawF =
                [ ind opts (n + 1)
                    <> unFieldName (errorFieldName f)
                    <> ": "
                    <> fmtType (errorFieldType f)
                    <> ","
                  | Located _ f <- fields
                ]
           in [header <> " {"] ++ dropTrailingComma opts rawF ++ [ind opts n <> "};"]

fmtErrorSetDecl :: FormatOptions -> Int -> Visibility -> ErrorSetDecl -> Lines
fmtErrorSetDecl opts n vis esd =
  let nameStr = unErrorName (locValue (errorSetDeclName esd))
      header = ind opts n <> fmtVis vis <> "errorset " <> nameStr <> " {"
      rawMems =
        [ ind opts (n + 1) <> showMem (locValue m) <> ","
          | m <- errorSetDeclMembers esd
        ]
   in [header] ++ dropTrailingComma opts rawMems ++ [ind opts n <> "};"]
  where
    showMem (ErrorMemberSingle (ErrorName nm)) = nm
    showMem (ErrorMemberSet (ErrorName nm)) = nm

fmtFFIDecl :: FormatOptions -> Int -> FFIDecl -> Lines
fmtFFIDecl opts n (FFIDecl lib funcs) =
  let header = ind opts n <> "extern \"" <> lib <> "\" {"
      flines = concatMap (fmtFFIFunc opts (n + 1)) funcs
   in [header] ++ flines ++ [ind opts n <> "}"]

fmtFFIFunc :: FormatOptions -> Int -> FFIFuncDecl -> Lines
fmtFFIFunc opts n f =
  let params = T.intercalate ", " [fmtParam (locValue p) | p <- ffiFuncParams f]
      ret = fmtQType (locValue (ffiFuncReturnType f))
   in [ind opts n <> "fn " <> unFuncName (locValue (ffiFuncName f)) <> "(" <> params <> ") -> " <> ret]

-- ---------------------------------------------------------------------------
-- Types

fmtQType :: QualifiedType -> Text
fmtQType (QualifiedType Const t) = "const " <> fmtType t
fmtQType (QualifiedType Mutable t) = fmtType t

fmtType :: Type -> Text
fmtType (TypePrimitive p) = T.pack (show p)
fmtType (TypeArray (ArrayType qt)) = "[" <> fmtQType qt <> "]"
fmtType (TypeFunction ft) = fmtFuncTypeText ft
fmtType (TypeStruct (TypeName nm)) = nm
fmtType (TypeResult (ResultType s e)) =
  "orerror(" <> fmtType s <> ", " <> unErrorName e <> ")"
fmtType (TypeOption t) = "option(" <> fmtType t <> ")"
fmtType (TypeDict k v) = "dict(" <> fmtType k <> ", " <> fmtType v <> ")"
fmtType (TypeNamed (TypeName nm)) = nm
fmtType (TypeVar (TypeName nm)) = nm
fmtType (TypeTuple ts) = "(" <> T.intercalate ", " (map fmtQType ts) <> ")"

fmtFuncTypeText :: FunctionType -> Text
fmtFuncTypeText (FunctionType params ret) =
  "fn("
    <> T.intercalate ", " [fmtFTParam (locValue p) | p <- params]
    <> ") -> "
    <> fmtQType (locValue ret)
  where
    fmtFTParam p = (if paramVariadic p then "..." else "") <> fmtQType (paramType p)

-- ---------------------------------------------------------------------------
-- Statements

fmtStmt :: CommentsMap -> FormatOptions -> Int -> Located (Stmt ()) -> Lines
fmtStmt cm opts n (Located sp stmt) = case stmt of
  StmtVarDecl (Located _ vname) (Located _ qt) mInit ->
    let base = ind opts n <> unVarName vname <> ": " <> fmtQType qt
     in [base <> maybe "" (\(Located _ e) -> " = " <> fmtExpr opts n e) mInit]
  StmtAssign (Located _ lv) op (Located _ e)
    | op == AssignAdd && isLitOne e ->
        [ind opts n <> fmtLValue opts n lv <> "++"]
    | op == AssignSub && isLitOne e ->
        [ind opts n <> fmtLValue opts n lv <> "--"]
    | otherwise ->
        [ind opts n <> fmtLValue opts n lv <> " " <> assignOpSymbol op <> " " <> fmtExpr opts n e]
  StmtExpr (Located _ e) ->
    [ind opts n <> fmtExpr opts n e]
  StmtReturn Nothing ->
    [ind opts n <> "return"]
  StmtReturn (Just (Located _ e)) ->
    [ind opts n <> "return " <> fmtExpr opts n e]
  StmtBreak ->
    [ind opts n <> "break"]
  StmtContinue ->
    [ind opts n <> "continue"]
  StmtBlock blk ->
    let openL = spanLine (spanStart sp)
     in [ind opts n <> "{"] ++ blockBody cm opts n openL blk ++ [ind opts n <> "}"]
  StmtIf (Located _ cond) thenBlk mElse ->
    let openL = spanLine (spanStart sp)
     in [ind opts n <> "if (" <> fmtExpr opts n cond <> ") {"]
          ++ blockBody cm opts n openL thenBlk
          ++ fmtElse cm opts n (blockEndLine thenBlk openL) mElse
  StmtWhile (Located _ cond) body ->
    let openL = spanLine (spanStart sp)
     in [ind opts n <> "while (" <> fmtExpr opts n cond <> ") {"]
          ++ blockBody cm opts n openL body
          ++ [ind opts n <> "}"]
  StmtFor mInit mCond mUpdate body ->
    let openL = spanLine (spanStart sp)
        initTxt = maybe "" (fmtForInit opts n) mInit
        condTxt = maybe "" (\(Located _ e) -> fmtExpr opts n e) mCond
        updTxt = maybe "" (fmtForUpdate opts n) mUpdate
        header =
          ind opts n
            <> "for ("
            <> initTxt
            <> "; "
            <> condTxt
            <> "; "
            <> updTxt
            <> ") {"
     in [header] ++ blockBody cm opts n openL body ++ [ind opts n <> "}"]
  StmtMatch (Located _ expr) arms ->
    [ind opts n <> "match " <> fmtExpr opts n expr <> " {"]
      ++ concatMap (fmtMatchArm cm opts n) arms
      ++ [ind opts n <> "}"]
  StmtTupleDecl vars (Located _ qt) (Located _ e) ->
    let names = "(" <> T.intercalate ", " [unVarName v | Located _ v <- vars] <> ")"
     in [ind opts n <> names <> ": " <> fmtQType qt <> " = " <> fmtExpr opts n e]

fmtElse :: CommentsMap -> FormatOptions -> Int -> Int -> Maybe (Block ()) -> Lines
fmtElse _ opts n _ Nothing = [ind opts n <> "}"]
fmtElse cm opts n elseOpenLine (Just elseBlk) =
  case blockStmts elseBlk of
    [Located _ (StmtIf (Located _ c2) then2 mElse2)] ->
      [ind opts n <> "} else if (" <> fmtExpr opts n c2 <> ") {"]
        ++ blockBody cm opts n elseOpenLine then2
        ++ fmtElse cm opts n (blockEndLine then2 elseOpenLine) mElse2
    _ ->
      [ind opts n <> "} else {"]
        ++ blockBody cm opts n elseOpenLine elseBlk
        ++ [ind opts n <> "}"]

fmtForInit :: FormatOptions -> Int -> ForInit () -> Text
fmtForInit opts n (ForInitDecl (Located _ vn) (Located _ qt) (Located _ e)) =
  unVarName vn <> ": " <> fmtQType qt <> " = " <> fmtExpr opts n e
fmtForInit opts n (ForInitExpr (Located _ e)) = fmtExpr opts n e

-- | Inline text for the for-loop update clause (no semicolon, no indent).
fmtForUpdate :: FormatOptions -> Int -> Located (Stmt ()) -> Text
fmtForUpdate opts n (Located _ stmt) = case stmt of
  StmtAssign (Located _ lv) op (Located _ e)
    | op == AssignAdd && isLitOne e -> fmtLValue opts n lv <> "++"
    | op == AssignSub && isLitOne e -> fmtLValue opts n lv <> "--"
    | otherwise -> fmtLValue opts n lv <> " " <> assignOpSymbol op <> " " <> fmtExpr opts n e
  StmtExpr (Located _ e) -> fmtExpr opts n e
  _ ->
    let ls = fmtStmt IM.empty opts 0 (Located (error "fmtForUpdate: dummy span") stmt)
     in T.intercalate "; " ls

-- | True when the expression is the integer literal 1 (base 10).
isLitOne :: Expr () -> Bool
isLitOne (ExprLiteral (LitInt (IntLiteral BaseDec 1))) = True
isLitOne _ = False

fmtMatchArm :: CommentsMap -> FormatOptions -> Int -> MatchArm () -> Lines
fmtMatchArm cm opts n (MatchArm pat body) =
  let patTxt = fmtMatchPat opts n pat
      bodyLines = appendSemi (fmtStmt cm opts n body)
      prefix = ind opts n <> patTxt <> " => "
      indLen = T.length (ind opts n)
   in case bodyLines of
        [] -> [prefix <> ";"]
        (first : rest) ->
          (prefix <> T.drop indLen first) : rest

fmtMatchPat :: FormatOptions -> Int -> MatchPattern () -> Text
fmtMatchPat _ _ (MatchOk (Located _ v)) = "ok(" <> unVarName v <> ")"
fmtMatchPat _ _ (MatchErr (Located _ e) (Located _ v)) =
  "err(" <> unErrorName e <> " " <> unVarName v <> ")"
fmtMatchPat _ _ (MatchSome (Located _ v)) = "some(" <> unVarName v <> ")"
fmtMatchPat _ _ MatchNone = "none"
fmtMatchPat opts n (MatchLit (Located _ e)) = fmtExpr opts n e
fmtMatchPat opts n (MatchRange (Located _ lo) (Located _ hi)) =
  fmtExpr opts n lo <> ".." <> fmtExpr opts n hi
fmtMatchPat _ _ MatchWildcard = "_"
fmtMatchPat _ _ (MatchTuple vars) =
  "(" <> T.intercalate ", " [unVarName v | Located _ v <- vars] <> ")"

-- ---------------------------------------------------------------------------
-- LValues

fmtLValue :: FormatOptions -> Int -> LValue () -> Text
fmtLValue _ _ (LVarRef (Located _ v)) = unVarName v
fmtLValue opts n (LArrayIndex (Located _ lv) (Located _ e)) =
  fmtLValue opts n lv <> "[" <> fmtExpr opts n e <> "]"
fmtLValue opts n (LFieldAccess (Located _ lv) (Located _ f)) =
  fmtLValue opts n lv <> "." <> unFieldName f

-- ---------------------------------------------------------------------------
-- Expressions

fmtExpr :: FormatOptions -> Int -> Expr () -> Text
fmtExpr opts _ (ExprLiteral lit) = fmtLit opts lit
fmtExpr _ _ (ExprVar (Located _ v)) = unVarName v
fmtExpr opts n (ExprBinary op (Located _ l) (Located _ r)) =
  fmtExpr opts n l <> " " <> binaryOpSymbol op <> " " <> fmtExpr opts n r
fmtExpr opts n (ExprUnary op (Located _ e)) =
  unaryOpSymbol op <> fmtExpr opts n e
fmtExpr opts n (ExprCall (Located _ fname) args) =
  unFuncName fname
    <> "("
    <> T.intercalate ", " [fmtExpr opts n (locValue a) | a <- args]
    <> ")"
fmtExpr opts n (ExprIndex (Located _ arr) (Located _ idx)) =
  fmtExpr opts n arr <> "[" <> fmtExpr opts n idx <> "]"
fmtExpr opts n (ExprField (Located _ e) (Located _ f)) =
  fmtExpr opts n e <> "." <> unFieldName f
fmtExpr opts n (ExprStructInit (Located _ tname) fields) =
  unTypeName tname
    <> " { "
    <> T.intercalate
      ", "
      [unFieldName (locValue fn) <> ": " <> fmtExpr opts n (locValue e) | (fn, e) <- fields]
    <> " }"
fmtExpr opts n (ExprArrayInit (Located _ t) elems) =
  "["
    <> fmtType t
    <> "]("
    <> T.intercalate ", " [fmtExpr opts n (locValue e) | e <- elems]
    <> ")"
fmtExpr opts n (ExprDictLit pairs) =
  if null pairs
    then "{}"
    else
      "{ "
        <> T.intercalate
          ", "
          [fmtExpr opts n (locValue k) <> ": " <> fmtExpr opts n (locValue v) | (k, v) <- pairs]
        <> " }"
fmtExpr opts n (ExprError (Located _ ename) fields) =
  "error "
    <> unErrorName ename
    <> if null fields
      then ""
      else
        " { "
          <> T.intercalate
            ", "
            [unFieldName (locValue fn) <> ": " <> fmtExpr opts n (locValue e) | (fn, e) <- fields]
          <> " }"
fmtExpr opts n (ExprTry (Located _ e)) = "try " <> fmtExpr opts n e
fmtExpr opts n (ExprMust (Located _ e)) = "must " <> fmtExpr opts n e
fmtExpr opts n (ExprSome (Located _ e)) = "some(" <> fmtExpr opts n e <> ")"
fmtExpr _ _ ExprNone = "none"
fmtExpr opts _ (ExprLambda params ret body) =
  let ps = T.intercalate ", " [fmtParam (locValue p) | p <- params]
      r = fmtQType (locValue ret)
      bodyTxt =
        T.intercalate " " $
          concatMap (appendSemi . fmtStmt IM.empty opts 0) (blockStmts body)
   in "fn(" <> ps <> ") -> " <> r <> " { " <> bodyTxt <> " }"
fmtExpr opts n (ExprParen (Located _ e)) = "(" <> fmtExpr opts n e <> ")"
fmtExpr opts n (ExprCast (Located _ e) (Located _ t)) =
  fmtType t <> "(" <> fmtExpr opts n e <> ")"
fmtExpr opts n (ExprTupleInit elems) =
  "(" <> T.intercalate ", " [fmtExpr opts n (locValue e) | e <- elems] <> ")"

-- ---------------------------------------------------------------------------
-- Literals

fmtLit :: FormatOptions -> Literal (Located (Expr ())) -> Text
fmtLit _ (LitInt (IntLiteral base v)) = fmtInt base v
fmtLit _ (LitFloat (FloatLiteral v)) = fmtFloat v
fmtLit _ (LitString (StringLiteral s)) = "\"" <> escStr s <> "\""
fmtLit _ (LitBool (BoolLiteral b)) = if b then "true" else "false"
fmtLit opts (LitArray (ArrayLiteral elems)) =
  "[" <> T.intercalate ", " [fmtExpr opts 0 (locValue e) | e <- elems] <> "]"

fmtInt :: IntBase -> Integer -> Text
fmtInt BaseDec v = T.pack (show v)
fmtInt BaseHex v
  | v < 0 = "-0x" <> T.pack (showHex (-v) "")
  | otherwise = "0x" <> T.pack (showHex v "")
fmtInt BaseOct v
  | v < 0 = "-0o" <> T.pack (showOct (-v) "")
  | otherwise = "0o" <> T.pack (showOct v "")
fmtInt BaseBin v
  | v < 0 = "-0b" <> showBin (-v)
  | otherwise = "0b" <> showBin v

showBin :: Integer -> Text
showBin 0 = "0"
showBin n = T.pack (go n "")
  where
    go 0 acc = acc
    go x acc = go (x `div` 2) ((if even x then '0' else '1') : acc)

fmtFloat :: Double -> Text
fmtFloat v = T.pack (show v)

escStr :: Text -> Text
escStr = T.concatMap esc
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c = T.singleton c
