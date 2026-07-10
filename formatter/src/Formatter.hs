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
    Located (..),
    ModuleName (..),
    TypeName (..),
    VarName (..),
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

-- | Format all statements in a block at indent level n+1 with terminators.
blockBody :: FormatOptions -> Int -> Block () -> Lines
blockBody opts n blk =
  concatMap (appendSemi . fmtStmt opts (n + 1)) (blockStmts blk)

-- | Remove trailing comma from the last line unless optTrailingComma.
dropTrailingComma :: FormatOptions -> Lines -> Lines
dropTrailingComma _ [] = []
dropTrailingComma opts ls
  | optTrailingComma opts = ls
  | otherwise = init ls ++ [T.dropEnd 1 (last ls)]

-- ---------------------------------------------------------------------------
-- Program

formatProgram :: FormatOptions -> Program () -> Text
formatProgram opts prog =
  let decls = map locValue (programDecls prog)
      (imps, others) = partition isImport decls
      sorted =
        if optReorderImports opts
          then sortBy (comparing importKey) imps
          else imps
      impLines = map (fmtImport . getImport) sorted
      otherBlocks = map (fmtDecl opts 0) others
      sections = filter (not . null) $ impLines : otherBlocks
   in T.unlines (intercalate [""] sections)

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

fmtDecl :: FormatOptions -> Int -> Decl () -> Lines
fmtDecl opts n (DeclFunction vis fd) = fmtFuncDecl opts n vis fd
fmtDecl opts n (DeclStruct vis sd) = fmtStructDecl opts n vis sd
fmtDecl _ _ (DeclImport _) = []
fmtDecl opts n (DeclError vis ed) = fmtErrorDecl opts n vis ed
fmtDecl opts n (DeclErrorSet vis esd) = fmtErrorSetDecl opts n vis esd
fmtDecl opts n (DeclFFI ffi) = fmtFFIDecl opts n ffi

fmtVis :: Visibility -> Text
fmtVis Public = ""
fmtVis Static = "static "

fmtFuncDecl :: FormatOptions -> Int -> Visibility -> FunctionDecl () -> Lines
fmtFuncDecl opts n vis fd =
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
   in [sig] ++ blockBody opts n (funcDeclBody fd) ++ [ind opts n <> "}"]

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

fmtStmt :: FormatOptions -> Int -> Located (Stmt ()) -> Lines
fmtStmt opts n (Located _ stmt) = case stmt of
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
    [ind opts n <> "{"] ++ blockBody opts n blk ++ [ind opts n <> "}"]
  StmtIf (Located _ cond) thenBlk mElse ->
    [ind opts n <> "if (" <> fmtExpr opts n cond <> ") {"]
      ++ blockBody opts n thenBlk
      ++ fmtElse opts n mElse
  StmtWhile (Located _ cond) body ->
    [ind opts n <> "while (" <> fmtExpr opts n cond <> ") {"]
      ++ blockBody opts n body
      ++ [ind opts n <> "}"]
  StmtFor mInit mCond mUpdate body ->
    let initTxt = maybe "" (fmtForInit opts n) mInit
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
     in [header] ++ blockBody opts n body ++ [ind opts n <> "}"]
  StmtMatch (Located _ expr) arms ->
    [ind opts n <> "match " <> fmtExpr opts n expr <> " {"]
      ++ concatMap (fmtMatchArm opts n) arms
      ++ [ind opts n <> "}"]

fmtElse :: FormatOptions -> Int -> Maybe (Block ()) -> Lines
fmtElse opts n Nothing = [ind opts n <> "}"]
fmtElse opts n (Just elseBlk) =
  case blockStmts elseBlk of
    [Located _ (StmtIf (Located _ c2) then2 mElse2)] ->
      [ind opts n <> "} else if (" <> fmtExpr opts n c2 <> ") {"]
        ++ blockBody opts n then2
        ++ fmtElse opts n mElse2
    _ ->
      [ind opts n <> "} else {"]
        ++ blockBody opts n elseBlk
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
    let ls = fmtStmt opts 0 (Located (error "fmtForUpdate: dummy span") stmt)
     in T.intercalate "; " ls

-- | True when the expression is the integer literal 1 (base 10).
isLitOne :: Expr () -> Bool
isLitOne (ExprLiteral (LitInt (IntLiteral BaseDec 1))) = True
isLitOne _ = False

fmtMatchArm :: FormatOptions -> Int -> MatchArm () -> Lines
fmtMatchArm opts n (MatchArm pat body) =
  let patTxt = fmtMatchPat opts n pat
      bodyLines = appendSemi (fmtStmt opts n body)
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
          concatMap (appendSemi . fmtStmt opts 0) (blockStmts body)
   in "fn(" <> ps <> ") -> " <> r <> " { " <> bodyTxt <> " }"
fmtExpr opts n (ExprParen (Located _ e)) = "(" <> fmtExpr opts n e <> ")"
fmtExpr opts n (ExprCast (Located _ e) (Located _ t)) =
  fmtType t <> "(" <> fmtExpr opts n e <> ")"

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
