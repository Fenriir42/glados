module Parser.Stmt where

import AST.Types.AST
  ( Block (Block),
    Expr (..),
    ForInit (..),
    MatchArm (..),
    MatchPattern (..),
    Stmt (..),
  )
import AST.Types.Common (ErrorName (..), FieldName (..), Located (..), SourceSpan (..), TypeName (..), VarName (..), getSpan)
import AST.Types.Literal (IntBase (..), IntLiteral (..), Literal (..))
import AST.Types.Operator (AssignOp (AssignAdd, AssignSub))
import Parser.Expr (parseExpr)
import Parser.LValue (parseLValue)
import Parser.Operator (parseAssignOp)
import Parser.Type (parseQualifiedType)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchKeyword,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseBlock :: TokenParser (Located (Block ann))
parseBlock = do
  Located startSpan _ <- matchSymbol "{"
  stmts <- MP.endBy parseStmt (matchSymbol ";")
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = startSpan <> endSpan
  return $ Located combinedSpan (Block combinedSpan stmts)

parseStmtAssign :: TokenParser (Located (Stmt ann))
parseStmtAssign = do
  Located lvalueSpan lvalue <- parseLValue
  Located opSpan assignOp <- parseAssignOp
  Located exprSpan expr <- parseExpr
  let combinedSpan = lvalueSpan <> opSpan <> exprSpan
  return $ Located combinedSpan (StmtAssign (Located lvalueSpan lvalue) assignOp (Located exprSpan expr))

parseStmtVarDecl :: TokenParser (Located (Stmt ann))
parseStmtVarDecl = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  Located colonSpan _ <- matchSymbol ":"
  Located typeSpan qtype <- parseQualifiedType
  assignment <- MP.optional $ do
    Located assignSpan _ <- matchSymbol "="
    Located exprSpan expr <- parseExpr
    return (Located assignSpan (), Located exprSpan expr)
  case assignment of
    Just (Located assignSpan _, Located exprSpan expr) ->
      let combinedSpan = span <> colonSpan <> typeSpan <> assignSpan <> exprSpan
       in return $ Located combinedSpan (StmtVarDecl (Located span (VarName name)) (Located typeSpan qtype) (Just (Located exprSpan expr)))
    Nothing ->
      let combinedSpan = span <> colonSpan <> typeSpan
       in return $ Located combinedSpan (StmtVarDecl (Located span (VarName name)) (Located typeSpan qtype) Nothing)

parseStmtExpr :: TokenParser (Located (Stmt ann))
parseStmtExpr = do
  Located exprSpan expr <- parseExpr
  return $ Located exprSpan (StmtExpr (Located exprSpan expr))

parseStmtReturn :: TokenParser (Located (Stmt ann))
parseStmtReturn = do
  Located span _ <- matchKeyword "return"
  maybeExpr <- MP.optional parseExpr
  case maybeExpr of
    Just (Located exprSpan expr) ->
      let combinedSpan = span <> exprSpan
       in return $ Located combinedSpan (StmtReturn (Just (Located exprSpan expr)))
    Nothing ->
      return $ Located span (StmtReturn Nothing)

parseStmtBreak :: TokenParser (Located (Stmt ann))
parseStmtBreak = do
  Located span _ <- matchKeyword "break"
  return $ Located span StmtBreak

parseStmtContinue :: TokenParser (Located (Stmt ann))
parseStmtContinue = do
  Located span _ <- matchKeyword "continue"
  return $ Located span StmtContinue

parseStmtBlock :: TokenParser (Located (Stmt ann))
parseStmtBlock = do
  Located span block <- parseBlock
  return $ Located span (StmtBlock block)

parseStmtWhile :: TokenParser (Located (Stmt ann))
parseStmtWhile = do
  Located whileSpan _ <- matchKeyword "while"
  Located lpan _ <- matchSymbol "("
  Located condSpan condExpr <- parseExpr
  Located rpan _ <- matchSymbol ")"
  Located bodySpan bodyBlock <- parseBlock
  let combinedSpan = whileSpan <> lpan <> condSpan <> rpan <> bodySpan
  return $ Located combinedSpan (StmtWhile (Located condSpan condExpr) bodyBlock)

parseForInit :: TokenParser (ForInit ann)
parseForInit = do
  MP.choice
    [ do
        Located _ var <- parseStmtVarDecl
        case var of
          StmtVarDecl name qtype (Just initExpr) ->
            return $ ForInitDecl name qtype initExpr
          _ -> fail "Expected variable declaration with initializer in for loop",
      do
        Located exprSpan expr <- parseExpr
        return $ ForInitExpr (Located exprSpan expr)
    ]

parseStmtFor :: TokenParser (Located (Stmt ann))
parseStmtFor = do
  Located forSpan _ <- matchKeyword "for"
  Located lpanSpan _ <- matchSymbol "("
  maybeInit <- MP.optional parseForInit
  Located semi1Span _ <- matchSymbol ";"
  maybeCond <- MP.optional parseExpr
  Located semi2Span _ <- matchSymbol ";"
  maybePost <- MP.optional parseStmt
  Located rpanSpan _ <- matchSymbol ")"
  Located bodySpan bodyBlock <- parseBlock
  let combinedSpan = forSpan <> lpanSpan <> semi1Span <> semi2Span <> rpanSpan <> bodySpan
  return $ Located combinedSpan (StmtFor maybeInit maybeCond maybePost bodyBlock)

parseStmtIf :: TokenParser (Located (Stmt ann))
parseStmtIf = do
  Located ifSpan _ <- matchKeyword "if"
  Located lpanSpan _ <- matchSymbol "("
  Located condSpan condExpr <- parseExpr
  Located rpanSpan _ <- matchSymbol ")"
  Located thenSpan thenBlock <- parseBlock

  maybeElse <- MP.optional parseElseChain

  case maybeElse of
    Just (elseSpan, elseStmt) ->
      let combinedSpan = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan <> elseSpan
       in return $ Located combinedSpan (StmtIf (Located condSpan condExpr) thenBlock (Just elseStmt))
    Nothing ->
      let combinedSpan = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan
       in return $ Located combinedSpan (StmtIf (Located condSpan condExpr) thenBlock Nothing)

-- Parse the else chain (either "else if" or "else")
parseElseChain :: TokenParser (SourceSpan, Block ann)
parseElseChain = do
  Located elseSpan _ <- matchKeyword "else"
  maybeIf <- MP.optional (matchKeyword "if")
  case maybeIf of
    Just (Located ifSpan _) -> do
      Located lpanSpan _ <- matchSymbol "("
      Located condSpan condExpr <- parseExpr
      Located rpanSpan _ <- matchSymbol ")"
      Located thenSpan thenBlock <- parseBlock

      maybeNextElse <- MP.optional parseElseChain

      let (totalSpan, stmt) = case maybeNextElse of
            Just (nextElseSpan, nextElseStmt) ->
              let span = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan <> nextElseSpan
               in (elseSpan <> span, StmtIf (Located condSpan condExpr) thenBlock (Just nextElseStmt))
            Nothing ->
              let span = ifSpan <> lpanSpan <> condSpan <> rpanSpan <> thenSpan
               in (elseSpan <> span, StmtIf (Located condSpan condExpr) thenBlock Nothing)
      return (totalSpan, Block totalSpan [Located totalSpan stmt])
    Nothing -> do
      Located elseBlockSpan elseBlock <- parseBlock
      return (elseSpan <> elseBlockSpan, elseBlock)

isOkIdent :: Located TokenContent -> Bool
isOkIdent (Located _ (TokIdentifier "ok")) = True
isOkIdent _ = False

isErrIdent :: Located TokenContent -> Bool
isErrIdent (Located _ (TokIdentifier "err")) = True
isErrIdent _ = False

isWildcardIdent :: Located TokenContent -> Bool
isWildcardIdent (Located _ (TokIdentifier "_")) = True
isWildcardIdent _ = False

isSomePat :: Located TokenContent -> Bool
isSomePat (Located _ (TokIdentifier "some")) = True
isSomePat _ = False

isNonePat :: Located TokenContent -> Bool
isNonePat (Located _ (TokIdentifier "none")) = True
isNonePat _ = False

parseMatchPattern :: TokenParser (MatchPattern ann)
parseMatchPattern =
  MP.choice
    [ -- ok(v)
      MP.try $ do
        _ <- MP.satisfy isOkIdent
        _ <- matchSymbol "("
        Located vspan (TokIdentifier v) <- MP.satisfy isIdentifier
        _ <- matchSymbol ")"
        return (MatchOk (Located vspan (VarName v))),
      -- err(ErrorName v)
      MP.try $ do
        _ <- MP.satisfy isErrIdent
        _ <- matchSymbol "("
        Located espan (TokIdentifier ename) <- MP.satisfy isIdentifier
        Located vspan (TokIdentifier v) <- MP.satisfy isIdentifier
        _ <- matchSymbol ")"
        return (MatchErr (Located espan (ErrorName ename)) (Located vspan (VarName v))),
      -- some(v)
      MP.try $ do
        _ <- MP.satisfy isSomePat
        _ <- matchSymbol "("
        Located vspan (TokIdentifier v) <- MP.satisfy isIdentifier
        _ <- matchSymbol ")"
        return (MatchSome (Located vspan (VarName v))),
      -- none
      MP.try $ do
        _ <- MP.satisfy isNonePat
        return MatchNone,
      -- range: expr..expr  (must be tried before MatchLit)
      MP.try $ do
        lo <- parseExpr
        _ <- matchSymbol ".."
        MatchRange lo <$> parseExpr,
      -- wildcard: _
      MP.try $ do
        _ <- MP.satisfy isWildcardIdent
        return MatchWildcard,
      -- tuple destructure: (x, y)
      MP.try $ do
        _ <- matchSymbol "("
        vars <-
          MP.sepBy1
            ( do
                Located vspan (TokIdentifier v) <- MP.satisfy isIdentifier
                return (Located vspan (VarName v))
            )
            (matchSymbol ",")
        _ <- matchSymbol ")"
        case vars of
          [_] -> fail "single-variable tuple pattern not supported"
          _ -> return (MatchTuple vars),
      -- struct destructure: { x, y }
      MP.try $ do
        _ <- matchSymbol "{"
        fields <-
          MP.sepBy1
            ( do
                Located fspan (TokIdentifier f) <- MP.satisfy isIdentifier
                return (Located fspan (FieldName f))
            )
            (matchSymbol ",")
        _ <- matchSymbol "}"
        return (MatchStruct fields),
      -- enum variant pattern: Direction.North
      MP.try $ do
        Located tspan (TokIdentifier tname) <- MP.satisfy isIdentifier
        _ <- matchSymbol "."
        Located vspan (TokIdentifier vname) <- MP.satisfy isIdentifier
        return (MatchEnumVariant (Located tspan (TypeName tname)) (Located vspan (TypeName vname))),
      -- literal / expression
      MatchLit <$> parseExpr
    ]

parseMatchArm :: TokenParser (MatchArm ann)
parseMatchArm = do
  pat <- parseMatchPattern
  _ <- matchSymbol "=>"
  MatchArm pat <$> parseStmt

parseStmtMatch :: TokenParser (Located (Stmt ann))
parseStmtMatch = do
  Located matchSpan _ <- matchKeyword "match"
  subj <- parseExpr
  _ <- matchSymbol "{"
  arms <- MP.endBy parseMatchArm (matchSymbol ";")
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = matchSpan <> endSpan
  return $ Located combinedSpan (StmtMatch subj arms)

-- | Parse a struct destructuring declaration: @{ x, y }: Point = expr@
parseStmtStructDecl :: TokenParser (Located (Stmt ann))
parseStmtStructDecl = MP.try $ do
  Located startSpan _ <- matchSymbol "{"
  fields <-
    MP.sepBy1
      ( do
          Located fspan (TokIdentifier f) <- MP.satisfy isIdentifier
          return (Located fspan (FieldName f))
      )
      (matchSymbol ",")
  _ <- matchSymbol "}"
  _ <- matchSymbol ":"
  Located qtSpan qt <- parseQualifiedType
  _ <- matchSymbol "="
  Located exprSpan initExpr <- parseExpr
  let combinedSpan = startSpan <> qtSpan <> exprSpan
  return $
    Located
      combinedSpan
      (StmtStructDecl fields (Located qtSpan qt) (Located exprSpan initExpr))

-- | Parse a tuple destructuring declaration: @(x, y): (int, str) = expr@
parseStmtTupleDecl :: TokenParser (Located (Stmt ann))
parseStmtTupleDecl = MP.try $ do
  Located startSpan _ <- matchSymbol "("
  vars <-
    MP.sepBy1
      ( do
          Located vspan (TokIdentifier v) <- MP.satisfy isIdentifier
          return (Located vspan (VarName v))
      )
      (matchSymbol ",")
  _ <- matchSymbol ")"
  _ <- matchSymbol ":"
  Located qtSpan qt <- parseQualifiedType
  _ <- matchSymbol "="
  Located exprSpan initExpr <- parseExpr
  let combinedSpan = startSpan <> qtSpan <> exprSpan
  case vars of
    [_] -> fail "single-variable tuple decl not supported"
    _ ->
      return $
        Located
          combinedSpan
          (StmtTupleDecl vars (Located qtSpan qt) (Located exprSpan initExpr))

parseStmt :: TokenParser (Located (Stmt ann))
parseStmt =
  MP.choice
    [ MP.try parseStmtStructDecl,
      MP.try parseStmtTupleDecl,
      MP.try parseStmtVarDecl,
      MP.try parseStmtAssign,
      MP.try $ do
        expr <- parseLValue
        Located span _ <- matchSymbol "++"
        let combinedSpan = getSpan expr <> span
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignAdd litOne),
      MP.try $ do
        expr <- parseLValue
        Located span _ <- matchSymbol "--"
        let combinedSpan = getSpan expr <> span
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignSub litOne),
      do
        Located span _ <- matchSymbol "++"
        expr <- parseLValue
        let combinedSpan = span <> getSpan expr
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignAdd litOne),
      do
        Located span _ <- matchSymbol "--"
        expr <- parseLValue
        let combinedSpan = span <> getSpan expr
            litOne = Located span (ExprLiteral (LitInt (IntLiteral BaseDec 1)))
        return $ Located combinedSpan (StmtAssign expr AssignSub litOne),
      parseStmtExpr,
      parseStmtReturn,
      parseStmtBreak,
      parseStmtContinue,
      parseStmtBlock,
      parseStmtWhile,
      parseStmtFor,
      parseStmtIf,
      parseStmtMatch
    ]
