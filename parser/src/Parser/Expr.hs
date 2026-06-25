module Parser.Expr where

import AST.Types.AST
  ( Expr (..),
  )
import AST.Types.Common (FieldName (..), FuncName (..), Located (..), SourceSpan, TypeName (..), VarName (..), getSpan, unLocated)
import AST.Types.Literal (Literal (..), StringLiteral (..))
import AST.Types.Operator (binaryOpPrecedence)
import AST.Types.Type (Type (..))
import Data.List (foldl')
import qualified Data.Text as T
import Parser.Literal (parseLiteral)
import Parser.Operator (parseBinaryOp, parseUnaryOp)
import Parser.Type (parsePrimitiveType)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    isInterpChunk,
    isInterpEnd,
    matchKeyword,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseExprLiteral :: TokenParser (Located (Expr ann))
parseExprLiteral = do
  lit <- parseLiteral parseExpr
  return $ Located (getSpan lit) (ExprLiteral (unLocated lit))

-- | Parse an identifier, splitting @a.b.c@ tokens into nested @ExprField@
-- nodes (the lexer combines dotted names into a single token).
parseExprVar :: TokenParser (Located (Expr ann))
parseExprVar = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  case T.splitOn "." name of
    [single] -> return $ Located span (ExprVar (Located span (VarName single)))
    (base : fields) ->
      let baseExpr = Located span (ExprVar (Located span (VarName base)))
       in return $
            foldl'
              (\e f -> Located span (ExprField e (Located span (FieldName f))))
              baseExpr
              fields
    _ -> return $ Located span (ExprVar (Located span (VarName name)))

parseExprCall :: TokenParser (Located (Expr ann))
parseExprCall = do
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol "("
  args <- MP.sepBy parseExpr (matchSymbol ",")
  Located endSpan _ <- matchSymbol ")"
  return $ Located (nameSpan <> endSpan) (ExprCall (Located nameSpan (FuncName name)) args)

-- | Parse a qualified function call of the form @module.function(args)@.
-- Produces an @ExprCall@ with name @"module.function"@.
parseExprDottedCall :: TokenParser (Located (Expr ann))
parseExprDottedCall = do
  Located modSpan (TokIdentifier modName) <- MP.satisfy isIdentifier
  _ <- matchSymbol "."
  Located fnSpan (TokIdentifier fnName) <- MP.satisfy isIdentifier
  _ <- matchSymbol "("
  args <- MP.sepBy parseExpr (matchSymbol ",")
  Located endSpan _ <- matchSymbol ")"
  let qualName = FuncName (modName <> T.singleton '.' <> fnName)
  return $ Located (modSpan <> endSpan) (ExprCall (Located (modSpan <> fnSpan) qualName) args)

-- | Parse a struct initialiser: @TypeName { field: expr, ... }@
parseExprStructInit :: TokenParser (Located (Expr ann))
parseExprStructInit = do
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol "{"
  fields <- MP.sepEndBy parseFieldInit (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  return $
    Located
      (nameSpan <> endSpan)
      (ExprStructInit (Located nameSpan (TypeName name)) fields)
  where
    parseFieldInit = do
      Located fnSpan (TokIdentifier fname) <- MP.satisfy isIdentifier
      _ <- matchSymbol ":"
      expr <- parseExpr
      return (Located fnSpan (FieldName fname), expr)

-- | Parse a primary expression followed by zero or more @[i]@ or @.field@ suffixes.
parseExprAccessChain :: TokenParser (Located (Expr ann))
parseExprAccessChain = do
  base <- parseExprVar MP.<|> parseExprParen
  suffixes <- MP.many (MP.try parseFieldSuffix MP.<|> parseIndexSuffix)
  return $ foldl (\e f -> f e) base suffixes
  where
    parseFieldSuffix = do
      _ <- matchSymbol "."
      Located fieldSpan (TokIdentifier fname) <- MP.satisfy isIdentifier
      return $ \e ->
        Located (getSpan e <> fieldSpan) (ExprField e (Located fieldSpan (FieldName fname)))
    parseIndexSuffix = do
      Located startSpan _ <- matchSymbol "["
      i <- parseExpr
      Located endSpan _ <- matchSymbol "]"
      return $ \e ->
        Located (getSpan e <> startSpan <> getSpan i <> endSpan) (ExprIndex e i)

-- | Parse a backtick interpolated string: \`text {expr} text\`
-- Desugars to nested @string.concat@ calls with @string.to_str@ wrapping each
-- interpolated expression, so no new AST node or VM instruction is needed.
parseExprInterp :: TokenParser (Located (Expr ann))
parseExprInterp = do
  Located startSpan (TokInterpChunk firstText) <- MP.satisfy isInterpChunk
  parts <- collectParts
  let allParts = mkTextPart startSpan firstText ++ parts
  return $ Located startSpan (buildConcat startSpan allParts)
  where
    collectParts :: TokenParser [Located (Expr ann)]
    collectParts = do
      mEnd <- MP.optional (MP.satisfy isInterpEnd)
      case mEnd of
        Just _ -> return []
        Nothing -> do
          _ <- matchSymbol "{"
          expr <- parseExpr
          _ <- matchSymbol "}"
          Located chunkSpan (TokInterpChunk text) <- MP.satisfy isInterpChunk
          let exprStr = wrapToStr expr
              textParts = mkTextPart chunkSpan text
          rest <- collectParts
          return (exprStr : textParts ++ rest)

    mkTextPart :: SourceSpan -> T.Text -> [Located (Expr ann)]
    mkTextPart sp text
      | T.null text = []
      | otherwise = [Located sp (ExprLiteral (LitString (StringLiteral text)))]

    wrapToStr :: Located (Expr ann) -> Located (Expr ann)
    wrapToStr expr =
      let sp = getSpan expr
       in Located sp (ExprCall (Located sp (FuncName "string.to_str")) [expr])

    buildConcat :: SourceSpan -> [Located (Expr ann)] -> Expr ann
    buildConcat _ [] = ExprLiteral (LitString (StringLiteral ""))
    buildConcat _ [x] = unLocated x
    buildConcat _ (x : xs) = unLocated (foldl' mkConcat x xs)

    mkConcat :: Located (Expr ann) -> Located (Expr ann) -> Located (Expr ann)
    mkConcat l r =
      let sp = getSpan l <> getSpan r
       in Located sp (ExprCall (Located sp (FuncName "string.concat")) [l, r])

parseExprCast :: TokenParser (Located (Expr ann))
parseExprCast = do
  Located startSpan primitiv <- parsePrimitiveType
  _ <- matchSymbol "("
  expr <- parseExpr
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> endSpan) (ExprCast expr (Located startSpan (TypePrimitive primitiv)))

parseExprParen :: TokenParser (Located (Expr ann))
parseExprParen = do
  Located startSpan _ <- matchSymbol "("
  expr <- parseExpr
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> endSpan) (ExprParen expr)

parseExprMust :: TokenParser (Located (Expr ann))
parseExprMust = do
  Located startSpan _ <- matchKeyword "must"
  expr <- parseExpr
  return $ Located (startSpan <> getSpan expr) (ExprMust expr)

parsePrimary :: TokenParser (Located (Expr ann))
parsePrimary =
  MP.choice
    [ parseExprInterp,
      parseExprLiteral,
      MP.try parseExprDottedCall,
      MP.try parseExprCall,
      parseExprCast,
      parseExprParen,
      MP.try parseExprStructInit,
      parseExprAccessChain
    ]

parseUnary :: TokenParser (Located (Expr ann))
parseUnary =
  MP.choice
    [ parseExprMust,
      do
        op <- parseUnaryOp
        expr <- parseUnary
        let combinedSpan = getSpan op <> getSpan expr
        return $ Located combinedSpan (ExprUnary (unLocated op) expr),
      parsePrimary
    ]

parseBinary :: Int -> TokenParser (Located (Expr ann))
parseBinary minPrec = do
  left <- parseUnary
  parseBinaryRHS minPrec left
  where
    parseBinaryRHS :: Int -> Located (Expr ann) -> TokenParser (Located (Expr ann))
    parseBinaryRHS minPrec' left = do
      -- Peek first so we never consume an op we won't use.
      maybeOp <- MP.optional (MP.try (MP.lookAhead parseBinaryOp))
      case maybeOp of
        Nothing -> return left
        Just op -> do
          let prec = binaryOpPrecedence (unLocated op)
          if prec < minPrec'
            then return left
            else do
              _ <- parseBinaryOp -- now consume for real
              right <- parseBinary (prec + 1) -- Left-associative
              let combinedSpan = getSpan left <> getSpan op <> getSpan right
              let newExpr = Located combinedSpan (ExprBinary (unLocated op) left right)
              parseBinaryRHS minPrec' newExpr

parseExpr :: TokenParser (Located (Expr ann))
parseExpr = parseBinary 0
