module Parser.LValue where

import AST.Types.AST (LValue (LArrayIndex, LFieldAccess, LVarRef))
import AST.Types.Common (FieldName (..), Located (..), VarName (..))
import Data.List (foldl')
import qualified Data.Text as T
import Parser.Expr (parseExpr)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchSymbol,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

-- | Parse an identifier lvalue, splitting @a.b.c@ tokens into nested
-- @LFieldAccess@ nodes (the lexer combines dotted names into a single token).
parseLVarRef :: TokenParser (Located (LValue ann))
parseLVarRef = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  case T.splitOn "." name of
    [single] -> return $ Located span (LVarRef (Located span (VarName single)))
    (base : fields) ->
      let baseLV = Located span (LVarRef (Located span (VarName base)))
       in return $
            foldl'
              (\lv f -> Located span (LFieldAccess lv (Located span (FieldName f))))
              baseLV
              fields
    _ -> return $ Located span (LVarRef (Located span (VarName name)))

parseLArrayIndex :: Located (LValue ann) -> TokenParser (Located (LValue ann))
parseLArrayIndex base = do
  _ <- matchSymbol "["
  index <- parseExpr
  Located endSpan _ <- matchSymbol "]"
  let Located baseSpan _ = base
  return $ Located (baseSpan <> endSpan) (LArrayIndex base index)

parseLFieldAccess :: Located (LValue ann) -> TokenParser (Located (LValue ann))
parseLFieldAccess base = do
  _ <- matchSymbol "."
  Located fieldSpan (TokIdentifier fname) <- MP.satisfy isIdentifier
  let Located baseSpan _ = base
  return $ Located (baseSpan <> fieldSpan) (LFieldAccess base (Located fieldSpan (FieldName fname)))

parseLValue :: TokenParser (Located (LValue ann))
parseLValue =
  MP.choice
    [ do
        base <- parseLVarRef
        parseLValueSuffixes base,
      parseLVarRef
    ]
  where
    parseLValueSuffixes lvalue = do
      maybeSuffix <-
        MP.optional $
          MP.try (parseLFieldAccess lvalue) MP.<|> parseLArrayIndex lvalue
      case maybeSuffix of
        Just newLValue -> parseLValueSuffixes newLValue
        Nothing -> return lvalue
