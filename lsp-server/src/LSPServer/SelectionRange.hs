module LSPServer.SelectionRange (findSelectionRange) where

import AST.Types.Common (FuncName, SourceSpan)
import AST.Types.Type (FunctionType, Type)
import Data.List (nubBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import LSPServer.Span (containsPos, spanLen, spanToRange)
import qualified Language.LSP.Protocol.Types as LSP

-- | Build an expand-selection chain for the given cursor position.
-- Spans are ordered innermost -> outermost via the `_parent` chain.
-- Sources: expression spans (tcTypes), block spans (foldingRanges),
--          function full-body spans (funcSymbols).
findSelectionRange ::
  Map SourceSpan Type ->
  [SourceSpan] ->
  [(FuncName, FunctionType, SourceSpan, SourceSpan)] ->
  Int ->
  Int ->
  Maybe LSP.SelectionRange
findSelectionRange exprTypes blockSpans funcSymbols lspLine lspCol =
  case chain of
    [] -> Nothing
    sps -> Just (buildChain sps)
  where
    allSpans =
      Map.keys exprTypes
        ++ blockSpans
        ++ [fullSpan | (_, _, _, fullSpan) <- funcSymbols]

    chain =
      nubBy (\a b -> spanToRange a == spanToRange b)
        . sortBy (comparing spanLen)
        . filter (containsPos lspLine lspCol)
        $ allSpans

-- | Build a linked chain from innermost to outermost.
-- [small, medium, large] ->
--   SR small (Just (SR medium (Just (SR large Nothing))))
buildChain :: [SourceSpan] -> LSP.SelectionRange
buildChain sps =
  foldr1
    (\(LSP.SelectionRange r _) outer -> LSP.SelectionRange r (Just outer))
    (map (\sp -> LSP.SelectionRange (spanToRange sp) Nothing) sps)
