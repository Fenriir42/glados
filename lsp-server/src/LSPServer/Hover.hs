module LSPServer.Hover (findTypeAtPos, makeHover) where

import AST.Types.Common (Column (..), Line (..), SourcePos (..), SourceSpan (..))
import AST.Types.Type (Type)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

findTypeAtPos :: Map SourceSpan Type -> Int -> Int -> Maybe (Type, SourceSpan)
findTypeAtPos typeMap lspLine lspChar =
  case filter (\(sp, _) -> containsPos sp lspLine lspChar) (Map.toList typeMap) of
    [] -> Nothing
    pairs -> Just $ let (sp, ty) = minimumBy (comparing (spanLength . fst)) pairs in (ty, sp)

containsPos :: SourceSpan -> Int -> Int -> Bool
containsPos sp line col =
  let startLine = unLine (posLine (spanStart sp)) - 1
      startCol = unColumn (posColumn (spanStart sp)) - 1
      endLine = unLine (posLine (spanEnd sp)) - 1
      endCol = unColumn (posColumn (spanEnd sp)) - 1
   in (startLine < line || (startLine == line && startCol <= col))
        && (line < endLine || (line == endLine && col < endCol))

spanLength :: SourceSpan -> Int
spanLength sp =
  let sl = unLine (posLine (spanStart sp))
      el = unLine (posLine (spanEnd sp))
      sc = unColumn (posColumn (spanStart sp))
      ec = unColumn (posColumn (spanEnd sp))
   in (el - sl) * 10000 + (ec - sc)

makeHover :: Type -> SourceSpan -> LSP.Hover
makeHover ty sp =
  LSP.Hover
    (LSP.InL (LSP.mkMarkdownCodeBlock "quant" (T.pack (show ty))))
    (Just (spanToRange sp))

spanToRange :: SourceSpan -> LSP.Range
spanToRange ss =
  LSP.Range
    ( LSP.Position
        ((fromIntegral (unLine (posLine (spanStart ss))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanStart ss))) - 1) :: LSP.UInt)
    )
    ( LSP.Position
        ((fromIntegral (unLine (posLine (spanEnd ss))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanEnd ss))) - 1) :: LSP.UInt)
    )
