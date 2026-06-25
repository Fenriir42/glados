module LSPServer.Span
  ( spanToRange,
    containsPos,
    spanLen,
    smallest,
    findFuncAtPos,
  )
where

import AST.Types.Common (Column (..), FuncName, Line (..), SourcePos (..), SourceSpan (..))
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import qualified Language.LSP.Protocol.Types as LSP

spanToRange :: SourceSpan -> LSP.Range
spanToRange sp =
  LSP.Range
    ( LSP.Position
        ((fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanStart sp))) - 1) :: LSP.UInt)
    )
    ( LSP.Position
        ((fromIntegral (unLine (posLine (spanEnd sp))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanEnd sp))) - 1) :: LSP.UInt)
    )

containsPos :: Int -> Int -> SourceSpan -> Bool
containsPos line col sp =
  let startLine = unLine (posLine (spanStart sp)) - 1
      startCol = unColumn (posColumn (spanStart sp)) - 1
      endLine = unLine (posLine (spanEnd sp)) - 1
      endCol = unColumn (posColumn (spanEnd sp)) - 1
   in (startLine < line || (startLine == line && startCol <= col))
        && (line < endLine || (line == endLine && col < endCol))

spanLen :: SourceSpan -> Int
spanLen sp =
  (unLine (posLine (spanEnd sp)) - unLine (posLine (spanStart sp))) * 10000
    + (unColumn (posColumn (spanEnd sp)) - unColumn (posColumn (spanStart sp)))

-- | Smallest span in the map that contains the cursor, or Nothing.
smallest :: Map SourceSpan a -> Int -> Int -> Maybe (SourceSpan, a)
smallest m lspLine lspCol =
  case filter (containsPos lspLine lspCol . fst) (Map.toList m) of
    [] -> Nothing
    pairs -> Just (minimumBy (comparing (spanLen . fst)) pairs)

-- | If the cursor falls within any function's definition span, return that name.
-- Used to handle highlight / references / rename when the cursor is on the fn name
-- at its declaration site rather than at a call site.
findFuncAtPos :: Map FuncName SourceSpan -> Int -> Int -> Maybe FuncName
findFuncAtPos defSites lspLine lspCol =
  listToMaybe [fname | (fname, sp) <- Map.toList defSites, containsPos lspLine lspCol sp]
