module LSPServer.Folding (makeFoldingRanges) where

import AST.Types.Common (Line (..), SourcePos (..), SourceSpan (..))
import qualified Language.LSP.Protocol.Types as LSP

makeFoldingRanges :: [SourceSpan] -> [LSP.FoldingRange]
makeFoldingRanges = map toFoldingRange
  where
    toFoldingRange sp =
      LSP.FoldingRange
        ((fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt)
        Nothing
        ((fromIntegral (unLine (posLine (spanEnd sp))) - 1) :: LSP.UInt)
        Nothing
        (Just LSP.FoldingRangeKind_Region)
        Nothing
