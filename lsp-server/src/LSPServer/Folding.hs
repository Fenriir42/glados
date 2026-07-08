module LSPServer.Folding (makeFoldingRanges) where

import AST.Types.Common (Line (..), SourcePos (..), SourceSpan (..))
import qualified Language.LSP.Protocol.Types as LSP

makeFoldingRanges :: [SourceSpan] -> [LSP.FoldingRange]
makeFoldingRanges = concatMap toFoldingRange
  where
    toFoldingRange sp =
      let startLine = (fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt
          closingLine = (fromIntegral (unLine (posLine (spanEnd sp))) - 1) :: LSP.UInt
          -- Keep the closing brace line visible (RA-style: opening + closing remain,
          -- only body lines are hidden).
          endLine = if closingLine > startLine then closingLine - 1 else closingLine
       in [ LSP.FoldingRange startLine Nothing endLine Nothing (Just LSP.FoldingRangeKind_Region) Nothing
            | endLine > startLine
          ]
