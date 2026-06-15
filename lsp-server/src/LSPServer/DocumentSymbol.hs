module LSPServer.DocumentSymbol (makeDocumentSymbols) where

import AST.Types.Common (Column (..), FuncName (..), Line (..), SourcePos (..), SourceSpan (..))
import AST.Types.Type (FunctionType)
import LSPServer.Hover (renderSig)
import qualified Language.LSP.Protocol.Types as LSP

makeDocumentSymbols :: [(FuncName, FunctionType, SourceSpan, SourceSpan)] -> [LSP.DocumentSymbol]
makeDocumentSymbols = map toSymbol
  where
    toSymbol (fname, ft, nameSpan, fullSpan) =
      LSP.DocumentSymbol
        (unFuncName fname)
        (Just (renderSig fname ft))
        LSP.SymbolKind_Function
        Nothing
        Nothing
        (spanToRange fullSpan)
        (spanToRange nameSpan)
        Nothing

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
