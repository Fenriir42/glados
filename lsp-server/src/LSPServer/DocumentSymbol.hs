module LSPServer.DocumentSymbol (makeDocumentSymbols) where

import AST.Types.Common (FuncName (..), SourceSpan)
import AST.Types.Type (FunctionType)
import LSPServer.Hover (renderSig)
import LSPServer.Span (spanToRange)
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
