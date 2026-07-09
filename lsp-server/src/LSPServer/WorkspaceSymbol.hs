module LSPServer.WorkspaceSymbol (findWorkspaceSymbols) where

import AST.Types.Common (FuncName (..), SourceSpan, TypeName (..))
import AST.Types.Type (FunctionType)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Span (spanToRange)
import qualified Language.LSP.Protocol.Types as LSP

-- | Return all functions and structs in the file whose name contains the
-- query as a case-insensitive substring.  An empty query returns everything.
findWorkspaceSymbols ::
  [(FuncName, FunctionType, SourceSpan, SourceSpan)] ->
  Map TypeName SourceSpan ->
  FilePath ->
  Text ->
  [LSP.WorkspaceSymbol]
findWorkspaceSymbols funcSymbols structDefSites fp query =
  funcSyms ++ structSyms
  where
    funcSyms =
      [ LSP.WorkspaceSymbol (unFuncName fname) LSP.SymbolKind_Function Nothing Nothing (LSP.InL (loc nameSpan)) Nothing
        | (fname, _ft, nameSpan, _) <- funcSymbols,
          matches (unFuncName fname)
      ]

    structSyms =
      [ LSP.WorkspaceSymbol tname LSP.SymbolKind_Struct Nothing Nothing (LSP.InL (loc sp)) Nothing
        | (TypeName tname, sp) <- Map.toList structDefSites,
          matches tname
      ]

    loc sp = LSP.Location (LSP.filePathToUri fp) (spanToRange sp)

    matches name
      | T.null query = True
      | otherwise = T.toCaseFold query `T.isInfixOf` T.toCaseFold name
