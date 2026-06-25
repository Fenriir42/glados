module LSPServer.Rename (prepareRename, findRename) where

import AST.Types.Common (FuncName (..), SourceSpan, VarName (..))
import AST.Types.Type (FunctionType)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import LSPServer.Span (findFuncAtPos, smallest, spanToRange)
import qualified Language.LSP.Protocol.Types as LSP

-- | Return the name range if the cursor is on a renameable symbol, else Nothing.
-- User-defined functions and local variables are renameable; stdlib/builtins are not.
prepareRename ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map FuncName SourceSpan ->
  Map SourceSpan (VarName, SourceSpan) ->
  Int ->
  Int ->
  Maybe (LSP.Range, Text)
prepareRename callSites defSites varUseSites lspLine lspCol =
  case smallest callSites lspLine lspCol of
    Just (sp, (fname, _))
      | Map.member fname defSites ->
          Just (spanToRange sp, unFuncName fname)
    _ ->
      case findFuncAtPos defSites lspLine lspCol of
        Just fname ->
          let sp = defSites Map.! fname
           in Just (spanToRange sp, unFuncName fname)
        Nothing ->
          case smallest varUseSites lspLine lspCol of
            Just (sp, (vname, _)) -> Just (spanToRange sp, unVarName vname)
            Nothing -> Nothing

-- | Build a WorkspaceEdit renaming the symbol at cursor everywhere in the file.
findRename ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map FuncName SourceSpan ->
  Map SourceSpan (VarName, SourceSpan) ->
  FilePath ->
  Int ->
  Int ->
  Text ->
  Maybe LSP.WorkspaceEdit
findRename callSites defSites varUseSites currentFile lspLine lspCol newName =
  funcRename `orElse` varRename
  where
    funcRename =
      case resolveFuncName of
        Nothing -> Nothing
        Just fname ->
          case Map.lookup fname defSites of
            Nothing -> Nothing
            Just defSpan ->
              let callSpans = [sp | (sp, (fn, _)) <- Map.toList callSites, fn == fname]
                  allSpans = defSpan : callSpans
               in Just (mkEdit currentFile allSpans newName)

    resolveFuncName =
      case smallest callSites lspLine lspCol of
        Just (_, (fname, _)) | Map.member fname defSites -> Just fname
        _ -> findFuncAtPos defSites lspLine lspCol

    varRename =
      case smallest varUseSites lspLine lspCol of
        Nothing -> Nothing
        Just (_, (_, defSp)) ->
          let allSpans = [sp | (sp, (_, d)) <- Map.toList varUseSites, d == defSp]
           in Just (mkEdit currentFile allSpans newName)

    orElse (Just x) _ = Just x
    orElse Nothing y = y

mkEdit :: FilePath -> [SourceSpan] -> Text -> LSP.WorkspaceEdit
mkEdit fp spans newName =
  let edits = map (\sp -> LSP.TextEdit (spanToRange sp) newName) spans
      uri = LSP.filePathToUri fp
   in LSP.WorkspaceEdit (Just (Map.singleton uri edits)) Nothing Nothing
