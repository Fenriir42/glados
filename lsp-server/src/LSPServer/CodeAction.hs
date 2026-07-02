module LSPServer.CodeAction (makeCodeActions) where

import AST.Types.AST (ImportDecl (..), ModulePath (..))
import AST.Types.Common (Located (..), ModuleName (..), SourceSpan, VarName (..), unModuleName, unVarName)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Span (spanToRange)
import qualified Language.LSP.Protocol.Types as LSP

-- | Build quick-fix code actions for the diagnostics passed by VS Code.
-- Only diagnostics tagged DiagnosticTag_Unnecessary are handled.
makeCodeActions ::
  Map SourceSpan (VarName, SourceSpan) ->
  [(SourceSpan, ImportDecl)] ->
  FilePath ->
  LSP.Range ->
  [LSP.Diagnostic] ->
  [LSP.CodeAction]
makeCodeActions varDeclSites importDecls fp _range =
  concatMap (actionsForDiag varDeclSites importDecls fp)

actionsForDiag ::
  Map SourceSpan (VarName, SourceSpan) ->
  [(SourceSpan, ImportDecl)] ->
  FilePath ->
  LSP.Diagnostic ->
  [LSP.CodeAction]
actionsForDiag varDeclSites importDecls fp diag
  | not (isUnnecessary diag) = []
  | otherwise =
      catMaybes
        [ varRemoveAction varDeclSites fp diag,
          importRemoveAction importDecls fp diag
        ]

-- | True when the diagnostic carries DiagnosticTag_Unnecessary.
-- Positional pattern on Diagnostic: range, severity, code, codeDesc, source, msg, tags, relInfo, data_
isUnnecessary :: LSP.Diagnostic -> Bool
isUnnecessary (LSP.Diagnostic _ _ _ _ _ _ mTags _ _) =
  case mTags of
    Just tags -> LSP.DiagnosticTag_Unnecessary `elem` tags
    Nothing -> False

-- | Quick fix: remove an unused variable declaration.
-- Uses the diagnostic range's start line (from the name span) for deletion because
-- stmtSpan.spanStart is corrupted by voidSpann ("/dev/null") from parseQualifiedType.
varRemoveAction ::
  Map SourceSpan (VarName, SourceSpan) ->
  FilePath ->
  LSP.Diagnostic ->
  Maybe LSP.CodeAction
varRemoveAction varDeclSites fp diag = do
  let LSP.Diagnostic diagRange _ _ _ _ _ _ _ _ = diag
      LSP.Range (LSP.Position nameLine _) _ = diagRange
  (_, (vname, _)) <-
    case [(nameSp, v) | (nameSp, v) <- Map.toList varDeclSites, spanToRange nameSp == diagRange] of
      [x] -> Just x
      _ -> Nothing
  Just (deleteLineAction fp ("Remove unused variable `" <> unVarName vname <> "`") nameLine)

-- | Quick fix: remove an entire unused import line.
-- Triggered when the diagnostic range matches the full import decl span (all names unused).
importRemoveAction ::
  [(SourceSpan, ImportDecl)] ->
  FilePath ->
  LSP.Diagnostic ->
  Maybe LSP.CodeAction
importRemoveAction importDecls fp diag = do
  let LSP.Diagnostic diagRange _ _ _ _ _ _ _ _ = diag
  (importSp, importDecl) <-
    case [(sp, d) | (sp, d) <- importDecls, spanToRange sp == diagRange] of
      (x : _) -> Just x
      [] -> Nothing
  let modName = modPathText (importPath importDecl)
      LSP.Range (LSP.Position importLine _) _ = spanToRange importSp
  Just (deleteLineAction fp ("Remove import `" <> modName <> "`") importLine)

-- | Build a WorkspaceEdit that deletes exactly one source line by its 0-indexed line number.
deleteLineAction :: FilePath -> Text -> LSP.UInt -> LSP.CodeAction
deleteLineAction fp title line =
  let deleteRange = LSP.Range (LSP.Position line 0) (LSP.Position (line + 1) 0)
      edit = LSP.TextEdit deleteRange ""
      uri = LSP.filePathToUri fp
      wsEdit = LSP.WorkspaceEdit (Just (Map.singleton uri [edit])) Nothing Nothing
   in LSP.CodeAction
        title
        (Just LSP.CodeActionKind_QuickFix)
        Nothing
        (Just True)
        Nothing
        (Just wsEdit)
        Nothing
        Nothing

modPathText :: ModulePath -> Text
modPathText mp =
  T.intercalate "." [unModuleName n | Located _ n <- modulePathParts mp]
