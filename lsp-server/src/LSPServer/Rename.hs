module LSPServer.Rename (prepareRename, findRename) where

import AST.Types.Common (Column (..), FuncName (..), Line (..), SourcePos (..), SourceSpan (..), VarName (..))
import AST.Types.Type (FunctionType)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
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
      case smallest callSites lspLine lspCol of
        Nothing -> Nothing
        Just (_, (fname, _)) ->
          case Map.lookup fname defSites of
            Nothing -> Nothing
            Just defSpan ->
              let callSpans = [sp | (sp, (fn, _)) <- Map.toList callSites, fn == fname]
                  allSpans = defSpan : callSpans
               in Just (mkEdit currentFile allSpans newName)

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

smallest :: Map SourceSpan a -> Int -> Int -> Maybe (SourceSpan, a)
smallest m lspLine lspCol =
  case filter (containsPos lspLine lspCol . fst) (Map.toList m) of
    [] -> Nothing
    pairs -> Just (minimumBy (comparing (spanLen . fst)) pairs)

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
