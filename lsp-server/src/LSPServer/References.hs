module LSPServer.References (findReferences) where

import AST.Types.Common (Column (..), FuncName, Line (..), SourcePos (..), SourceSpan (..), VarName)
import AST.Types.Type (FunctionType)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Language.LSP.Protocol.Types as LSP

findReferences ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Map FuncName SourceSpan ->
  Map SourceSpan (VarName, SourceSpan) ->
  FilePath ->
  Bool ->
  Int ->
  Int ->
  [LSP.Location]
findReferences callSites builtinSites defSites varUseSites currentFile includeDecl lspLine lspCol =
  maybe varRefs funcRefs (findNameAtPos callSites builtinSites lspLine lspCol)
  where
    funcRefs fname =
      let callLocs =
            [ toLocation currentFile sp
              | (sp, (fn, _)) <- Map.toList callSites,
                fn == fname
            ]
          builtinLocs =
            [ toLocation currentFile sp
              | (sp, fn) <- Map.toList builtinSites,
                fn == fname
            ]
          defLoc =
            if includeDecl
              then case Map.lookup fname defSites of
                Nothing -> []
                Just sp -> [toLocation currentFile sp]
              else []
       in defLoc ++ callLocs ++ builtinLocs

    varRefs =
      case smallest varUseSites lspLine lspCol of
        Nothing -> []
        Just (_, (_, defSp)) ->
          [ toLocation currentFile sp
            | (sp, (_, d)) <- Map.toList varUseSites,
              d == defSp,
              includeDecl || sp /= defSp
          ]

findNameAtPos ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Int ->
  Int ->
  Maybe FuncName
findNameAtPos callSites builtinSites lspLine lspCol =
  case smallest callSites lspLine lspCol of
    Just (_, (fname, _)) -> Just fname
    Nothing -> fmap snd (smallest builtinSites lspLine lspCol)

toLocation :: FilePath -> SourceSpan -> LSP.Location
toLocation fp sp = LSP.Location (LSP.filePathToUri fp) (spanToRange sp)

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
