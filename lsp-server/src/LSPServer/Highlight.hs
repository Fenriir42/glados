module LSPServer.Highlight (findHighlights) where

import AST.Types.Common (Column (..), FuncName, Line (..), SourcePos (..), SourceSpan (..), VarName)
import AST.Types.Type (FunctionType)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Language.LSP.Protocol.Types as LSP

findHighlights ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Map FuncName SourceSpan ->
  Map SourceSpan (VarName, SourceSpan) ->
  Int ->
  Int ->
  [LSP.DocumentHighlight]
findHighlights callSites builtinSites defSites varUseSites lspLine lspCol =
  funcHighlights `orElse` varHighlights
  where
    funcHighlights =
      case findNameAtPos callSites builtinSites lspLine lspCol of
        Nothing -> []
        Just fname ->
          let calls =
                [ LSP.DocumentHighlight (spanToRange sp) (Just LSP.DocumentHighlightKind_Read)
                  | (sp, (fn, _)) <- Map.toList callSites,
                    fn == fname
                ]
              builtins =
                [ LSP.DocumentHighlight (spanToRange sp) (Just LSP.DocumentHighlightKind_Read)
                  | (sp, fn) <- Map.toList builtinSites,
                    fn == fname
                ]
              defH = case Map.lookup fname defSites of
                Nothing -> []
                Just sp ->
                  [LSP.DocumentHighlight (spanToRange sp) (Just LSP.DocumentHighlightKind_Write)]
           in defH ++ calls ++ builtins

    varHighlights =
      case smallest varUseSites lspLine lspCol of
        Nothing -> []
        Just (_, (_, defSp)) ->
          [ LSP.DocumentHighlight (spanToRange sp) kind
            | (sp, (_, d)) <- Map.toList varUseSites,
              d == defSp,
              let kind =
                    if sp == defSp
                      then Just LSP.DocumentHighlightKind_Write
                      else Just LSP.DocumentHighlightKind_Read
          ]

    orElse [] ys = ys
    orElse xs _ = xs

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
