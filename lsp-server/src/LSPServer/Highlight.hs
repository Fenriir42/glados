module LSPServer.Highlight (findHighlights) where

import AST.Types.Common (FuncName, SourceSpan, VarName)
import AST.Types.Type (FunctionType)
import Data.Map (Map)
import qualified Data.Map as Map
import LSPServer.Span (findFuncAtPos, smallest, spanToRange)
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
    -- Resolve the function name whether cursor is on a call site or the definition.
    resolveFuncName =
      case smallest callSites lspLine lspCol of
        Just (_, (fname, _)) -> Just fname
        Nothing -> case fmap snd (smallest builtinSites lspLine lspCol) of
          Just fname -> Just fname
          Nothing -> findFuncAtPos defSites lspLine lspCol

    funcHighlights =
      case resolveFuncName of
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
