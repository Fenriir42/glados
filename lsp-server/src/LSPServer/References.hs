module LSPServer.References (findReferences) where

import AST.Types.Common (FilePath' (..), FuncName, SourcePos (..), SourceSpan (..), VarName)
import AST.Types.Type (FunctionType)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import LSPServer.Span (findFuncAtPos, smallest, spanToRange)
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
  maybe varRefs funcRefs resolveFuncName
  where
    resolveFuncName =
      case smallest callSites lspLine lspCol of
        Just (_, (fname, _)) -> Just fname
        Nothing -> case fmap snd (smallest builtinSites lspLine lspCol) of
          Just fname -> Just fname
          Nothing -> findFuncAtPos defSites lspLine lspCol

    funcRefs fname =
      let callLocs =
            [ toLocation sp
              | (sp, (fn, _)) <- Map.toList callSites,
                fn == fname
            ]
          builtinLocs =
            [ toLocation sp
              | (sp, fn) <- Map.toList builtinSites,
                fn == fname
            ]
          defLoc =
            if includeDecl
              then case Map.lookup fname defSites of
                Nothing -> []
                Just sp -> [toLocation sp]
              else []
       in defLoc ++ callLocs ++ builtinLocs

    varRefs =
      case smallest varUseSites lspLine lspCol of
        Nothing -> []
        Just (_, (_, defSp)) ->
          [ toLocation sp
            | (sp, (_, d)) <- Map.toList varUseSites,
              d == defSp,
              includeDecl || sp /= defSp
          ]

    toLocation sp =
      let fp = case T.unpack (unFilePath (posFile (spanStart sp))) of
            "" -> currentFile
            p -> p
       in LSP.Location (LSP.filePathToUri fp) (spanToRange sp)
