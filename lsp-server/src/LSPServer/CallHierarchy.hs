module LSPServer.CallHierarchy
  ( prepareCallHierarchy,
    incomingCalls,
    outgoingCalls,
  )
where

import AST.Types.Common (FuncName (..), SourceSpan)
import AST.Types.Type (FunctionType)
import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import LSPServer.Hover (renderSig)
import LSPServer.Span (findFuncAtPos, smallest, spanToRange)
import qualified Language.LSP.Protocol.Types as LSP

-- | Find the function under the cursor and return it as a CallHierarchyItem.
prepareCallHierarchy ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map FuncName SourceSpan ->
  [(FuncName, FunctionType, SourceSpan, SourceSpan)] ->
  FilePath ->
  Int ->
  Int ->
  [LSP.CallHierarchyItem]
prepareCallHierarchy callSites defSites funcSymbols fp lspLine lspCol =
  case resolveFuncName of
    Nothing -> []
    Just fname -> case findSymbol fname of
      Nothing -> []
      Just item -> [item]
  where
    resolveFuncName =
      case smallest callSites lspLine lspCol of
        Just (_, (fname, _)) -> Just fname
        Nothing -> findFuncAtPos defSites lspLine lspCol

    findSymbol fname =
      case [(ft, nameSp, fullSp) | (fn, ft, nameSp, fullSp) <- funcSymbols, fn == fname] of
        [(ft, nameSp, fullSp)] -> Just (makeItem fp fname ft nameSp fullSp)
        _ -> Nothing

-- | Given a CallHierarchyItem, return all functions that call it.
incomingCalls ::
  Map FuncName [(FuncName, SourceSpan)] ->
  [(FuncName, FunctionType, SourceSpan, SourceSpan)] ->
  FilePath ->
  LSP.CallHierarchyItem ->
  [LSP.CallHierarchyIncomingCall]
incomingCalls callsByFunc funcSymbols fp item =
  case extractFuncName item of
    Nothing -> []
    Just targetName ->
      [ LSP.CallHierarchyIncomingCall callerItem callSpans
        | (callerName, calls) <- Map.toList callsByFunc,
          let callSpans = [spanToRange sp | (callee, sp) <- calls, callee == targetName],
          not (null callSpans),
          Just callerItem <- [findSymbol callerName]
      ]
  where
    findSymbol fname =
      case [(ft, nameSp, fullSp) | (fn, ft, nameSp, fullSp) <- funcSymbols, fn == fname] of
        [(ft, nameSp, fullSp)] -> Just (makeItem fp fname ft nameSp fullSp)
        _ -> Nothing

-- | Given a CallHierarchyItem, return all functions it calls (user-defined only).
outgoingCalls ::
  Map FuncName [(FuncName, SourceSpan)] ->
  [(FuncName, FunctionType, SourceSpan, SourceSpan)] ->
  FilePath ->
  LSP.CallHierarchyItem ->
  [LSP.CallHierarchyOutgoingCall]
outgoingCalls callsByFunc funcSymbols fp item =
  case extractFuncName item of
    Nothing -> []
    Just callerName ->
      case Map.lookup callerName callsByFunc of
        Nothing -> []
        Just calls ->
          let grouped = Map.fromListWith (++) [(callee, [sp]) | (callee, sp) <- calls]
           in [ LSP.CallHierarchyOutgoingCall calleeItem (map spanToRange spans)
                | (calleeName, spans) <- Map.toList grouped,
                  Just calleeItem <- [findSymbol calleeName]
              ]
  where
    findSymbol fname =
      case [(ft, nameSp, fullSp) | (fn, ft, nameSp, fullSp) <- funcSymbols, fn == fname] of
        [(ft, nameSp, fullSp)] -> Just (makeItem fp fname ft nameSp fullSp)
        _ -> Nothing

makeItem :: FilePath -> FuncName -> FunctionType -> SourceSpan -> SourceSpan -> LSP.CallHierarchyItem
makeItem fp fname ft nameSp fullSp =
  LSP.CallHierarchyItem
    (unFuncName fname)
    LSP.SymbolKind_Function
    Nothing
    (Just (renderSig fname ft))
    (LSP.filePathToUri fp)
    (spanToRange fullSp)
    (spanToRange nameSp)
    (Just (toJSON (unFuncName fname)))

extractFuncName :: LSP.CallHierarchyItem -> Maybe FuncName
extractFuncName (LSP.CallHierarchyItem _ _ _ _ _ _ _ mData) =
  case mData of
    Just v -> case fromJSON v of
      Success t -> Just (FuncName t)
      _ -> Nothing
    Nothing -> Nothing
