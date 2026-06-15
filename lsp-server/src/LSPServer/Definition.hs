module LSPServer.Definition (findDefinition) where

import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
    VarName,
  )
import AST.Types.Type (FunctionType)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

-- | Find the definition location of the symbol under the cursor.
-- Returns Nothing for VM builtins (println, print, etc.) which have no source file.
findDefinition ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Map FuncName SourceSpan ->
  Map FuncName (FilePath, SourceSpan) ->
  Map SourceSpan (VarName, SourceSpan) ->
  FilePath ->
  Int ->
  Int ->
  Maybe LSP.Location
findDefinition callSites builtinSites userDefSites stdlibDefSites varUseSites currentFile lspLine lspCol =
  findFuncDef `orElse` findVarDef
  where
    findFuncDef = do
      fname <- findFuncNameAtPos callSites builtinSites lspLine lspCol
      lookupFuncDef fname

    findVarDef = do
      (_, (_, defSp)) <- smallest varUseSites lspLine lspCol
      Just (spanToLocation currentFile defSp)

    lookupFuncDef fname =
      case Map.lookup fname userDefSites of
        Just sp -> Just (spanToLocation currentFile sp)
        Nothing ->
          let bare = stripModulePrefix fname
           in case Map.lookup fname stdlibDefSites of
                Just (fp, sp) -> Just (spanToLocation fp sp)
                Nothing ->
                  case Map.lookup bare stdlibDefSites of
                    Just (fp, sp) -> Just (spanToLocation fp sp)
                    Nothing -> Nothing

    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Find the function name at the given position by scanning call sites.
findFuncNameAtPos ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Int ->
  Int ->
  Maybe FuncName
findFuncNameAtPos callSites builtinSites lspLine lspCol =
  case smallest callSites lspLine lspCol of
    Just (_, (fname, _)) -> Just fname
    Nothing ->
      case smallest builtinSites lspLine lspCol of
        Just (_, fname) -> Just fname
        Nothing -> Nothing

smallest :: Map SourceSpan a -> Int -> Int -> Maybe (SourceSpan, a)
smallest m lspLine lspCol =
  case filter (containsPos lspLine lspCol . fst) (Map.toList m) of
    [] -> Nothing
    pairs -> Just (minimumBy (comparing (spanLength . fst)) pairs)

containsPos :: Int -> Int -> SourceSpan -> Bool
containsPos line col sp =
  let startLine = unLine (posLine (spanStart sp)) - 1
      startCol = unColumn (posColumn (spanStart sp)) - 1
      endLine = unLine (posLine (spanEnd sp)) - 1
      endCol = unColumn (posColumn (spanEnd sp)) - 1
   in (startLine < line || (startLine == line && startCol <= col))
        && (line < endLine || (line == endLine && col < endCol))

spanLength :: SourceSpan -> Int
spanLength sp =
  (unLine (posLine (spanEnd sp)) - unLine (posLine (spanStart sp))) * 10000
    + (unColumn (posColumn (spanEnd sp)) - unColumn (posColumn (spanStart sp)))

spanToLocation :: FilePath -> SourceSpan -> LSP.Location
spanToLocation fp sp =
  LSP.Location
    (LSP.filePathToUri fp)
    ( LSP.Range
        ( LSP.Position
            ((fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt)
            ((fromIntegral (unColumn (posColumn (spanStart sp))) - 1) :: LSP.UInt)
        )
        ( LSP.Position
            ((fromIntegral (unLine (posLine (spanEnd sp))) - 1) :: LSP.UInt)
            ((fromIntegral (unColumn (posColumn (spanEnd sp))) - 1) :: LSP.UInt)
        )
    )

-- | Strip the module prefix from a qualified name: "math.sqrt" -> "math.sqrt"
-- is looked up first; the bare "sqrt" is a fallback.
stripModulePrefix :: FuncName -> FuncName
stripModulePrefix (FuncName n) =
  FuncName (snd (T.breakOnEnd "." n))
