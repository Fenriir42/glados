module LSPServer.TypeDefinition (findTypeDefinition) where

import AST.Types.Common
  ( Column (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
    TypeName (..),
  )
import AST.Types.Type (ArrayType (..), QualifiedType (..), Type (..))
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Language.LSP.Protocol.Types as LSP

-- | Given the type of each expression span and struct declaration spans,
-- find the struct definition for the type of whatever is under the cursor.
findTypeDefinition ::
  Map SourceSpan Type ->
  Map TypeName SourceSpan ->
  FilePath ->
  Int ->
  Int ->
  Maybe LSP.Location
findTypeDefinition exprTypes structDefSites fp lspLine lspCol = do
  (_, ty) <- smallest exprTypes lspLine lspCol
  tname <- structTypeName ty
  defSp <- Map.lookup tname structDefSites
  Just (spanToLocation fp defSp)

-- | Extract the TypeName if this type is (or contains) a struct.
-- Unwraps one level of TypeArray so `[ParseError]` also works.
structTypeName :: Type -> Maybe TypeName
structTypeName (TypeStruct n) = Just n
structTypeName (TypeArray (ArrayType qt)) =
  case qualType qt of
    TypeStruct n -> Just n
    _ -> Nothing
structTypeName _ = Nothing

-- ---------------------------------------------------------------------------
-- Shared helpers (mirrors Definition.hs)

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
