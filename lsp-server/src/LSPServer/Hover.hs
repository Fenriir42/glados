module LSPServer.Hover (findHoverAtPos, renderSig, showType, knownBuiltins) where

import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    Located (..),
    SourcePos (..),
    SourceSpan (..),
    VarName (..),
    unLocated,
  )
import AST.Types.Type
  ( FunctionType (..),
    PrimitiveType (..),
    Type (..),
    paramName,
    paramType,
    qualType,
  )
import Data.List (intercalate, minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

-- | Find hover information at the given 0-based LSP position.
-- Priority: call-site signatures > builtin docs > inferred type.
findHoverAtPos ::
  Map SourceSpan Type ->
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Map FuncName Text ->
  Int ->
  Int ->
  Maybe LSP.Hover
findHoverAtPos typeMap callSites builtinSites docs lspLine lspChar =
  callSiteHover `orElse` builtinHover `orElse` typeHover
  where
    callSiteHover = do
      (sp, (fname, ft)) <- smallest callSites lspLine lspChar
      let doc = lookupDoc fname docs
      return (makeCallHover fname ft doc sp)

    builtinHover = do
      (sp, fname) <- smallest builtinSites lspLine lspChar
      return (makeBuiltinHover fname sp)

    typeHover = do
      (sp, ty) <- smallest typeMap lspLine lspChar
      return (makeTypeHover ty sp)

    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Look up a doc string, falling back to the bare name if the qualified name
-- (e.g. "math.sqrt") isn't found directly.
lookupDoc :: FuncName -> Map FuncName Text -> Maybe Text
lookupDoc fname docs =
  case Map.lookup fname docs of
    Just doc -> Just doc
    Nothing ->
      let bare = FuncName (snd (T.breakOnEnd "." (unFuncName fname)))
       in if bare == fname then Nothing else Map.lookup bare docs

-- | The entry with the smallest span that contains the given position.
smallest :: Map SourceSpan a -> Int -> Int -> Maybe (SourceSpan, a)
smallest m lspLine lspChar =
  case filter (containsPos lspLine lspChar . fst) (Map.toList m) of
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

-- ---------------------------------------------------------------------------
-- Hover rendering

makeCallHover :: FuncName -> FunctionType -> Maybe Text -> SourceSpan -> LSP.Hover
makeCallHover fname ft mDoc sp =
  let sig = renderSig fname ft
      content = case mDoc of
        Nothing ->
          LSP.InL (LSP.mkMarkdownCodeBlock "quant" sig)
        Just doc ->
          LSP.InL $
            LSP.MarkupContent LSP.MarkupKind_Markdown $
              "```quant\n" <> sig <> "\n```\n\n" <> T.strip doc
   in LSP.Hover content (Just (spanToRange sp))

makeBuiltinHover :: FuncName -> SourceSpan -> LSP.Hover
makeBuiltinHover fname sp =
  let content = case Map.lookup fname knownBuiltins of
        Just (sig, doc) ->
          LSP.InL $
            LSP.MarkupContent LSP.MarkupKind_Markdown $
              "```quant\n" <> sig <> "\n```\n\n" <> doc
        Nothing ->
          LSP.InL (LSP.mkMarkdownCodeBlock "quant" (unFuncName fname))
   in LSP.Hover content (Just (spanToRange sp))

makeTypeHover :: Type -> SourceSpan -> LSP.Hover
makeTypeHover ty sp =
  LSP.Hover
    (LSP.InL (LSP.mkMarkdownCodeBlock "quant" (showType ty)))
    (Just (spanToRange sp))

-- | Human-readable type, using "void" for the no-value primitive.
showType :: Type -> Text
showType (TypePrimitive PrimNone) = "void"
showType t = T.pack (show t)

renderSig :: FuncName -> FunctionType -> Text
renderSig (FuncName fname) ft =
  "fn " <> fname <> "(" <> params <> ") -> " <> ret
  where
    params = T.pack $ intercalate ", " (map renderParam (funcParams ft))
    ret = showType (qualType (unLocated (funcReturnType ft)))
    renderParam (Located _ p) =
      T.unpack (unVarName (paramName p)) <> ": " <> T.unpack (showType (qualType (paramType p)))

-- | Hand-written signatures and doc strings for VM builtins that are not
-- defined in any .qa file.
knownBuiltins :: Map FuncName (Text, Text)
knownBuiltins =
  Map.fromList
    [ ( FuncName "println",
        ( "fn println(value: any) -> void",
          "Prints `value` followed by a newline to stdout. Accepts any type."
        )
      ),
      ( FuncName "print",
        ( "fn print(value: any) -> void",
          "Prints `value` without a trailing newline. Accepts any type."
        )
      ),
      ( FuncName "len",
        ( "fn len(collection: string | [any]) -> int",
          "Returns the number of characters in a string, or the number of elements in an array."
        )
      ),
      ( FuncName "push",
        ( "fn push(arr: [any], value: any) -> void",
          "Appends `value` to the end of `arr` in place."
        )
      ),
      ( FuncName "pop",
        ( "fn pop(arr: [any]) -> any",
          "Removes and returns the last element of `arr`."
        )
      )
    ]

spanToRange :: SourceSpan -> LSP.Range
spanToRange ss =
  LSP.Range
    ( LSP.Position
        ((fromIntegral (unLine (posLine (spanStart ss))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanStart ss))) - 1) :: LSP.UInt)
    )
    ( LSP.Position
        ((fromIntegral (unLine (posLine (spanEnd ss))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanEnd ss))) - 1) :: LSP.UInt)
    )
