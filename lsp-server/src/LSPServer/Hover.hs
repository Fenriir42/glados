module LSPServer.Hover
  ( findHoverAtPos,
    renderSig,
    showType,
    renderDoc,
    knownBuiltins,
  )
where

import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    Located (..),
    SourcePos (..),
    SourceSpan (..),
    TypeName (..),
    VarName (..),
    unLocated,
    unTypeName,
  )
import AST.Types.Type
  ( ArrayType (..),
    Constness (..),
    FunctionType (..),
    Parameter (..),
    PrimitiveType (..),
    QualifiedType (..),
    Type (..),
    paramName,
    paramType,
    paramVariadic,
    qualConstness,
    qualType,
  )
import Data.List (minimumBy, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

-- ---------------------------------------------------------------------------
-- Public entry point

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

-- ---------------------------------------------------------------------------
-- Doc lookup

lookupDoc :: FuncName -> Map FuncName Text -> Maybe Text
lookupDoc fname docs =
  case Map.lookup fname docs of
    Just doc -> Just doc
    Nothing ->
      let bare = FuncName (snd (T.breakOnEnd "." (unFuncName fname)))
       in if bare == fname then Nothing else Map.lookup bare docs

-- ---------------------------------------------------------------------------
-- Span helpers

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
-- Hover construction

makeCallHover :: FuncName -> FunctionType -> Maybe Text -> SourceSpan -> LSP.Hover
makeCallHover fname ft mDoc sp =
  let sig = renderSig fname ft
      origin = moduleOrigin fname
      header = "```quant\n" <> sig <> "\n```"
      fromLine = maybe "" (\m -> "\n*from `" <> m <> "`*") origin
      docPart = maybe "" (\raw -> "\n\n" <> renderDoc raw) mDoc
      markdown = header <> fromLine <> docPart
   in LSP.Hover
        (LSP.InL (LSP.MarkupContent LSP.MarkupKind_Markdown markdown))
        (Just (spanToRange sp))

makeBuiltinHover :: FuncName -> SourceSpan -> LSP.Hover
makeBuiltinHover fname sp =
  let content = case Map.lookup fname knownBuiltins of
        Just (sig, doc) ->
          let header = "```quant\n" <> sig <> "\n```"
              docPart = "\n\n" <> renderDoc doc
           in LSP.InL (LSP.MarkupContent LSP.MarkupKind_Markdown (header <> docPart))
        Nothing ->
          LSP.InL (LSP.mkMarkdownCodeBlock "quant" (unFuncName fname))
   in LSP.Hover content (Just (spanToRange sp))

makeTypeHover :: Type -> SourceSpan -> LSP.Hover
makeTypeHover ty sp =
  LSP.Hover
    (LSP.InL (LSP.mkMarkdownCodeBlock "quant" (showType ty)))
    (Just (spanToRange sp))

-- | Infer the module name from a qualified function name (e.g. "math.sqrt" -> Just "math").
moduleOrigin :: FuncName -> Maybe Text
moduleOrigin (FuncName n) =
  let (prefix, _) = T.breakOnEnd "." n
   in if T.null prefix then Nothing else Just (T.dropEnd 1 prefix)

-- ---------------------------------------------------------------------------
-- Structured doc-comment parsing

data DocSections = DocSections
  { docBody :: Text,
    docParams :: [(Text, Text)],
    docReturn :: Maybe Text,
    docExample :: Maybe Text
  }

-- | Parse raw doc text into structured sections.
-- Lines starting with @param, @return, @example are recognised as tags.
-- Everything before the first tag is treated as prose.
parseDoc :: Text -> DocSections
parseDoc raw =
  let ls = T.lines raw
      (bodyLines, tagLines) = break isTagLine ls
      body = T.stripEnd (T.unlines bodyLines)
      (params, ret, ex) = parseTags tagLines
   in DocSections body params ret ex
  where
    isTagLine l = T.isPrefixOf "@" (T.stripStart l)

parseTags :: [Text] -> ([(Text, Text)], Maybe Text, Maybe Text)
parseTags = go [] Nothing Nothing
  where
    isTagLine t = T.isPrefixOf "@" (T.stripStart t)

    go ps ret ex [] = (reverse ps, ret, ex)
    go ps ret ex (l : ls)
      | "@param " `T.isPrefixOf` s =
          let rest = T.strip (T.drop 7 s)
              (pname, pdesc) = T.breakOn " " rest
           in go ((T.strip pname, T.strip (T.drop 1 pdesc)) : ps) ret ex ls
      | "@return " `T.isPrefixOf` s =
          go ps (Just (T.strip (T.drop 8 s))) ex ls
      | "@return" == s =
          go ps Nothing ex ls
      | "@example" `T.isPrefixOf` s =
          let (exLines, rest_) = break isTagLine ls
           in go ps ret (Just (T.unlines exLines)) rest_
      | otherwise = go ps ret ex ls
      where
        s = T.stripStart l

-- | Render parsed doc sections to Markdown.
renderDocSections :: DocSections -> Text
renderDocSections secs =
  T.intercalate "\n\n" $
    filter
      (not . T.null)
      [ docBody secs,
        renderParamSection (docParams secs),
        renderReturnSection (docReturn secs),
        renderExampleSection (docExample secs)
      ]

renderParamSection :: [(Text, Text)] -> Text
renderParamSection [] = ""
renderParamSection ps =
  T.intercalate "\n" $
    "**Parameters**" : map (\(n, d) -> "- `" <> n <> "` \x2014 " <> d) ps

renderReturnSection :: Maybe Text -> Text
renderReturnSection Nothing = ""
renderReturnSection (Just d)
  | T.null d = ""
  | otherwise = "**Returns** \x2014 " <> d

renderExampleSection :: Maybe Text -> Text
renderExampleSection Nothing = ""
renderExampleSection (Just code) =
  "**Example**\n```quant\n" <> T.strip code <> "\n```"

-- | Convert raw doc text to rendered Markdown, handling @param / @return / @example tags.
renderDoc :: Text -> Text
renderDoc = renderDocSections . parseDoc

-- ---------------------------------------------------------------------------
-- Type and signature rendering

-- | Human-readable Quant type string.
showType :: Type -> Text
showType (TypePrimitive PrimNone) = "void"
showType (TypePrimitive PrimString) = "str"
showType (TypePrimitive p) = T.pack (show p)
showType (TypeArray (ArrayType qt)) = "[" <> showQT qt <> "]"
showType (TypeFunction ft) =
  "(" <> T.intercalate ", " paramTypes <> ") -> " <> showQT (unLocated (funcReturnType ft))
  where
    paramTypes =
      map
        (\(Located _ p) -> (if paramVariadic p then "..." else "") <> showQT (paramType p))
        (funcParams ft)
showType t = T.pack (show t)

-- | Render a QualifiedType, including the const qualifier when present.
showQT :: QualifiedType -> Text
showQT qt =
  (if qualConstness qt == Const then "const " else "") <> showType (qualType qt)

-- | Collect unique type-variable names referenced in a FunctionType.
-- Used to detect generic functions and render their type-param list.
collectTypeVars :: FunctionType -> [TypeName]
collectTypeVars ft =
  nub $
    concatMap (tvInType . qualType . paramType . unLocated) (funcParams ft)
      ++ tvInType (qualType (unLocated (funcReturnType ft)))

tvInType :: Type -> [TypeName]
tvInType (TypeVar n) = [n]
tvInType (TypeArray (ArrayType qt)) = tvInType (qualType qt)
tvInType (TypeFunction ft) = collectTypeVars ft
tvInType _ = []

-- | Render a full function signature, including generic type parameters when present.
renderSig :: FuncName -> FunctionType -> Text
renderSig (FuncName fname) ft =
  "fn " <> fname <> typeParamStr <> "(" <> params <> ") -> " <> ret
  where
    tvs = collectTypeVars ft
    typeParamStr
      | null tvs = ""
      | otherwise = "[" <> T.intercalate ", " (map unTypeName tvs) <> "]"
    params = T.intercalate ", " (map renderParam (funcParams ft))
    ret = showQT (unLocated (funcReturnType ft))
    renderParam (Located _ p) =
      unVarName (paramName p)
        <> ": "
        <> (if paramVariadic p then "..." else "")
        <> showQT (paramType p)

-- ---------------------------------------------------------------------------
-- Span conversion

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

-- ---------------------------------------------------------------------------
-- Built-in function signatures and documentation

-- | Hand-written signatures and structured doc strings for VM builtins.
-- Doc strings support @param, @return, and @example tags.
knownBuiltins :: Map FuncName (Text, Text)
knownBuiltins =
  Map.fromList
    [ ( FuncName "println",
        ( "fn println(value: any) -> void",
          "Print `value` to stdout followed by a newline.\n\
          \\n\
          \Accepts any type; the value is converted to its string representation.\n\
          \\n\
          \@param value The value to print"
        )
      ),
      ( FuncName "print",
        ( "fn print(value: any) -> void",
          "Print `value` to stdout without a trailing newline.\n\
          \\n\
          \Accepts any type; the value is converted to its string representation.\n\
          \\n\
          \@param value The value to print"
        )
      ),
      ( FuncName "len",
        ( "fn len(collection: str | [any]) -> int",
          "Return the number of characters in a string, or elements in an array.\n\
          \\n\
          \@param collection A string or array value\n\
          \@return Number of bytes in the string, or number of elements in the array"
        )
      ),
      ( FuncName "push",
        ( "fn push(arr: [any], value: any) -> void",
          "Append `value` to the end of `arr` in place.\n\
          \\n\
          \@param arr The array to mutate\n\
          \@param value The element to append"
        )
      ),
      ( FuncName "pop",
        ( "fn pop(arr: [any]) -> any",
          "Remove and return the last element of `arr`.\n\
          \\n\
          \Panics at runtime if the array is empty.\n\
          \\n\
          \@param arr The array to mutate\n\
          \@return The removed last element"
        )
      )
    ]
