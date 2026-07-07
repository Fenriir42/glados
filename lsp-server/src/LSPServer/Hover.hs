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
  Map.fromList $
    standaloneBuiltins
      ++ mathBuiltins
      ++ stringBuiltins
      ++ arrayBuiltins
      ++ ioBuiltins
      ++ sysBuiltins
      ++ fileBuiltins
      ++ bufBuiltins
      ++ dictBuiltins
      ++ jsonBuiltins
      ++ socketBuiltins

b :: Text -> Text -> Text -> (FuncName, (Text, Text))
b name sig doc = (FuncName name, (sig, doc))

standaloneBuiltins :: [(FuncName, (Text, Text))]
standaloneBuiltins =
  [ b
      "println"
      "fn println(value: any) -> void"
      "Print `value` to stdout followed by a newline.\n\nAccepts any type.",
    b
      "print"
      "fn print(value: any) -> void"
      "Print `value` to stdout without a trailing newline.\n\nAccepts any type.",
    b
      "len"
      "fn len(collection: str | [any]) -> int"
      "Return the number of characters in a string, or elements in an array.",
    b
      "push"
      "fn push(arr: [any], value: any) -> void"
      "Append `value` to the end of `arr` in place.",
    b
      "pop"
      "fn pop(arr: [any]) -> any"
      "Remove and return the last element of `arr`. Panics if the array is empty."
  ]

mathBuiltins :: [(FuncName, (Text, Text))]
mathBuiltins =
  [ b "math.sqrt" "fn math.sqrt(x: float) -> float" "Square root of `x`.",
    b "math.abs" "fn math.abs(x: int) -> int" "Absolute value of `x`.",
    b "math.fabs" "fn math.fabs(x: float) -> float" "Absolute value of `x` (float).",
    b "math.floor" "fn math.floor(x: float) -> int" "Round `x` down to the nearest integer.",
    b "math.ceil" "fn math.ceil(x: float) -> int" "Round `x` up to the nearest integer.",
    b "math.round" "fn math.round(x: float) -> int" "Round `x` to the nearest integer.",
    b "math.pow" "fn math.pow(base: float, exp: float) -> float" "Raise `base` to the power `exp`.",
    b "math.exp" "fn math.exp(x: float) -> float" "e raised to the power `x`.",
    b "math.log" "fn math.log(x: float) -> float" "Natural logarithm of `x`.",
    b "math.sin" "fn math.sin(x: float) -> float" "Sine of `x` (radians).",
    b "math.cos" "fn math.cos(x: float) -> float" "Cosine of `x` (radians).",
    b "math.tan" "fn math.tan(x: float) -> float" "Tangent of `x` (radians).",
    b "math.asin" "fn math.asin(x: float) -> float" "Arc sine of `x` (radians).",
    b "math.acos" "fn math.acos(x: float) -> float" "Arc cosine of `x` (radians).",
    b "math.atan" "fn math.atan(x: float) -> float" "Arc tangent of `x` (radians).",
    b "math.atan2" "fn math.atan2(y: float, x: float) -> float" "Arc tangent of `y/x`, using signs to determine the quadrant.",
    b "math.min" "fn math.min(a: int, b: int) -> int" "Return the smaller of `a` and `b`.",
    b "math.max" "fn math.max(a: int, b: int) -> int" "Return the larger of `a` and `b`.",
    b "math.fmin" "fn math.fmin(a: float, b: float) -> float" "Return the smaller of `a` and `b` (float).",
    b "math.fmax" "fn math.fmax(a: float, b: float) -> float" "Return the larger of `a` and `b` (float)."
  ]

stringBuiltins :: [(FuncName, (Text, Text))]
stringBuiltins =
  [ b "string.len" "fn string.len(s: str) -> int" "Number of characters in `s`.",
    b "string.concat" "fn string.concat(a: str, b: str) -> str" "Concatenate `a` and `b`.",
    b "string.substring" "fn string.substring(s: str, start: int, end: int) -> str" "Slice `s` from `start` (inclusive) to `end` (exclusive).",
    b "string.char_at" "fn string.char_at(s: str, i: int) -> str" "Single character at index `i`.",
    b "string.contains" "fn string.contains(s: str, sub: str) -> bool" "True if `sub` appears anywhere in `s`.",
    b "string.starts_with" "fn string.starts_with(s: str, prefix: str) -> bool" "True if `s` begins with `prefix`.",
    b "string.ends_with" "fn string.ends_with(s: str, suffix: str) -> bool" "True if `s` ends with `suffix`.",
    b "string.index_of" "fn string.index_of(s: str, sub: str) -> int" "First index of `sub` in `s`, or -1 if not found.",
    b "string.last_index_of" "fn string.last_index_of(s: str, sub: str) -> int" "Last index of `sub` in `s`, or -1 if not found.",
    b "string.to_upper" "fn string.to_upper(s: str) -> str" "Convert `s` to uppercase.",
    b "string.to_lower" "fn string.to_lower(s: str) -> str" "Convert `s` to lowercase.",
    b "string.trim" "fn string.trim(s: str) -> str" "Remove leading and trailing whitespace.",
    b "string.trim_left" "fn string.trim_left(s: str) -> str" "Remove leading whitespace.",
    b "string.trim_right" "fn string.trim_right(s: str) -> str" "Remove trailing whitespace.",
    b "string.reverse" "fn string.reverse(s: str) -> str" "Reverse `s`.",
    b "string.replace" "fn string.replace(s: str, old: str, new: str) -> str" "Replace all occurrences of `old` with `new`.",
    b "string.replace_first" "fn string.replace_first(s: str, old: str, new: str) -> str" "Replace the first occurrence of `old` with `new`.",
    b "string.repeat" "fn string.repeat(s: str, n: int) -> str" "Repeat `s` exactly `n` times.",
    b "string.is_empty" "fn string.is_empty(s: str) -> bool" "True if `s` has zero characters.",
    b "string.to_str" "fn string.to_str(value: any) -> str" "Convert any value to its string representation.",
    b "string.from_int" "fn string.from_int(n: int) -> str" "Format an integer as a decimal string.",
    b "string.from_float" "fn string.from_float(f: float) -> str" "Format a float as a string.",
    b "string.to_int" "fn string.to_int(s: str) -> int" "Parse `s` as a decimal integer. Returns 0 on failure.",
    b "string.to_float" "fn string.to_float(s: str) -> float" "Parse `s` as a float. Returns 0.0 on failure.",
    b "string.hash" "fn string.hash(s: str) -> int" "FNV-1a 32-bit hash of `s`. Always non-negative.",
    b "string.split" "fn string.split(s: str, sep: str) -> [str]" "Split `s` on every occurrence of `sep`. Returns an array of parts.",
    b "string.join" "fn string.join(parts: [str], sep: str) -> str" "Join `parts` into a single string with `sep` between each element."
  ]

arrayBuiltins :: [(FuncName, (Text, Text))]
arrayBuiltins =
  [ b "array.len" "fn array.len(arr: [any]) -> int" "Number of elements in `arr`.",
    b "array.push" "fn array.push(arr: [any], value: any) -> void" "Append `value` to `arr` in place.",
    b "array.pop" "fn array.pop(arr: [any]) -> any" "Remove and return the last element of `arr`."
  ]

ioBuiltins :: [(FuncName, (Text, Text))]
ioBuiltins =
  [ b "io.print" "fn io.print(value: any) -> void" "Print `value` to stdout without a newline.",
    b "io.println" "fn io.println(value: any) -> void" "Print `value` to stdout followed by a newline.",
    b "io.read" "fn io.read() -> str" "Read one line from stdin (strips the trailing newline)."
  ]

sysBuiltins :: [(FuncName, (Text, Text))]
sysBuiltins =
  [ b "sys.exit" "fn sys.exit(code: int) -> void" "Terminate the process with exit code `code`.",
    b "sys.time" "fn sys.time() -> int" "Current Unix timestamp in seconds.",
    b "sys.time_millis" "fn sys.time_millis() -> int" "CPU time in milliseconds.",
    b "sys.sleep" "fn sys.sleep(ms: int) -> void" "Pause execution for `ms` milliseconds.",
    b "sys.argc" "fn sys.argc() -> int" "Number of command-line arguments.",
    b "sys.args" "fn sys.args() -> [str]" "Command-line arguments as an array of strings.",
    b "sys.env" "fn sys.env(name: str) -> str" "Read environment variable `name`. Returns \"\" if unset.",
    b "sys.set_env" "fn sys.set_env(name: str, value: str) -> bool" "Set environment variable `name` to `value`.",
    b "sys.platform" "fn sys.platform() -> str" "OS name: `\"linux\"`, `\"macos\"`, or `\"windows\"`.",
    b "sys.hostname" "fn sys.hostname() -> str" "Machine hostname.",
    b "sys.getcwd" "fn sys.getcwd() -> str" "Current working directory.",
    b "sys.chdir" "fn sys.chdir(path: str) -> bool" "Change working directory. Returns true on success.",
    b "sys.system" "fn sys.system(cmd: str) -> int" "Run a shell command and return its exit code.",
    b "sys.write" "fn sys.write(fd: int, data: str) -> int" "Write `data` to file descriptor `fd`. Returns bytes written or -1.",
    b "sys.read" "fn sys.read(fd: int, n: int) -> str" "Read up to `n` bytes from file descriptor `fd`.",
    b "sys.open" "fn sys.open(path: str, flags: int) -> int" "Open a file and return a file descriptor. Use `sys.o_*` constants for flags.",
    b "sys.close" "fn sys.close(fd: int) -> bool" "Close file descriptor `fd`.",
    b "sys.isatty" "fn sys.isatty(fd: int) -> bool" "True if `fd` is connected to a terminal.",
    b "sys.stdout_fd" "fn sys.stdout_fd() -> int" "File descriptor number for stdout (1).",
    b "sys.stderr_fd" "fn sys.stderr_fd() -> int" "File descriptor number for stderr (2).",
    b "sys.stdin_fd" "fn sys.stdin_fd() -> int" "File descriptor number for stdin (0)."
  ]

fileBuiltins :: [(FuncName, (Text, Text))]
fileBuiltins =
  [ b "file.read" "fn file.read(path: str) -> str" "Read the entire contents of `path` as a string. Returns \"\" on error.",
    b "file.write" "fn file.write(path: str, content: str) -> bool" "Write `content` to `path`, replacing any existing contents.",
    b "file.append" "fn file.append(path: str, content: str) -> bool" "Append `content` to `path`.",
    b "file.exists" "fn file.exists(path: str) -> bool" "True if the file at `path` exists.",
    b "file.delete" "fn file.delete(path: str) -> bool" "Delete the file at `path`.",
    b "file.rename" "fn file.rename(old: str, new: str) -> bool" "Rename (move) a file.",
    b "file.size" "fn file.size(path: str) -> int" "Size of the file at `path` in bytes. Returns -1 on error.",
    b "file.lines" "fn file.lines(path: str) -> [str]" "Read `path` and return its lines as an array of strings."
  ]

bufBuiltins :: [(FuncName, (Text, Text))]
bufBuiltins =
  [ b "buf.new" "fn buf.new() -> [str]" "Create a new empty string buffer backed by a `[str]` array.",
    b "buf.write" "fn buf.write(b: [str], s: str) -> void" "Append `s` to buffer `b`.",
    b "buf.writeln" "fn buf.writeln(b: [str], s: str) -> void" "Append `s` followed by a newline to buffer `b`.",
    b "buf.to_str" "fn buf.to_str(b: [str]) -> str" "Return all buffer contents joined into a single string.",
    b "buf.len" "fn buf.len(b: [str]) -> int" "Total number of characters stored in the buffer.",
    b "buf.clear" "fn buf.clear(b: [str]) -> void" "Remove all contents from the buffer.",
    b "buf.flush" "fn buf.flush(b: [str], fd: int) -> int" "Write the buffer to file descriptor `fd`, clear it, and return bytes written."
  ]

dictBuiltins :: [(FuncName, (Text, Text))]
dictBuiltins =
  [ b "dict.has" "fn dict.has(d: dict(K,V), key: K) -> bool" "True if `key` is present in `d`.",
    b "dict.len" "fn dict.len(d: dict(K,V)) -> int" "Number of key-value pairs in `d`.",
    b "dict.delete" "fn dict.delete(d: dict(K,V), key: K) -> void" "Remove `key` from `d`. No-op if the key is absent.",
    b "dict.keys" "fn dict.keys(d: dict(K,V)) -> [K]" "Return all keys of `d` as an array.",
    b "dict.values" "fn dict.values(d: dict(K,V)) -> [V]" "Return all values of `d` as an array."
  ]

jsonBuiltins :: [(FuncName, (Text, Text))]
jsonBuiltins =
  [ b "json.encode" "fn json.encode(value: any) -> str" "Encode any Quant value (int, float, bool, str, array, dict, struct) as a JSON string.",
    b "json.decode_str" "fn json.decode_str(s: str, key: str) -> str" "Extract a string field from a JSON object string. Runtime error if absent or wrong type.",
    b "json.decode_int" "fn json.decode_int(s: str, key: str) -> int" "Extract an integer field from a JSON object string.",
    b "json.decode_float" "fn json.decode_float(s: str, key: str) -> float" "Extract a float field from a JSON object string.",
    b "json.decode_bool" "fn json.decode_bool(s: str, key: str) -> bool" "Extract a boolean field from a JSON object string.",
    b "json.has" "fn json.has(s: str, key: str) -> bool" "True if `key` exists in the JSON object string `s`.",
    b "json.is_null" "fn json.is_null(s: str, key: str) -> bool" "True if the value of `key` in the JSON object string `s` is `null`.",
    b "json.keys" "fn json.keys(s: str) -> [str]" "Return all keys of the JSON object string `s` as an array.",
    b "json.parse" "fn json.parse(s: str) -> dict(str, str)" "Parse a JSON object into a `dict(str, str)`, coercing all values to strings."
  ]

socketBuiltins :: [(FuncName, (Text, Text))]
socketBuiltins =
  [ b "socket.connect" "fn socket.connect(host: str, port: int) -> int" "Open a TCP connection to `host:port`. Returns a socket id (>= 0) or -1 on failure.",
    b "socket.listen" "fn socket.listen(port: int, backlog: int) -> int" "Bind and listen on `port`. Returns a server socket id or -1 on failure.",
    b "socket.accept" "fn socket.accept(server: int) -> int" "Accept one incoming TCP connection. Blocks until a client connects. Returns client socket id or -1.",
    b "socket.send" "fn socket.send(sock: int, data: str) -> int" "Send `data` on `sock`. Returns bytes sent or -1 on error.",
    b "socket.recv" "fn socket.recv(sock: int, n: int) -> str" "Receive up to `n` bytes from `sock`. Returns `\"\"` on close or error.",
    b "socket.close" "fn socket.close(sock: int) -> bool" "Close `sock` and release its resources.",
    b "socket.peer_addr" "fn socket.peer_addr(sock: int) -> str" "Remote address of `sock` as `\"host:port\"`, or `\"\"` on error."
  ]
