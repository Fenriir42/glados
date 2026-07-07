module LSPServer.Completion (makeCompletionItems) where

import AST.Types.Common (ErrorName (..), FuncName (..), Located (..), SourceSpan, VarName (..), unErrorName)
import AST.Types.Type (FunctionType (..), paramName)
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Hover (knownBuiltins, renderDoc, renderSig)
import qualified Language.LSP.Protocol.Types as LSP

-- | Build completion items from all known functions, keywords, error names, and local variables.
-- Filters by the identifier prefix at the cursor before constructing items.
makeCompletionItems ::
  Map FuncName FunctionType ->
  Map FuncName Text ->
  [ErrorName] ->
  Map SourceSpan (VarName, SourceSpan) ->
  Text ->
  Int ->
  Int ->
  [LSP.CompletionItem]
makeCompletionItems funcEnv docs errorNames varDeclSites fileText lspLine lspCol =
  let prefix = getPrefix fileText lspLine lspCol
      prefixLower = T.toLower prefix
      matches lbl = prefixLower `T.isPrefixOf` T.toLower lbl
      funcItems =
        [ mkFuncItem docs kv
          | kv@(FuncName lbl, _) <- Map.toList funcEnv,
            matches lbl
        ]
      builtinItems =
        [ mkBuiltinItem kv
          | kv@(FuncName lbl, _) <- Map.toList knownBuiltins,
            matches lbl
        ]
      keywordItems = [mkKeywordSnippetItem kw | kw <- quantKeywords, matches kw]
      errorItems = [mkErrorNameItem e | e <- errorNames, matches (unErrorName e)]
      varNames = nub [vn | (vn, _) <- Map.elems varDeclSites]
      varItems = [mkVarItem vn | vn@(VarName lbl) <- varNames, matches lbl]
   in funcItems ++ builtinItems ++ keywordItems ++ errorItems ++ varItems

-- | Extract the identifier prefix at the cursor (may include a single '.').
getPrefix :: Text -> Int -> Int -> Text
getPrefix fileText line col =
  let ls = T.lines fileText
   in if line >= length ls
        then T.empty
        else
          let lineText = ls !! line
              beforeCursor = T.take col lineText
           in T.takeWhileEnd (\c -> c == '.' || c == '_' || isAlphaNum c) beforeCursor

mkFuncItem :: Map FuncName Text -> (FuncName, FunctionType) -> LSP.CompletionItem
mkFuncItem docs (fname, ft) =
  LSP.CompletionItem
    (unFuncName fname)
    Nothing
    (Just LSP.CompletionItemKind_Function)
    Nothing
    (Just (renderSig fname ft))
    (fmap (LSP.InR . LSP.MarkupContent LSP.MarkupKind_Markdown . renderDoc) (Map.lookup fname docs))
    Nothing
    Nothing
    Nothing
    Nothing
    (Just (insertSnippet fname ft))
    (Just LSP.InsertTextFormat_Snippet)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

mkBuiltinItem :: (FuncName, (Text, Text)) -> LSP.CompletionItem
mkBuiltinItem (fname, (sig, doc)) =
  LSP.CompletionItem
    (unFuncName fname)
    Nothing
    (Just LSP.CompletionItemKind_Function)
    Nothing
    (Just sig)
    (Just (LSP.InR (LSP.MarkupContent LSP.MarkupKind_Markdown (renderDoc doc))))
    Nothing
    Nothing
    Nothing
    Nothing
    (Map.lookup fname builtinSnippets)
    (Just LSP.InsertTextFormat_Snippet)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

mkKeywordSnippetItem :: Text -> LSP.CompletionItem
mkKeywordSnippetItem kw =
  let (insertText, insertKind) = case Map.lookup kw keywordSnippets of
        Just snippet -> (snippet, LSP.InsertTextFormat_Snippet)
        Nothing -> (kw, LSP.InsertTextFormat_PlainText)
   in LSP.CompletionItem
        kw
        Nothing
        (Just LSP.CompletionItemKind_Keyword)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (Just insertText)
        (Just insertKind)
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

mkVarItem :: VarName -> LSP.CompletionItem
mkVarItem (VarName name) =
  LSP.CompletionItem
    name
    Nothing
    (Just LSP.CompletionItemKind_Variable)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    (Just name)
    (Just LSP.InsertTextFormat_PlainText)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

mkErrorNameItem :: ErrorName -> LSP.CompletionItem
mkErrorNameItem (ErrorName name) =
  LSP.CompletionItem
    name
    Nothing
    (Just LSP.CompletionItemKind_Class)
    Nothing
    (Just "error type")
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

keywordSnippets :: Map Text Text
keywordSnippets =
  Map.fromList
    [ ("fn", "fn ${1:name}(${2:params}) -> ${3:void} {\n\t$0\n}"),
      ("if", "if (${1:condition}) {\n\t$0\n}"),
      ("while", "while (${1:condition}) {\n\t$0\n}"),
      ("for", "for (${1:init}; ${2:cond}; ${3:step}) {\n\t$0\n}"),
      ("return", "return $1"),
      ("match", "match (${1:expr}) {\n\tsome(${2:x}) => { $0 },\n\tnone => { }\n}"),
      ("struct", "struct ${1:Name} {\n\t${2:field}: ${3:int}\n}"),
      ("error", "error ${1:ErrorName};"),
      ("orerror", "orerror(${1:T}, ${2:ErrorName})"),
      ("try", "try $1"),
      ("must", "must $1")
    ]

-- | Snippet insert text for a user-defined function: `name($1:param1, $2:param2)`.
insertSnippet :: FuncName -> FunctionType -> Text
insertSnippet (FuncName fname) ft =
  let ps = funcParams ft
   in if null ps
        then fname <> "()"
        else
          let snippets =
                zipWith
                  ( \i (Located _ p) ->
                      "${" <> T.pack (show (i :: Int)) <> ":" <> unVarName (paramName p) <> "}"
                  )
                  [1 ..]
                  ps
           in fname <> "(" <> T.intercalate ", " snippets <> ")"

-- | Hand-written snippet insert text for VM builtins.
builtinSnippets :: Map FuncName Text
builtinSnippets =
  Map.fromList
    [ (FuncName "println", "println($1)"),
      (FuncName "print", "print($1)"),
      (FuncName "len", "len($1)"),
      (FuncName "push", "push($1, $2)"),
      (FuncName "pop", "pop($1)"),
      -- math
      (FuncName "math.sqrt", "math.sqrt($1)"),
      (FuncName "math.abs", "math.abs($1)"),
      (FuncName "math.fabs", "math.fabs($1)"),
      (FuncName "math.floor", "math.floor($1)"),
      (FuncName "math.ceil", "math.ceil($1)"),
      (FuncName "math.round", "math.round($1)"),
      (FuncName "math.pow", "math.pow($1, $2)"),
      (FuncName "math.exp", "math.exp($1)"),
      (FuncName "math.log", "math.log($1)"),
      (FuncName "math.sin", "math.sin($1)"),
      (FuncName "math.cos", "math.cos($1)"),
      (FuncName "math.tan", "math.tan($1)"),
      (FuncName "math.asin", "math.asin($1)"),
      (FuncName "math.acos", "math.acos($1)"),
      (FuncName "math.atan", "math.atan($1)"),
      (FuncName "math.atan2", "math.atan2($1, $2)"),
      (FuncName "math.min", "math.min($1, $2)"),
      (FuncName "math.max", "math.max($1, $2)"),
      (FuncName "math.fmin", "math.fmin($1, $2)"),
      (FuncName "math.fmax", "math.fmax($1, $2)"),
      -- string
      (FuncName "string.len", "string.len($1)"),
      (FuncName "string.concat", "string.concat($1, $2)"),
      (FuncName "string.substring", "string.substring($1, $2, $3)"),
      (FuncName "string.char_at", "string.char_at($1, $2)"),
      (FuncName "string.contains", "string.contains($1, $2)"),
      (FuncName "string.starts_with", "string.starts_with($1, $2)"),
      (FuncName "string.ends_with", "string.ends_with($1, $2)"),
      (FuncName "string.index_of", "string.index_of($1, $2)"),
      (FuncName "string.last_index_of", "string.last_index_of($1, $2)"),
      (FuncName "string.to_upper", "string.to_upper($1)"),
      (FuncName "string.to_lower", "string.to_lower($1)"),
      (FuncName "string.trim", "string.trim($1)"),
      (FuncName "string.trim_left", "string.trim_left($1)"),
      (FuncName "string.trim_right", "string.trim_right($1)"),
      (FuncName "string.reverse", "string.reverse($1)"),
      (FuncName "string.replace", "string.replace($1, $2, $3)"),
      (FuncName "string.replace_first", "string.replace_first($1, $2, $3)"),
      (FuncName "string.repeat", "string.repeat($1, $2)"),
      (FuncName "string.is_empty", "string.is_empty($1)"),
      (FuncName "string.to_str", "string.to_str($1)"),
      (FuncName "string.from_int", "string.from_int($1)"),
      (FuncName "string.from_float", "string.from_float($1)"),
      (FuncName "string.to_int", "string.to_int($1)"),
      (FuncName "string.to_float", "string.to_float($1)"),
      (FuncName "string.hash", "string.hash($1)"),
      (FuncName "string.split", "string.split($1, $2)"),
      (FuncName "string.join", "string.join($1, $2)"),
      -- array
      (FuncName "array.len", "array.len($1)"),
      (FuncName "array.push", "array.push($1, $2)"),
      (FuncName "array.pop", "array.pop($1)"),
      -- io
      (FuncName "io.print", "io.print($1)"),
      (FuncName "io.println", "io.println($1)"),
      (FuncName "io.read", "io.read()"),
      -- sys
      (FuncName "sys.exit", "sys.exit($1)"),
      (FuncName "sys.time", "sys.time()"),
      (FuncName "sys.time_millis", "sys.time_millis()"),
      (FuncName "sys.sleep", "sys.sleep($1)"),
      (FuncName "sys.argc", "sys.argc()"),
      (FuncName "sys.args", "sys.args()"),
      (FuncName "sys.env", "sys.env($1)"),
      (FuncName "sys.set_env", "sys.set_env($1, $2)"),
      (FuncName "sys.platform", "sys.platform()"),
      (FuncName "sys.hostname", "sys.hostname()"),
      (FuncName "sys.getcwd", "sys.getcwd()"),
      (FuncName "sys.chdir", "sys.chdir($1)"),
      (FuncName "sys.system", "sys.system($1)"),
      (FuncName "sys.write", "sys.write($1, $2)"),
      (FuncName "sys.read", "sys.read($1, $2)"),
      (FuncName "sys.open", "sys.open($1, $2)"),
      (FuncName "sys.close", "sys.close($1)"),
      (FuncName "sys.isatty", "sys.isatty($1)"),
      (FuncName "sys.stdout_fd", "sys.stdout_fd()"),
      (FuncName "sys.stderr_fd", "sys.stderr_fd()"),
      (FuncName "sys.stdin_fd", "sys.stdin_fd()"),
      -- file
      (FuncName "file.read", "file.read($1)"),
      (FuncName "file.write", "file.write($1, $2)"),
      (FuncName "file.append", "file.append($1, $2)"),
      (FuncName "file.exists", "file.exists($1)"),
      (FuncName "file.delete", "file.delete($1)"),
      (FuncName "file.rename", "file.rename($1, $2)"),
      (FuncName "file.size", "file.size($1)"),
      (FuncName "file.lines", "file.lines($1)"),
      -- buf
      (FuncName "buf.new", "buf.new()"),
      (FuncName "buf.write", "buf.write($1, $2)"),
      (FuncName "buf.writeln", "buf.writeln($1, $2)"),
      (FuncName "buf.to_str", "buf.to_str($1)"),
      (FuncName "buf.len", "buf.len($1)"),
      (FuncName "buf.clear", "buf.clear($1)"),
      (FuncName "buf.flush", "buf.flush($1, $2)"),
      -- dict
      (FuncName "dict.has", "dict.has($1, $2)"),
      (FuncName "dict.len", "dict.len($1)"),
      (FuncName "dict.delete", "dict.delete($1, $2)"),
      (FuncName "dict.keys", "dict.keys($1)"),
      (FuncName "dict.values", "dict.values($1)"),
      -- json
      (FuncName "json.encode", "json.encode($1)"),
      (FuncName "json.decode_str", "json.decode_str($1, $2)"),
      (FuncName "json.decode_int", "json.decode_int($1, $2)"),
      (FuncName "json.decode_float", "json.decode_float($1, $2)"),
      (FuncName "json.decode_bool", "json.decode_bool($1, $2)"),
      (FuncName "json.has", "json.has($1, $2)"),
      (FuncName "json.is_null", "json.is_null($1, $2)"),
      (FuncName "json.keys", "json.keys($1)"),
      (FuncName "json.parse", "json.parse($1)"),
      -- socket
      (FuncName "socket.connect", "socket.connect($1, $2)"),
      (FuncName "socket.listen", "socket.listen($1, $2)"),
      (FuncName "socket.accept", "socket.accept($1)"),
      (FuncName "socket.send", "socket.send($1, $2)"),
      (FuncName "socket.recv", "socket.recv($1, $2)"),
      (FuncName "socket.close", "socket.close($1)"),
      (FuncName "socket.peer_addr", "socket.peer_addr($1)")
    ]

quantKeywords :: [Text]
quantKeywords =
  [ "fn",
    "if",
    "else",
    "while",
    "for",
    "return",
    "break",
    "continue",
    "match",
    "some",
    "none",
    "struct",
    "import",
    "from",
    "const",
    "static",
    "true",
    "false",
    "int",
    "float",
    "str",
    "bool",
    "void",
    "error",
    "orerror",
    "try",
    "must"
  ]
