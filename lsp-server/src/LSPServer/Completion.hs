module LSPServer.Completion (makeCompletionItems) where

import AST.Types.Common (ErrorName (..), FuncName (..), Located (..), VarName (..), unErrorName)
import AST.Types.Type (FunctionType (..), paramName)
import Data.Char (isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Hover (knownBuiltins, renderSig)
import qualified Language.LSP.Protocol.Types as LSP

-- | Build completion items from all known functions, keywords, and error names.
-- Filters by the identifier prefix at the cursor before constructing items.
makeCompletionItems ::
  Map FuncName FunctionType ->
  Map FuncName Text ->
  [ErrorName] ->
  Text ->
  Int ->
  Int ->
  [LSP.CompletionItem]
makeCompletionItems funcEnv docs errorNames fileText lspLine lspCol =
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
   in funcItems ++ builtinItems ++ keywordItems ++ errorItems

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
    (fmap (LSP.InR . LSP.MarkupContent LSP.MarkupKind_Markdown) (Map.lookup fname docs))
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
    (Just (LSP.InR (LSP.MarkupContent LSP.MarkupKind_Markdown doc)))
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
      (FuncName "pop", "pop($1)")
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
