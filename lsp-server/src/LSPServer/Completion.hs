module LSPServer.Completion (makeCompletionItems) where

import AST.Types.Common (FuncName (..), Located (..), VarName (..))
import AST.Types.Type (FunctionType (..), paramName)
import Data.Char (isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Hover (knownBuiltins, renderSig)
import qualified Language.LSP.Protocol.Types as LSP

-- | Build completion items from all known functions and keywords.
-- Filters by the identifier prefix at the cursor before constructing items.
makeCompletionItems ::
  Map FuncName FunctionType ->
  Map FuncName Text ->
  Text ->
  Int ->
  Int ->
  [LSP.CompletionItem]
makeCompletionItems funcEnv docs fileText lspLine lspCol =
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
      keywordItems = [mkKeywordItem kw | kw <- quantKeywords, matches kw]
   in funcItems ++ builtinItems ++ keywordItems

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

mkKeywordItem :: Text -> LSP.CompletionItem
mkKeywordItem kw =
  LSP.CompletionItem
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
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

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
    "import",
    "from",
    "true",
    "false",
    "int",
    "float",
    "string",
    "bool"
  ]
