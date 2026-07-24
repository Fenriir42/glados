module LSPServer.OnTypeFormat (onTypeFormat) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

-- | Produce edits for on-type formatting.
-- ch is the character just typed (already in the document).
-- lspLine / lspCol are the LSP cursor position AFTER the character was inserted.
onTypeFormat :: Text -> Int -> Int -> Char -> [LSP.TextEdit]
onTypeFormat src lspLine lspCol ch
  | ch == '\n' = handleNewline src lspLine
  | ch == '}' = handleClosingBrace src lspLine lspCol
  | otherwise = []

-- | After Enter: indent the new line to match the previous line, plus one
-- extra level if the previous line ended with '{'.
handleNewline :: Text -> Int -> [LSP.TextEdit]
handleNewline src newLine =
  let ls = T.lines src
      prevLine =
        if newLine > 0 && newLine - 1 < length ls
          then ls !! (newLine - 1)
          else T.empty
      prevIndent = T.takeWhile (== ' ') prevLine
      extra =
        if T.isSuffixOf "{" (T.stripEnd prevLine) then "    " else T.empty
      indent = prevIndent <> extra
   in [ LSP.TextEdit
          ( LSP.Range
              (LSP.Position (fromIntegral newLine) 0)
              (LSP.Position (fromIntegral newLine) 0)
          )
          indent
        | not (T.null indent)
      ]

-- | After }: align the current line's indentation to match the opening '{'.
handleClosingBrace :: Text -> Int -> Int -> [LSP.TextEdit]
handleClosingBrace src lspLine lspCol =
  let ls = T.lines src
      curLine = if lspLine < length ls then ls !! lspLine else T.empty
      curIndent = T.takeWhile (== ' ') curLine
   in case findMatchingBraceIndent ls lspLine (lspCol - 1) of
        Nothing -> []
        Just matchIndent ->
          [ LSP.TextEdit
              ( LSP.Range
                  (LSP.Position (fromIntegral lspLine) 0)
                  ( LSP.Position
                      (fromIntegral lspLine)
                      (fromIntegral (T.length curIndent))
                  )
              )
              matchIndent
            | matchIndent /= curIndent
          ]

-- | Scan backwards from (startLine, startCol-1) counting brace nesting.
-- Returns the leading whitespace of the line containing the matching '{'.
findMatchingBraceIndent :: [Text] -> Int -> Int -> Maybe Text
findMatchingBraceIndent ls startLine startCol =
  go startLine (startCol - 1) 1
  where
    go :: Int -> Int -> Int -> Maybe Text
    go line col depth
      | line < 0 = Nothing
      | col < 0 =
          let prev = line - 1
           in if prev < 0
                then Nothing
                else
                  let prevLine = ls !! prev
                   in go prev (T.length prevLine - 1) depth
      | otherwise =
          let curLine = if line < length ls then ls !! line else T.empty
              ch = if col < T.length curLine then T.index curLine col else ' '
           in case ch of
                '{' | depth == 1 -> Just (T.takeWhile (== ' ') curLine)
                '{' -> go line (col - 1) (depth - 1)
                '}' -> go line (col - 1) (depth + 1)
                _ -> go line (col - 1) depth
