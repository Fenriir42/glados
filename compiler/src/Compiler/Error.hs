-- | Rust-style pretty-printing for compile errors: file/line/column,
-- source line, and a caret pointing at the problem.
module Compiler.Error (displayError) where

import AST.Types.Common
  ( Column (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
    displaySpan,
  )
import Compiler.Codegen (CompileError, errSpan, errorMessage)
import qualified Data.Text as T

-- ANSI helpers
esc :: String -> String
esc code = "\ESC[" ++ code ++ "m"

reset, bold, red, cyan :: String
reset = esc "0"
bold = esc "1"
red = esc "31"
cyan = esc "36"

-- | Render a compile error with source-line context.
-- @sourceLines@ is the file split on newlines (1-indexed via @!!@).
displayError :: CompileError -> [String] -> String
displayError err sourceLines =
  let span' = errSpan err
      startPos = spanStart span'
      lineNo = unLine (posLine startPos)
      colStart = unColumn (posColumn startPos)
      colEnd = unColumn (posColumn (spanEnd span'))
      lineStr = show lineNo
      pad = replicate (length lineStr) ' '
      srcLine =
        if lineNo >= 1 && lineNo <= length sourceLines
          then sourceLines !! (lineNo - 1)
          else ""
      caretLen = max 1 (if colEnd > colStart then colEnd - colStart else 1)
      caret =
        replicate (colStart - 1) ' '
          ++ bold
          ++ red
          ++ replicate caretLen '^'
          ++ reset
      loc = T.unpack (displaySpan span')
      msg = errorMessage err
   in unlines
        [ bold ++ red ++ "error" ++ reset ++ ": " ++ bold ++ msg ++ reset,
          " " ++ bold ++ cyan ++ pad ++ " --> " ++ reset ++ loc,
          " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset,
          " " ++ bold ++ cyan ++ lineStr ++ "  |" ++ reset ++ " " ++ srcLine,
          " " ++ bold ++ cyan ++ pad ++ "  |" ++ reset ++ " " ++ caret
        ]
