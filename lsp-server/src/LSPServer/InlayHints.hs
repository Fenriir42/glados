module LSPServer.InlayHints (makeInlayHints) where

import AST.Types.Common
  ( Column (..),
    FuncName,
    Line (..),
    Located (..),
    SourcePos (..),
    SourceSpan (..),
    VarName (..),
  )
import AST.Types.Type (FunctionType (..), Type, paramName)
import Data.Map (Map)
import qualified Data.Map as Map
import LSPServer.Hover (showType)
import qualified Language.LSP.Protocol.Types as LSP

-- | Emit parameter-name hints and match-binding type hints within the visible range.
makeInlayHints ::
  Map SourceSpan (FuncName, FunctionType, [SourceSpan]) ->
  Map SourceSpan Type ->
  LSP.Range ->
  [LSP.InlayHint]
makeInlayHints callWithArgs varDeclTypes visibleRange =
  concatMap mkParamHints (Map.toList callWithArgs)
    ++ concatMap mkTypeHint (Map.toList varDeclTypes)
  where
    -- Parameter-name hints: show `paramName:` before each arg for calls with >= 2 params.
    mkParamHints (_, (_, ft, argSpans))
      | length (funcParams ft) < 2 = []
      | otherwise =
          [ LSP.InlayHint
              (spanStartPos argSp)
              (LSP.InL (unVarName (paramName (locValue p)) <> ":"))
              (Just LSP.InlayHintKind_Parameter)
              Nothing
              Nothing
              Nothing
              (Just True)
              Nothing
            | (p, argSp) <- zip (funcParams ft) argSpans,
              spanVisible argSp
          ]

    -- Type hints for match-arm bindings: show `: type` after the binding name.
    mkTypeHint (sp, ty)
      | not (spanVisible sp) = []
      | otherwise =
          [ LSP.InlayHint
              (spanEndPos sp)
              (LSP.InL (": " <> showType ty))
              (Just LSP.InlayHintKind_Type)
              Nothing
              Nothing
              Nothing
              (Just True)
              Nothing
          ]

    spanStartPos sp =
      LSP.Position
        ((fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanStart sp))) - 1) :: LSP.UInt)

    spanEndPos sp =
      LSP.Position
        ((fromIntegral (unLine (posLine (spanEnd sp))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanEnd sp))) - 1) :: LSP.UInt)

    spanVisible sp =
      let spLine = (fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt
          LSP.Range (LSP.Position rStart _) (LSP.Position rEnd _) = visibleRange
       in spLine >= rStart && spLine <= rEnd
