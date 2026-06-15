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
import AST.Types.Type (FunctionType (..), paramName)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Language.LSP.Protocol.Types as LSP

-- | Emit parameter-name hints at the start of each argument for calls with >= 2 params.
makeInlayHints ::
  Map SourceSpan (FuncName, FunctionType, [SourceSpan]) ->
  LSP.Range ->
  [LSP.InlayHint]
makeInlayHints callWithArgs visibleRange =
  concatMap mkHints (Map.toList callWithArgs)
  where
    mkHints (_, (_, ft, argSpans))
      | length (funcParams ft) < 2 = []
      | otherwise =
          [ LSP.InlayHint
              (argStartPos argSp)
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

    argStartPos sp =
      LSP.Position
        ((fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt)
        ((fromIntegral (unColumn (posColumn (spanStart sp))) - 1) :: LSP.UInt)

    spanVisible sp =
      let spLine = (fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt
          LSP.Range (LSP.Position rStart _) (LSP.Position rEnd _) = visibleRange
       in spLine >= rStart && spLine <= rEnd
