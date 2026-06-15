module LSPServer.SignatureHelp (findSignatureHelp) where

import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    Located (..),
    SourcePos (..),
    SourceSpan (..),
    VarName (..),
  )
import AST.Types.Type
  ( FunctionType (..),
    Parameter (..),
    paramName,
    paramType,
    qualType,
  )
import Data.List (findIndex, minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Hover (renderSig, showType)
import qualified Language.LSP.Protocol.Types as LSP

-- | Find signature help for the active function call at the cursor.
findSignatureHelp ::
  Map SourceSpan (FuncName, FunctionType, [SourceSpan]) ->
  Map FuncName Text ->
  Int ->
  Int ->
  Maybe LSP.SignatureHelp
findSignatureHelp callWithArgs docs lspLine lspCol = do
  (_, (fname, ft, argSpans)) <- smallestCall callWithArgs lspLine lspCol
  let sig = renderSig fname ft
      params = funcParams ft
      paramInfos = zipWith (curry (makeParamInfo fname ft)) [0 ..] params
      activeIdx = fromIntegral (activeParam argSpans lspLine lspCol)
      doc = Map.lookup fname docs
      sigInfo =
        LSP.SignatureInformation
          sig
          (fmap (LSP.InR . LSP.MarkupContent LSP.MarkupKind_Markdown) doc)
          (Just paramInfos)
          Nothing
  return $
    LSP.SignatureHelp
      [sigInfo]
      (Just 0)
      (Just (LSP.InL activeIdx))

-- | The smallest call-with-args span containing the cursor.
smallestCall ::
  Map SourceSpan (FuncName, FunctionType, [SourceSpan]) ->
  Int ->
  Int ->
  Maybe (SourceSpan, (FuncName, FunctionType, [SourceSpan]))
smallestCall m lspLine lspCol =
  case filter (containsPos lspLine lspCol . fst) (Map.toList m) of
    [] -> Nothing
    pairs -> Just (minimumBy (comparing (spanLength . fst)) pairs)

-- | Which parameter index the cursor is at (0-based).
activeParam :: [SourceSpan] -> Int -> Int -> Int
activeParam [] _ _ = 0
activeParam argSpans lspLine lspCol =
  case findIndex (containsPos lspLine lspCol) argSpans of
    Just i -> i
    Nothing ->
      let before = length (filter (endsBeforePos lspLine lspCol) argSpans)
       in min before (length argSpans - 1)

endsBeforePos :: Int -> Int -> SourceSpan -> Bool
endsBeforePos line col sp =
  let endLine = unLine (posLine (spanEnd sp)) - 1
      endCol = unColumn (posColumn (spanEnd sp)) - 1
   in endLine < line || (endLine == line && endCol <= col)

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

-- | Build a ParameterInformation using byte-range offsets into the rendered signature.
makeParamInfo :: FuncName -> FunctionType -> (Int, Located Parameter) -> LSP.ParameterInformation
makeParamInfo fname ft (idx, _) =
  let (start, end_) = nthParamRange fname ft idx
   in LSP.ParameterInformation
        (LSP.InR (fromIntegral start, fromIntegral end_))
        Nothing

-- | Byte range of the idx-th parameter within the rendered signature string.
-- Signature format: "fn name(p0: T0, p1: T1, ...) -> R"
nthParamRange :: FuncName -> FunctionType -> Int -> (Int, Int)
nthParamRange (FuncName fname) ft idx =
  let prefix = T.length ("fn " <> fname <> "(")
      paramTexts = map renderParam (funcParams ft)
      go _ [] _ = (0, 0)
      go offset (p : ps) i
        | i == 0 = (offset, offset + T.length p)
        | otherwise = go (offset + T.length p + 2) ps (i - 1)
   in go prefix paramTexts idx
  where
    renderParam (Located _ p) =
      unVarName (paramName p) <> ": " <> showType (qualType (paramType p))
