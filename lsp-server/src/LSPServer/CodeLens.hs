module LSPServer.CodeLens (makeCodeLens) where

import AST.Types.Common
  ( Column (..),
    FilePath' (..),
    FuncName (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
  )
import AST.Types.Type (FunctionType)
import Data.Aeson (toJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Span (spanToRange)
import qualified Language.LSP.Protocol.Types as LSP

makeCodeLens ::
  FilePath ->
  [(FuncName, FunctionType, SourceSpan, SourceSpan)] ->
  Map SourceSpan (FuncName, FunctionType) ->
  [LSP.CodeLens]
makeCodeLens fp funcSymbols callSites = concatMap toLenses funcSymbols
  where
    fileUri = LSP.filePathToUri fp

    toLenses (fname, _, nameSpan, _) =
      let refLens = toRefLens fname nameSpan
       in if fname == FuncName "main"
            then [toRunLens nameSpan, refLens]
            else [refLens]

    toRefLens fname nameSpan =
      let refSpans = [sp | (sp, (fn, _)) <- Map.toList callSites, fn == fname]
          count = length refSpans
          label = refLabel count
          defPos = spanStartPos nameSpan
          refLocs = map toRefLoc refSpans
          cmd =
            LSP.Command
              label
              "quant-lsp.showReferences"
              (Just [toJSON fileUri, toJSON defPos, toJSON refLocs])
       in LSP.CodeLens (spanToRange nameSpan) (Just cmd) Nothing

    toRunLens nameSpan =
      let cmd =
            LSP.Command
              "Run"
              "quant-lsp.runFile"
              (Just [toJSON (T.pack fp)])
       in LSP.CodeLens (spanToRange nameSpan) (Just cmd) Nothing

toRefLoc :: SourceSpan -> LSP.Location
toRefLoc sp =
  let fp = T.unpack (unFilePath (posFile (spanStart sp)))
   in LSP.Location (LSP.filePathToUri fp) (spanToRange sp)

refLabel :: Int -> Text
refLabel 0 = "0 references"
refLabel 1 = "1 reference"
refLabel n = T.pack (show n) <> " references"

spanStartPos :: SourceSpan -> LSP.Position
spanStartPos sp =
  LSP.Position
    ((fromIntegral (unLine (posLine (spanStart sp))) - 1) :: LSP.UInt)
    ((fromIntegral (unColumn (posColumn (spanStart sp))) - 1) :: LSP.UInt)
