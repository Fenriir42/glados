module LSPServer.Analyze (analyzeText) where

import AST.Types.AST (Program (..))
import AST.Types.Common
  ( Column (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
  )
import AST.Types.Type (Type)
import Control.Applicative (many)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
  ( Diagnostic (..),
    DiagnosticSeverity (..),
    Position (..),
    Range (..),
    UInt,
  )
import Lexer (parseRawTokens)
import Parser.Decl (parseDecl)
import Text.Megaparsec
  ( ParseErrorBundle (..),
    ShowErrorComponent,
    TraversableStream,
    VisualStream,
    errorOffset,
    parseErrorTextPretty,
    pstateSourcePos,
    reachOffset,
    runParser,
  )
import qualified Text.Megaparsec as MP
import TypeChecker (TypeCheckResult (..), typeCheck)
import TypeChecker.Error (TypeCheckError, tcErrMessage, tcErrSpan)

analyzeText :: FilePath -> Text -> ([Diagnostic], Map SourceSpan Type)
analyzeText fp text =
  case runParser parseRawTokens fp text of
    Left bundle ->
      (bundleToDiags bundle, Map.empty)
    Right tokens ->
      case runParser (many parseDecl) fp tokens of
        Left bundle ->
          (bundleToDiags bundle, Map.empty)
        Right decls ->
          let result = typeCheck (Program decls)
              diags = map tcErrToDiag (tcErrors result)
           in (diags, tcTypes result)

bundleToDiags ::
  (TraversableStream s, VisualStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e ->
  [Diagnostic]
bundleToDiags (ParseErrorBundle errs initPS) =
  let (_, diags) = foldl go (initPS, []) (NE.toList errs)
   in reverse diags
  where
    go (ps, acc) err =
      let off = errorOffset err
          (_, ps') = reachOffset off ps
          sp = pstateSourcePos ps'
          msg = T.pack (parseErrorTextPretty err)
       in (ps', mkPosDiag sp msg : acc)

mkPosDiag :: MP.SourcePos -> Text -> Diagnostic
mkPosDiag sp msg =
  let lspLine = (fromIntegral (MP.unPos (MP.sourceLine sp)) - 1) :: UInt
      lspChar = (fromIntegral (MP.unPos (MP.sourceColumn sp)) - 1) :: UInt
      pos = Position lspLine lspChar
   in mkDiag (Range pos pos) msg

tcErrToDiag :: TypeCheckError -> Diagnostic
tcErrToDiag err = mkDiag (spanToRange (tcErrSpan err)) (T.pack (tcErrMessage err))

spanToRange :: SourceSpan -> Range
spanToRange ss =
  Range
    ( Position
        ((fromIntegral (unLine (posLine (spanStart ss))) - 1) :: UInt)
        ((fromIntegral (unColumn (posColumn (spanStart ss))) - 1) :: UInt)
    )
    ( Position
        ((fromIntegral (unLine (posLine (spanEnd ss))) - 1) :: UInt)
        ((fromIntegral (unColumn (posColumn (spanEnd ss))) - 1) :: UInt)
    )

mkDiag :: Range -> Text -> Diagnostic
mkDiag range msg =
  Diagnostic
    { _range = range,
      _severity = Just DiagnosticSeverity_Error,
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "quant",
      _message = msg,
      _tags = Nothing,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }
