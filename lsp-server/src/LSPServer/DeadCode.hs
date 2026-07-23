module LSPServer.DeadCode (deadCodeDiags) where

import AST.Types.Common (FuncName (..), SourceSpan, unFuncName)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import LSPServer.Span (spanToRange)
import Language.LSP.Protocol.Types
  ( Diagnostic (..),
    DiagnosticSeverity (..),
    DiagnosticTag (..),
  )

-- | Hint diagnostics for functions defined in a file that have no callers
-- anywhere in the indexed workspace.  Uses DiagnosticTag_Unnecessary so
-- editors render them greyed-out, matching Rust Analyzer's dead_code lint.
deadCodeDiags ::
  -- | Functions defined in this file (name -> name-span)
  Map FuncName SourceSpan ->
  -- | All function names that appear as a call target anywhere in the workspace
  Set FuncName ->
  [Diagnostic]
deadCodeDiags defSites calledNames =
  [ mkDeadDiag sp (unFuncName fname)
    | (fname, sp) <- Map.toList defSites,
      fname /= FuncName "main",
      fname `Set.notMember` calledNames
  ]

mkDeadDiag :: SourceSpan -> Text -> Diagnostic
mkDeadDiag sp name =
  Diagnostic
    { _range = spanToRange sp,
      _severity = Just DiagnosticSeverity_Hint,
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "quant",
      _message = "Function `" <> name <> "` is never used",
      _tags = Just [DiagnosticTag_Unnecessary],
      _relatedInformation = Nothing,
      _data_ = Nothing
    }
