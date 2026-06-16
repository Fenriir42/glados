module LSPServer.SemanticTokens (buildSemanticTokens) where

import AST.Types.Common
  ( Column (..),
    FilePath' (..),
    FuncName,
    Line (..),
    SourcePos (..),
    SourceSpan (..),
    VarName,
  )
import AST.Types.Type (FunctionType)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP

-- | Build a SemanticTokens response from the analysed file state.
-- Token types emitted:
--   function  - all call sites (user + builtin) and definition name spans
--   variable  - all variable use and declaration spans
-- Only spans whose posFile matches currentFile are emitted.
buildSemanticTokens ::
  Map SourceSpan (FuncName, FunctionType) ->
  Map SourceSpan FuncName ->
  Map FuncName SourceSpan ->
  Map SourceSpan (VarName, SourceSpan) ->
  FilePath ->
  LSP.SemanticTokens
buildSemanticTokens callSites builtinSites funcDefSites varUseSites currentFile =
  case LSP.makeSemanticTokens LSP.defaultSemanticTokensLegend absTokens of
    Left _ -> LSP.SemanticTokens Nothing []
    Right st -> st
  where
    absTokens = map (uncurry toAbsolute) (Map.toAscList tokenMap)

    inFile sp = posFile (spanStart sp) == FilePath' (T.pack currentFile)

    tokenMap :: Map SourceSpan (LSP.SemanticTokenTypes, [LSP.SemanticTokenModifiers])
    tokenMap =
      Map.unions
        [ -- Function call sites (user-defined)
          Map.map (const (LSP.SemanticTokenTypes_Function, [])) callSites,
          -- Function call sites (VM builtins)
          Map.map (const (LSP.SemanticTokenTypes_Function, [])) builtinSites,
          -- Function definition name spans (filter to current file only)
          Map.fromList
            [ (sp, (LSP.SemanticTokenTypes_Function, [LSP.SemanticTokenModifiers_Declaration]))
              | (_, sp) <- Map.toList funcDefSites,
                inFile sp
            ],
          -- Variable use spans (call sites are already from this file)
          Map.mapWithKey
            ( \sp (_, defSp) ->
                let mods = [LSP.SemanticTokenModifiers_Declaration | sp == defSp]
                 in (LSP.SemanticTokenTypes_Variable, mods)
            )
            varUseSites,
          -- Variable declaration spans (may not appear as ExprVar keys)
          Map.fromList
            [ (defSp, (LSP.SemanticTokenTypes_Variable, [LSP.SemanticTokenModifiers_Declaration]))
              | (_, defSp) <- Map.elems varUseSites,
                inFile defSp
            ]
        ]

    toAbsolute :: SourceSpan -> (LSP.SemanticTokenTypes, [LSP.SemanticTokenModifiers]) -> LSP.SemanticTokenAbsolute
    toAbsolute sp (ty, mods) =
      let sl = fromIntegral (unLine (posLine (spanStart sp))) - 1
          sc = fromIntegral (unColumn (posColumn (spanStart sp))) - 1
          el = fromIntegral (unLine (posLine (spanEnd sp))) - 1
          ec = fromIntegral (unColumn (posColumn (spanEnd sp))) - 1
          len = if (sl :: LSP.UInt) == el then ec - sc else 0
       in LSP.SemanticTokenAbsolute sl sc len ty mods
