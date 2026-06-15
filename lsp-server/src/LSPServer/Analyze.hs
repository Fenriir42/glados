{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSPServer.Analyze (analyzeText) where

import AST.Types.AST (Program (..))
import AST.Types.Common
  ( Column (..),
    FuncName (..),
    Line (..),
    SourcePos (..),
    SourceSpan (..),
  )
import AST.Types.Type (FunctionType, Type)
import Compiler.Import (resolveImports)
import Control.Applicative (many)
import Control.Exception (SomeException, catch)
import Data.List (isSuffixOf)
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
import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, utf8)
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
import TypeChecker (TypeCheckResult (..), tcBuiltinCallSites, tcCallSites, tcErrors, tcTypes, typeCheck)
import TypeChecker.Error (TypeCheckError, tcErrMessage, tcErrSpan)

-- | Lex, resolve imports, type-check a source file.
-- Returns diagnostics, a span→type map (for hover), a span→(name,sig) map
-- (for function-signature hover), and a name→doc map (for doc-comment hover).
analyzeText ::
  FilePath ->
  Text ->
  IO ([Diagnostic], Map SourceSpan Type, Map SourceSpan (FuncName, FunctionType), Map SourceSpan FuncName, Map FuncName Text)
analyzeText fp text = do
  cwd <- getCurrentDirectory
  let stdlibDir = cwd </> "std"
  case runParser parseRawTokens fp text of
    Left bundle ->
      return (bundleToDiags bundle, Map.empty, Map.empty, Map.empty, Map.empty)
    Right tokens ->
      case runParser (many parseDecl) fp tokens of
        Left bundle ->
          return (bundleToDiags bundle, Map.empty, Map.empty, Map.empty, Map.empty)
        Right rawDecls -> do
          resolvedOrErr <- resolveImports stdlibDir rawDecls
          let decls = case resolvedOrErr of
                Left _ -> rawDecls
                Right ds -> ds
              result = typeCheck (Program decls)
              diags = map tcErrToDiag (tcErrors result)
              userDocs = extractDocs text
          stdDocs <- extractStdlibDocs stdlibDir
          let allDocs = Map.union userDocs stdDocs
          return (diags, tcTypes result, tcCallSites result, tcBuiltinCallSites result, allDocs)

-- ---------------------------------------------------------------------------
-- Doc-comment extraction
-- Scans source lines for `// ...` comments immediately before `fn name(`.
-- A blank line or non-comment resets the accumulator.

extractDocs :: Text -> Map FuncName Text
extractDocs src =
  Map.fromList $ go [] (T.lines src)
  where
    go _ [] = []
    go acc (l : rest) =
      let stripped = T.strip l
       in if T.isPrefixOf "//" stripped
            then
              let doc = T.strip (T.drop 2 stripped)
               in go (acc ++ [doc]) rest
            else case parseFnName stripped of
              Just name -> (FuncName name, T.strip (T.unlines acc)) : go [] rest
              Nothing -> go [] rest

    parseFnName t
      | T.isPrefixOf "fn " t =
          let afterFn = T.drop 3 t
              name = T.takeWhile (\c -> c == '_' || c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) afterFn
           in if T.null name then Nothing else Just name
      | otherwise = Nothing

-- | Load doc comments from every *.qa file in the stdlib directory.
-- Uses explicit UTF-8 encoding to avoid locale-dependent failures.
extractStdlibDocs :: FilePath -> IO (Map FuncName Text)
extractStdlibDocs dir = do
  files <- listDirectory dir `catch` \(_ :: SomeException) -> return []
  let qaFiles = filter (".qa" `isSuffixOf`) files
  maps <- mapM readDocs qaFiles
  return (Map.unions maps)
  where
    readDocs f =
      ( do
          h <- openFile (dir </> f) ReadMode
          hSetEncoding h utf8
          contents <- hGetContents h
          let !t = T.pack contents
          return (extractDocs t)
      )
        `catch` \(_ :: SomeException) -> return Map.empty

-- ---------------------------------------------------------------------------
-- Parse error → LSP diagnostic conversion

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
