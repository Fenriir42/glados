{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSPServer.Analyze (AnalyzeResult (..), analyzeText, emptyResult) where

import AST.Types.AST (Block (..), Decl (..), ErrorDecl (..), FunctionDecl (..), Program (..), Stmt (..))
import AST.Types.Common
  ( Column (..),
    ErrorName (..),
    FilePath' (..),
    FuncName (..),
    Line (..),
    Located (..),
    Offset (..),
    SourcePos (..),
    SourceSpan (..),
    VarName,
    locSpan,
    unLocated,
  )
import AST.Types.Type (FunctionType (..), Type)
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
import System.FilePath (takeBaseName, (</>))
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
import TypeChecker
  ( TypeCheckResult (..),
    tcBuiltinCallSites,
    tcCallSites,
    tcCallWithArgs,
    tcErrors,
    tcFuncDefSites,
    tcFuncEnv,
    tcTypes,
    tcVarUseSites,
    typeCheck,
  )
import TypeChecker.Error (TypeCheckError, tcErrMessage, tcErrSpan)

data AnalyzeResult = AnalyzeResult
  { arDiagnostics :: [Diagnostic],
    arTypes :: Map SourceSpan Type,
    arCallSites :: Map SourceSpan (FuncName, FunctionType),
    arBuiltinCallSites :: Map SourceSpan FuncName,
    arDocs :: Map FuncName Text,
    arFuncEnv :: Map FuncName FunctionType,
    arFuncDefSites :: Map FuncName SourceSpan,
    arStdlibDefSites :: Map FuncName (FilePath, SourceSpan),
    arCallWithArgs :: Map SourceSpan (FuncName, FunctionType, [SourceSpan]),
    arFuncSymbols :: [(FuncName, FunctionType, SourceSpan, SourceSpan)],
    arFoldingRanges :: [SourceSpan],
    arVarUseSites :: Map SourceSpan (VarName, SourceSpan),
    arErrorNames :: [ErrorName]
  }

emptyResult :: [Diagnostic] -> AnalyzeResult
emptyResult diags =
  AnalyzeResult diags Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty [] [] Map.empty []

-- | Lex, resolve imports, type-check a source file.
analyzeText :: FilePath -> Text -> IO AnalyzeResult
analyzeText fp text = do
  cwd <- getCurrentDirectory
  let stdlibDir = cwd </> "std"
  case runParser parseRawTokens fp text of
    Left bundle ->
      return (emptyResult (bundleToDiags bundle))
    Right tokens ->
      case runParser (many parseDecl) fp tokens of
        Left bundle ->
          return (emptyResult (bundleToDiags bundle))
        Right rawDecls -> do
          resolvedOrErr <- resolveImports stdlibDir rawDecls
          let decls = case resolvedOrErr of
                Left _ -> rawDecls
                Right ds -> ds
              result = typeCheck (Program decls)
              diags = map tcErrToDiag (tcErrors result)
              userDocs = extractDocs text
              funcSymbols =
                [ let nameSpan = locSpan (funcDeclName fd)
                      bodySpan = blockSpan (funcDeclBody fd)
                      fullSpan = SourceSpan (spanStart nameSpan) (spanEnd bodySpan)
                   in ( unLocated (funcDeclName fd),
                        FunctionType (funcDeclParams fd) (funcDeclReturnType fd),
                        nameSpan,
                        fullSpan
                      )
                  | Located _ (DeclFunction _ fd) <- rawDecls
                ]
              foldingRanges = collectFoldingRanges rawDecls
              errorNames =
                [ unLocated (errorDeclName ed)
                  | Located _ (DeclError _ ed) <- rawDecls
                ]
          stdDocs <- extractStdlibDocs stdlibDir
          stdDefSites <- extractStdlibDefSites stdlibDir
          let allDocs = Map.union userDocs stdDocs
          return $
            AnalyzeResult
              diags
              (tcTypes result)
              (tcCallSites result)
              (tcBuiltinCallSites result)
              allDocs
              (tcFuncEnv result)
              (tcFuncDefSites result)
              stdDefSites
              (tcCallWithArgs result)
              funcSymbols
              foldingRanges
              (tcVarUseSites result)
              errorNames

-- ---------------------------------------------------------------------------
-- Folding range collection
-- Collects all Block spans for fold regions (function bodies, if/while/for blocks).

collectFoldingRanges :: [Located (Decl ())] -> [SourceSpan]
collectFoldingRanges = concatMap collectDecl
  where
    collectDecl (Located _ (DeclFunction _ fd)) = collectBlock (funcDeclBody fd)
    collectDecl _ = []

    collectBlock (Block sp stmts) =
      sp : concatMap (collectStmt . unLocated) stmts

    collectStmt (StmtIf _ thenBlock mElse) =
      collectBlock thenBlock ++ maybe [] collectBlock mElse
    collectStmt (StmtWhile _ body) = collectBlock body
    collectStmt (StmtFor _ _ _ body) = collectBlock body
    collectStmt (StmtBlock block) = collectBlock block
    collectStmt _ = []

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

-- | Load function definition spans from every *.qa file in the stdlib directory.
-- Stores under the module-qualified name (e.g. "math.sqrt") for lookup by call sites.
extractStdlibDefSites :: FilePath -> IO (Map FuncName (FilePath, SourceSpan))
extractStdlibDefSites dir = do
  files <- listDirectory dir `catch` \(_ :: SomeException) -> return []
  let qaFiles = filter (".qa" `isSuffixOf`) files
  maps <- mapM readDefSites qaFiles
  return (Map.unions maps)
  where
    readDefSites f = do
      let fp = dir </> f
          modName = takeBaseName f
      textOrErr <-
        ( do
            h <- openFile fp ReadMode
            hSetEncoding h utf8
            contents <- hGetContents h
            let !t = T.pack contents
            return (Right t)
          )
          `catch` \(_ :: SomeException) -> return (Left ())
      case textOrErr of
        Left _ -> return Map.empty
        Right text ->
          case runParser parseRawTokens fp text of
            Left _ -> return (lineScannedDefSites fp modName text)
            Right tokens ->
              case runParser (many parseDecl) fp tokens of
                Left _ -> return (lineScannedDefSites fp modName text)
                Right rawDecls ->
                  let funcEntries =
                        [ let bareName = unLocated (funcDeclName fd)
                              qualName = FuncName (T.pack modName <> "." <> unFuncName bareName)
                              sp = locSpan (funcDeclName fd)
                           in [(qualName, (fp, sp)), (bareName, (fp, sp))]
                          | Located _ (DeclFunction _ fd) <- rawDecls
                        ]
                   in return (Map.fromList (concat funcEntries))

-- | Fallback for stdlib files that fail to parse: scan lines for `fn name(`.
lineScannedDefSites :: FilePath -> String -> Text -> Map FuncName (FilePath, SourceSpan)
lineScannedDefSites fp modName text =
  Map.fromList $ go 1 (T.lines text)
  where
    go _ [] = []
    go lineNum (l : rest) =
      let stripped = T.strip l
       in case parseFnNameAt stripped lineNum of
            Just (name, sp) ->
              let qualName = FuncName (T.pack modName <> "." <> name)
                  bareName = FuncName name
               in (qualName, (fp, sp)) : (bareName, (fp, sp)) : go (lineNum + 1) rest
            Nothing -> go (lineNum + 1) rest

    parseFnNameAt t lineNum
      | T.isPrefixOf "fn " t =
          let afterFn = T.drop 3 t
              name = T.takeWhile (\c -> c == '_' || c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) afterFn
              col = 4
              endCol = col + T.length name
              fp' = FilePath' (T.pack fp)
              startPos = SourcePos fp' (Line lineNum) (Column col) (Offset 0)
              endPos = SourcePos fp' (Line lineNum) (Column endCol) (Offset 0)
           in if T.null name
                then Nothing
                else Just (name, SourceSpan startPos endPos)
      | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Parse error -> LSP diagnostic conversion

-- | Strip ANSI SGR escape sequences from a string.
-- showErrorComponent in Error.hs embeds ANSI unconditionally; LSP diagnostics
-- must always be plain text so VS Code can render them cleanly.
stripAnsi :: Text -> Text
stripAnsi = T.pack . go . T.unpack
  where
    go [] = []
    go ('\ESC' : '[' : rest) = go (drop 1 (dropWhile (/= 'm') rest))
    go (c : cs) = c : go cs

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
          msg = stripAnsi (T.pack (parseErrorTextPretty err))
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
