{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSPServer.Analyze (AnalyzeResult (..), analyzeText, emptyResult) where

import AST.Types.AST (Block (..), Decl (..), ErrorDecl (..), FunctionDecl (..), ImplDecl (..), ImplForDecl (..), ImportDecl (..), ImportTarget (..), ModulePath (..), Program (..), Stmt (..), StructDecl (..))
import AST.Types.Common
  ( Column (..),
    ErrorName (..),
    FilePath' (..),
    FuncName (..),
    Line (..),
    Located (..),
    ModuleName (..),
    Offset (..),
    SourcePos (..),
    SourceSpan (..),
    TypeName (..),
    VarName (..),
    locSpan,
    unFuncName,
    unLocated,
    unModuleName,
    unVarName,
  )
import AST.Types.Type (FunctionType (..), StructField, Type)
import Compiler.Import (resolveImports)
import Control.Applicative (many)
import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum)
import Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types
  ( Diagnostic (..),
    DiagnosticSeverity (..),
    DiagnosticTag (..),
    Position (..),
    Range (..),
    UInt,
  )
import Lexer (parseRawTokens)
import Parser.Decl (parseDecl)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>))
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
    tcVarDeclSites,
    tcVarUseSites,
    typeCheck,
  )
import TypeChecker.Error (TypeCheckError (..), tcErrMessage, tcErrSpan)

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
    arErrorNames :: [ErrorName],
    arVarDeclSites :: Map SourceSpan (VarName, SourceSpan),
    arImportDecls :: [(SourceSpan, ImportDecl)],
    arCallsByFunc :: Map FuncName [(FuncName, SourceSpan)],
    arVarDeclTypes :: Map SourceSpan Type,
    arStructDefs :: Map TypeName [StructField],
    arStructDefSites :: Map TypeName SourceSpan
  }

emptyResult :: [Diagnostic] -> AnalyzeResult
emptyResult diags =
  AnalyzeResult diags Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty [] [] Map.empty [] Map.empty [] Map.empty Map.empty Map.empty Map.empty

-- | Resolve the Quant standard library directory using the same priority order
-- as the CLI: QUANT_STDLIB env var, system install, then ./std fallback.
resolveStdlib :: FilePath -> IO FilePath
resolveStdlib cwd = do
  env <- lookupEnv "QUANT_STDLIB"
  case env of
    Just d -> return d
    Nothing -> do
      let sys = "/usr/local/share/quant/lib"
      ok <- doesDirectoryExist sys
      if ok then return sys else return (cwd </> "std")

-- | Walk up the directory tree from @start@ looking for @quant.toml@.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot dir = do
  exists <- doesFileExist (dir </> "quant.toml")
  if exists
    then return (Just dir)
    else do
      let parent = takeDirectory dir
      if parent == dir then return Nothing else findProjectRoot parent

-- | Collect import search paths for a source file.
-- Always includes the file's own directory and the stdlib.
-- If a quant.toml is found above the file, also adds <root>/src and <root>
-- so that user modules are visible from test files in a sibling directory.
importPaths :: FilePath -> FilePath -> IO [FilePath]
importPaths fp stdlibDir = do
  let fileDir = takeDirectory fp
  mRoot <- findProjectRoot fileDir
  let extra = case mRoot of
        Nothing -> []
        Just root -> [root </> "src", root]
      candidates = fileDir : extra ++ [stdlibDir]
      unique = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] candidates
  return unique

-- | Lex, resolve imports, type-check a source file.
analyzeText :: FilePath -> Text -> IO AnalyzeResult
analyzeText fp text = do
  cwd <- getCurrentDirectory
  stdlibDir <- resolveStdlib cwd
  impPaths <- importPaths fp stdlibDir
  case runParser parseRawTokens fp text of
    Left bundle ->
      return (emptyResult (bundleToDiags bundle))
    Right tokens ->
      case runParser (many parseDecl) fp tokens of
        Left bundle ->
          return (emptyResult (bundleToDiags bundle))
        Right rawDecls -> do
          let importDecls =
                [(sp, d) | Located sp (DeclImport d) <- rawDecls]
          resolvedOrErr <- resolveImports impPaths rawDecls
          stdDocs <- extractStdlibDocs stdlibDir
          stdDefSites <- extractStdlibDefSites stdlibDir
          let decls = case resolvedOrErr of
                Left _ -> rawDecls
                Right ds -> ds
              result = typeCheck (Program decls)
              diags = map (tcErrToDiagWith stdDefSites) (tcErrors result)
              usedFuncNames =
                Set.fromList $
                  map fst (Map.elems (tcCallSites result))
                    ++ Map.elems (tcBuiltinCallSites result)
              unusedVarDiags =
                computeUnusedVarDiags (tcVarDeclSites result) (tcVarUseSites result) (tcCallSites result)
              unusedImportDiags =
                computeUnusedImportDiags importDecls usedFuncNames
              allDiags = diags ++ unusedVarDiags ++ unusedImportDiags
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
                  ++ [ let nameSpan = locSpan (funcDeclName fd)
                           bodySpan = blockSpan (funcDeclBody fd)
                           fullSpan = SourceSpan (spanStart nameSpan) (spanEnd bodySpan)
                        in ( unLocated (funcDeclName fd),
                             FunctionType (funcDeclParams fd) (funcDeclReturnType fd),
                             nameSpan,
                             fullSpan
                           )
                       | Located _ (DeclImpl _ idecl) <- rawDecls,
                         Located _ fd <- implMethods idecl
                     ]
                  ++ [ let nameSpan = locSpan (funcDeclName fd)
                           bodySpan = blockSpan (funcDeclBody fd)
                           fullSpan = SourceSpan (spanStart nameSpan) (spanEnd bodySpan)
                        in ( unLocated (funcDeclName fd),
                             FunctionType (funcDeclParams fd) (funcDeclReturnType fd),
                             nameSpan,
                             fullSpan
                           )
                       | Located _ (DeclImplFor _ ifdecl) <- rawDecls,
                         Located _ fd <- implForMethods ifdecl
                     ]
              foldingRanges = collectFoldingRanges rawDecls
              callsByFunc =
                Map.fromList
                  [ ( fname,
                      [ (callee, sp)
                        | (sp, (callee, _)) <- Map.toList (tcCallSites result),
                          spanContains fullSpan sp
                      ]
                    )
                    | (fname, _, _, fullSpan) <- funcSymbols
                  ]
              errorNames =
                [ unLocated (errorDeclName ed)
                  | Located _ (DeclError _ ed) <- rawDecls
                ]
          let allDocs = Map.union userDocs stdDocs
              -- Types for match-arm bindings (ok/err/some): nameSp == stmtSp because
              -- recordVarDecl is called with vsp for both args.  Regular StmtVarDecl
              -- uses different spans, so it's excluded here (type is already explicit).
              varDeclTypes =
                Map.fromList
                  [ (nameSp, t)
                    | (nameSp, (_, stmtSp)) <- Map.toList (tcVarDeclSites result),
                      nameSp == stmtSp,
                      Just t <- [Map.lookup nameSp (tcTypes result)]
                  ]
              structDefs =
                Map.fromList
                  [ (unLocated (structDeclName sd), map unLocated (structDeclFields sd))
                    | Located _ (DeclStruct _ sd) <- rawDecls
                  ]
              structDefSites =
                Map.fromList
                  [ (unLocated (structDeclName sd), locSpan (structDeclName sd))
                    | Located _ (DeclStruct _ sd) <- rawDecls
                  ]
          return $
            AnalyzeResult
              allDiags
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
              (tcVarDeclSites result)
              importDecls
              callsByFunc
              varDeclTypes
              structDefs
              structDefSites

-- ---------------------------------------------------------------------------
-- Folding range collection
-- Collects all Block spans for fold regions (function bodies, if/while/for blocks).

collectFoldingRanges :: [Located (Decl ())] -> [SourceSpan]
collectFoldingRanges = concatMap collectDecl
  where
    collectDecl (Located _ (DeclFunction _ fd)) = collectBlock (funcDeclBody fd)
    collectDecl (Located _ (DeclImpl _ idecl)) =
      concatMap (collectBlock . funcDeclBody . unLocated) (implMethods idecl)
    collectDecl (Located _ (DeclImplFor _ ifdecl)) =
      concatMap (collectBlock . funcDeclBody . unLocated) (implForMethods ifdecl)
    collectDecl _ = []

    collectBlock (Block sp stmts) =
      sp : concatMap (collectStmt . unLocated) stmts

    collectStmt (StmtIf _ thenBlock mElse) =
      collectBlock thenBlock ++ maybe [] collectBlock mElse
    collectStmt (StmtWhile _ body) = collectBlock body
    collectStmt (StmtFor _ _ _ body) = collectBlock body
    collectStmt (StmtBlock block) = collectBlock block
    collectStmt (StmtMatch _ _) = []
    collectStmt _ = []

-- | True when `inner` is fully enclosed by `outer` (line/col comparison).
spanContains :: SourceSpan -> SourceSpan -> Bool
spanContains outer inner =
  let cmp sp = (unLine (posLine sp), unColumn (posColumn sp))
   in cmp (spanStart outer) <= cmp (spanStart inner)
        && cmp (spanEnd inner) <= cmp (spanEnd outer)

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
      | T.isPrefixOf "fn " t = extractName (T.drop 3 t)
      | T.isPrefixOf "static fn " t = extractName (T.drop 10 t)
      | otherwise = Nothing
      where
        extractName after =
          let name = T.takeWhile (\c -> c == '_' || isAlphaNum c) after
           in if T.null name then Nothing else Just name

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

    parseFnNameAt t lineNum =
      let (afterFn, col)
            | T.isPrefixOf "fn " t = (T.drop 3 t, 4)
            | T.isPrefixOf "static fn " t = (T.drop 10 t, 11)
            | otherwise = ("", 0)
          name = T.takeWhile (\c -> c == '_' || isAlphaNum c) afterFn
          endCol = col + T.length name
          fp' = FilePath' (T.pack fp)
          startPos = SourcePos fp' (Line lineNum) (Column col) (Offset 0)
          endPos = SourcePos fp' (Line lineNum) (Column endCol) (Offset 0)
       in if T.null name
            then Nothing
            else Just (name, SourceSpan startPos endPos)

-- ---------------------------------------------------------------------------
-- Unused-symbol detection

-- | Produce Hint diagnostics with DiagnosticTag_Unnecessary for local
-- variables that are declared but never read.
computeUnusedVarDiags ::
  Map SourceSpan (VarName, SourceSpan) ->
  Map SourceSpan (VarName, SourceSpan) ->
  Map SourceSpan (FuncName, FunctionType) ->
  [Diagnostic]
computeUnusedVarDiags declSites useSites callSites =
  let readDecls =
        Set.fromList
          [ declSp
            | (useSp, (_, declSp)) <- Map.toList useSites,
              useSp /= declSp
          ]
      calledNames =
        Set.fromList [fn | (_, (fn, _)) <- Map.toList callSites]
   in [ makeHintDiag (spanToRange nameSp) ("`" <> unVarName vname <> "` is declared but never used")
        | (nameSp, (vname, _)) <- Map.toList declSites,
          not (nameSp `Set.member` readDecls),
          FuncName (unVarName vname) `Set.notMember` calledNames
      ]

-- | Produce Hint diagnostics for imported names that are never called.
-- Wildcard imports (@from M import *@) are silently skipped.
computeUnusedImportDiags ::
  [(SourceSpan, ImportDecl)] ->
  Set.Set FuncName ->
  [Diagnostic]
computeUnusedImportDiags importDecls usedNames = concatMap check importDecls
  where
    check (sp, ImportDecl modPath ImportAll) =
      let prefix = modStr modPath <> "."
          anyUsed = any (\(FuncName n) -> prefix `T.isPrefixOf` n) (Set.toList usedNames)
       in [ makeHintDiag (spanToRange sp) ("Module `" <> modStr modPath <> "` is imported but never used")
            | not anyUsed
          ]
    check (sp, ImportDecl _ (ImportNames names)) =
      let unused = [Located nameSp vn | Located nameSp vn <- names, FuncName (unVarName vn) `Set.notMember` usedNames]
       in if null unused
            then []
            else
              if length unused == length names
                then -- All names unused: dim entire import line
                  [makeHintDiag (spanToRange sp) (unusedMsg unused)]
                else -- Some names unused: dim each unused name individually
                  [ makeHintDiag (spanToRange nameSp) ("`" <> unVarName vn <> "` is imported but never used")
                    | Located nameSp vn <- unused
                  ]
    check _ = []

    modStr mp = T.intercalate "." (map (unModuleName . unLocated) (modulePathParts mp))

    unusedMsg locs =
      let ns = T.intercalate ", " ["`" <> unVarName vn <> "`" | Located _ vn <- locs]
       in ns <> (if length locs == 1 then " is" else " are") <> " imported but never used"

makeHintDiag :: Range -> Text -> Diagnostic
makeHintDiag range msg =
  Diagnostic
    { _range = range,
      _severity = Just DiagnosticSeverity_Hint,
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "quant",
      _message = msg,
      _tags = Just [DiagnosticTag_Unnecessary],
      _relatedInformation = Nothing,
      _data_ = Nothing
    }

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

tcErrToDiagWith :: Map FuncName (FilePath, SourceSpan) -> TypeCheckError -> Diagnostic
tcErrToDiagWith stdlibDefs err@(TCUndefinedFunc sp fname) =
  case Map.lookup fname stdlibDefs of
    Just (libFp, _) ->
      let modName = T.pack (dropExtension (takeBaseName libFp))
       in makeWarnDiag
            (spanToRange sp)
            ( "function `"
                <> unFuncName fname
                <> "` is not explicitly imported from `"
                <> modName
                <> "`"
            )
    Nothing -> tcErrToDiag err
tcErrToDiagWith _ err = tcErrToDiag err

makeWarnDiag :: Range -> Text -> Diagnostic
makeWarnDiag range msg =
  Diagnostic
    { _range = range,
      _severity = Just DiagnosticSeverity_Warning,
      _code = Nothing,
      _codeDescription = Nothing,
      _source = Just "quant",
      _message = msg,
      _tags = Nothing,
      _relatedInformation = Nothing,
      _data_ = Nothing
    }

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
