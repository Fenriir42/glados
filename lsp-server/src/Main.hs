{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import AST.Types.AST (ImportDecl, Program (..))
import AST.Types.Common (ErrorName, FuncName, SourceSpan, TypeName, VarName)
import AST.Types.Type (FunctionType, StructField, Type)
import qualified Config
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
    writeTVar,
  )
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Formatter (formatProgram)
import LSPServer.Analyze (AnalyzeResult (..), analyzeText)
import LSPServer.CallHierarchy (incomingCalls, outgoingCalls, prepareCallHierarchy)
import LSPServer.CodeAction (makeCodeActions)
import LSPServer.CodeLens (makeCodeLens)
import LSPServer.Completion (makeCompletionItems)
import LSPServer.DeadCode (deadCodeDiags)
import LSPServer.Definition (findDefinition)
import LSPServer.DocumentSymbol (makeDocumentSymbols)
import LSPServer.Folding (makeFoldingRanges)
import LSPServer.Highlight (findHighlights)
import qualified LSPServer.Hover as Hover
import LSPServer.InlayHints (makeInlayHints)
import LSPServer.OnTypeFormat (onTypeFormat)
import LSPServer.References (findReferences)
import LSPServer.Rename (findRename, prepareRename)
import LSPServer.SelectionRange (findSelectionRange)
import LSPServer.SemanticTokens (buildSemanticTokens)
import LSPServer.SignatureHelp (findSignatureHelp)
import LSPServer.TypeDefinition (findTypeDefinition)
import LSPServer.WorkspaceSymbol (findWorkspaceSymbols)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import Lib (lexString)
import Parser.Decl (parseDecl)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Text.Megaparsec as MP

data FileState = FileState
  { fsTypes :: Map SourceSpan Type,
    fsCallSites :: Map SourceSpan (FuncName, FunctionType),
    fsBuiltinCallSites :: Map SourceSpan FuncName,
    fsDocs :: Map FuncName Text,
    fsFuncEnv :: Map FuncName FunctionType,
    fsFuncDefSites :: Map FuncName SourceSpan,
    fsStdlibDefSites :: Map FuncName (FilePath, SourceSpan),
    fsCallWithArgs :: Map SourceSpan (FuncName, FunctionType, [SourceSpan]),
    fsFuncSymbols :: [(FuncName, FunctionType, SourceSpan, SourceSpan)],
    fsFoldingRanges :: [SourceSpan],
    fsVarUseSites :: Map SourceSpan (VarName, SourceSpan),
    fsErrorNames :: [ErrorName],
    fsVarDeclSites :: Map SourceSpan (VarName, SourceSpan),
    fsImportDecls :: [(SourceSpan, ImportDecl)],
    fsCallsByFunc :: Map FuncName [(FuncName, SourceSpan)],
    fsVarDeclTypes :: Map SourceSpan Type,
    fsStructDefs :: Map TypeName [StructField],
    fsStructDefSites :: Map TypeName SourceSpan,
    fsFilePath :: FilePath,
    fsFileText :: Text
  }

type State = Map LSP.NormalizedUri FileState

main :: IO ()
main = do
  stateVar <- newTVarIO Map.empty
  rootVar <- newTVarIO (Nothing :: Maybe FilePath)
  code <-
    runServer $
      ServerDefinition
        { parseConfig = const $ const $ Right (),
          onConfigChange = const $ pure (),
          defaultConfig = (),
          configSection = "quant-lsp",
          doInitialize = \env req -> do
            let TRequestMessage _ _ _ params = req
                mRoot = extractWorkspaceRoot params
            atomically $ writeTVar rootVar mRoot
            pure (Right env),
          staticHandlers = \_caps -> mkHandlers stateVar rootVar,
          interpretHandler = \env -> Iso (runLspT env) liftIO,
          options = serverOptions
        }
  exitWith (if code == 0 then ExitSuccess else ExitFailure code)

-- | Extract the workspace root path from InitializeParams.
extractWorkspaceRoot :: LSP.InitializeParams -> Maybe FilePath
extractWorkspaceRoot params =
  case params._rootUri of
    LSP.InL uri -> LSP.uriToFilePath uri
    _ -> Nothing

serverOptions :: Options
serverOptions =
  Language.LSP.Server.defaultOptions
    { optTextDocumentSync =
        Just
          LSP.TextDocumentSyncOptions
            { LSP._openClose = Just True,
              LSP._change = Just LSP.TextDocumentSyncKind_Full,
              LSP._willSave = Nothing,
              LSP._willSaveWaitUntil = Nothing,
              LSP._save = Just (LSP.InR (LSP.SaveOptions {LSP._includeText = Just False}))
            },
      optCompletionTriggerCharacters = Just ['.'],
      optSignatureHelpTriggerCharacters = Just ['(', ','],
      optCodeActionKinds = Just [LSP.CodeActionKind_QuickFix],
      optDocumentOnTypeFormattingTriggerCharacters = Just ('\n' :| ['}'])
    }

mkHandlers :: TVar State -> TVar (Maybe FilePath) -> Handlers (LspM ())
mkHandlers stateVar rootVar =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_msg -> do
        env <- getLspEnv
        mRoot <- liftIO $ readTVarIO rootVar
        case mRoot of
          Nothing -> return ()
          Just root ->
            liftIO $ void $ forkIO $ do
              runLspT env $ sendIndexingStatus True
              indexWorkspace stateVar root
              runLspT env $ sendIndexingStatus False,
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_msg -> pure (),
      notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \msg -> do
        let TNotificationMessage _ _ (LSP.DidChangeWatchedFilesParams changes) = msg
        liftIO $ forM_ changes $ \(LSP.FileEvent changeUri changeType) -> do
          let nuri = LSP.toNormalizedUri changeUri
              fp = fromMaybe "" (LSP.uriToFilePath changeUri)
          case changeType of
            LSP.FileChangeType_Deleted ->
              atomically $ modifyTVar' stateVar (Map.delete nuri)
            _ ->
              analyzeFromDisk stateVar fp,
      notificationHandler SMethod_TextDocumentDidClose $ \_msg -> pure (),
      notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let TNotificationMessage _ _ (LSP.DidOpenTextDocumentParams td) = msg
            LSP.TextDocumentItem uri _ ver _ = td
            nuri = LSP.toNormalizedUri uri
        analyzeAndPublish stateVar nuri (Just ver),
      notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let TNotificationMessage _ _ (LSP.DidChangeTextDocumentParams vtd _) = msg
            LSP.VersionedTextDocumentIdentifier uri ver = vtd
            nuri = LSP.toNormalizedUri uri
        analyzeAndPublish stateVar nuri (Just ver),
      notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let TNotificationMessage _ _ (LSP.DidSaveTextDocumentParams tdId _) = msg
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        analyzeAndPublish stateVar nuri Nothing,
      -- Hover
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.HoverParams tdId pos _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                case Hover.findHoverAtPos
                  (fsTypes fs)
                  (fsCallSites fs)
                  (fsBuiltinCallSites fs)
                  (fsDocs fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar) of
                  Nothing -> LSP.InR LSP.Null
                  Just h -> LSP.InL h
        responder (Right result),
      -- Completion
      requestHandler SMethod_TextDocumentCompletion $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.CompletionParams tdId pos _ _ _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let items = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                makeCompletionItems
                  (fsFuncEnv fs)
                  (fsDocs fs)
                  (fsErrorNames fs)
                  (fsVarDeclSites fs)
                  (fsFileText fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar)
        responder (Right (LSP.InL items)),
      -- Go to definition
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.DefinitionParams tdId pos _ _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR (LSP.InR LSP.Null)
              Just fs ->
                case findDefinition
                  (fsCallSites fs)
                  (fsBuiltinCallSites fs)
                  (fsFuncDefSites fs)
                  (fsStdlibDefSites fs)
                  (fsVarUseSites fs)
                  (fsFilePath fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar) of
                  Nothing -> LSP.InR (LSP.InR LSP.Null)
                  Just loc -> LSP.InL (LSP.Definition (LSP.InL loc))
        responder (Right result),
      -- Go to type definition (Ctrl+click on value -> jump to its struct declaration)
      requestHandler SMethod_TextDocumentTypeDefinition $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.TypeDefinitionParams tdId pos _ _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR (LSP.InR LSP.Null)
              Just fs ->
                case findTypeDefinition
                  (fsTypes fs)
                  (fsStructDefSites fs)
                  (fsFilePath fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar) of
                  Nothing -> LSP.InR (LSP.InR LSP.Null)
                  Just loc -> LSP.InL (LSP.Definition (LSP.InL loc))
        responder (Right result),
      -- Signature help
      requestHandler SMethod_TextDocumentSignatureHelp $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.SignatureHelpParams tdId pos _ _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                case findSignatureHelp
                  (fsCallWithArgs fs)
                  (fsDocs fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar) of
                  Nothing -> LSP.InR LSP.Null
                  Just sh -> LSP.InL sh
        responder (Right result),
      -- Document symbols (OUTLINE panel)
      requestHandler SMethod_TextDocumentDocumentSymbol $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.DocumentSymbolParams _ _ tdId) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let syms = case Map.lookup nuri st of
              Nothing -> []
              Just fs -> makeDocumentSymbols (fsFuncSymbols fs)
        responder (Right (LSP.InR (LSP.InL syms))),
      -- Workspace symbol search (Cmd+T / Quick Open)
      requestHandler SMethod_WorkspaceSymbol $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.WorkspaceSymbolParams _ _ query) = req
        st <- liftIO $ readTVarIO stateVar
        let syms =
              concatMap
                ( \fs ->
                    findWorkspaceSymbols
                      (fsFuncSymbols fs)
                      (fsStructDefSites fs)
                      (fsFilePath fs)
                      query
                )
                (Map.elems st)
        responder (Right (LSP.InR (LSP.InL syms))),
      -- Document highlight (occurrences glow)
      requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.DocumentHighlightParams tdId pos _ _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let highlights = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                findHighlights
                  (fsCallSites fs)
                  (fsBuiltinCallSites fs)
                  (fsFuncDefSites fs)
                  (fsVarUseSites fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar)
        responder (Right (LSP.InL highlights)),
      -- Find references (cross-file: searches all indexed files)
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.ReferenceParams tdId pos _ _ ctx) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            LSP.ReferenceContext includeDecl = ctx
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let allCallSites = Map.unions (map fsCallSites (Map.elems st))
            allBuiltins = Map.unions (map fsBuiltinCallSites (Map.elems st))
            locs = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                findReferences
                  allCallSites
                  allBuiltins
                  (fsFuncDefSites fs)
                  (fsVarUseSites fs)
                  (fsFilePath fs)
                  includeDecl
                  (fromIntegral lspLine)
                  (fromIntegral lspChar)
        responder (Right (LSP.InL locs)),
      -- Prepare rename (validate cursor is on renameable symbol)
      requestHandler SMethod_TextDocumentPrepareRename $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.PrepareRenameParams tdId pos _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                case prepareRename
                  (fsCallSites fs)
                  (fsFuncDefSites fs)
                  (fsVarUseSites fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar) of
                  Nothing -> LSP.InR LSP.Null
                  Just (range, _name) ->
                    LSP.InL (LSP.PrepareRenameResult (LSP.InL range))
        responder (Right result),
      -- Rename (cross-file: produces edits in all files that reference the symbol)
      requestHandler SMethod_TextDocumentRename $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.RenameParams _ tdId pos newName) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let allCallSites = Map.unions (map fsCallSites (Map.elems st))
            result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                case findRename
                  allCallSites
                  (fsFuncDefSites fs)
                  (fsVarUseSites fs)
                  (fsFilePath fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar)
                  newName of
                  Nothing -> LSP.InR LSP.Null
                  Just edit -> LSP.InL edit
        responder (Right result),
      -- Folding ranges
      requestHandler SMethod_TextDocumentFoldingRange $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.FoldingRangeParams _ _ tdId) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let ranges = case Map.lookup nuri st of
              Nothing -> []
              Just fs -> makeFoldingRanges (fsFoldingRanges fs)
        responder (Right (LSP.InL ranges)),
      -- Selection range (Shift+Alt+Right to expand selection by AST node)
      requestHandler SMethod_TextDocumentSelectionRange $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.SelectionRangeParams _ _ tdId positions) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                let srs =
                      [ case findSelectionRange
                          (fsTypes fs)
                          (fsFoldingRanges fs)
                          (fsFuncSymbols fs)
                          (fromIntegral lspLine)
                          (fromIntegral lspCol) of
                          Nothing -> LSP.SelectionRange (LSP.Range pos pos) Nothing
                          Just sr -> sr
                        | pos@(LSP.Position lspLine lspCol) <- positions
                      ]
                 in LSP.InL srs
        responder (Right result),
      -- Inlay hints (parameter names)
      requestHandler SMethod_TextDocumentInlayHint $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.InlayHintParams _ tdId range) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let hints = case Map.lookup nuri st of
              Nothing -> []
              Just fs -> makeInlayHints (fsCallWithArgs fs) (fsVarDeclTypes fs) range
        responder (Right (LSP.InL hints)),
      -- Code actions (quick fixes for unused symbols)
      requestHandler SMethod_TextDocumentCodeAction $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.CodeActionParams _ _ tdId range ctx) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.CodeActionContext diags _ _ = ctx
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let actions = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                makeCodeActions
                  (fsVarDeclSites fs)
                  (fsImportDecls fs)
                  (fsStdlibDefSites fs)
                  (fsStructDefs fs)
                  (fsFileText fs)
                  (fsFilePath fs)
                  range
                  diags
        responder (Right (LSP.InL (map LSP.InR actions))),
      -- Semantic tokens (full)
      requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.SemanticTokensParams _ _ tdId) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                LSP.InL $
                  buildSemanticTokens
                    (fsCallSites fs)
                    (fsBuiltinCallSites fs)
                    (fsFuncDefSites fs)
                    (fsVarUseSites fs)
                    (fsFilePath fs)
        responder (Right result),
      -- Code lens ("N references" / "Run" above fn main), cross-file reference count
      requestHandler SMethod_TextDocumentCodeLens $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.CodeLensParams _ _ tdId) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let allCallSites = Map.unions (map fsCallSites (Map.elems st))
            lenses = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                makeCodeLens
                  (fsFilePath fs)
                  (fsFuncSymbols fs)
                  allCallSites
        responder (Right (LSP.InL lenses)),
      -- Call hierarchy: prepare
      requestHandler SMethod_TextDocumentPrepareCallHierarchy $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.CallHierarchyPrepareParams tdId pos _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let items = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                prepareCallHierarchy
                  (fsCallSites fs)
                  (fsFuncDefSites fs)
                  (fsFuncSymbols fs)
                  (fsFilePath fs)
                  (fromIntegral lspLine)
                  (fromIntegral lspChar)
        responder (Right (LSP.InL items)),
      -- Call hierarchy: incoming calls (who calls this function)
      requestHandler SMethod_CallHierarchyIncomingCalls $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.CallHierarchyIncomingCallsParams _ _ chItem) = req
            LSP.CallHierarchyItem _ _ _ _ itemUri _ _ _ = chItem
            nuri = LSP.toNormalizedUri itemUri
        st <- liftIO $ readTVarIO stateVar
        let calls = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                incomingCalls
                  (fsCallsByFunc fs)
                  (fsFuncSymbols fs)
                  (fsFilePath fs)
                  chItem
        responder (Right (LSP.InL calls)),
      -- Document formatting (Shift+Alt+F)
      requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.DocumentFormattingParams _ tdId _) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                let src = fsFileText fs
                 in case parseForFormat (fsFilePath fs) src of
                      Left _ -> LSP.InR LSP.Null
                      Right prog ->
                        let formatted = formatProgram Config.defaultOptions src prog
                            end = LSP.Position (fromIntegral (length (T.lines src))) 0
                            edit = LSP.TextEdit (LSP.Range (LSP.Position 0 0) end) formatted
                         in LSP.InL [edit]
        responder (Right result),
      -- Call hierarchy: outgoing calls (what this function calls)
      requestHandler SMethod_CallHierarchyOutgoingCalls $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.CallHierarchyOutgoingCallsParams _ _ chItem) = req
            LSP.CallHierarchyItem _ _ _ _ itemUri _ _ _ = chItem
            nuri = LSP.toNormalizedUri itemUri
        st <- liftIO $ readTVarIO stateVar
        let calls = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                outgoingCalls
                  (fsCallsByFunc fs)
                  (fsFuncSymbols fs)
                  (fsFilePath fs)
                  chItem
        responder (Right (LSP.InL calls)),
      -- On-type formatting (auto-indent after { and })
      requestHandler SMethod_TextDocumentOnTypeFormatting $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.DocumentOnTypeFormattingParams tdId pos ch _) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspCol = pos
            nuri = LSP.toNormalizedUri uri
        mFile <- getVirtualFile nuri
        let edits = case mFile of
              Nothing -> []
              Just vf ->
                case T.uncons ch of
                  Nothing -> []
                  Just (c, _) ->
                    onTypeFormat
                      (virtualFileText vf)
                      (fromIntegral lspLine)
                      (fromIntegral lspCol)
                      c
        responder (Right (LSP.InL edits))
    ]

sendIndexingStatus :: Bool -> LspM () ()
sendIndexingStatus indexing =
  sendNotification
    (SMethod_CustomMethod (Proxy :: Proxy "$/quant/indexingStatus"))
    (object ["indexing" .= indexing])

parseForFormat :: FilePath -> Text -> Either String (Program ())
parseForFormat label src =
  case lexString (T.unpack src) of
    Left err -> Left err
    Right tokens ->
      case runParser (MP.many parseDecl) label tokens of
        Left err -> Left (errorBundlePretty err)
        Right decls -> Right (Program decls)

-- | Construct a FileState from an analysis result.
makeFileState :: AnalyzeResult -> FilePath -> Text -> FileState
makeFileState result =
  FileState
    (arTypes result)
    (arCallSites result)
    (arBuiltinCallSites result)
    (arDocs result)
    (arFuncEnv result)
    (arFuncDefSites result)
    (arStdlibDefSites result)
    (arCallWithArgs result)
    (arFuncSymbols result)
    (arFoldingRanges result)
    (arVarUseSites result)
    (arErrorNames result)
    (arVarDeclSites result)
    (arImportDecls result)
    (arCallsByFunc result)
    (arVarDeclTypes result)
    (arStructDefs result)
    (arStructDefSites result)

-- | Analyse a file from disk and store it in the state (no diagnostics published).
analyzeFromDisk :: TVar State -> FilePath -> IO ()
analyzeFromDisk stateVar fp = do
  mText <- readFileSafe fp
  case mText of
    Nothing -> return ()
    Just text -> do
      result <- analyzeText fp text
      let nuri = LSP.toNormalizedUri (LSP.filePathToUri fp)
          fs = makeFileState result fp text
      atomically $ modifyTVar' stateVar (Map.insert nuri fs)

-- | Index all .qa files under root in a background thread.
indexWorkspace :: TVar State -> FilePath -> IO ()
indexWorkspace stateVar root = do
  files <- findQaFiles root
  forM_ files (analyzeFromDisk stateVar)

-- | Recursively find all .qa files under a directory.
findQaFiles :: FilePath -> IO [FilePath]
findQaFiles dir = do
  entries <- catchIOError (listDirectory dir) (const (return []))
  let paths = map (dir </>) entries
  results <- mapM classify paths
  return (concat results)
  where
    classify p = do
      isDir <- catchIOError (doesDirectoryExist p) (const (return False))
      if isDir
        then findQaFiles p
        else return [p | ".qa" `isSuffixOf` p]

readFileSafe :: FilePath -> IO (Maybe Text)
readFileSafe fp =
  catchIOError (Just <$> TIO.readFile fp) (const (return Nothing))

analyzeAndPublish ::
  TVar State ->
  LSP.NormalizedUri ->
  Maybe LSP.Int32 ->
  LspM () ()
analyzeAndPublish stateVar nuri version = do
  mFile <- getVirtualFile nuri
  case mFile of
    Nothing -> return ()
    Just vf -> do
      let text = virtualFileText vf
          filePath = fromMaybe "<unknown>" (LSP.uriToFilePath (LSP.fromNormalizedUri nuri))
      result <- liftIO $ analyzeText filePath text
      let fs = makeFileState result filePath text
      liftIO $ atomically $ modifyTVar' stateVar (Map.insert nuri fs)
      st <- liftIO $ readTVarIO stateVar
      let calledNames =
            Set.fromList
              [fn | fsj <- Map.elems st, (_, (fn, _)) <- Map.toList (fsCallSites fsj)]
          dead = deadCodeDiags (fsFuncDefSites fs) calledNames
      publishDiagnostics 100 nuri version (partitionBySource (arDiagnostics result ++ dead))
