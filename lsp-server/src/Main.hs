module Main (main) where

import AST.Types.Common (FuncName, SourceSpan, VarName)
import AST.Types.Type (FunctionType, Type)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import LSPServer.Analyze (AnalyzeResult (..), analyzeText)
import LSPServer.Completion (makeCompletionItems)
import LSPServer.Definition (findDefinition)
import LSPServer.DocumentSymbol (makeDocumentSymbols)
import LSPServer.Folding (makeFoldingRanges)
import LSPServer.Highlight (findHighlights)
import qualified LSPServer.Hover as Hover
import LSPServer.InlayHints (makeInlayHints)
import LSPServer.References (findReferences)
import LSPServer.Rename (findRename, prepareRename)
import LSPServer.SemanticTokens (buildSemanticTokens)
import LSPServer.SignatureHelp (findSignatureHelp)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import System.Exit (ExitCode (..), exitWith)

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
    fsFilePath :: FilePath,
    fsFileText :: Text
  }

type State = Map LSP.NormalizedUri FileState

main :: IO ()
main = do
  stateVar <- newTVarIO Map.empty
  code <-
    runServer $
      ServerDefinition
        { parseConfig = const $ const $ Right (),
          onConfigChange = const $ pure (),
          defaultConfig = (),
          configSection = "quant-lsp",
          doInitialize = \env _req -> pure (Right env),
          staticHandlers = \_caps -> mkHandlers stateVar,
          interpretHandler = \env -> Iso (runLspT env) liftIO,
          options = serverOptions
        }
  exitWith (if code == 0 then ExitSuccess else ExitFailure code)

serverOptions :: Options
serverOptions =
  defaultOptions
    { optTextDocumentSync =
        Just
          LSP.TextDocumentSyncOptions
            { LSP._openClose = Just True,
              LSP._change = Just LSP.TextDocumentSyncKind_Full,
              LSP._willSave = Nothing,
              LSP._willSaveWaitUntil = Nothing,
              LSP._save = Nothing
            },
      optCompletionTriggerCharacters = Just ['.'],
      optSignatureHelpTriggerCharacters = Just ['(', ',']
    }

mkHandlers :: TVar State -> Handlers (LspM ())
mkHandlers stateVar =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_msg -> pure (),
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_msg -> pure (),
      notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \_msg -> pure (),
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
      -- Find references
      requestHandler SMethod_TextDocumentReferences $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.ReferenceParams tdId pos _ _ ctx) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            LSP.ReferenceContext includeDecl = ctx
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let locs = case Map.lookup nuri st of
              Nothing -> []
              Just fs ->
                findReferences
                  (fsCallSites fs)
                  (fsBuiltinCallSites fs)
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
      -- Rename
      requestHandler SMethod_TextDocumentRename $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.RenameParams _ tdId pos newName) = req
            LSP.TextDocumentIdentifier uri = tdId
            LSP.Position lspLine lspChar = pos
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let result = case Map.lookup nuri st of
              Nothing -> LSP.InR LSP.Null
              Just fs ->
                case findRename
                  (fsCallSites fs)
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
      -- Inlay hints (parameter names)
      requestHandler SMethod_TextDocumentInlayHint $ \req responder -> do
        let TRequestMessage _ _ _ (LSP.InlayHintParams _ tdId range) = req
            LSP.TextDocumentIdentifier uri = tdId
            nuri = LSP.toNormalizedUri uri
        st <- liftIO $ readTVarIO stateVar
        let hints = case Map.lookup nuri st of
              Nothing -> []
              Just fs -> makeInlayHints (fsCallWithArgs fs) range
        responder (Right (LSP.InL hints)),
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
        responder (Right result)
    ]

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
      let fs =
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
              filePath
              text
      liftIO $ atomically $ modifyTVar' stateVar (Map.insert nuri fs)
      publishDiagnostics 100 nuri version (partitionBySource (arDiagnostics result))
