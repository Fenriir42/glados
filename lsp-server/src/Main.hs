module Main (main) where

import AST.Types.Common (FuncName, SourceSpan)
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
import LSPServer.Analyze (analyzeText)
import qualified LSPServer.Hover as Hover
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
    fsDocs :: Map FuncName Text
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
            }
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
      (diags, typeMap, callSites, builtinSites, docs) <- liftIO $ analyzeText filePath text
      let fs = FileState typeMap callSites builtinSites docs
      liftIO $ atomically $ modifyTVar' stateVar (Map.insert nuri fs)
      publishDiagnostics 100 nuri version (partitionBySource diags)
