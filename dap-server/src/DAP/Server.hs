module DAP.Server (runServer) where

import AST.Types.AST (Program (..))
import AST.Types.Common
  ( ErrorName (..),
    FuncName (..),
    VarName (..),
  )
import Compiler.Bytecode (Bytecode)
import qualified Compiler.Bytecode as BC
import Compiler.Codegen (compileProgram)
import Compiler.Import (resolveImports)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
  ( MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryReadMVar,
  )
import Control.Concurrent.STM
  ( TVar,
    atomically,
    newTVarIO,
    readTVarIO,
    writeTVar,
  )
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import DAP.DebugInfo (DebugInfo, buildDebugInfo, instrLine, lineInstrs)
import DAP.Protocol
  ( makeEvent,
    makeResponse,
    readDAPMessage,
    writeDAPMessage,
  )
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Lib (lexFile)
import Parser.Decl (parseDecl)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO
  ( BufferMode (..),
    Handle,
    hSetBinaryMode,
    hSetBuffering,
    stdin,
    stdout,
  )
import System.Posix.IO (createPipe, fdToHandle)
import Text.Megaparsec (many, runParser)
import TypeChecker (TypeCheckResult (..), tcAllCallMap, tcErrors, typeCheck)
import VM.Interpreter
  ( Frame (..),
    VMError (..),
    VMState (..),
    runDebugProgram,
  )

-- ---------------------------------------------------------------------------
-- State types

data StepMode
  = RunFree
  | StepInMode
  | StepOverMode Int

data PausedInfo = PausedInfo
  { piFunc :: FuncName,
    piVMSt :: VMState
  }

data ResumeCmd
  = CmdContinue
  | CmdNext
  | CmdStepIn
  | CmdTerminate

data RunState = RunState
  { rsBreakpoints :: TVar (Set.Set (FuncName, Int)),
    rsStepping :: TVar StepMode,
    rsPaused :: IORef (Maybe PausedInfo),
    rsResume :: MVar ResumeCmd,
    rsTerminate :: TVar Bool,
    rsInfo :: DebugInfo,
    rsSourceFile :: FilePath
  }

data SessionPhase
  = PhaseInit
  | PhaseConfigured FilePath Bool (Map.Map FilePath [Int])
  | PhaseRunning RunState
  | PhaseDone

data DAPSession = DAPSession
  { sessOut :: Chan Value,
    sessSeq :: IORef Int,
    sessPhase :: IORef SessionPhase
  }

-- ---------------------------------------------------------------------------
-- Entry point

runServer :: IO ()
runServer = do
  (rdFd, wrFd) <- createPipe
  dapOut <- hDuplicate stdout
  wrHandle <- fdToHandle wrFd
  hSetBuffering wrHandle LineBuffering
  hDuplicateTo wrHandle stdout
  hSetBinaryMode dapOut True
  hSetBinaryMode stdin True

  outChan <- newChan
  seqRef <- newIORef 1
  phaseRef <- newIORef PhaseInit

  let session = DAPSession outChan seqRef phaseRef

  void $ forkIO $ writerLoop dapOut outChan seqRef
  rdHandle <- fdToHandle rdFd
  hSetBuffering rdHandle LineBuffering
  void $ forkIO $ captureLoop rdHandle outChan seqRef

  mainLoop session

writerLoop :: Handle -> Chan Value -> IORef Int -> IO ()
writerLoop h chan seqRef = do
  msg <- readChan chan
  writeDAPMessage h seqRef msg
  writerLoop h chan seqRef

captureLoop :: Handle -> Chan Value -> IORef Int -> IO ()
captureLoop h chan seqRef = do
  r <- try (TIO.hGetLine h) :: IO (Either SomeException Text)
  case r of
    Left _ -> return ()
    Right line -> do
      ev <-
        makeEvent
          seqRef
          "output"
          (Just (object ["category" .= ("stdout" :: Text), "output" .= (line <> "\n")]))
      writeChan chan ev
      captureLoop h chan seqRef

mainLoop :: DAPSession -> IO ()
mainLoop session = do
  msg <- readDAPMessage stdin
  case msg of
    Nothing -> return ()
    Just val -> do
      handleMessage session val
      phase <- readIORef (sessPhase session)
      case phase of
        PhaseDone -> return ()
        _ -> mainLoop session

-- ---------------------------------------------------------------------------
-- Message dispatch

handleMessage :: DAPSession -> Value -> IO ()
handleMessage session (Object km) = do
  let cmd = getStr "command" km
      seq' = getInt "seq" km
      args = case KM.lookup "arguments" km of
        Just (Object a) -> a
        _ -> KM.empty
  dispatch session seq' cmd args
handleMessage _ _ = return ()

dispatch :: DAPSession -> Int -> Text -> KM.KeyMap Value -> IO ()
dispatch session seq' cmd args = case cmd of
  "initialize" -> handleInitialize session seq'
  "launch" -> handleLaunch session seq' args
  "setBreakpoints" -> handleSetBreakpoints session seq' args
  "configurationDone" -> handleConfigurationDone session seq'
  "threads" -> handleThreads session seq'
  "stackTrace" -> handleStackTrace session seq'
  "scopes" -> handleScopes session seq' args
  "variables" -> handleVariables session seq' args
  "continue" -> handleResume session seq' cmd CmdContinue
  "next" -> handleResume session seq' cmd CmdNext
  "stepIn" -> handleResume session seq' cmd CmdStepIn
  "stepOut" -> handleResume session seq' cmd CmdContinue
  "pause" -> sendOk session seq' cmd
  "disconnect" -> handleDisconnect session seq'
  "terminate" -> handleDisconnect session seq'
  _ -> sendOk session seq' cmd

-- ---------------------------------------------------------------------------
-- Handlers

handleInitialize :: DAPSession -> Int -> IO ()
handleInitialize session seq' = do
  let caps =
        object
          [ "supportsConfigurationDoneRequest" .= True,
            "supportsDelayedStackTraceLoading" .= False,
            "supportsSetVariable" .= False,
            "supportsConditionalBreakpoints" .= False,
            "supportsStepBack" .= False
          ]
  resp <- makeResponse (sessSeq session) seq' "initialize" True (Just caps)
  writeChan (sessOut session) resp
  ev <- makeEvent (sessSeq session) "initialized" Nothing
  writeChan (sessOut session) ev

handleLaunch :: DAPSession -> Int -> KM.KeyMap Value -> IO ()
handleLaunch session seq' args = do
  let prog = T.unpack (getStr "program" args)
      stopOnEntry = getBool "stopOnEntry" args
  writeIORef (sessPhase session) (PhaseConfigured prog stopOnEntry Map.empty)
  sendOk session seq' "launch"

handleSetBreakpoints :: DAPSession -> Int -> KM.KeyMap Value -> IO ()
handleSetBreakpoints session seq' args = do
  let srcPath = case KM.lookup "source" args of
        Just (Object src) -> T.unpack (getStr "path" src)
        _ -> ""
      bpLines =
        [ round n
          | Just (Array arr) <- [KM.lookup "breakpoints" args],
            Object bp <- foldr (:) [] arr,
            Number n <- [getField "line" bp]
        ]
  phase <- readIORef (sessPhase session)
  case phase of
    PhaseConfigured prog stop bpMap ->
      writeIORef
        (sessPhase session)
        (PhaseConfigured prog stop (Map.insert srcPath bpLines bpMap))
    PhaseRunning rs -> do
      let di = rsInfo rs
          newBPs =
            Set.fromList
              [ (fname, idx)
                | line <- bpLines,
                  (fname, idx) <- lineInstrs di line
              ]
      atomically $ writeTVar (rsBreakpoints rs) newBPs
    _ -> return ()
  let verified = [object ["verified" .= True, "line" .= (l :: Int)] | l <- bpLines]
  resp <-
    makeResponse
      (sessSeq session)
      seq'
      "setBreakpoints"
      True
      (Just (object ["breakpoints" .= verified]))
  writeChan (sessOut session) resp

handleConfigurationDone :: DAPSession -> Int -> IO ()
handleConfigurationDone session seq' = do
  sendOk session seq' "configurationDone"
  phase <- readIORef (sessPhase session)
  case phase of
    PhaseConfigured prog stopOnEntry bpMap -> do
      result <- compileForDebug prog
      case result of
        Left err -> sendOutput session ("compile error: " <> err <> "\n")
        Right bcs -> startDebugSession session prog stopOnEntry bpMap bcs
    _ -> return ()

startDebugSession ::
  DAPSession ->
  FilePath ->
  Bool ->
  Map.Map FilePath [Int] ->
  [Bytecode] ->
  IO ()
startDebugSession session srcFile stopOnEntry bpMap bcs = do
  let di = buildDebugInfo bcs
      initBPs =
        Set.fromList
          [ (fname, idx)
            | lines' <- Map.elems bpMap,
              line <- lines',
              (fname, idx) <- lineInstrs di line
          ]
      initStep = if stopOnEntry then StepInMode else RunFree

  bpsVar <- newTVarIO initBPs
  stepVar <- newTVarIO initStep
  pauseRef <- newIORef Nothing
  resumeMV <- newEmptyMVar
  termVar <- newTVarIO False

  let rs = RunState bpsVar stepVar pauseRef resumeMV termVar di srcFile
  writeIORef (sessPhase session) (PhaseRunning rs)

  ev <-
    makeEvent
      (sessSeq session)
      "thread"
      (Just (object ["threadId" .= (1 :: Int), "reason" .= ("started" :: Text)]))
  writeChan (sessOut session) ev

  void $ forkIO $ do
    result <-
      try (runDebugProgram (makeHook session rs) bcs) ::
        IO (Either SomeException (Either VMError BC.Value))
    writeIORef (sessPhase session) PhaseDone
    case result of
      Left ex ->
        unless (isTerminate ex) $
          sendOutput session ("exception: " <> T.pack (show ex) <> "\n")
      Right (Left err) ->
        sendOutput session (T.pack (prettyVMError err) <> "\n")
      Right (Right _) ->
        return ()
    termEv <- makeEvent (sessSeq session) "terminated" (Just (object []))
    writeChan (sessOut session) termEv
    void $ forkIO $ do
      threadDelay 500000
      ev2 <-
        makeEvent
          (sessSeq session)
          "exited"
          (Just (object ["exitCode" .= (0 :: Int)]))
      writeChan (sessOut session) ev2
      threadDelay 200000
      exitSuccess

isTerminate :: SomeException -> Bool
isTerminate ex = "debug-terminate" `T.isInfixOf` T.pack (show ex)

makeHook :: DAPSession -> RunState -> VMState -> IO ()
makeHook session rs vmst = do
  terminate <- readTVarIO (rsTerminate rs)
  when terminate $ ioError (userError "debug-terminate")
  let instrIdx = vmIP vmst - 1
      curFunc = vmCurrentFunc vmst
  bps <- readTVarIO (rsBreakpoints rs)
  mode <- readTVarIO (rsStepping rs)
  let depth = length (vmCallStack vmst)
      atBP = Set.member (curFunc, instrIdx) bps
      atStep = case mode of
        RunFree -> False
        StepInMode -> True
        StepOverMode d -> depth <= d
  when (atBP || atStep) $ do
    atomically $ writeTVar (rsStepping rs) RunFree
    writeIORef (rsPaused rs) (Just (PausedInfo curFunc vmst))
    let reason = if atBP then "breakpoint" else "step" :: Text
    ev <-
      makeEvent
        (sessSeq session)
        "stopped"
        ( Just
            ( object
                [ "reason" .= reason,
                  "threadId" .= (1 :: Int),
                  "allThreadsStopped" .= True
                ]
            )
        )
    writeChan (sessOut session) ev
    cmd <- takeMVar (rsResume rs)
    writeIORef (rsPaused rs) Nothing
    case cmd of
      CmdTerminate -> ioError (userError "debug-terminate")
      CmdNext -> atomically $ writeTVar (rsStepping rs) (StepOverMode depth)
      CmdStepIn -> atomically $ writeTVar (rsStepping rs) StepInMode
      CmdContinue -> return ()

handleThreads :: DAPSession -> Int -> IO ()
handleThreads session seq' = do
  resp <-
    makeResponse
      (sessSeq session)
      seq'
      "threads"
      True
      (Just (object ["threads" .= [object ["id" .= (1 :: Int), "name" .= ("main" :: Text)]]]))
  writeChan (sessOut session) resp

handleStackTrace :: DAPSession -> Int -> IO ()
handleStackTrace session seq' = do
  phase <- readIORef (sessPhase session)
  case phase of
    PhaseRunning rs -> do
      mpi <- readIORef (rsPaused rs)
      case mpi of
        Nothing -> sendOk session seq' "stackTrace"
        Just pinfo -> do
          let di = rsInfo rs
              src = rsSourceFile rs
              mkFrame fid fname ip =
                object
                  [ "id" .= (fid :: Int),
                    "name" .= unFuncName fname,
                    "source" .= object ["path" .= src, "name" .= src],
                    "line" .= fromMaybe (0 :: Int) (instrLine di fname ip),
                    "column" .= (1 :: Int)
                  ]
              cur = mkFrame 0 (piFunc pinfo) (vmIP (piVMSt pinfo) - 1)
              saved =
                [ mkFrame (i + 1) (fFunc f) (fIP f - 1)
                  | (i, f) <- zip [0 ..] (vmCallStack (piVMSt pinfo))
                ]
              frames = cur : saved
          resp <-
            makeResponse
              (sessSeq session)
              seq'
              "stackTrace"
              True
              ( Just
                  ( object
                      [ "stackFrames" .= frames,
                        "totalFrames" .= length frames
                      ]
                  )
              )
          writeChan (sessOut session) resp
    _ -> sendOk session seq' "stackTrace"

handleScopes :: DAPSession -> Int -> KM.KeyMap Value -> IO ()
handleScopes session seq' args = do
  let frameId = getInt "frameId" args
      varRef = 1001 + frameId
  phase <- readIORef (sessPhase session)
  nVars <- case phase of
    PhaseRunning rs -> do
      mpi <- readIORef (rsPaused rs)
      return $ case mpi of
        Nothing -> 0
        Just pinfo ->
          if frameId == 0
            then Map.size (vmLocals (piVMSt pinfo))
            else case drop (frameId - 1) (vmCallStack (piVMSt pinfo)) of
              (f : _) -> Map.size (fLocals f)
              [] -> 0
    _ -> return 0
  let scope =
        object
          [ "name" .= ("Locals" :: Text),
            "variablesReference" .= varRef,
            "expensive" .= False,
            "namedVariables" .= nVars
          ]
  resp <-
    makeResponse
      (sessSeq session)
      seq'
      "scopes"
      True
      (Just (object ["scopes" .= [scope]]))
  writeChan (sessOut session) resp

handleVariables :: DAPSession -> Int -> KM.KeyMap Value -> IO ()
handleVariables session seq' args = do
  let varRef = getInt "variablesReference" args
  phase <- readIORef (sessPhase session)
  vars <- case phase of
    PhaseRunning rs -> do
      mpi <- readIORef (rsPaused rs)
      return $ case mpi of
        Nothing -> []
        Just pinfo ->
          let vmst = piVMSt pinfo
              strings = vmStrings vmst
              frameIdx = varRef - 1001
              locals
                | varRef >= 1001 && varRef < 2000 =
                    if frameIdx == 0
                      then vmLocals vmst
                      else case drop (frameIdx - 1) (vmCallStack vmst) of
                        (f : _) -> fLocals f
                        [] -> Map.empty
                | otherwise = Map.empty
           in if varRef >= 1001 && varRef < 2000
                then
                  [ mkVar (T.pack (T.unpack (unVarName name))) val vmst strings
                    | (name, val) <- Map.toList locals
                  ]
                else
                  if varRef >= 2000 && varRef < 3000
                    then expandArray vmst strings (varRef - 2000)
                    else
                      if varRef >= 3000 && varRef < 4000
                        then expandDict vmst strings (varRef - 3000)
                        else
                          if varRef >= 4000
                            then expandStruct vmst strings (varRef - 4000)
                            else []
    _ -> return []
  resp <-
    makeResponse
      (sessSeq session)
      seq'
      "variables"
      True
      (Just (object ["variables" .= vars]))
  writeChan (sessOut session) resp

mkVar :: Text -> BC.Value -> VMState -> [Text] -> Value
mkVar name val vmst strings =
  object
    [ "name" .= name,
      "value" .= displayValue vmst strings val,
      "type" .= valueType val,
      "variablesReference" .= heapRef val
    ]

expandArray :: VMState -> [Text] -> Int -> [Value]
expandArray vmst strings aid =
  case Map.lookup aid (vmHeap vmst) of
    Nothing -> []
    Just arr ->
      [ mkVar (T.pack (show idx)) v vmst strings
        | (idx, v) <- Map.toAscList arr
      ]

expandDict :: VMState -> [Text] -> Int -> [Value]
expandDict vmst strings did =
  case Map.lookup did (vmDictHeap vmst) of
    Nothing -> []
    Just d ->
      [ mkVar (displayValue vmst strings k) v vmst strings
        | (k, v) <- Map.toList d
      ]

expandStruct :: VMState -> [Text] -> Int -> [Value]
expandStruct vmst strings sid =
  case Map.lookup sid (vmStructHeap vmst) of
    Nothing -> []
    Just fields -> [mkVar field v vmst strings | (field, v) <- Map.toList fields]

heapRef :: BC.Value -> Int
heapRef (BC.VArrayRef aid) = 2000 + aid
heapRef (BC.VDictRef did) = 3000 + did
heapRef (BC.VStructRef sid) = 4000 + sid
heapRef _ = 0

displayValue :: VMState -> [Text] -> BC.Value -> Text
displayValue _ strings (BC.VStringRef i)
  | i < length strings = strings !! i
  | otherwise = "<str>"
displayValue _ _ (BC.VString t) = t
displayValue _ _ (BC.VInt n) = T.pack (show n)
displayValue _ _ (BC.VFloat f) = T.pack (show f)
displayValue _ _ (BC.VBool True) = "true"
displayValue _ _ (BC.VBool False) = "false"
displayValue _ _ BC.VUnit = "void"
displayValue _ _ (BC.VFunction fn) = unFuncName fn
displayValue _ _ (BC.VErrorVal e _) = unErrorName e
displayValue _ _ (BC.VPointer addr) = "0x" <> T.pack (show addr)
displayValue _ _ (BC.VClosure fn _) = unFuncName fn
displayValue _ _ (BC.VTask tid) = "task#" <> T.pack (show tid)
displayValue vmst _ (BC.VArrayRef aid) =
  case Map.lookup aid (vmHeap vmst) of
    Nothing -> "[...]"
    Just arr -> "[" <> T.pack (show (Map.size arr)) <> " items]"
displayValue vmst _ (BC.VDictRef did) =
  case Map.lookup did (vmDictHeap vmst) of
    Nothing -> "{...}"
    Just d -> "{" <> T.pack (show (Map.size d)) <> " entries}"
displayValue vmst _ (BC.VStructRef sid) =
  case Map.lookup sid (vmStructHeap vmst) of
    Nothing -> "struct{}"
    Just fields -> "struct{" <> T.intercalate ", " (Map.keys fields) <> "}"

valueType :: BC.Value -> Text
valueType (BC.VInt _) = "int"
valueType (BC.VFloat _) = "float"
valueType (BC.VBool _) = "bool"
valueType (BC.VString _) = "str"
valueType (BC.VStringRef _) = "str"
valueType BC.VUnit = "void"
valueType (BC.VArrayRef _) = "array"
valueType (BC.VDictRef _) = "dict"
valueType (BC.VStructRef _) = "struct"
valueType (BC.VFunction _) = "fn"
valueType (BC.VErrorVal _ _) = "error"
valueType (BC.VPointer _) = "ptr"
valueType (BC.VClosure _ _) = "fn"
valueType (BC.VTask _) = "task"

handleResume :: DAPSession -> Int -> Text -> ResumeCmd -> IO ()
handleResume session seq' cmd resumeCmd = do
  sendOk session seq' cmd
  phase <- readIORef (sessPhase session)
  case phase of
    PhaseRunning rs -> do
      ev <-
        makeEvent
          (sessSeq session)
          "continued"
          (Just (object ["threadId" .= (1 :: Int), "allThreadsContinued" .= True]))
      writeChan (sessOut session) ev
      putMVar (rsResume rs) resumeCmd
    _ -> return ()

handleDisconnect :: DAPSession -> Int -> IO ()
handleDisconnect session seq' = do
  sendOk session seq' "disconnect"
  phase <- readIORef (sessPhase session)
  case phase of
    PhaseRunning rs -> do
      atomically $ writeTVar (rsTerminate rs) True
      mOld <- tryReadMVar (rsResume rs)
      case mOld of
        Just _ -> return ()
        Nothing -> putMVar (rsResume rs) CmdTerminate
    _ -> return ()
  writeIORef (sessPhase session) PhaseDone

-- ---------------------------------------------------------------------------
-- Compilation helpers

compileForDebug :: FilePath -> IO (Either Text [Bytecode])
compileForDebug filePath = do
  stdlibDir <- resolveStdlib
  r <- try (doCompile stdlibDir filePath) :: IO (Either SomeException [Bytecode])
  return $ case r of
    Left ex -> Left (T.pack (show ex))
    Right bc -> Right bc

doCompile :: FilePath -> FilePath -> IO [Bytecode]
doCompile stdlibDir fp = do
  tokens <-
    lexFile fp >>= either (ioError . userError) return
  rawDecls <-
    case runParser (many parseDecl) fp tokens of
      Left err -> ioError (userError (show err))
      Right ds -> return ds
  decls <-
    resolveImports [takeDirectory fp, stdlibDir] rawDecls
      >>= either (ioError . userError) return
  let tcResult = typeCheck (Program decls)
      typeErrs = tcErrors tcResult
  unless (null typeErrs) $
    ioError (userError ("type check failed with " ++ show (length typeErrs) ++ " error(s)"))
  case compileProgram (tcAllCallMap tcResult) (tcEnumVariantSpans tcResult) (tcDynMethodCalls tcResult) (Program decls) of
    Left err -> ioError (userError (show err))
    Right bc -> return bc

resolveStdlib :: IO FilePath
resolveStdlib = do
  args <- getArgs
  case argPairs args of
    Just d -> return d
    Nothing -> do
      envM <- lookupEnv "QUANT_STDLIB"
      case envM of
        Just d -> return d
        Nothing -> do
          let sys = "/usr/local/share/quant/lib"
          ok <- doesDirectoryExist sys
          return (if ok then sys else "./std")
  where
    argPairs [] = Nothing
    argPairs [_] = Nothing
    argPairs ("--stdlib" : v : _) = Just v
    argPairs (_ : rest) = argPairs rest

prettyVMError :: VMError -> String
prettyVMError = go
  where
    go (VMRuntimeError msg) = "runtime error: " ++ msg
    go (VMUndefinedVar v) = "undefined variable `" ++ T.unpack (unVarName v) ++ "`"
    go (VMUndefinedFunction f) = "undefined function `" ++ T.unpack (unFuncName f) ++ "`"
    go (VMStackUnderflow ctx) = "stack underflow in " ++ ctx
    go (VMOutOfBounds i len) = "index " ++ show i ++ " out of bounds (length " ++ show len ++ ")"
    go (VMTypeMismatch msg) = "type mismatch: " ++ msg
    go (VMInContext _ _ inner) = go inner

-- ---------------------------------------------------------------------------
-- Helpers

sendOk :: DAPSession -> Int -> Text -> IO ()
sendOk session seq' cmd = do
  resp <- makeResponse (sessSeq session) seq' cmd True Nothing
  writeChan (sessOut session) resp

sendOutput :: DAPSession -> Text -> IO ()
sendOutput session msg = do
  ev <-
    makeEvent
      (sessSeq session)
      "output"
      (Just (object ["category" .= ("stderr" :: Text), "output" .= msg]))
  writeChan (sessOut session) ev

getStr :: Text -> KM.KeyMap Value -> Text
getStr k km = case KM.lookup (K.fromText k) km of
  Just (String t) -> t
  _ -> ""

getBool :: Text -> KM.KeyMap Value -> Bool
getBool k km = case KM.lookup (K.fromText k) km of
  Just (Bool b) -> b
  _ -> False

getInt :: Text -> KM.KeyMap Value -> Int
getInt k km = case KM.lookup (K.fromText k) km of
  Just (Number n) -> round n
  _ -> 0

getField :: Text -> KM.KeyMap Value -> Value
getField k km = case KM.lookup (K.fromText k) km of
  Just v -> v
  _ -> Null
