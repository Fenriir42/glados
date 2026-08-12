-- | Stack-based bytecode interpreter for the Quant VM.
module VM.Interpreter
  ( VMError (..),
    VMState (..),
    Frame (..),
    runProgram,
    runFunction,
    runFunctionCov,
    runFunctionLineCov,
    runDebugProgram,
  )
where

import AST.Types.Common (ErrorName (..), FieldName (..), FuncName (..), TypeName (..), VarName, unTypeName)
import Compiler.Bytecode
  ( BinaryOp (..),
    Bytecode (..),
    CRetType (..),
    CastType (..),
    FunctionRef (..),
    Instruction (..),
    InstructionPointer (..),
    UnaryOp (..),
    Value (..),
  )
import Control.Exception (SomeException, try)
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.State (StateT, gets, modify, runStateT)
import qualified Control.Monad.State as S
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.LibFFI
  ( argCDouble,
    argInt64,
    argPtr,
    argString,
    argWord32,
    callFFI,
    retCDouble,
    retInt64,
    retPtr,
    retString,
    retVoid,
    retWord32,
  )
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castFunPtrToPtr, freeHaskellFunPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.Posix.DynamicLinker (DL, RTLDFlags (..), dlopen, dlsym)
import System.Posix.IO (fdWrite)
import System.Posix.Types (ByteCount, Fd (..))
import Text.Regex.TDFA (getAllTextMatches, (=~))
import VM.Builtins (applyFormat, callBuiltin, isBuiltin, isHeapBuiltin)

-- ---------------------------------------------------------------------------
-- Types

data VMError
  = VMRuntimeError String
  | VMUndefinedVar VarName
  | VMUndefinedFunction FuncName
  | VMStackUnderflow String
  | VMOutOfBounds Int Int
  | VMTypeMismatch String
  | -- | Wraps an error with the function name and instruction offset where it occurred.
    VMInContext FuncName Int VMError
  deriving stock (Show, Eq)

-- | A saved call frame (pushed on function call, restored on return).
data Frame = Frame
  { fLocals :: Map VarName Value,
    fIP :: Int,
    fInstrs :: V.Vector Instruction,
    fStrings :: [Text],
    fFunc :: FuncName
  }

-- | A task's full execution context, saved while it is not the running task.
data TaskCtx = TaskCtx
  { tStack :: [Value],
    tLocals :: Map VarName Value,
    tIP :: Int,
    tInstrs :: V.Vector Instruction,
    tStrings :: [Text],
    tCallStack :: [Frame],
    tFunc :: FuncName
  }

-- | Scheduler entry for a task that is not currently running.
data TaskEntry
  = -- | Runnable, waiting for a scheduler slot
    TaskReady TaskCtx
  | -- | Suspended until the given task id completes
    TaskWaiting Int TaskCtx
  | -- | Finished with this (string-resolved) result
    TaskDone Value

-- | A function compiled for execution: its instruction stream as an O(1)-index
-- 'V.Vector' and its string pool, built once at load time rather than rebuilt
-- on every call.  'cfBytecode' keeps the original around for metadata (arity).
data CompiledFn = CompiledFn
  { cfInstrs :: V.Vector Instruction,
    cfStrings :: [Text],
    cfBytecode :: Bytecode
  }

compileFn :: Bytecode -> CompiledFn
compileFn bc = CompiledFn (V.fromList (bytecodeInstructions bc)) (bytecodeStrings bc) bc

data VMState = VMState
  { vmStack :: [Value],
    vmLocals :: Map VarName Value,
    vmIP :: Int,
    vmInstrs :: V.Vector Instruction,
    vmStrings :: [Text],
    vmCallStack :: [Frame],
    vmHeap :: Map Int (Map Int Value),
    vmDictHeap :: Map Int (Map Value Value),
    vmStructHeap :: Map Int (Map Text Value),
    -- | Maps struct heap IDs to their declared type name (for dynamic dispatch).
    vmStructTypes :: Map Int TypeName,
    vmSocketHeap :: Map Int NS.Socket,
    vmFFILibs :: Map Text DL,
    vmNextId :: Int,
    vmFunctions :: Map FuncName CompiledFn,
    vmCurrentFunc :: FuncName,
    vmCoverage :: Maybe (IORef (Set.Set FuncName)),
    -- | Per-function hit line numbers for line coverage.
    vmLineCov :: Maybe (IORef (Map.Map FuncName (Set.Set Int))),
    -- | Per-branch outcome tracking: (funcName, sourceLine) -> outcomes seen.
    vmBranchCov :: Maybe (IORef (Map.Map (FuncName, Int) (Set.Set Bool))),
    -- | Optional debug hook called at each ICovMark (pauses VM for DAP).
    vmDebugHook :: Maybe (VMState -> IO ()),
    -- | Suspended/finished tasks by id (the running task has no entry).
    vmTasks :: Map Int TaskEntry,
    -- | Ids of TaskReady tasks in FIFO scheduling order.
    vmReadyQueue :: [Int],
    -- | Id of the running task; 0 is the main task.
    vmCurrentTask :: Int,
    -- | Next fresh task id (task 0 is main and is never allocated).
    vmNextTaskId :: Int
  }

type VM a = ExceptT VMError (StateT VMState IO) a

-- ---------------------------------------------------------------------------
-- Public entry point

-- | Load all bytecodes and execute @main@, returning its return value.
runProgram :: [Bytecode] -> IO (Either VMError Value)
runProgram bytecodes = do
  let funcs = Map.fromList [(bytecodeFunction bc, compileFn bc) | bc <- bytecodes]
  case Map.lookup (FuncName "main") funcs of
    Nothing -> return $ Left $ VMUndefinedFunction (FuncName "main")
    Just mainBc -> do
      let initState =
            VMState
              { vmStack = [],
                vmLocals = Map.empty,
                vmIP = 0,
                vmInstrs = cfInstrs mainBc,
                vmStrings = cfStrings mainBc,
                vmCallStack = [],
                vmHeap = Map.empty,
                vmDictHeap = Map.empty,
                vmStructHeap = Map.empty,
                vmStructTypes = Map.empty,
                vmSocketHeap = Map.empty,
                vmFFILibs = Map.empty,
                vmNextId = 0,
                vmFunctions = funcs,
                vmCurrentFunc = FuncName "main",
                vmCoverage = Nothing,
                vmLineCov = Nothing,
                vmBranchCov = Nothing,
                vmDebugHook = Nothing,
                vmTasks = Map.empty,
                vmReadyQueue = [],
                vmCurrentTask = 0,
                vmNextTaskId = 1
              }
      (result, _) <- runStateT (runExceptT execLoop) initState
      return result

-- | Load all bytecodes and execute a named function directly (no @main@ required).
runFunction :: FuncName -> [Bytecode] -> IO (Either VMError Value)
runFunction fname bytecodes = do
  let funcs = Map.fromList [(bytecodeFunction bc, compileFn bc) | bc <- bytecodes]
  case Map.lookup fname funcs of
    Nothing -> return $ Left $ VMUndefinedFunction fname
    Just bc -> do
      let initState =
            VMState
              { vmStack = [],
                vmLocals = Map.empty,
                vmIP = 0,
                vmInstrs = cfInstrs bc,
                vmStrings = cfStrings bc,
                vmCallStack = [],
                vmHeap = Map.empty,
                vmDictHeap = Map.empty,
                vmStructHeap = Map.empty,
                vmStructTypes = Map.empty,
                vmSocketHeap = Map.empty,
                vmFFILibs = Map.empty,
                vmNextId = 0,
                vmFunctions = funcs,
                vmCurrentFunc = fname,
                vmCoverage = Nothing,
                vmLineCov = Nothing,
                vmBranchCov = Nothing,
                vmDebugHook = Nothing,
                vmTasks = Map.empty,
                vmReadyQueue = [],
                vmCurrentTask = 0,
                vmNextTaskId = 1
              }
      (result, _) <- runStateT (runExceptT execLoop) initState
      return result

-- | Like 'runFunction' but records every user-function call into @covRef@.
runFunctionCov :: IORef (Set.Set FuncName) -> FuncName -> [Bytecode] -> IO (Either VMError Value)
runFunctionCov covRef fname bytecodes = do
  let funcs = Map.fromList [(bytecodeFunction bc, compileFn bc) | bc <- bytecodes]
  case Map.lookup fname funcs of
    Nothing -> return $ Left $ VMUndefinedFunction fname
    Just bc -> do
      let initState =
            VMState
              { vmStack = [],
                vmLocals = Map.empty,
                vmIP = 0,
                vmInstrs = cfInstrs bc,
                vmStrings = cfStrings bc,
                vmCallStack = [],
                vmHeap = Map.empty,
                vmDictHeap = Map.empty,
                vmStructHeap = Map.empty,
                vmStructTypes = Map.empty,
                vmSocketHeap = Map.empty,
                vmFFILibs = Map.empty,
                vmNextId = 0,
                vmFunctions = funcs,
                vmCurrentFunc = fname,
                vmCoverage = Just covRef,
                vmLineCov = Nothing,
                vmBranchCov = Nothing,
                vmDebugHook = Nothing,
                vmTasks = Map.empty,
                vmReadyQueue = [],
                vmCurrentTask = 0,
                vmNextTaskId = 1
              }
      (result, _) <- runStateT (runExceptT execLoop) initState
      return result

-- | Like 'runFunctionCov' but also records per-function hit line numbers via
-- 'ICovMark' instructions and per-branch outcomes via 'ICovBranch' instructions.
runFunctionLineCov ::
  IORef (Set.Set FuncName) ->
  IORef (Map.Map FuncName (Set.Set Int)) ->
  IORef (Map.Map (FuncName, Int) (Set.Set Bool)) ->
  FuncName ->
  [Bytecode] ->
  IO (Either VMError Value)
runFunctionLineCov covRef lineCovRef branchCovRef fname bytecodes = do
  let funcs = Map.fromList [(bytecodeFunction bc, compileFn bc) | bc <- bytecodes]
  case Map.lookup fname funcs of
    Nothing -> return $ Left $ VMUndefinedFunction fname
    Just bc -> do
      let initState =
            VMState
              { vmStack = [],
                vmLocals = Map.empty,
                vmIP = 0,
                vmInstrs = cfInstrs bc,
                vmStrings = cfStrings bc,
                vmCallStack = [],
                vmHeap = Map.empty,
                vmDictHeap = Map.empty,
                vmStructHeap = Map.empty,
                vmStructTypes = Map.empty,
                vmSocketHeap = Map.empty,
                vmFFILibs = Map.empty,
                vmNextId = 0,
                vmFunctions = funcs,
                vmCurrentFunc = fname,
                vmCoverage = Just covRef,
                vmLineCov = Just lineCovRef,
                vmBranchCov = Just branchCovRef,
                vmDebugHook = Nothing,
                vmTasks = Map.empty,
                vmReadyQueue = [],
                vmCurrentTask = 0,
                vmNextTaskId = 1
              }
      (result, _) <- runStateT (runExceptT execLoop) initState
      return result

-- | Like 'runProgram' but calls @hook@ at each 'ICovMark' instruction,
-- enabling step-by-step debugging.  The hook may block to pause execution.
runDebugProgram :: (VMState -> IO ()) -> [Bytecode] -> IO (Either VMError Value)
runDebugProgram hook bytecodes = do
  let funcs = Map.fromList [(bytecodeFunction bc, compileFn bc) | bc <- bytecodes]
  case Map.lookup (FuncName "main") funcs of
    Nothing -> return $ Left $ VMUndefinedFunction (FuncName "main")
    Just mainBc -> do
      let initState =
            VMState
              { vmStack = [],
                vmLocals = Map.empty,
                vmIP = 0,
                vmInstrs = cfInstrs mainBc,
                vmStrings = cfStrings mainBc,
                vmCallStack = [],
                vmHeap = Map.empty,
                vmDictHeap = Map.empty,
                vmStructHeap = Map.empty,
                vmStructTypes = Map.empty,
                vmSocketHeap = Map.empty,
                vmFFILibs = Map.empty,
                vmNextId = 0,
                vmFunctions = funcs,
                vmCurrentFunc = FuncName "main",
                vmCoverage = Nothing,
                vmLineCov = Nothing,
                vmBranchCov = Nothing,
                vmDebugHook = Just hook,
                vmTasks = Map.empty,
                vmReadyQueue = [],
                vmCurrentTask = 0,
                vmNextTaskId = 1
              }
      (result, _) <- runStateT (runExceptT execLoop) initState
      return result

-- ---------------------------------------------------------------------------
-- Task scheduler

-- | Snapshot the running task's execution context.
saveTaskCtx :: VM TaskCtx
saveTaskCtx = do
  s <- S.get
  return
    TaskCtx
      { tStack = vmStack s,
        tLocals = vmLocals s,
        tIP = vmIP s,
        tInstrs = vmInstrs s,
        tStrings = vmStrings s,
        tCallStack = vmCallStack s,
        tFunc = vmCurrentFunc s
      }

-- | Install a saved execution context as the running one.
restoreTaskCtx :: TaskCtx -> VM ()
restoreTaskCtx c = modify $ \s ->
  s
    { vmStack = tStack c,
      vmLocals = tLocals c,
      vmIP = tIP c,
      vmInstrs = tInstrs c,
      vmStrings = tStrings c,
      vmCallStack = tCallStack c,
      vmCurrentFunc = tFunc c
    }

-- | Switch to the next ready task; a task must be ready or every task is
-- blocked on an await that can never complete.
scheduleNext :: VM ()
scheduleNext = do
  q <- gets vmReadyQueue
  case q of
    [] -> throwError $ VMRuntimeError "async deadlock: every task is awaiting a task that cannot complete"
    (tid : rest) -> do
      tasks <- gets vmTasks
      case Map.lookup tid tasks of
        Just (TaskReady ctx) -> do
          modify $ \s ->
            s
              { vmReadyQueue = rest,
                vmCurrentTask = tid,
                vmTasks = Map.delete tid (vmTasks s)
              }
          restoreTaskCtx ctx
        _ -> do
          modify $ \s -> s {vmReadyQueue = rest}
          scheduleNext

-- | Finish the running task with the given (string-resolved) result.
-- For the main task the whole program is done; otherwise record the result,
-- wake every task awaiting this one, and switch to the next ready task.
completeTask :: Value -> VM (Maybe Value)
completeTask v = do
  cur <- gets vmCurrentTask
  if cur == 0
    then return (Just v)
    else do
      tasks <- gets vmTasks
      let wake (TaskWaiting w ctx) | w == cur = TaskReady ctx {tStack = v : tStack ctx}
          wake e = e
          woken = [tid | (tid, TaskWaiting w _) <- Map.toList tasks, w == cur]
      modify $ \s ->
        s
          { vmTasks = Map.insert cur (TaskDone v) (Map.map wake tasks),
            vmReadyQueue = vmReadyQueue s ++ woken
          }
      scheduleNext
      return Nothing

-- ---------------------------------------------------------------------------
-- Execution loop

execLoop :: VM Value
execLoop = do
  ip <- gets vmIP
  instrs <- gets vmInstrs
  if ip >= V.length instrs
    then do
      frames <- gets vmCallStack
      case frames of
        [] -> do
          mv <- completeTask VUnit
          maybe execLoop return mv
        _ -> return VUnit
    else do
      let instr = instrs V.! ip
      modify $ \s -> s {vmIP = ip + 1}
      mRet <-
        execInstr instr `catchError` \e ->
          case e of
            VMInContext {} -> throwError e -- already tagged, don't double-wrap
            _ -> do
              func <- gets vmCurrentFunc
              throwError $ VMInContext func ip e
      maybe execLoop return mRet

-- | Execute one instruction.  Returns @Just v@ when the current activation
-- has finished (IRet with an empty call stack), @Nothing@ otherwise.
execInstr :: Instruction -> VM (Maybe Value)
execInstr = \case
  IPush v -> push v >> return Nothing
  IPop -> pop "IPop" >> return Nothing
  IDup -> do
    v <- peek "IDup"
    push v
    return Nothing
  IBinary op -> do
    b <- pop "IBinary (right)"
    a <- pop "IBinary (left)"
    r <- evalBinary op a b
    push r
    return Nothing
  IUnary op -> do
    a <- pop "IUnary"
    r <- evalUnary op a
    push r
    return Nothing
  ILoad var -> do
    locals <- gets vmLocals
    case Map.lookup var locals of
      Nothing -> throwError $ VMUndefinedVar var
      Just v -> push v >> return Nothing
  IStore var -> do
    v <- pop "IStore"
    modify $ \s -> s {vmLocals = Map.insert var v (vmLocals s)}
    return Nothing
  IJump (InstructionPointer ip) -> do
    modify $ \s -> s {vmIP = ip}
    return Nothing
  IJumpTrue (InstructionPointer ip) -> do
    v <- pop "IJumpTrue"
    case v of
      VBool True -> modify $ \s -> s {vmIP = ip}
      VBool False -> return ()
      VInt 0 -> return ()
      VInt _ -> modify $ \s -> s {vmIP = ip}
      other -> throwError $ VMTypeMismatch $ "IJumpTrue: expected bool, got " ++ show other
    return Nothing
  IJumpFalse (InstructionPointer ip) -> do
    v <- pop "IJumpFalse"
    case v of
      VBool False -> modify $ \s -> s {vmIP = ip}
      VBool True -> return ()
      VInt 0 -> modify $ \s -> s {vmIP = ip}
      VInt _ -> return ()
      other -> throwError $ VMTypeMismatch $ "IJumpFalse: expected bool, got " ++ show other
    return Nothing
  ICall (FunctionRef fname) argc -> do
    strings <- gets vmStrings
    funcs <- gets vmFunctions
    -- User-defined and imported functions take priority over builtins so that
    -- `from string import len` can shadow the array heap-builtin `len`.
    case Map.lookup fname funcs of
      Just bc -> do
        covM <- gets vmCoverage
        S.liftIO $ case covM of
          Just ref -> modifyIORef ref (Set.insert fname)
          Nothing -> return ()
        saveFrame
        -- Resolve VStringRef values in the arguments before switching to the
        -- callee's string pool; indices are only valid in the caller's pool
        -- and would be out-of-bounds in the callee's.
        stk <- gets vmStack
        pool <- gets vmStrings
        let (callArgs, rest) = splitAt argc stk
            resolvedArgs = map (resolveStringRef pool) callArgs
        mapM_ (\case VArrayRef aid -> resolveArrayStrings pool aid; _ -> return ()) callArgs
        modify $ \s ->
          s
            { vmStack = resolvedArgs ++ rest,
              vmLocals = Map.empty,
              vmIP = 0,
              vmInstrs = cfInstrs bc,
              vmStrings = cfStrings bc,
              vmCurrentFunc = fname
            }
        return Nothing
      Nothing ->
        if isHeapBuiltin (unFuncName fname)
          then do
            args <- popN argc
            result <- callHeapBuiltin (unFuncName fname) args
            push result
            return Nothing
          else
            if isBuiltin (unFuncName fname)
              then do
                args <- popN argc
                result <- S.liftIO $ callBuiltin (unFuncName fname) strings args
                push result
                return Nothing
              else throwError $ VMUndefinedFunction fname
  IRet -> do
    retVal <- pop "IRet"
    -- Resolve any VStringRef in the return value while still in the callee's
    -- string pool; once we restore the caller's frame the index is invalid.
    calleeStrings <- gets vmStrings
    let resolvedRetVal = resolveStringRef calleeStrings retVal
    frames <- gets vmCallStack
    case frames of
      [] -> completeTask resolvedRetVal
      (frame : rest) -> do
        modify $ \s ->
          s
            { vmLocals = fLocals frame,
              vmIP = fIP frame,
              vmInstrs = fInstrs frame,
              vmStrings = fStrings frame,
              vmCallStack = rest,
              vmStack = resolvedRetVal : vmStack s,
              vmCurrentFunc = fFunc frame
            }
        return Nothing
  INop -> return Nothing
  ICovMark lineNo -> do
    mRef <- gets vmLineCov
    case mRef of
      Nothing -> return ()
      Just ref -> do
        func <- gets vmCurrentFunc
        S.liftIO $ modifyIORef ref (Map.insertWith Set.union func (Set.singleton lineNo))
    hookM <- gets vmDebugHook
    case hookM of
      Nothing -> return ()
      Just hook -> S.get >>= S.liftIO . hook
    return Nothing
  ICovBranch branchId -> do
    mRef <- gets vmBranchCov
    case mRef of
      Nothing -> return Nothing
      Just ref -> do
        v <- peek "ICovBranch"
        let wasTaken = case v of
              VBool b -> b
              VInt 0 -> False
              VInt _ -> True
              _ -> False
        func <- gets vmCurrentFunc
        S.liftIO $ modifyIORef ref (Map.insertWith Set.union (func, branchId) (Set.singleton wasTaken))
        return Nothing
  INewDict -> do
    nid <- gets vmNextId
    modify $ \s ->
      s
        { vmDictHeap = Map.insert nid Map.empty (vmDictHeap s),
          vmNextId = nid + 1
        }
    push (VDictRef nid)
    return Nothing
  INewArray -> do
    nid <- gets vmNextId
    modify $ \s ->
      s
        { vmHeap = Map.insert nid Map.empty (vmHeap s),
          vmNextId = nid + 1
        }
    push (VArrayRef nid)
    return Nothing
  IArrayGet -> do
    idx <- pop "IArrayGet (index)"
    ref <- pop "IArrayGet (ref)"
    case (ref, idx) of
      (VArrayRef aid, VInt i) -> do
        heap <- gets vmHeap
        case Map.lookup aid heap of
          Nothing -> throwError $ VMRuntimeError $ "Array #" ++ show aid ++ " not found"
          Just arr ->
            case Map.lookup (fromIntegral i) arr of
              Nothing -> throwError $ VMOutOfBounds (fromIntegral i) (Map.size arr)
              Just v -> push v >> return Nothing
      (VDictRef did, key) -> do
        dheap <- gets vmDictHeap
        strings <- gets vmStrings
        let resolvedKey = resolveStringRef strings key
        case Map.lookup did dheap of
          Nothing -> throwError $ VMRuntimeError $ "Dict #" ++ show did ++ " not found"
          Just d ->
            case Map.lookup resolvedKey d of
              Nothing -> throwError $ VMRuntimeError $ "Dict key not found: " ++ show resolvedKey
              Just v -> push v >> return Nothing
      _ -> throwError $ VMTypeMismatch $ "IArrayGet: bad types " ++ show ref ++ " " ++ show idx
  IArrayGetOrNew -> do
    idx <- pop "IArrayGetOrNew (index)"
    ref <- pop "IArrayGetOrNew (ref)"
    case (ref, idx) of
      (VArrayRef aid, VInt i) -> do
        heap <- gets vmHeap
        case Map.lookup aid heap of
          Nothing -> throwError $ VMRuntimeError $ "Array #" ++ show aid ++ " not found"
          Just arr -> case Map.lookup (fromIntegral i) arr of
            Just v@(VArrayRef _) -> push v >> return Nothing
            _ -> do
              nid <- gets vmNextId
              modify $ \s ->
                s
                  { vmHeap =
                      Map.insert nid Map.empty $
                        Map.adjust (Map.insert (fromIntegral i) (VArrayRef nid)) aid (vmHeap s),
                    vmNextId = nid + 1
                  }
              push (VArrayRef nid)
              return Nothing
      _ -> throwError $ VMTypeMismatch $ "IArrayGetOrNew: bad types " ++ show ref ++ " " ++ show idx
  IArraySet -> do
    val <- pop "IArraySet (value)"
    idx <- pop "IArraySet (index)"
    ref <- pop "IArraySet (ref)"
    strings <- gets vmStrings
    -- Resolve pool refs before they enter the heap (see 'heapPush').
    let val' = resolveStringRef strings val
    case (ref, idx) of
      (VArrayRef aid, VInt i) -> do
        modify $ \s ->
          s
            { vmHeap = Map.adjust (Map.insert (fromIntegral i) val') aid (vmHeap s)
            }
        return Nothing
      (VDictRef did, key) -> do
        let resolvedKey = resolveStringRef strings key
        modify $ \s ->
          s
            { vmDictHeap = Map.adjust (Map.insert resolvedKey val') did (vmDictHeap s)
            }
        return Nothing
      _ -> throwError $ VMTypeMismatch $ "IArraySet: bad types " ++ show ref ++ " " ++ show idx
  ICast ct -> do
    v <- pop "ICast"
    r <- evalCast ct v
    push r
    return Nothing
  INewStruct tname -> do
    sid <- gets vmNextId
    modify $ \s ->
      s
        { vmStructHeap = Map.insert sid Map.empty (vmStructHeap s),
          vmStructTypes = Map.insert sid tname (vmStructTypes s),
          vmNextId = sid + 1
        }
    push (VStructRef sid)
    return Nothing
  IDynMethodCall methodName argc -> do
    stk <- gets vmStack
    -- The receiver (self, first parameter) is pushed last and sits on top.
    receiver <- peek "IDynMethodCall"
    resolvedName <- case receiver of
      VStructRef sid -> do
        types <- gets vmStructTypes
        case Map.lookup sid types of
          Just tname -> return $ FuncName (unTypeName tname <> "." <> methodName)
          Nothing -> throwError $ VMRuntimeError $ "IDynMethodCall: no type for struct " ++ show sid
      other -> throwError $ VMRuntimeError $ "IDynMethodCall: receiver is not a struct: " ++ show other
    funcs <- gets vmFunctions
    strings <- gets vmStrings
    case Map.lookup resolvedName funcs of
      Just bc -> do
        saveFrame
        let (callArgs, rest) = splitAt argc stk
            resolvedArgs = map (resolveStringRef strings) callArgs
        modify $ \s ->
          s
            { vmStack = resolvedArgs ++ rest,
              vmLocals = Map.empty,
              vmIP = 0,
              vmInstrs = cfInstrs bc,
              vmStrings = cfStrings bc,
              vmCurrentFunc = resolvedName
            }
        return Nothing
      Nothing -> throwError $ VMUndefinedFunction resolvedName
  ISpawn (FunctionRef fname) argc -> do
    funcs <- gets vmFunctions
    case Map.lookup fname funcs of
      Nothing -> throwError $ VMUndefinedFunction fname
      Just bc -> do
        covM <- gets vmCoverage
        S.liftIO $ case covM of
          Just ref -> modifyIORef ref (Set.insert fname)
          Nothing -> return ()
        -- Resolve string refs against the spawner's pool: the new task starts
        -- with its own (the callee's) pool where the indices would be invalid.
        stk <- gets vmStack
        pool <- gets vmStrings
        let (callArgs, rest) = splitAt argc stk
            resolvedArgs = map (resolveStringRef pool) callArgs
        mapM_ (\case VArrayRef aid -> resolveArrayStrings pool aid; _ -> return ()) callArgs
        tid <- gets vmNextTaskId
        let ctx =
              TaskCtx
                { tStack = resolvedArgs,
                  tLocals = Map.empty,
                  tIP = 0,
                  tInstrs = cfInstrs bc,
                  tStrings = cfStrings bc,
                  tCallStack = [],
                  tFunc = fname
                }
        modify $ \s ->
          s
            { vmNextTaskId = tid + 1,
              vmStack = VTask tid : rest,
              vmTasks = Map.insert tid (TaskReady ctx) (vmTasks s),
              vmReadyQueue = vmReadyQueue s ++ [tid]
            }
        return Nothing
  IAwait -> do
    v <- pop "IAwait"
    case v of
      VTask tid -> do
        tasks <- gets vmTasks
        case Map.lookup tid tasks of
          Just (TaskDone val) -> push val >> return Nothing
          Just _ -> do
            -- The awaited task has not finished: park the current task and
            -- hand control to the scheduler.  On wake-up the awaited value is
            -- already on our (saved) stack and the ip is past this IAwait.
            ctx <- saveTaskCtx
            cur <- gets vmCurrentTask
            modify $ \s -> s {vmTasks = Map.insert cur (TaskWaiting tid ctx) (vmTasks s)}
            scheduleNext
            return Nothing
          Nothing -> throwError $ VMRuntimeError $ "IAwait: unknown task #" ++ show tid
      other -> throwError $ VMTypeMismatch $ "IAwait: expected a task handle, got " ++ show other
  IFieldGet (FieldName fname) -> do
    ref <- pop "IFieldGet"
    case ref of
      VStructRef sid -> do
        sh <- gets vmStructHeap
        case Map.lookup sid sh of
          Nothing -> throwError $ VMRuntimeError $ "Struct #" ++ show sid ++ " not found"
          Just fields ->
            case Map.lookup fname fields of
              Nothing -> throwError $ VMRuntimeError $ "Field '" ++ T.unpack fname ++ "' not found in struct #" ++ show sid
              Just v -> push v >> return Nothing
      VErrorVal _ fields ->
        case lookup (FieldName fname) fields of
          Nothing -> throwError $ VMRuntimeError $ "Field '" ++ T.unpack fname ++ "' not found in error value"
          Just v -> push v >> return Nothing
      _ -> throwError $ VMTypeMismatch $ "IFieldGet: expected struct or error ref, got " ++ show ref
  IFieldSet (FieldName fname) -> do
    val <- pop "IFieldSet (value)"
    ref <- pop "IFieldSet (ref)"
    strings <- gets vmStrings
    let resolvedVal = resolveStringRef strings val
    case ref of
      VStructRef sid -> do
        modify $ \s ->
          s {vmStructHeap = Map.adjust (Map.insert fname resolvedVal) sid (vmStructHeap s)}
        return Nothing
      _ -> throwError $ VMTypeMismatch $ "IFieldSet: expected struct ref, got " ++ show ref
  INewError (ErrorName ename) fnames -> do
    -- Field values were pushed in fnames order (first field deepest on stack).
    -- Pop in reverse so we pair each name with the value that was pushed for it.
    vals <- mapM (\fn -> pop ("INewError field " ++ T.unpack (unFieldName fn))) (reverse fnames)
    -- Resolve VStringRef values eagerly so they survive being passed across
    -- function boundaries where the string pool index would be invalid.
    strings <- gets vmStrings
    let resolvedVals = map (resolveStringRef strings) vals
    push (VErrorVal (ErrorName ename) (zip (reverse fnames) resolvedVals))
    return Nothing
  ITryOp -> do
    v <- peek "ITryOp"
    case v of
      VErrorVal {} -> do
        _ <- pop "ITryOp (propagate)"
        frames <- gets vmCallStack
        case frames of
          [] -> return (Just v)
          (frame : rest) -> do
            modify $ \s ->
              s
                { vmLocals = fLocals frame,
                  vmIP = fIP frame,
                  vmInstrs = fInstrs frame,
                  vmStrings = fStrings frame,
                  vmCallStack = rest,
                  vmStack = v : vmStack s,
                  vmCurrentFunc = fFunc frame
                }
            return Nothing
      _ -> return Nothing
  IMustOp -> do
    v <- peek "IMustOp"
    case v of
      VErrorVal (ErrorName ename) _ ->
        throwError $ VMRuntimeError $ "must: unwrapped error `" ++ T.unpack ename ++ "`"
      _ -> return Nothing
  IIsOk -> do
    v <- peek "IIsOk"
    case v of
      VErrorVal {} -> push (VBool False) >> return Nothing
      _ -> push (VBool True) >> return Nothing
  IIsErr (ErrorName ename) -> do
    v <- peek "IIsErr"
    case v of
      VErrorVal (ErrorName n) _ -> push (VBool (n == ename)) >> return Nothing
      _ -> push (VBool False) >> return Nothing
  ILoadFunc fname -> push (VFunction fname) >> return Nothing
  IMakeClosure fname capVars -> do
    locals <- gets vmLocals
    let captured = [(v, Map.findWithDefault VUnit v locals) | v <- capVars]
    push (VClosure fname captured)
    return Nothing
  ICallIndirect argc -> do
    strings <- gets vmStrings
    funcs <- gets vmFunctions
    stk <- gets vmStack
    -- Stack layout: [arg_0 (TOS), ..., arg_n-1, callable, rest...]
    let (callArgs, rest) = splitAt argc stk
        resolvedArgs = map (resolveStringRef strings) callArgs
    (fname, initLocals, remaining) <- case rest of
      (VFunction fn : rest') -> return (fn, Map.empty, rest')
      (VClosure fn caps : rest') -> return (fn, Map.fromList caps, rest')
      _ -> throwError $ VMRuntimeError "ICallIndirect: no function value on stack"
    case Map.lookup fname funcs of
      Just bc -> do
        covM <- gets vmCoverage
        S.liftIO $ case covM of
          Just ref -> modifyIORef ref (Set.insert fname)
          Nothing -> return ()
        saveFrame
        mapM_ (\case VArrayRef aid -> resolveArrayStrings strings aid; _ -> return ()) callArgs
        modify $ \s ->
          s
            { vmStack = resolvedArgs ++ remaining,
              vmLocals = initLocals,
              vmIP = 0,
              vmInstrs = cfInstrs bc,
              vmStrings = cfStrings bc,
              vmCurrentFunc = fname
            }
        return Nothing
      Nothing -> do
        modify $ \s -> s {vmStack = remaining}
        result <-
          if isHeapBuiltin (unFuncName fname)
            then callHeapBuiltin (unFuncName fname) resolvedArgs
            else
              if isBuiltin (unFuncName fname)
                then S.liftIO $ callBuiltin (unFuncName fname) strings resolvedArgs
                else throwError $ VMUndefinedFunction fname
        push result
        return Nothing
  ICallFFI lib sym retTy argc -> do
    args <- popN argc
    strings <- gets vmStrings
    let resolvedArgs = map (resolveStringRef strings) args
    dl <- do
      libs <- gets vmFFILibs
      case Map.lookup lib libs of
        Just d -> return d
        Nothing -> do
          d <- S.liftIO $ dlopen (T.unpack lib) [RTLD_LAZY]
          modify $ \s -> s {vmFFILibs = Map.insert lib d (vmFFILibs s)}
          return d
    funPtr <- S.liftIO $ dlsym dl (T.unpack sym)
    let isFunVal v = case v of VFunction _ -> True; VClosure _ _ -> True; _ -> False
    if not (any isFunVal resolvedArgs)
      then do
        result <- S.liftIO $ callWithFFIArgs funPtr retTy resolvedArgs
        push result
        return Nothing
      else do
        -- Callback args: wrap each function value as a C function pointer
        -- that re-enters the interpreter.  State is threaded through an
        -- IORef so heap mutations made by callbacks survive; the first
        -- callback error is rethrown once the C call returns.
        st <- S.get
        funcs <- gets vmFunctions
        stRef <- S.liftIO $ newIORef st
        errRef <- S.liftIO $ newIORef Nothing
        wrapped <-
          S.liftIO $
            mapM
              ( \v ->
                  if isFunVal v
                    then do
                      fp <- wrapCallback stRef errRef v (callbackArity funcs v)
                      return (VPointer (fromIntegral (ptrToWordPtr (castFunPtrToPtr fp))), Just fp)
                    else return (v, Nothing)
              )
              resolvedArgs
        result <- S.liftIO $ callWithFFIArgs funPtr retTy (map fst wrapped)
        S.liftIO $ mapM_ freeHaskellFunPtr [fp | (_, Just fp) <- wrapped]
        -- Merge callback-visible state back into the running VM (heaps,
        -- allocation counters, task table); keep our own control state.
        st' <- S.liftIO $ readIORef stRef
        modify $ \s ->
          s
            { vmHeap = vmHeap st',
              vmDictHeap = vmDictHeap st',
              vmStructHeap = vmStructHeap st',
              vmStructTypes = vmStructTypes st',
              vmSocketHeap = vmSocketHeap st',
              vmFFILibs = vmFFILibs st',
              vmNextId = vmNextId st',
              vmNextTaskId = vmNextTaskId st',
              vmTasks = vmTasks st'
            }
        mErr <- S.liftIO $ readIORef errRef
        mapM_ throwError mErr
        push result
        return Nothing

-- ---------------------------------------------------------------------------
-- Heap-aware builtins (array.len, array.push, array.pop, sys.exit)

callHeapBuiltin :: Text -> [Value] -> VM Value
callHeapBuiltin name args = case (name, args) of
  -- len / array.len : [int] -> int
  (n, [VArrayRef aid]) | n `elem` ["len", "array.len"] -> do
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> throwError $ VMRuntimeError $ "Array #" ++ show aid ++ " not found"
      Just arr -> return $ VInt (fromIntegral (Map.size arr))

  -- push / array.push : [int] -> int -> void  (modifies in place)
  (n, [VArrayRef aid, val]) | n `elem` ["push", "array.push"] -> do
    strings <- gets vmStrings
    let val' = resolveStringRef strings val
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> throwError $ VMRuntimeError $ "Array #" ++ show aid ++ " not found"
      Just arr -> do
        let nextIdx = if Map.null arr then 0 else fst (Map.findMax arr) + 1
        modify $ \s -> s {vmHeap = Map.adjust (Map.insert nextIdx val') aid (vmHeap s)}
        return VUnit

  -- pop / array.pop : [int] -> int  (removes last element)
  (n, [VArrayRef aid]) | n `elem` ["pop", "array.pop"] -> do
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> throwError $ VMRuntimeError $ "Array #" ++ show aid ++ " not found"
      Just arr | Map.null arr -> return VUnit
      Just arr -> do
        let (maxIdx, val) = Map.findMax arr
        modify $ \s -> s {vmHeap = Map.adjust (Map.delete maxIdx) aid (vmHeap s)}
        return val

  -- sys.exit : int -> void
  ("sys.exit", [VInt code]) ->
    S.liftIO $ exitWith (if code == 0 then ExitSuccess else ExitFailure (fromIntegral code))
  -- sys.args : () -> [str]
  ("sys.args", []) -> do
    sysArgs <- S.liftIO getArgs
    aid <- allocArray
    mapM_ (heapPush aid . VString . T.pack) sysArgs
    return $ VArrayRef aid

  -- string.split : str -> str -> [str]
  ("string.split", [s, delim]) -> do
    strings <- gets vmStrings
    let txt = resolveValue strings s
        sep = resolveValue strings delim
        parts = T.splitOn sep txt
    aid <- allocArray
    mapM_ (heapPush aid . VString) parts
    return $ VArrayRef aid

  -- string.join : [str] -> str -> str
  ("string.join", [VArrayRef aid, delim]) -> do
    strings <- gets vmStrings
    let sep = resolveValue strings delim
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> return $ VString ""
      Just arr -> do
        let elems = map snd (Map.toAscList arr)
            texts = map (resolveValue strings) elems
        return $ VString (T.intercalate sep texts)
  -- buf.new : () -> [str]
  ("buf.new", []) -> do
    VArrayRef <$> allocArray

  -- buf.write : [str] -> str -> void
  ("buf.write", [VArrayRef aid, s]) -> do
    heapPush aid s
    return VUnit

  -- buf.writeln : [str] -> str -> void
  ("buf.writeln", [VArrayRef aid, s]) -> do
    strings <- gets vmStrings
    let txt = resolveValue strings s
    heapPush aid (VString txt)
    heapPush aid (VString "\n")
    return VUnit

  -- buf.to_str : [str] -> str
  ("buf.to_str", [VArrayRef aid]) -> do
    strings <- gets vmStrings
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> return $ VString ""
      Just arr -> do
        let elems = map snd (Map.toAscList arr)
            texts = map (resolveValue strings) elems
        return $ VString (T.concat texts)

  -- buf.len : [str] -> int   (total character count)
  ("buf.len", [VArrayRef aid]) -> do
    strings <- gets vmStrings
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> return $ VInt 0
      Just arr -> do
        let elems = map snd (Map.toAscList arr)
            texts = map (resolveValue strings) elems
        return $ VInt (fromIntegral (sum (map T.length texts)))

  -- buf.clear : [str] -> void
  ("buf.clear", [VArrayRef aid]) -> do
    modify $ \s -> s {vmHeap = Map.insert aid Map.empty (vmHeap s)}
    return VUnit

  -- buf.flush : [str] -> int -> int  (join, write to fd, clear, return bytes written)
  ("buf.flush", [VArrayRef aid, VInt fd]) -> do
    strings <- gets vmStrings
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> return $ VInt 0
      Just arr -> do
        let elems = map snd (Map.toAscList arr)
            combined = T.concat (map (resolveValue strings) elems)
        r <- S.liftIO (try (fdWrite (Fd (fromIntegral fd)) (T.unpack combined)) :: IO (Either SomeException ByteCount))
        modify $ \s -> s {vmHeap = Map.insert aid Map.empty (vmHeap s)}
        return $ VInt (either (const (-1)) fromIntegral r)

  -- file.lines : str -> [str]
  ("file.lines", [path]) -> do
    strings <- gets vmStrings
    let p = T.unpack (resolveValue strings path)
    r <- S.liftIO (try (TIO.readFile p) :: IO (Either SomeException T.Text))
    aid <- allocArray
    case r of
      Left _ -> return ()
      Right txt -> mapM_ (heapPush aid . VString) (T.lines txt)
    return $ VArrayRef aid
  -- dict.has : dict(K,V) -> K -> bool
  ("dict.has", [VDictRef did, key]) -> do
    strings <- gets vmStrings
    let resolvedKey = resolveStringRef strings key
    dheap <- gets vmDictHeap
    case Map.lookup did dheap of
      Nothing -> return $ VBool False
      Just d -> return $ VBool (Map.member resolvedKey d)

  -- dict.len : dict(K,V) -> int
  ("dict.len", [VDictRef did]) -> do
    dheap <- gets vmDictHeap
    case Map.lookup did dheap of
      Nothing -> return $ VInt 0
      Just d -> return $ VInt (fromIntegral (Map.size d))

  -- dict.delete : dict(K,V) -> K -> void
  ("dict.delete", [VDictRef did, key]) -> do
    strings <- gets vmStrings
    let resolvedKey = resolveStringRef strings key
    modify $ \s ->
      s {vmDictHeap = Map.adjust (Map.delete resolvedKey) did (vmDictHeap s)}
    return VUnit

  -- dict.keys : dict(str,V) -> [str]
  ("dict.keys", [VDictRef did]) -> do
    dheap <- gets vmDictHeap
    aid <- allocArray
    case Map.lookup did dheap of
      Nothing -> return ()
      Just d -> mapM_ (heapPush aid) (Map.keys d)
    return $ VArrayRef aid

  -- dict.values : dict(K,V) -> [V]
  ("dict.values", [VDictRef did]) -> do
    dheap <- gets vmDictHeap
    aid <- allocArray
    case Map.lookup did dheap of
      Nothing -> return ()
      Just d -> mapM_ (heapPush aid) (Map.elems d)
    return $ VArrayRef aid
  -- json.encode : value -> str
  ("json.encode", [val]) -> do
    st <- S.get
    strings <- gets vmStrings
    let jv = quantToAeson st strings val
    return $ VString (TE.decodeUtf8 (BL.toStrict (Aeson.encode jv)))
  -- json.decode_str : str -> str -> str
  ("json.decode_str", [s, key]) -> do
    strings <- gets vmStrings
    let jsStr = resolveStr' strings s
        keyStr = resolveStr' strings key
    obj <- parseJsonObj jsStr
    case AesonKM.lookup (AesonKey.fromText keyStr) obj of
      Just (Aeson.String t) -> return (VString t)
      Just (Aeson.Number n) -> return (VString (T.pack (show (floor n :: Int))))
      Just (Aeson.Bool b) -> return (VString (if b then "true" else "false"))
      Just Aeson.Null -> return (VString "null")
      Just v -> return (VString (TE.decodeUtf8 (BL.toStrict (Aeson.encode v))))
      Nothing -> throwError $ VMRuntimeError $ "json.decode_str: key not found: " ++ T.unpack keyStr
  -- json.decode_int : str -> str -> int
  ("json.decode_int", [s, key]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    let keyStr = resolveStr' strings key
    case AesonKM.lookup (AesonKey.fromText keyStr) obj of
      Just (Aeson.Number n) -> case Scientific.toBoundedInteger n :: Maybe Int of
        Just i -> return (VInt (fromIntegral i))
        Nothing -> return (VInt (floor (Scientific.toRealFloat n :: Double)))
      Just _ -> throwError $ VMRuntimeError $ "json.decode_int: field '" ++ T.unpack keyStr ++ "' is not a number"
      Nothing -> throwError $ VMRuntimeError $ "json.decode_int: key not found: " ++ T.unpack keyStr
  -- json.decode_float : str -> str -> float
  ("json.decode_float", [s, key]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    let keyStr = resolveStr' strings key
    case AesonKM.lookup (AesonKey.fromText keyStr) obj of
      Just (Aeson.Number n) -> return (VFloat (Scientific.toRealFloat n))
      Just _ -> throwError $ VMRuntimeError $ "json.decode_float: field '" ++ T.unpack keyStr ++ "' is not a number"
      Nothing -> throwError $ VMRuntimeError $ "json.decode_float: key not found: " ++ T.unpack keyStr
  -- json.decode_bool : str -> str -> bool
  ("json.decode_bool", [s, key]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    let keyStr = resolveStr' strings key
    case AesonKM.lookup (AesonKey.fromText keyStr) obj of
      Just (Aeson.Bool b) -> return (VBool b)
      Just _ -> throwError $ VMRuntimeError $ "json.decode_bool: field '" ++ T.unpack keyStr ++ "' is not a boolean"
      Nothing -> throwError $ VMRuntimeError $ "json.decode_bool: key not found: " ++ T.unpack keyStr
  -- json.has : str -> str -> bool
  ("json.has", [s, key]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    let keyStr = resolveStr' strings key
    return $ VBool $ AesonKM.member (AesonKey.fromText keyStr) obj
  -- json.is_null : str -> str -> bool
  ("json.is_null", [s, key]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    let keyStr = resolveStr' strings key
    return $ VBool $ AesonKM.lookup (AesonKey.fromText keyStr) obj == Just Aeson.Null
  -- json.keys : str -> [str]
  ("json.keys", [s]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    aid <- allocArray
    mapM_ (heapPush aid . VString . AesonKey.toText) (AesonKM.keys obj)
    return (VArrayRef aid)
  -- json.parse : str -> dict(str, str)  -- all values coerced to string
  ("json.parse", [s]) -> do
    strings <- gets vmStrings
    obj <- parseJsonObj (resolveStr' strings s)
    did <- allocDict
    mapM_ (\(k, v) -> dictInsert did (VString (AesonKey.toText k)) (aesonToStr v)) (AesonKM.toList obj)
    return (VDictRef did)
  -- socket.connect : str -> int -> int  (TCP client; returns socket id or -1)
  ("socket.connect", [host, VInt port]) -> do
    strings <- gets vmStrings
    let h = T.unpack (resolveStr' strings host)
    r <- S.liftIO $ try $ do
      infos <-
        NS.getAddrInfo
          (Just NS.defaultHints {NS.addrSocketType = NS.Stream})
          (Just h)
          (Just (show port))
      case infos of
        [] -> return Nothing
        (ai : _) -> do
          sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
          NS.connect sock (NS.addrAddress ai)
          return (Just sock)
    case (r :: Either SomeException (Maybe NS.Socket)) of
      Left _ -> return (VInt (-1))
      Right Nothing -> return (VInt (-1))
      Right (Just sock) -> VInt . fromIntegral <$> allocSocket sock

  -- socket.listen : int -> int -> int  (TCP server; returns socket id or -1)
  ("socket.listen", [VInt port, VInt backlog]) -> do
    r <- S.liftIO $ try $ do
      infos <-
        NS.getAddrInfo
          (Just NS.defaultHints {NS.addrSocketType = NS.Stream, NS.addrFlags = [NS.AI_PASSIVE]})
          Nothing
          (Just (show port))
      case infos of
        [] -> ioError $ userError "socket.listen: getAddrInfo returned no results"
        (ai : _) -> do
          sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
          NS.setSocketOption sock NS.ReuseAddr 1
          NS.bind sock (NS.addrAddress ai)
          NS.listen sock (fromIntegral backlog)
          return sock
    case (r :: Either SomeException NS.Socket) of
      Left _ -> return (VInt (-1))
      Right sock -> VInt . fromIntegral <$> allocSocket sock

  -- socket.accept : int -> int  (blocks until a client connects; returns client socket id or -1)
  ("socket.accept", [VInt sid]) -> do
    sockHeap <- gets vmSocketHeap
    case Map.lookup (fromIntegral sid) sockHeap of
      Nothing -> return (VInt (-1))
      Just srv -> do
        r <- S.liftIO $ try $ NS.accept srv
        case (r :: Either SomeException (NS.Socket, NS.SockAddr)) of
          Left _ -> return (VInt (-1))
          Right (conn, _) -> VInt . fromIntegral <$> allocSocket conn

  -- socket.send : int -> str -> int  (returns bytes sent or -1)
  ("socket.send", [VInt sid, val]) -> do
    strings <- gets vmStrings
    let bs = TE.encodeUtf8 (resolveStr' strings val)
    sockHeap <- gets vmSocketHeap
    case Map.lookup (fromIntegral sid) sockHeap of
      Nothing -> return (VInt (-1))
      Just sock -> do
        r <- S.liftIO $ try $ NSB.send sock bs
        case (r :: Either SomeException Int) of
          Left _ -> return (VInt (-1))
          Right n -> return (VInt (fromIntegral n))

  -- socket.recv : int -> int -> str  (returns received data or "" on close/error)
  ("socket.recv", [VInt sid, VInt n]) -> do
    sockHeap <- gets vmSocketHeap
    case Map.lookup (fromIntegral sid) sockHeap of
      Nothing -> return (VString "")
      Just sock -> do
        r <- S.liftIO $ try $ NSB.recv sock (fromIntegral n)
        case (r :: Either SomeException BS.ByteString) of
          Left _ -> return (VString "")
          Right bs -> return (VString (TE.decodeUtf8Lenient bs))

  -- socket.close : int -> bool
  ("socket.close", [VInt sid]) -> do
    sockHeap <- gets vmSocketHeap
    case Map.lookup (fromIntegral sid) sockHeap of
      Nothing -> return (VBool False)
      Just sock -> do
        r <- S.liftIO $ try $ NS.close sock
        case (r :: Either SomeException ()) of
          Left _ -> return (VBool False)
          Right _ -> do
            modify $ \s -> s {vmSocketHeap = Map.delete (fromIntegral sid) (vmSocketHeap s)}
            return (VBool True)

  -- socket.peer_addr : int -> str  (returns "host:port" of the remote end)
  ("socket.peer_addr", [VInt sid]) -> do
    sockHeap <- gets vmSocketHeap
    case Map.lookup (fromIntegral sid) sockHeap of
      Nothing -> return (VString "")
      Just sock -> do
        r <- S.liftIO $ try $ NS.getPeerName sock
        case (r :: Either SomeException NS.SockAddr) of
          Left _ -> return (VString "")
          Right addr -> return (VString (T.pack (show addr)))
  -- string.format : str -> [any] -> str  (variadic args packed into array)
  ("string.format", [fmt, VArrayRef aid]) -> do
    strings <- gets vmStrings
    let fmtStr = resolveValue strings fmt
    heap <- gets vmHeap
    let vals = case Map.lookup aid heap of
          Nothing -> []
          Just arr -> map snd (Map.toAscList arr)
    return $ VString (applyFormat fmtStr strings vals)

  -- regex.match : str -> str -> bool
  ("regex.match", [pat, txt]) -> do
    strings <- gets vmStrings
    let p = T.unpack (resolveValue strings pat)
        t = T.unpack (resolveValue strings txt)
    return $ VBool (t =~ p :: Bool)

  -- regex.find : str -> str -> str  (first match, or "" if none)
  ("regex.find", [pat, txt]) -> do
    strings <- gets vmStrings
    let p = T.unpack (resolveValue strings pat)
        t = T.unpack (resolveValue strings txt)
    return $ VString (T.pack (t =~ p :: String))

  -- regex.find_all : str -> str -> [str]
  ("regex.find_all", [pat, txt]) -> do
    strings <- gets vmStrings
    let p = T.unpack (resolveValue strings pat)
        t = T.unpack (resolveValue strings txt)
        matches = getAllTextMatches (t =~ p) :: [String]
    aid <- allocArray
    mapM_ (heapPush aid . VString . T.pack) matches
    return $ VArrayRef aid

  -- regex.replace : str -> str -> str -> str  (replace all non-overlapping matches)
  ("regex.replace", [pat, txt, repl]) -> do
    strings <- gets vmStrings
    let p = T.unpack (resolveValue strings pat)
        t = T.unpack (resolveValue strings txt)
        r = T.unpack (resolveValue strings repl)
    return $ VString (T.pack (regexReplaceAll p r t))

  -- regex.split : str -> str -> [str]
  ("regex.split", [pat, txt]) -> do
    strings <- gets vmStrings
    let p = T.unpack (resolveValue strings pat)
        t = T.unpack (resolveValue strings txt)
    aid <- allocArray
    mapM_ (heapPush aid . VString . T.pack) (regexSplit p t)
    return $ VArrayRef aid
  -- ptr.null : () -> ptr
  ("ptr.null", []) -> return $ VPointer 0
  -- ptr.is_null : ptr -> bool
  ("ptr.is_null", [VPointer addr]) -> return $ VBool (addr == 0)
  -- ptr.to_int : ptr -> int
  ("ptr.to_int", [VPointer addr]) -> return $ VInt (fromIntegral addr)
  -- ptr.from_int : int -> ptr
  ("ptr.from_int", [VInt n]) -> return $ VPointer (fromIntegral n)
  -- ptr.add : ptr -> int -> ptr  (byte offset)
  ("ptr.add", [VPointer addr, VInt off]) ->
    return $ VPointer (fromIntegral (fromIntegral addr + off))
  -- ptr.read_int32 : ptr -> int
  ("ptr.read_int32", [VPointer addr]) -> do
    n <- S.liftIO (peekByteOff (wordPtrToPtr (fromIntegral addr)) 0 :: IO Int32)
    return $ VInt (fromIntegral n)
  -- ptr.write_int32 : ptr -> int -> void
  ("ptr.write_int32", [VPointer addr, VInt v]) -> do
    S.liftIO (pokeByteOff (wordPtrToPtr (fromIntegral addr)) 0 (fromIntegral v :: Int32))
    return VUnit
  -- ptr.read_int64 : ptr -> int
  ("ptr.read_int64", [VPointer addr]) -> do
    n <- S.liftIO (peekByteOff (wordPtrToPtr (fromIntegral addr)) 0 :: IO Int64)
    return $ VInt (fromIntegral n)
  -- ptr.write_int64 : ptr -> int -> void
  ("ptr.write_int64", [VPointer addr, VInt v]) -> do
    S.liftIO (pokeByteOff (wordPtrToPtr (fromIntegral addr)) 0 (fromIntegral v :: Int64))
    return VUnit
  -- ptr.read_float64 : ptr -> float
  ("ptr.read_float64", [VPointer addr]) -> do
    d <- S.liftIO (peekByteOff (wordPtrToPtr (fromIntegral addr)) 0 :: IO Double)
    return $ VFloat d
  -- ptr.write_float64 : ptr -> float -> void
  ("ptr.write_float64", [VPointer addr, VFloat d]) -> do
    S.liftIO (pokeByteOff (wordPtrToPtr (fromIntegral addr)) 0 d)
    return VUnit
  -- ptr.read_str : ptr -> str  (NUL-terminated C string; "" for NULL)
  ("ptr.read_str", [VPointer addr]) ->
    if addr == 0
      then return (VString "")
      else S.liftIO (VString . T.pack <$> peekCString (wordPtrToPtr (fromIntegral addr)))
  -- assert : bool -> str -> void
  ("assert", rawArgs@[_, _]) -> do
    strings <- gets vmStrings
    case map (resolveStringRef strings) rawArgs of
      [VBool True, _] -> return VUnit
      [VBool False, VString msg] ->
        throwError $ VMRuntimeError $ "assertion failed: " ++ T.unpack msg
      _ -> throwError $ VMRuntimeError "assert: expected (bool, str)"
  -- assert_eq : any -> any -> void
  ("assert_eq", rawArgs@[_, _]) -> do
    strings <- gets vmStrings
    case map (resolveStringRef strings) rawArgs of
      [a, b] ->
        if a == b
          then return VUnit
          else
            throwError $
              VMRuntimeError $
                "assertion failed: expected " ++ showVal a ++ ", got " ++ showVal b
      _ -> throwError $ VMRuntimeError "assert_eq: expected (any, any)"
  -- fail : str -> void
  ("fail", rawArgs@[_]) -> do
    strings <- gets vmStrings
    case map (resolveStringRef strings) rawArgs of
      [VString msg] -> throwError $ VMRuntimeError $ T.unpack msg
      _ -> throwError $ VMRuntimeError "fail: expected (str)"
  _ ->
    throwError $
      VMRuntimeError $
        "Heap builtin '" ++ show name ++ "' called with bad args: " ++ show args

-- ---------------------------------------------------------------------------
-- Value display (used by assert_eq)

showVal :: Value -> String
showVal (VInt n) = show n
showVal (VFloat f) = show f
showVal (VBool True) = "true"
showVal (VBool False) = "false"
showVal (VString s) = T.unpack s
showVal (VStringRef _) = "<str>"
showVal VUnit = "void"
showVal (VPointer p) = "0x" ++ show p
showVal (VArrayRef _) = "[...]"
showVal (VDictRef _) = "{...}"
showVal (VStructRef _) = "struct(...)"
showVal (VFunction f) = T.unpack (unFuncName f)
showVal (VClosure f _) = "<closure:" ++ T.unpack (unFuncName f) ++ ">"
showVal (VErrorVal e _) = T.unpack (unErrorName e)
showVal (VTask tid) = "<task:" ++ show tid ++ ">"

-- ---------------------------------------------------------------------------
-- Regex helpers

regexReplaceAll :: String -> String -> String -> String
regexReplaceAll pat repl = go
  where
    go "" = ""
    go s = case s =~ pat :: (String, String, String) of
      (_, "", _) -> s
      (pre, _, post) -> pre ++ repl ++ go post

regexSplit :: String -> String -> [String]
regexSplit pat = go
  where
    go "" = [""]
    go s = case s =~ pat :: (String, String, String) of
      (_, "", _) -> [s]
      (pre, _, post) -> pre : go post

-- ---------------------------------------------------------------------------
-- JSON helpers

resolveStr' :: [Text] -> Value -> Text
resolveStr' strings (VStringRef i)
  | i < length strings = strings !! i
resolveStr' _ (VString t) = t
resolveStr' _ v = T.pack (show v)

parseJsonObj :: Text -> VM (AesonKM.KeyMap Aeson.Value)
parseJsonObj s =
  case Aeson.decode (BL.fromStrict (TE.encodeUtf8 s)) of
    Just (Aeson.Object obj) -> return obj
    Just _ -> throwError $ VMRuntimeError "json: expected a JSON object"
    Nothing -> throwError $ VMRuntimeError $ "json: invalid JSON: " ++ T.unpack s

aesonToStr :: Aeson.Value -> Value
aesonToStr (Aeson.String t) = VString t
aesonToStr (Aeson.Number n) = case Scientific.toBoundedInteger n :: Maybe Int of
  Just i -> VString (T.pack (show i))
  Nothing -> VString (T.pack (show (Scientific.toRealFloat n :: Double)))
aesonToStr (Aeson.Bool True) = VString "true"
aesonToStr (Aeson.Bool False) = VString "false"
aesonToStr Aeson.Null = VString "null"
aesonToStr v = VString (TE.decodeUtf8 (BL.toStrict (Aeson.encode v)))

quantToAeson :: VMState -> [Text] -> Value -> Aeson.Value
quantToAeson st strings val = case val of
  VInt n -> Aeson.toJSON n
  VFloat f -> Aeson.toJSON f
  VBool b -> Aeson.Bool b
  VUnit -> Aeson.Null
  VString t -> Aeson.String t
  VStringRef i
    | i < length strings -> Aeson.String (strings !! i)
    | otherwise -> Aeson.Null
  VArrayRef aid -> case Map.lookup aid (vmHeap st) of
    Nothing -> Aeson.Array V.empty
    Just m -> Aeson.Array (V.fromList (map (quantToAeson st strings) (Map.elems m)))
  VDictRef did -> case Map.lookup did (vmDictHeap st) of
    Nothing -> Aeson.Object AesonKM.empty
    Just d ->
      Aeson.Object $
        AesonKM.fromList
          [ (AesonKey.fromText (resolveStr' strings k), quantToAeson st strings v)
            | (k, v) <- Map.toList d
          ]
  VStructRef sid -> case Map.lookup sid (vmStructHeap st) of
    Nothing -> Aeson.Object AesonKM.empty
    Just m ->
      Aeson.Object $
        AesonKM.fromList
          [(AesonKey.fromText f, quantToAeson st strings v) | (f, v) <- Map.toList m]
  _ -> Aeson.String (T.pack (show val))

allocSocket :: NS.Socket -> VM Int
allocSocket sock = do
  sid <- gets vmNextId
  modify $ \s -> s {vmSocketHeap = Map.insert sid sock (vmSocketHeap s), vmNextId = sid + 1}
  return sid

allocDict :: VM Int
allocDict = do
  nid <- gets vmNextId
  modify $ \s -> s {vmDictHeap = Map.insert nid Map.empty (vmDictHeap s), vmNextId = nid + 1}
  return nid

dictInsert :: Int -> Value -> Value -> VM ()
dictInsert did k v =
  modify $ \s -> s {vmDictHeap = Map.adjust (Map.insert k v) did (vmDictHeap s)}

-- ---------------------------------------------------------------------------
-- Helpers

-- ---------------------------------------------------------------------------
-- Heap helpers

allocArray :: VM Int
allocArray = do
  aid <- gets vmNextId
  modify $ \s ->
    s
      { vmHeap = Map.insert aid Map.empty (vmHeap s),
        vmNextId = aid + 1
      }
  return aid

heapPush :: Int -> Value -> VM ()
heapPush aid val = do
  -- Resolve string-pool references eagerly: a VStringRef stored in the heap
  -- would otherwise be re-resolved against a different function's pool once
  -- the array crosses a call boundary.  (The native backend resolves pool
  -- refs to immortal pointers at push time, so this keeps the two in step.)
  strings <- gets vmStrings
  let val' = resolveStringRef strings val
  heap <- gets vmHeap
  case Map.lookup aid heap of
    Nothing -> return ()
    Just arr -> do
      let nextIdx = if Map.null arr then 0 else fst (Map.findMax arr) + 1
      modify $ \s -> s {vmHeap = Map.adjust (Map.insert nextIdx val') aid (vmHeap s)}

resolveStringRef :: [Text] -> Value -> Value
resolveStringRef strings (VStringRef i)
  | i < length strings = VString (strings !! i)
resolveStringRef _ v = v

-- | Deep-resolve VStringRef values inside an array on the heap.
resolveArrayStrings :: [Text] -> Int -> VM ()
resolveArrayStrings strings aid = do
  heap <- gets vmHeap
  case Map.lookup aid heap of
    Nothing -> return ()
    Just elems -> do
      let resolved = Map.map (resolveStringRef strings) elems
      modify $ \s -> s {vmHeap = Map.insert aid resolved (vmHeap s)}

resolveValue :: [Text] -> Value -> Text
resolveValue strings (VStringRef i) = strings !! i
resolveValue _ (VString t) = t
resolveValue _ (VInt n) = T.pack (show n)
resolveValue _ (VFloat f) = T.pack (show f)
resolveValue _ (VBool b) = if b then "true" else "false"
resolveValue _ _ = ""

-- ---------------------------------------------------------------------------
-- FFI callbacks

-- | The supported C callback shape: two opaque pointers in, int out
-- (the qsort/bsearch comparator signature).
-- Callback wrappers by arity (all pointer args, int-class return).  A C
-- callback expecting `void` simply ignores the returned rax, so a single
-- CInt-returning family covers both int- and void-returning callbacks.
type CFn0 = IO CInt

type CFn1 = Ptr () -> IO CInt

type CFn2 = Ptr () -> Ptr () -> IO CInt

type CFn3 = Ptr () -> Ptr () -> Ptr () -> IO CInt

type CFn4 = Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO CInt

foreign import ccall "wrapper" mkCb0 :: CFn0 -> IO (FunPtr CFn0)

foreign import ccall "wrapper" mkCb1 :: CFn1 -> IO (FunPtr CFn1)

foreign import ccall "wrapper" mkCb2 :: CFn2 -> IO (FunPtr CFn2)

foreign import ccall "wrapper" mkCb3 :: CFn3 -> IO (FunPtr CFn3)

foreign import ccall "wrapper" mkCb4 :: CFn4 -> IO (FunPtr CFn4)

-- | Run a Quant function value to completion in a state derived from @st@:
-- fresh stack/locals/task machinery, shared heaps and function table.
invokeQuantValue :: VMState -> Value -> [Value] -> IO (Either VMError (Value, VMState))
invokeQuantValue st fnVal cbArgs = do
  let (fname, capturedLocals) = case fnVal of
        VClosure fn caps -> (fn, Map.fromList caps)
        VFunction fn -> (fn, Map.empty)
        _ -> (FuncName "", Map.empty)
  case Map.lookup fname (vmFunctions st) of
    Nothing -> return $ Left $ VMUndefinedFunction fname
    Just bc -> do
      let callSt =
            st
              { vmStack = cbArgs,
                vmLocals = capturedLocals,
                vmIP = 0,
                vmInstrs = cfInstrs bc,
                vmStrings = cfStrings bc,
                vmCallStack = [],
                vmCurrentFunc = fname,
                -- The callback body is its own root task.
                vmCurrentTask = 0,
                vmReadyQueue = []
              }
      (result, st') <- runStateT (runExceptT execLoop) callSt
      return $ case result of
        Left err -> Left err
        Right v -> Right (v, st')

-- | Number of leading parameters a function takes, read from its prologue
-- (codegen emits one @IStore@ per parameter before the body).  Used to pick
-- the right callback wrapper arity for a function passed to C.
callbackArity :: Map FuncName CompiledFn -> Value -> Int
callbackArity funcs v =
  case Map.lookup fname funcs of
    Just cf -> length (takeWhile isStore (bytecodeInstructions (cfBytecode cf)))
    Nothing -> 2
  where
    fname = case v of
      VFunction fn -> fn
      VClosure fn _ -> fn
      _ -> FuncName ""
    isStore (IStore _) = True
    isStore _ = False

-- | Wrap a Quant function value as a C function pointer of the given arity
-- (all pointer args, int-class return).  The callback re-enters the
-- interpreter against the shared state ref; heap mutations persist across
-- invocations.  The first callback error is captured in @errRef@ (a C caller
-- cannot unwind a Haskell exception) and rethrown by the FFI call site.
wrapCallback :: IORef VMState -> IORef (Maybe VMError) -> Value -> Int -> IO (FunPtr ())
wrapCallback stRef errRef fnVal arity =
  case arity of
    0 -> castFunPtr <$> mkCb0 (run [])
    1 -> castFunPtr <$> mkCb1 (\a -> run [a])
    3 -> castFunPtr <$> mkCb3 (\a b c -> run [a, b, c])
    4 -> castFunPtr <$> mkCb4 (\a b c d -> run [a, b, c, d])
    _ -> castFunPtr <$> mkCb2 (\a b -> run [a, b])
  where
    toVal p = VPointer (fromIntegral (ptrToWordPtr p))
    run ptrs = do
      st <- readIORef stRef
      r <- invokeQuantValue st fnVal (map toVal ptrs)
      case r of
        Left err -> do
          modifyIORef errRef (\case Nothing -> Just err; je -> je)
          return 0
        Right (v, st') -> do
          writeIORef stRef st'
          return $ case v of
            VInt n -> fromIntegral n
            VBool True -> 1
            _ -> 0

-- ---------------------------------------------------------------------------
-- FFI dispatch

-- | Call a C function through libffi.  String args use argString (allocs a
-- CString internally; minor leak per call, acceptable for scripting use).
-- String return values are peeked immediately and converted to Text.
callWithFFIArgs :: FunPtr () -> CRetType -> [Value] -> IO Value
callWithFFIArgs funPtr retTy vals = go vals []
  where
    go [] ffArgs = dispatch (reverse ffArgs)
    go (VInt n : rest) ffArgs = go rest (argInt64 (fromIntegral n) : ffArgs)
    go (VFloat f : rest) ffArgs = go rest (argCDouble (realToFrac f) : ffArgs)
    go (VBool b : rest) ffArgs = go rest (argWord32 (if b then 1 else 0) : ffArgs)
    go (VString s : rest) ffArgs = go rest (argString (T.unpack s) : ffArgs)
    go (VPointer addr : rest) ffArgs =
      go rest (argPtr (wordPtrToPtr (fromIntegral addr) :: Ptr ()) : ffArgs)
    go (_ : rest) ffArgs = go rest ffArgs

    dispatch ffArgs = case retTy of
      CRetVoid -> callFFI funPtr retVoid ffArgs >> return VUnit
      CRetInt -> VInt . fromIntegral <$> callFFI funPtr retInt64 ffArgs
      CRetFloat -> VFloat . realToFrac <$> callFFI funPtr retCDouble ffArgs
      CRetBool -> VBool . (/= 0) <$> callFFI funPtr retWord32 ffArgs
      CRetStr -> VString . T.pack <$> callFFI funPtr retString ffArgs
      CRetPtr -> do
        p <- callFFI funPtr (retPtr retVoid) ffArgs
        return $ VPointer (fromIntegral (ptrToWordPtr p))

-- ---------------------------------------------------------------------------
-- Stack helpers

push :: Value -> VM ()
push v = modify $ \s -> s {vmStack = v : vmStack s}

pop :: String -> VM Value
pop ctx = do
  stk <- gets vmStack
  case stk of
    [] -> throwError $ VMStackUnderflow ctx
    (v : vs) -> do
      modify $ \s -> s {vmStack = vs}
      return v

peek :: String -> VM Value
peek ctx = do
  stk <- gets vmStack
  case stk of
    [] -> throwError $ VMStackUnderflow ctx
    (v : _) -> return v

popN :: Int -> VM [Value]
popN 0 = return []
popN n = do
  v <- pop "popN"
  vs <- popN (n - 1)
  return (v : vs)

saveFrame :: VM ()
saveFrame = do
  s <- S.get
  let frame =
        Frame
          { fLocals = vmLocals s,
            fIP = vmIP s,
            fInstrs = vmInstrs s,
            fStrings = vmStrings s,
            fFunc = vmCurrentFunc s
          }
  modify $ \st -> st {vmCallStack = frame : vmCallStack st}

-- ---------------------------------------------------------------------------
-- Binary operations

evalBinary :: BinaryOp -> Value -> Value -> VM Value
evalBinary BOpAdd (VInt a) (VInt b) = return $ VInt (a + b)
evalBinary BOpAdd (VFloat a) (VFloat b) = return $ VFloat (a + b)
evalBinary BOpAdd (VInt a) (VFloat b) = return $ VFloat (fromIntegral a + b)
evalBinary BOpAdd (VFloat a) (VInt b) = return $ VFloat (a + fromIntegral b)
evalBinary BOpSub (VInt a) (VInt b) = return $ VInt (a - b)
evalBinary BOpSub (VFloat a) (VFloat b) = return $ VFloat (a - b)
evalBinary BOpSub (VInt a) (VFloat b) = return $ VFloat (fromIntegral a - b)
evalBinary BOpSub (VFloat a) (VInt b) = return $ VFloat (a - fromIntegral b)
evalBinary BOpMul (VInt a) (VInt b) = return $ VInt (a * b)
evalBinary BOpMul (VFloat a) (VFloat b) = return $ VFloat (a * b)
evalBinary BOpMul (VInt a) (VFloat b) = return $ VFloat (fromIntegral a * b)
evalBinary BOpMul (VFloat a) (VInt b) = return $ VFloat (a * fromIntegral b)
evalBinary BOpDiv (VInt a) (VInt b)
  | b == 0 = throwError $ VMRuntimeError "Division by zero"
  | otherwise = return $ VInt (a `div` b)
evalBinary BOpDiv (VFloat a) (VFloat b) = return $ VFloat (a / b)
evalBinary BOpDiv (VInt a) (VFloat b) = return $ VFloat (fromIntegral a / b)
evalBinary BOpDiv (VFloat a) (VInt b) = return $ VFloat (a / fromIntegral b)
evalBinary BOpMod (VInt a) (VInt b)
  | b == 0 = throwError $ VMRuntimeError "Modulo by zero"
  | otherwise = return $ VInt (a `mod` b)
evalBinary BOpEq a b = do
  strings <- gets vmStrings
  return $ VBool (valEq (resolveStringRef strings a) (resolveStringRef strings b))
evalBinary BOpNeq a b = do
  strings <- gets vmStrings
  return $ VBool (not (valEq (resolveStringRef strings a) (resolveStringRef strings b)))
evalBinary BOpLt a b = cmpOp (<) a b
evalBinary BOpLte a b = cmpOp (<=) a b
evalBinary BOpGt a b = cmpOp (>) a b
evalBinary BOpGte a b = cmpOp (>=) a b
evalBinary BOpAnd (VBool a) (VBool b) = return $ VBool (a && b)
evalBinary BOpOr (VBool a) (VBool b) = return $ VBool (a || b)
evalBinary BOpBitAnd (VInt a) (VInt b) = return $ VInt (fromIntegral (fromIntegral a .&. (fromIntegral b :: Int)))
evalBinary BOpBitOr (VInt a) (VInt b) = return $ VInt (fromIntegral (fromIntegral a .|. (fromIntegral b :: Int)))
evalBinary BOpBitXor (VInt a) (VInt b) = return $ VInt (fromIntegral (fromIntegral a `xor` (fromIntegral b :: Int)))
evalBinary BOpShl (VInt a) (VInt b) = return $ VInt (fromIntegral (fromIntegral a `shiftL` fromIntegral b :: Int))
evalBinary BOpShr (VInt a) (VInt b) = return $ VInt (fromIntegral (fromIntegral a `shiftR` fromIntegral b :: Int))
evalBinary op a b =
  throwError $ VMTypeMismatch $ "Binary " ++ show op ++ ": bad types " ++ show a ++ ", " ++ show b

valEq :: Value -> Value -> Bool
valEq (VInt a) (VInt b) = a == b
valEq (VFloat a) (VFloat b) = a == b
valEq (VBool a) (VBool b) = a == b
valEq (VString a) (VString b) = a == b
valEq (VErrorVal n1 _) (VErrorVal n2 _) = n1 == n2
valEq VUnit VUnit = True
valEq _ _ = False

cmpOp :: (Double -> Double -> Bool) -> Value -> Value -> VM Value
cmpOp f (VInt a) (VInt b) = return $ VBool (f (fromIntegral a) (fromIntegral b))
cmpOp f (VFloat a) (VFloat b) = return $ VBool (f a b)
cmpOp f (VInt a) (VFloat b) = return $ VBool (f (fromIntegral a) b)
cmpOp f (VFloat a) (VInt b) = return $ VBool (f a (fromIntegral b))
cmpOp _ a b = throwError $ VMTypeMismatch $ "Comparison: bad types " ++ show a ++ ", " ++ show b

-- ---------------------------------------------------------------------------
-- Unary operations

evalUnary :: UnaryOp -> Value -> VM Value
evalUnary UOpNeg (VInt n) = return $ VInt (negate n)
evalUnary UOpNeg (VFloat f) = return $ VFloat (negate f)
evalUnary UOpNot (VBool b) = return $ VBool (not b)
evalUnary UOpBitNot (VInt n) = return $ VInt (fromIntegral (complement (fromIntegral n :: Int)))
evalUnary op v = throwError $ VMTypeMismatch $ "Unary " ++ show op ++ " on " ++ show v

-- ---------------------------------------------------------------------------
-- Cast

evalCast :: CastType -> Value -> VM Value
evalCast CastToInt (VFloat f) = return $ VInt (truncate f)
evalCast CastToInt (VInt n) = return $ VInt n
evalCast CastToInt (VBool b) = return $ VInt (if b then 1 else 0)
evalCast CastToFloat (VInt n) = return $ VFloat (fromIntegral n)
evalCast CastToFloat (VFloat f) = return $ VFloat f
evalCast CastToBool (VInt 0) = return $ VBool False
evalCast CastToBool (VInt _) = return $ VBool True
evalCast CastToBool (VBool b) = return $ VBool b
evalCast CastToString (VInt n) = return $ VString (T.pack (show n))
evalCast CastToString (VFloat f) = return $ VString (T.pack (show f))
evalCast CastToString (VBool b) = return $ VString (if b then "true" else "false")
evalCast CastToString (VString t) = return $ VString t
evalCast CastToString (VStringRef i) = do
  strings <- gets vmStrings
  return $ VString (strings !! i)
evalCast CastToInt (VString t) =
  case reads (T.unpack t) of
    [(n, "")] -> return $ VInt n
    _ -> throwError $ VMTypeMismatch $ "Cannot cast string to int: " ++ T.unpack t
evalCast CastToFloat (VString t) =
  case reads (T.unpack t) of
    [(f, "")] -> return $ VFloat f
    _ -> throwError $ VMTypeMismatch $ "Cannot cast string to float: " ++ T.unpack t
evalCast ct v = throwError $ VMTypeMismatch $ "Cast " ++ show ct ++ " on " ++ show v
