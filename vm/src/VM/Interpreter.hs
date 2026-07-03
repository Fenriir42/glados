-- | Stack-based bytecode interpreter for the Quant VM.
module VM.Interpreter
  ( VMError (..),
    runProgram,
  )
where

import AST.Types.Common (ErrorName (..), FieldName (..), FuncName (..), VarName)
import Compiler.Bytecode
  ( BinaryOp (..),
    Bytecode (..),
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
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.Posix.IO (fdWrite)
import System.Posix.Types (ByteCount, Fd (..))
import VM.Builtins (callBuiltin, isBuiltin, isHeapBuiltin)

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
    fInstrs :: [Instruction],
    fStrings :: [Text],
    fFunc :: FuncName
  }

data VMState = VMState
  { vmStack :: [Value],
    vmLocals :: Map VarName Value,
    vmIP :: Int,
    vmInstrs :: [Instruction],
    vmStrings :: [Text],
    vmCallStack :: [Frame],
    vmHeap :: Map Int (Map Int Value),
    vmDictHeap :: Map Int (Map Value Value),
    vmStructHeap :: Map Int (Map Text Value),
    vmNextId :: Int,
    vmFunctions :: Map FuncName Bytecode,
    vmCurrentFunc :: FuncName
  }

type VM a = ExceptT VMError (StateT VMState IO) a

-- ---------------------------------------------------------------------------
-- Public entry point

-- | Load all bytecodes and execute @main@, returning its return value.
runProgram :: [Bytecode] -> IO (Either VMError Value)
runProgram bytecodes = do
  let funcs = Map.fromList [(bytecodeFunction bc, bc) | bc <- bytecodes]
  case Map.lookup (FuncName "main") funcs of
    Nothing -> return $ Left $ VMUndefinedFunction (FuncName "main")
    Just mainBc -> do
      let initState =
            VMState
              { vmStack = [],
                vmLocals = Map.empty,
                vmIP = 0,
                vmInstrs = bytecodeInstructions mainBc,
                vmStrings = bytecodeStrings mainBc,
                vmCallStack = [],
                vmHeap = Map.empty,
                vmDictHeap = Map.empty,
                vmStructHeap = Map.empty,
                vmNextId = 0,
                vmFunctions = funcs,
                vmCurrentFunc = FuncName "main"
              }
      (result, _) <- runStateT (runExceptT execLoop) initState
      return result

-- ---------------------------------------------------------------------------
-- Execution loop

execLoop :: VM Value
execLoop = do
  ip <- gets vmIP
  instrs <- gets vmInstrs
  if ip >= length instrs
    then return VUnit
    else do
      let instr = instrs !! ip
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
              vmInstrs = bytecodeInstructions bc,
              vmStrings = bytecodeStrings bc,
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
    frames <- gets vmCallStack
    case frames of
      [] -> return (Just retVal)
      (frame : rest) -> do
        modify $ \s ->
          s
            { vmLocals = fLocals frame,
              vmIP = fIP frame,
              vmInstrs = fInstrs frame,
              vmStrings = fStrings frame,
              vmCallStack = rest,
              vmStack = retVal : vmStack s,
              vmCurrentFunc = fFunc frame
            }
        return Nothing
  INop -> return Nothing
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
    case (ref, idx) of
      (VArrayRef aid, VInt i) -> do
        modify $ \s ->
          s
            { vmHeap = Map.adjust (Map.insert (fromIntegral i) val) aid (vmHeap s)
            }
        return Nothing
      (VDictRef did, key) -> do
        strings <- gets vmStrings
        let resolvedKey = resolveStringRef strings key
        modify $ \s ->
          s
            { vmDictHeap = Map.adjust (Map.insert resolvedKey val) did (vmDictHeap s)
            }
        return Nothing
      _ -> throwError $ VMTypeMismatch $ "IArraySet: bad types " ++ show ref ++ " " ++ show idx
  ICast ct -> do
    v <- pop "ICast"
    r <- evalCast ct v
    push r
    return Nothing
  INewStruct -> do
    sid <- gets vmNextId
    modify $ \s ->
      s
        { vmStructHeap = Map.insert sid Map.empty (vmStructHeap s),
          vmNextId = sid + 1
        }
    push (VStructRef sid)
    return Nothing
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
    case ref of
      VStructRef sid -> do
        modify $ \s ->
          s {vmStructHeap = Map.adjust (Map.insert fname val) sid (vmStructHeap s)}
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
  ICallIndirect argc -> do
    strings <- gets vmStrings
    funcs <- gets vmFunctions
    stk <- gets vmStack
    -- Stack layout: [arg_0 (TOS), ..., arg_n-1, VFunction, rest...]
    let (callArgs, rest) = splitAt argc stk
        resolvedArgs = map (resolveStringRef strings) callArgs
    case rest of
      (VFunction fname : remaining) ->
        case Map.lookup fname funcs of
          Just bc -> do
            saveFrame
            mapM_ (\case VArrayRef aid -> resolveArrayStrings strings aid; _ -> return ()) callArgs
            -- Remove VFunction from stack; callee sees [arg_0..arg_n-1, remaining...]
            modify $ \s ->
              s
                { vmStack = resolvedArgs ++ remaining,
                  vmLocals = Map.empty,
                  vmIP = 0,
                  vmInstrs = bytecodeInstructions bc,
                  vmStrings = bytecodeStrings bc,
                  vmCurrentFunc = fname
                }
            return Nothing
          Nothing -> do
            -- Builtin: strip args + VFunction from stack, call, push result
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
      _ -> throwError $ VMRuntimeError "ICallIndirect: no function value on stack"

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
    heap <- gets vmHeap
    case Map.lookup aid heap of
      Nothing -> throwError $ VMRuntimeError $ "Array #" ++ show aid ++ " not found"
      Just arr -> do
        let nextIdx = if Map.null arr then 0 else fst (Map.findMax arr) + 1
        modify $ \s -> s {vmHeap = Map.adjust (Map.insert nextIdx val) aid (vmHeap s)}
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
  _ ->
    throwError $
      VMRuntimeError $
        "Heap builtin '" ++ show name ++ "' called with bad args: " ++ show args

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
  heap <- gets vmHeap
  case Map.lookup aid heap of
    Nothing -> return ()
    Just arr -> do
      let nextIdx = if Map.null arr then 0 else fst (Map.findMax arr) + 1
      modify $ \s -> s {vmHeap = Map.adjust (Map.insert nextIdx val) aid (vmHeap s)}

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
evalBinary BOpEq a b = return $ VBool (valEq a b)
evalBinary BOpNeq a b = return $ VBool (not (valEq a b))
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
