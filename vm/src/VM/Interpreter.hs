-- | Stack-based bytecode interpreter for the Quant VM.
module VM.Interpreter
  ( VMError (..),
    runProgram,
  )
where

import AST.Types.Common (FieldName (..), FuncName (..), VarName)
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
import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.State (StateT, gets, modify, runStateT)
import qualified Control.Monad.State as S
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
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
      _ -> throwError $ VMTypeMismatch $ "IFieldGet: expected struct ref, got " ++ show ref
  IFieldSet (FieldName fname) -> do
    val <- pop "IFieldSet (value)"
    ref <- pop "IFieldSet (ref)"
    case ref of
      VStructRef sid -> do
        modify $ \s ->
          s {vmStructHeap = Map.adjust (Map.insert fname val) sid (vmStructHeap s)}
        return Nothing
      _ -> throwError $ VMTypeMismatch $ "IFieldSet: expected struct ref, got " ++ show ref

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
  _ ->
    throwError $
      VMRuntimeError $
        "Heap builtin '" ++ show name ++ "' called with bad args: " ++ show args

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
