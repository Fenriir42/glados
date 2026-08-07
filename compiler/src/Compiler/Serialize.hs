{-# OPTIONS_GHC -Wno-orphans #-}

--
-- File format:
--   magic   : 4 bytes  "QBC\0"
--   version : Word8    = 1
--   funcs   : [Bytecode]  (length-prefixed list)

-- | Binary serialization and deserialization for Quant bytecode (.qbc files).
module Compiler.Serialize
  ( encodeBytecodes,
    decodeBytecodes,
  )
where

import AST.Types.Common (ErrorName (..), FieldName (..), FuncName (..), TypeName (..), VarName (..))
import Compiler.Bytecode
import Data.Binary (Binary (..))
import Data.Binary.Get (Get, getByteString, runGetOrFail)
import Data.Binary.Put (putByteString, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)

-- ---------------------------------------------------------------------------
-- Magic / version

magic :: BS.ByteString
magic = "QBC\0"

currentVersion :: Word8
currentVersion = 1

-- ---------------------------------------------------------------------------
-- Public API

encodeBytecodes :: [Bytecode] -> BSL.ByteString
encodeBytecodes bcs = runPut $ do
  putByteString magic
  put currentVersion
  put bcs

decodeBytecodes :: BSL.ByteString -> Either String [Bytecode]
decodeBytecodes bs =
  case runGetOrFail parseBytecodes bs of
    Left (_, _, err) -> Left err
    Right (_, _, bcs) -> Right bcs

parseBytecodes :: Get [Bytecode]
parseBytecodes = do
  hdr <- getByteString 4
  if hdr /= magic
    then fail "Not a QBC file (bad magic bytes)"
    else do
      ver <- get :: Get Word8
      if ver /= currentVersion
        then fail $ "Unsupported QBC version: " ++ show ver
        else get

-- ---------------------------------------------------------------------------
-- Binary instances

-- Binary Text is provided by the text package.

instance Binary FuncName where
  put (FuncName t) = put t
  get = FuncName <$> get

instance Binary VarName where
  put (VarName t) = put t
  get = VarName <$> get

instance Binary FieldName where
  put (FieldName t) = put t
  get = FieldName <$> get

instance Binary ErrorName where
  put (ErrorName t) = put t
  get = ErrorName <$> get

instance Binary TypeName where
  put (TypeName t) = put t
  get = TypeName <$> get

instance Binary InstructionPointer where
  put (InstructionPointer i) = put i
  get = InstructionPointer <$> get

instance Binary FunctionRef where
  put (FunctionRef f) = put f
  get = FunctionRef <$> get

instance Binary CRetType where
  put CRetVoid = put (0 :: Word8)
  put CRetInt = put (1 :: Word8)
  put CRetFloat = put (2 :: Word8)
  put CRetStr = put (3 :: Word8)
  put CRetBool = put (4 :: Word8)
  put CRetPtr = put (5 :: Word8)
  get =
    (get :: Get Word8) >>= \case
      0 -> pure CRetVoid
      1 -> pure CRetInt
      2 -> pure CRetFloat
      3 -> pure CRetStr
      4 -> pure CRetBool
      5 -> pure CRetPtr
      t -> fail $ "Unknown CRetType tag: " ++ show t

instance Binary CastType where
  put CastToInt = put (0 :: Word8)
  put CastToFloat = put (1 :: Word8)
  put CastToBool = put (2 :: Word8)
  put CastToString = put (3 :: Word8)
  get =
    (get :: Get Word8) >>= \case
      0 -> pure CastToInt
      1 -> pure CastToFloat
      2 -> pure CastToBool
      3 -> pure CastToString
      t -> fail $ "Unknown CastType tag: " ++ show t

instance Binary Value where
  put (VInt n) = put (0 :: Word8) >> put n
  put (VFloat f) = put (1 :: Word8) >> put f
  put (VBool b) = put (2 :: Word8) >> put b
  put (VStringRef i) = put (3 :: Word8) >> put i
  put (VArrayRef i) = put (4 :: Word8) >> put i
  put VUnit = put (5 :: Word8)
  put (VString t) = put (6 :: Word8) >> put t
  put (VDictRef i) = put (10 :: Word8) >> put i
  put (VStructRef i) = put (7 :: Word8) >> put i
  put (VFunction fname) = put (8 :: Word8) >> put fname
  put (VErrorVal ename fields) = put (9 :: Word8) >> put ename >> put fields
  get =
    (get :: Get Word8) >>= \case
      0 -> VInt <$> get
      1 -> VFloat <$> get
      2 -> VBool <$> get
      3 -> VStringRef <$> get
      4 -> VArrayRef <$> get
      5 -> pure VUnit
      6 -> VString <$> get
      7 -> VStructRef <$> get
      8 -> VFunction <$> get
      9 -> VErrorVal <$> get <*> get
      10 -> VDictRef <$> get
      t -> fail $ "Unknown Value tag: " ++ show t

instance Binary BinaryOp where
  put op = put (fromIntegral (fromEnum op) :: Word8)
  get = toEnum . fromIntegral <$> (get :: Get Word8)

instance Binary UnaryOp where
  put op = put (fromIntegral (fromEnum op) :: Word8)
  get = toEnum . fromIntegral <$> (get :: Get Word8)

instance Binary Instruction where
  put i = case i of
    IPush v -> tag 0 >> put v
    IPop -> tag 1
    IDup -> tag 2
    IBinary op -> tag 3 >> put op
    IUnary op -> tag 4 >> put op
    ILoad var -> tag 5 >> put var
    IStore var -> tag 6 >> put var
    IJump ip -> tag 7 >> put ip
    IJumpTrue ip -> tag 8 >> put ip
    IJumpFalse ip -> tag 9 >> put ip
    ICall ref argc -> tag 10 >> put ref >> put (argc :: Int)
    IRet -> tag 11
    INop -> tag 12
    INewArray -> tag 13
    IArrayGet -> tag 14
    IArrayGetOrNew -> tag 15
    IArraySet -> tag 16
    ICast ct -> tag 17 >> put ct
    INewStruct tname -> tag 18 >> put tname
    IFieldGet f -> tag 19 >> put f
    IFieldSet f -> tag 20 >> put f
    INewError ename fnames -> tag 21 >> put ename >> put fnames
    ITryOp -> tag 22
    IMustOp -> tag 23
    IIsOk -> tag 24
    IIsErr ename -> tag 25 >> put ename
    ILoadFunc fname -> tag 26 >> put fname
    ICallIndirect argc -> tag 27 >> put (argc :: Int)
    INewDict -> tag 28
    ICallFFI lib sym retTy argc -> tag 29 >> put lib >> put sym >> put retTy >> put (argc :: Int)
    ICovMark n -> tag 30 >> put (n :: Int)
    ICovBranch n -> tag 31 >> put (n :: Int)
    IDynMethodCall mname argc -> tag 32 >> put mname >> put (argc :: Int)
    ISpawn ref argc -> tag 33 >> put ref >> put (argc :: Int)
    IAwait -> tag 34
    where
      tag n = put (n :: Word8)

  get =
    (get :: Get Word8) >>= \case
      0 -> IPush <$> get
      1 -> pure IPop
      2 -> pure IDup
      3 -> IBinary <$> get
      4 -> IUnary <$> get
      5 -> ILoad <$> get
      6 -> IStore <$> get
      7 -> IJump <$> get
      8 -> IJumpTrue <$> get
      9 -> IJumpFalse <$> get
      10 -> ICall <$> get <*> (get :: Get Int)
      11 -> pure IRet
      12 -> pure INop
      13 -> pure INewArray
      14 -> pure IArrayGet
      15 -> pure IArrayGetOrNew
      16 -> pure IArraySet
      17 -> ICast <$> get
      18 -> INewStruct <$> get
      19 -> IFieldGet <$> get
      20 -> IFieldSet <$> get
      21 -> INewError <$> get <*> get
      22 -> pure ITryOp
      23 -> pure IMustOp
      24 -> pure IIsOk
      25 -> IIsErr <$> get
      26 -> ILoadFunc <$> get
      27 -> ICallIndirect <$> get
      28 -> pure INewDict
      29 -> ICallFFI <$> get <*> get <*> get <*> get
      30 -> ICovMark <$> (get :: Get Int)
      31 -> ICovBranch <$> (get :: Get Int)
      32 -> IDynMethodCall <$> get <*> (get :: Get Int)
      33 -> ISpawn <$> get <*> (get :: Get Int)
      34 -> pure IAwait
      t -> fail $ "Unknown Instruction tag: " ++ show t

instance Binary Bytecode where
  put bc =
    put (bytecodeFunction bc)
      >> put (bytecodeInstructions bc)
      >> put (bytecodeEntry bc)
      >> put (bytecodeStrings bc)
  get = Bytecode <$> get <*> get <*> get <*> get
