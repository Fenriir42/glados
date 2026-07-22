-- | Human-readable disassembly of compiled bytecode.
module Compiler.Disasm (disassemble) where

import AST.Types.Common (ErrorName (..), FuncName (..), unFieldName, unFuncName, unVarName)
import Compiler.Bytecode
import Data.List (intercalate)
import qualified Data.Text as T

-- | Render a list of bytecodes as human-readable disassembly.
disassemble :: [Bytecode] -> String
disassemble = intercalate "\n" . map disassembleOne

disassembleOne :: Bytecode -> String
disassembleOne bc =
  unlines $
    [ "=== " ++ T.unpack (unFuncName (bytecodeFunction bc)) ++ " ===",
      "    entry: " ++ show (unInstructionPointer (bytecodeEntry bc))
    ]
      ++ strSection
      ++ instrLines
  where
    strSection
      | null (bytecodeStrings bc) = []
      | otherwise =
          "    strings:"
            : zipWith fmtStr [(0 :: Int) ..] (bytecodeStrings bc)

    fmtStr i s = "      [" ++ show i ++ "] " ++ show s

    instrLines =
      "    instructions:"
        : zipWith fmtInstr [(0 :: Int) ..] (bytecodeInstructions bc)

    fmtInstr i instr = "      " ++ pad i ++ "  " ++ showInstr instr

    pad i =
      let s = show i
       in replicate (4 - length s) ' ' ++ s

-- | Pretty-print a single instruction.
showInstr :: Instruction -> String
showInstr = \case
  IPush v -> "PUSH      " ++ showVal v
  IPop -> "POP"
  IDup -> "DUP"
  IBinary op -> "BINARY    " ++ showBOp op
  IUnary op -> "UNARY     " ++ showUOp op
  ILoad var -> "LOAD      " ++ T.unpack (unVarName var)
  IStore var -> "STORE     " ++ T.unpack (unVarName var)
  IJump ip -> "JUMP      " ++ showIP ip
  IJumpTrue ip -> "JUMP_T    " ++ showIP ip
  IJumpFalse ip -> "JUMP_F    " ++ showIP ip
  ICall ref argc ->
    "CALL      "
      ++ T.unpack (unFuncName (unFunctionRef ref))
      ++ " ("
      ++ show argc
      ++ " args)"
  IRet -> "RET"
  INop -> "NOP"
  INewDict -> "NEW_DICT"
  INewArray -> "NEW_ARRAY"
  IArrayGet -> "ARRAY_GET"
  IArrayGetOrNew -> "ARRAY_GET_OR_NEW"
  IArraySet -> "ARRAY_SET"
  ICast ct -> "CAST      " ++ showCast ct
  INewStruct -> "NEW_STRUCT"
  IFieldGet f -> "FIELD_GET  " ++ T.unpack (unFieldName f)
  IFieldSet f -> "FIELD_SET  " ++ T.unpack (unFieldName f)
  INewError (ErrorName n) fs ->
    "NEW_ERROR  " ++ T.unpack n ++ " [" ++ intercalate "," (map (T.unpack . unFieldName) fs) ++ "]"
  ITryOp -> "TRY"
  IMustOp -> "MUST"
  IIsOk -> "IS_OK"
  IIsErr (ErrorName n) -> "IS_ERR     " ++ T.unpack n
  ILoadFunc (FuncName n) -> "LOAD_FUNC  " ++ T.unpack n
  ICallIndirect argc -> "CALL_INDIR " ++ show argc
  ICallFFI lib sym _ret argc -> "CALL_FFI   " ++ T.unpack lib ++ ":" ++ T.unpack sym ++ " /" ++ show argc
  ICovMark n -> "COV_MARK   " ++ show n
  ICovBranch n -> "COV_BRANCH " ++ show n

showVal :: Value -> String
showVal (VInt n) = show n
showVal (VFloat f) = show f
showVal (VBool b) = if b then "true" else "false"
showVal (VString s) = show s
showVal (VStringRef i) = "str[" ++ show i ++ "]"
showVal (VArrayRef i) = "arr[" ++ show i ++ "]"
showVal (VDictRef i) = "dict[" ++ show i ++ "]"
showVal (VStructRef i) = "struct[" ++ show i ++ "]"
showVal (VErrorVal (ErrorName n) _) = "err(" ++ T.unpack n ++ ")"
showVal (VFunction (FuncName n)) = "fn(" ++ T.unpack n ++ ")"
showVal VUnit = "unit"

showIP :: InstructionPointer -> String
showIP (InstructionPointer i) = show i

showBOp :: BinaryOp -> String
showBOp BOpAdd = "+"
showBOp BOpSub = "-"
showBOp BOpMul = "*"
showBOp BOpDiv = "/"
showBOp BOpMod = "%"
showBOp BOpEq = "=="
showBOp BOpNeq = "!="
showBOp BOpLt = "<"
showBOp BOpLte = "<="
showBOp BOpGt = ">"
showBOp BOpGte = ">="
showBOp BOpAnd = "&&"
showBOp BOpOr = "||"
showBOp BOpBitAnd = "&"
showBOp BOpBitOr = "|"
showBOp BOpBitXor = "^"
showBOp BOpShl = "<<"
showBOp BOpShr = ">>"

showUOp :: UnaryOp -> String
showUOp UOpNot = "!"
showUOp UOpNeg = "-"
showUOp UOpBitNot = "~"

showCast :: CastType -> String
showCast CastToInt = "int"
showCast CastToFloat = "float"
showCast CastToBool = "bool"
showCast CastToString = "str"
