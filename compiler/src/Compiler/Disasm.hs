-- | Human-readable disassembly of compiled bytecode.
module Compiler.Disasm (disassemble) where

import AST.Types.Common (unFuncName, unVarName)
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
  INewArray -> "NEW_ARRAY"
  IArrayGet -> "ARRAY_GET"
  IArrayGetOrNew -> "ARRAY_GET_OR_NEW"
  IArraySet -> "ARRAY_SET"
  ICast ct -> "CAST      " ++ showCast ct

showVal :: Value -> String
showVal (VInt n) = show n
showVal (VFloat f) = show f
showVal (VBool b) = if b then "true" else "false"
showVal (VStringRef i) = "str[" ++ show i ++ "]"
showVal (VArrayRef i) = "arr[" ++ show i ++ "]"
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
