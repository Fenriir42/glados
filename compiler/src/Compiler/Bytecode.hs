-- | Bytecode representation for the stack-based virtual machine.
module Compiler.Bytecode
  ( -- * Instructions
    Instruction (..),
    BinaryOp (..),
    UnaryOp (..),
    Value (..),
    CastType (..),
    CRetType (..),

    -- * Bytecode
    Bytecode (..),

    -- * Addresses & References
    InstructionPointer (..),
    StackPointer (..),
    RegisterIndex (..),
    FunctionRef (..),
  )
where

import AST.Types.Common
  ( ErrorName (..),
    FieldName,
    FuncName,
    VarName,
  )
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Runtime value that can be stored on the stack or in memory.
data Value
  = VInt Integer
  | VFloat Double
  | VBool Bool
  | -- | index into the current function's static string pool
    VStringRef Int
  | -- | dynamically produced string (runtime only, never serialised)
    VString Text
  | VArrayRef Int
  | -- | reference into the dict heap
    VDictRef Int
  | -- | reference into the struct heap
    VStructRef Int
  | -- | error value: name + optional field payload
    VErrorVal ErrorName [(FieldName, Value)]
  | -- | reference to a named function (first-class function value)
    VFunction FuncName
  | -- | opaque C pointer stored as its raw address (runtime only)
    VPointer Word64
  | VUnit
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Value

-- | Target type for cast instructions.
data CastType
  = CastToInt
  | CastToFloat
  | CastToBool
  | CastToString
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance Hashable CastType

-- | C return type for FFI calls.
data CRetType
  = CRetVoid
  | CRetInt
  | CRetFloat
  | CRetStr
  | CRetBool
  | CRetPtr
  deriving stock (Show, Eq, Generic)

instance Hashable CRetType

-- | Binary operations that can be performed by the VM.
data BinaryOp
  = BOpAdd
  | BOpSub
  | BOpMul
  | BOpDiv
  | BOpMod
  | BOpEq
  | BOpNeq
  | BOpLt
  | BOpLte
  | BOpGt
  | BOpGte
  | BOpAnd
  | BOpOr
  | BOpBitAnd
  | BOpBitOr
  | BOpBitXor
  | BOpShl
  | BOpShr
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance Hashable BinaryOp

-- | Unary operations that can be performed by the VM.
data UnaryOp
  = UOpNot
  | UOpNeg
  | UOpBitNot
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance Hashable UnaryOp

-- | A single bytecode instruction.
--
-- Instructions operate on an implicit stack. Most instructions pop operands
-- from the stack and push results back.
data Instruction
  = -- | Push a constant value onto the stack
    IPush Value
  | -- | Pop the top value from the stack
    IPop
  | -- | Duplicate the top stack value
    IDup
  | -- | Binary operation: pop two values, push result
    IBinary BinaryOp
  | -- | Unary operation: pop one value, push result
    IUnary UnaryOp
  | -- | Load variable onto stack
    ILoad VarName
  | -- | Store top stack value into variable (does not pop)
    IStore VarName
  | -- | Unconditional jump to instruction pointer
    IJump InstructionPointer
  | -- | Jump if top stack value is true (pops the value)
    IJumpTrue InstructionPointer
  | -- | Jump if top stack value is false (pops the value)
    IJumpFalse InstructionPointer
  | -- | Call function (args pushed right-to-left, pushes return value)
    ICall FunctionRef Int
  | -- | Return from function (top of stack is return value)
    IRet
  | -- | No operation
    INop
  | -- | Create a new empty dict; push its reference
    INewDict
  | -- | Create a new empty array; push its reference
    INewArray
  | -- | Array read: pop index, pop array ref, push element
    IArrayGet
  | -- | Array read/create: like IArrayGet, but if slot is missing, allocates a new
    -- empty array there and returns its ref (used for writing through nested arrays)
    IArrayGetOrNew
  | -- | Array write: pop value, pop index, pop array ref, set element
    IArraySet
  | -- | Cast top of stack to the given type
    ICast CastType
  | -- | Allocate a new empty struct in the struct heap; push its VStructRef
    INewStruct
  | -- | Pop VStructRef, push the named field value
    IFieldGet FieldName
  | -- | Pop value then VStructRef; set the named field in the struct heap
    IFieldSet FieldName
  | -- | Build a VErrorVal: pop N (FieldName, Value) pairs, push VErrorVal
    INewError ErrorName [FieldName]
  | -- | If TOS is VErrorVal, return it from the current function; else no-op
    ITryOp
  | -- | If TOS is VErrorVal, panic; else no-op
    IMustOp
  | -- | Peek TOS: push VBool True if NOT a VErrorVal
    IIsOk
  | -- | Peek TOS: push VBool True if IS a VErrorVal with the given name
    IIsErr ErrorName
  | -- | Push a VFunction (named function reference) onto the stack
    ILoadFunc FuncName
  | -- | Call through a VFunction value: pop VFunction (below args), call it with argc args
    ICallIndirect Int
  | -- | Call a C function from a shared library via libffi
    ICallFFI
      -- | shared library path (e.g. "libm.so.6")
      Text
      -- | C symbol name
      Text
      -- | return type tag for dispatch
      CRetType
      -- | argument count
      Int
  deriving stock (Show, Eq, Generic)

instance Hashable Instruction

-- | An instruction pointer is an index into the bytecode instruction array.
newtype InstructionPointer = InstructionPointer {unInstructionPointer :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Hashable)

-- | A stack pointer tracks the current top of the stack.
newtype StackPointer = StackPointer {unStackPointer :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Hashable)

-- | An index into the VM's register file.
newtype RegisterIndex = RegisterIndex {unRegisterIndex :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Hashable)

-- | A reference to a function by name.
newtype FunctionRef = FunctionRef {unFunctionRef :: FuncName}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

-- | Compiled bytecode for a single function.
data Bytecode = Bytecode
  { bytecodeFunction :: FuncName,
    bytecodeInstructions :: [Instruction],
    bytecodeEntry :: InstructionPointer,
    bytecodeStrings :: [Text]
  }
  deriving stock (Show, Eq, Generic)
