{-# LANGUAGE LambdaCase #-}

module Compiler.Codegen
  ( compileProgram,
    compileFunction,
    CompileError (..),
    errSpan,
    errorMessage,
  )
where

import AST.Types.AST
  ( Block (..),
    Decl (..),
    Expr (..),
    ForInit (..),
    FunctionDecl (..),
    LValue (..),
    Program (..),
    Stmt (..),
  )
import AST.Types.Common
  ( FuncName (..),
    Located (..),
    SourceSpan,
    VarName (..),
    locSpan,
    unLocated,
  )
import AST.Types.Literal
  ( ArrayLiteral (..),
    BoolLiteral (..),
    FloatLiteral (..),
    IntLiteral (..),
    Literal (..),
    StringLiteral (..),
  )
import qualified AST.Types.Operator as Op
import AST.Types.Type (PrimitiveType (..), QualifiedType (..), Type (..), paramName, qualType)
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
import Control.Monad (forM_, unless, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State
  ( State,
    evalState,
    get,
    gets,
    modify,
    put,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Error type

data CompileError
  = UndefinedVariable SourceSpan VarName
  | UndefinedFunction SourceSpan FuncName
  | UnsupportedConstruct SourceSpan String
  | TypeMismatch SourceSpan String
  deriving stock (Show, Eq)

errSpan :: CompileError -> SourceSpan
errSpan (UndefinedVariable s _) = s
errSpan (UndefinedFunction s _) = s
errSpan (UnsupportedConstruct s _) = s
errSpan (TypeMismatch s _) = s

errorMessage :: CompileError -> String
errorMessage (UndefinedVariable _ v) =
  "undefined variable `" ++ T.unpack (unVarName v) ++ "`"
errorMessage (UndefinedFunction _ f) =
  "undefined function `" ++ T.unpack (unFuncName f) ++ "`"
errorMessage (UnsupportedConstruct _ msg) = msg
errorMessage (TypeMismatch _ msg) = "type mismatch: " ++ msg

-- ---------------------------------------------------------------------------
-- Known builtins

builtinFunctions :: Set FuncName
builtinFunctions =
  Set.fromList
    [ "print",
      "println",
      "io.print",
      "io.println",
      "math.sqrt",
      "math.abs",
      "math.fabs",
      "math.floor",
      "math.ceil",
      "math.round",
      "math.pow",
      "math.exp",
      "math.log",
      "math.sin",
      "math.cos",
      "math.min",
      "math.max",
      "math.fmin",
      "math.fmax",
      "array.len",
      "len",
      "array.push",
      "push",
      "array.pop",
      "pop",
      "sys.exit"
    ]

isKnownFunction :: FuncName -> Set FuncName -> Bool
isKnownFunction fname known =
  Set.member fname known
    || Set.member fname builtinFunctions
    || any (`T.isPrefixOf` unFuncName fname) ["math.", "string.", "sys.", "io."]

-- ---------------------------------------------------------------------------
-- Compile state

data CompileState = CompileState
  { csInstructionCounter :: Int,
    csInstructions :: [Instruction],
    csLocals :: Map VarName Int,
    csFunctions :: Map FuncName Bytecode,
    csStrings :: Map Text Int,
    csStringList :: [Text],
    csBreakJumps :: [InstructionPointer],
    csContinueJumps :: [InstructionPointer],
    -- scope tracking
    csScope :: Set VarName,
    csKnownFunctions :: Set FuncName,
    csTempCount :: Int
  }
  deriving stock (Show)

initialState :: CompileState
initialState =
  CompileState
    { csInstructionCounter = 0,
      csInstructions = [],
      csLocals = Map.empty,
      csFunctions = Map.empty,
      csStrings = Map.empty,
      csStringList = [],
      csBreakJumps = [],
      csContinueJumps = [],
      csScope = Set.empty,
      csKnownFunctions = Set.empty,
      csTempCount = 0
    }

type Compile a = ExceptT CompileError (State CompileState) a

-- ---------------------------------------------------------------------------
-- Helpers

emitInstruction :: Instruction -> Compile InstructionPointer
emitInstruction instr = do
  counter <- gets csInstructionCounter
  modify $ \s ->
    s
      { csInstructionCounter = counter + 1,
        csInstructions = csInstructions s ++ [instr]
      }
  return $ InstructionPointer counter

registerString :: Text -> Compile Int
registerString str = do
  strings <- gets csStrings
  case Map.lookup str strings of
    Just idx -> return idx
    Nothing -> do
      idx <- gets (length . csStringList)
      modify $ \s ->
        s
          { csStrings = Map.insert str idx (csStrings s),
            csStringList = csStringList s ++ [str]
          }
      return idx

addToScope :: VarName -> Compile ()
addToScope v = modify $ \s -> s {csScope = Set.insert v (csScope s)}

patchAll :: [InstructionPointer] -> InstructionPointer -> [Instruction] -> [Instruction]
patchAll addrs target instrs = foldr (\a is -> patchJump is a target) instrs addrs

-- ---------------------------------------------------------------------------
-- Program entry

compileProgram :: Program ann -> Either CompileError [Bytecode]
compileProgram (Program decls) =
  evalState (runExceptT go) initialState
  where
    go :: Compile [Bytecode]
    go = do
      let funcs =
            [ fd
              | decl <- decls,
                let d = unLocated decl,
                DeclFunction _ fd <- [d]
            ]
      -- First pass: register all user-defined function names so forward
      -- references and mutual recursion work.
      let userNames = Set.fromList [unLocated (funcDeclName fd) | fd <- funcs]
      modify $ \s -> s {csKnownFunctions = userNames}
      mapM compileFunction funcs

-- ---------------------------------------------------------------------------
-- Function

compileFunction :: FunctionDecl ann -> Compile Bytecode
compileFunction funcDecl = do
  let funcName = unLocated (funcDeclName funcDecl)
  oldState <- get
  put initialState {csKnownFunctions = csKnownFunctions oldState}
  -- Parameters are in scope from the start
  let paramNames = [paramName p | Located _ p <- funcDeclParams funcDecl]
  modify $ \s -> s {csScope = Set.fromList paramNames}
  mapM_ (\(Located _ p) -> void $ emitInstruction (IStore (paramName p))) (funcDeclParams funcDecl)
  compileBlock (funcDeclBody funcDecl)
  void $ emitInstruction (IPush VUnit)
  void $ emitInstruction IRet
  state <- get
  put oldState
  return
    Bytecode
      { bytecodeFunction = funcName,
        bytecodeInstructions = csInstructions state,
        bytecodeEntry = InstructionPointer 0,
        bytecodeStrings = csStringList state
      }

-- ---------------------------------------------------------------------------
-- Block / Statement

compileBlock :: Block ann -> Compile ()
compileBlock (Block _span stmts) = mapM_ compileLocatedStmt stmts

compileLocatedStmt :: Located (Stmt ann) -> Compile ()
compileLocatedStmt (Located _ stmt) = compileStmt stmt

compileStmt :: Stmt ann -> Compile ()
compileStmt = \case
  StmtVarDecl name typ mInit -> do
    case mInit of
      Just initExpr -> compileExpr (unLocated initExpr)
      Nothing ->
        case qualType (unLocated typ) of
          TypeArray _ ->
            void $ emitInstruction INewArray
          TypePrimitive PrimBool ->
            void $ emitInstruction (IPush (VBool False))
          TypePrimitive (PrimInt _) ->
            void $ emitInstruction (IPush (VInt 0))
          TypePrimitive (PrimFloat _) ->
            void $ emitInstruction (IPush (VFloat 0.0))
          _ ->
            void $ emitInstruction (IPush VUnit)
    void $ emitInstruction (IStore (unLocated name))
    addToScope (unLocated name)
  StmtAssign lvalue assignOp rhs ->
    case unLocated lvalue of
      LVarRef var ->
        case Op.assignOpToBinaryOp assignOp of
          Nothing -> do
            compileExpr (unLocated rhs)
            void $ emitInstruction (IStore (unLocated var))
          Just binOp -> do
            void $ emitInstruction (ILoad (unLocated var))
            compileExpr (unLocated rhs)
            void $ emitInstruction (IBinary (astBinaryOpToBytecode binOp))
            void $ emitInstruction (IStore (unLocated var))
      LArrayIndex base indexExpr ->
        case Op.assignOpToBinaryOp assignOp of
          Nothing -> do
            compileLValueReadForWrite lvalue base
            compileExpr (unLocated indexExpr)
            compileExpr (unLocated rhs)
            void $ emitInstruction IArraySet
          Just binOp -> do
            -- arr[i] op= x  →  arr[i] = arr[i] op x
            -- IArraySet needs [new_val, idx, ref] top→bottom.
            -- Use a temp var so we can push ref+idx AFTER computing new_val:
            --   1. get old_val: ILoad arr, IPush i, IArrayGet
            --   2. compute new_val: IPush x, IBinary → [new_val]
            --   3. IStore __tmp  (peek-store, keeps new_val on stack)
            --   4. IPop          → []
            --   5. ILoad arr → [ref]
            --   6. compile i  → [i, ref]
            --   7. ILoad __tmp → [new_val, i, ref]
            --   8. IArraySet ✓
            tmpN <- gets csTempCount
            modify $ \s -> s {csTempCount = tmpN + 1}
            let tmpVar = VarName (T.pack ("__arr_tmp_" ++ show tmpN))
            compileLValueReadForWrite lvalue base
            compileExpr (unLocated indexExpr)
            void $ emitInstruction IArrayGet
            compileExpr (unLocated rhs)
            void $ emitInstruction (IBinary (astBinaryOpToBytecode binOp))
            void $ emitInstruction (IStore tmpVar)
            compileLValueReadForWrite lvalue base
            compileExpr (unLocated indexExpr)
            void $ emitInstruction (ILoad tmpVar)
            void $ emitInstruction IArraySet
      LFieldAccess base locField ->
        case Op.assignOpToBinaryOp assignOp of
          Nothing -> do
            compileLValueReadForWrite lvalue base
            compileExpr (unLocated rhs)
            void $ emitInstruction (IFieldSet (unLocated locField))
          Just binOp -> do
            compileLValueReadForWrite lvalue base
            void $ emitInstruction IDup
            void $ emitInstruction (IFieldGet (unLocated locField))
            compileExpr (unLocated rhs)
            void $ emitInstruction (IBinary (astBinaryOpToBytecode binOp))
            void $ emitInstruction (IFieldSet (unLocated locField))
  StmtExpr expr -> do
    compileExpr (unLocated expr)
    void $ emitInstruction IPop
  StmtIf condExpr thenBlock mElseBlock -> do
    compileExpr (unLocated condExpr)
    jmpFalseAddr <- emitInstruction (IJumpFalse (InstructionPointer 0))
    compileBlock thenBlock
    jmpEndAddr <- emitInstruction (IJump (InstructionPointer 0))
    falseTarget <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) jmpFalseAddr falseTarget}
    mapM_ compileBlock mElseBlock
    endTarget <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) jmpEndAddr endTarget}
  StmtWhile condExpr body -> do
    loopStart <- gets (InstructionPointer . csInstructionCounter)
    compileExpr (unLocated condExpr)
    jmpFalseAddr <- emitInstruction (IJumpFalse (InstructionPointer 0))
    outerBreaks <- gets csBreakJumps
    outerContinues <- gets csContinueJumps
    modify $ \s -> s {csBreakJumps = [], csContinueJumps = []}
    compileBlock body
    innerContinues <- gets csContinueJumps
    modify $ \s ->
      s
        { csContinueJumps = [],
          csInstructions = patchAll innerContinues loopStart (csInstructions s)
        }
    void $ emitInstruction (IJump loopStart)
    exitAddr <- gets (InstructionPointer . csInstructionCounter)
    innerBreaks <- gets csBreakJumps
    modify $ \s ->
      s
        { csBreakJumps = outerBreaks,
          csContinueJumps = outerContinues,
          csInstructions =
            patchAll innerBreaks exitAddr $
              patchJump (csInstructions s) jmpFalseAddr exitAddr
        }
  StmtFor mInit mCond mPost body -> do
    compileForInit mInit
    loopStart <- gets (InstructionPointer . csInstructionCounter)
    mJmpExit <- case mCond of
      Nothing -> return Nothing
      Just condExpr -> do
        compileExpr (unLocated condExpr)
        addr <- emitInstruction (IJumpFalse (InstructionPointer 0))
        return (Just addr)
    outerBreaks <- gets csBreakJumps
    outerContinues <- gets csContinueJumps
    modify $ \s -> s {csBreakJumps = [], csContinueJumps = []}
    compileBlock body
    continueTarget <- gets (InstructionPointer . csInstructionCounter)
    innerContinues <- gets csContinueJumps
    modify $ \s ->
      s
        { csContinueJumps = [],
          csInstructions = patchAll innerContinues continueTarget (csInstructions s)
        }
    case mPost of
      Nothing -> return ()
      Just postStmt -> compileStmt (unLocated postStmt)
    void $ emitInstruction (IJump loopStart)
    exitAddr <- gets (InstructionPointer . csInstructionCounter)
    innerBreaks <- gets csBreakJumps
    modify $ \s ->
      s
        { csBreakJumps = outerBreaks,
          csContinueJumps = outerContinues,
          csInstructions =
            patchAll innerBreaks exitAddr $
              maybe id (\a is -> patchJump is a exitAddr) mJmpExit $
                csInstructions s
        }
  StmtReturn mExpr -> do
    case mExpr of
      Just expr -> compileExpr (unLocated expr)
      Nothing -> void $ emitInstruction (IPush VUnit)
    void $ emitInstruction IRet
  StmtBreak -> do
    addr <- emitInstruction (IJump (InstructionPointer 0))
    modify $ \s -> s {csBreakJumps = addr : csBreakJumps s}
  StmtContinue -> do
    addr <- emitInstruction (IJump (InstructionPointer 0))
    modify $ \s -> s {csContinueJumps = addr : csContinueJumps s}
  StmtBlock block -> compileBlock block

-- ---------------------------------------------------------------------------
-- For-loop init

compileForInit :: Maybe (ForInit ann) -> Compile ()
compileForInit Nothing = return ()
compileForInit (Just (ForInitDecl name _qtype initExpr)) = do
  compileExpr (unLocated initExpr)
  void $ emitInstruction (IStore (unLocated name))
  addToScope (unLocated name)
compileForInit (Just (ForInitExpr expr)) = do
  compileExpr (unLocated expr)
  void $ emitInstruction IPop

-- ---------------------------------------------------------------------------
-- LValue read-for-write (uses IArrayGetOrNew so missing inner arrays are
-- auto-created on writes like matrix[0][1] = x)

compileLValueReadForWrite :: Located (LValue ann) -> Located (LValue ann) -> Compile ()
compileLValueReadForWrite outerLoc inner = case unLocated inner of
  LVarRef var ->
    void $ emitInstruction (ILoad (unLocated var))
  LArrayIndex base indexExpr -> do
    compileLValueReadForWrite outerLoc base
    compileExpr (unLocated indexExpr)
    void $ emitInstruction IArrayGetOrNew
  LFieldAccess base locField -> do
    compileLValueReadForWrite outerLoc base
    void $ emitInstruction (IFieldGet (unLocated locField))

-- ---------------------------------------------------------------------------
-- Expression

compileExpr :: Expr ann -> Compile ()
compileExpr = \case
  ExprLiteral lit -> compileLiteral lit
  ExprVar var -> do
    scope <- gets csScope
    let vname = unLocated var
    unless (Set.member vname scope) $
      throwError $
        UndefinedVariable (locSpan var) vname
    void $ emitInstruction (ILoad vname)
  ExprBinary binOp left right -> do
    compileExpr (unLocated left)
    compileExpr (unLocated right)
    void $ emitInstruction (IBinary (astBinaryOpToBytecode binOp))
  ExprUnary unaryOp expr ->
    case unaryOp of
      Op.OpPos -> compileExpr (unLocated expr)
      _ -> do
        compileExpr (unLocated expr)
        void $ emitInstruction (IUnary (astUnaryOpToBytecode unaryOp))
  ExprCall funcName args -> do
    knownFns <- gets csKnownFunctions
    let fname = unLocated funcName
    unless (isKnownFunction fname knownFns) $
      throwError $
        UndefinedFunction (locSpan funcName) fname
    mapM_ (compileExpr . unLocated) (reverse args)
    void $ emitInstruction (ICall (FunctionRef fname) (length args))
  ExprIndex arrExpr indexExpr -> do
    compileExpr (unLocated arrExpr)
    compileExpr (unLocated indexExpr)
    void $ emitInstruction IArrayGet
  ExprField structExpr locField -> do
    compileExpr (unLocated structExpr)
    void $ emitInstruction (IFieldGet (unLocated locField))
  ExprStructInit _ fieldExprs -> do
    void $ emitInstruction INewStruct
    forM_ fieldExprs $ \(locFname, locExpr) -> do
      void $ emitInstruction IDup
      compileExpr (unLocated locExpr)
      void $ emitInstruction (IFieldSet (unLocated locFname))
  ExprArrayInit _typ elems -> do
    void $ emitInstruction INewArray
    mapM_
      ( \(i, elemExpr) -> do
          void $ emitInstruction IDup
          void $ emitInstruction (IPush (VInt i))
          compileExpr (unLocated elemExpr)
          void $ emitInstruction IArraySet
      )
      (zip [0 ..] elems)
  ExprError (Located _ ename) fields -> do
    mapM_ (\(_, valExpr) -> compileExpr (unLocated valExpr)) fields
    let fnames = map (unLocated . fst) fields
    void $ emitInstruction (INewError ename fnames)
  ExprTry innerExpr -> do
    compileExpr (unLocated innerExpr)
    void $ emitInstruction ITryOp
  ExprMust innerExpr -> do
    compileExpr (unLocated innerExpr)
    void $ emitInstruction IMustOp
  ExprParen expr -> compileExpr (unLocated expr)
  ExprCast expr castType -> do
    compileExpr (unLocated expr)
    let ct = case unLocated castType of
          TypePrimitive (PrimInt _) -> CastToInt
          TypePrimitive (PrimFloat _) -> CastToFloat
          TypePrimitive PrimBool -> CastToBool
          TypePrimitive PrimString -> CastToString
          _ -> CastToInt
    void $ emitInstruction (ICast ct)

-- ---------------------------------------------------------------------------
-- Literal

compileLiteral :: Literal (Located (Expr ann)) -> Compile ()
compileLiteral = \case
  LitInt intLit ->
    void $ emitInstruction (IPush (VInt (intValue intLit)))
  LitFloat floatLit ->
    void $ emitInstruction (IPush (VFloat (floatValue floatLit)))
  LitString stringLit -> do
    idx <- registerString (stringValue stringLit)
    void $ emitInstruction (IPush (VStringRef idx))
  LitBool boolLit ->
    void $ emitInstruction (IPush (VBool (boolValue boolLit)))
  LitArray (ArrayLiteral elems) -> do
    void $ emitInstruction INewArray
    mapM_
      ( \(i, elemExpr) -> do
          void $ emitInstruction IDup
          void $ emitInstruction (IPush (VInt i))
          compileExpr (unLocated elemExpr)
          void $ emitInstruction IArraySet
      )
      (zip [0 ..] elems)

-- ---------------------------------------------------------------------------
-- Operator mapping

astBinaryOpToBytecode :: Op.BinaryOp -> BinaryOp
astBinaryOpToBytecode = \case
  Op.OpAdd -> BOpAdd
  Op.OpSub -> BOpSub
  Op.OpMul -> BOpMul
  Op.OpDiv -> BOpDiv
  Op.OpMod -> BOpMod
  Op.OpEq -> BOpEq
  Op.OpNeq -> BOpNeq
  Op.OpLt -> BOpLt
  Op.OpLte -> BOpLte
  Op.OpGt -> BOpGt
  Op.OpGte -> BOpGte
  Op.OpAnd -> BOpAnd
  Op.OpOr -> BOpOr
  Op.OpBitAnd -> BOpBitAnd
  Op.OpBitOr -> BOpBitOr
  Op.OpBitXor -> BOpBitXor
  Op.OpShl -> BOpShl
  Op.OpShr -> BOpShr

astUnaryOpToBytecode :: Op.UnaryOp -> UnaryOp
astUnaryOpToBytecode = \case
  Op.OpNot -> UOpNot
  Op.OpNeg -> UOpNeg
  Op.OpBitNot -> UOpBitNot
  Op.OpPos -> UOpNeg -- unreachable: handled as identity in compileExpr

patchJump :: [Instruction] -> InstructionPointer -> InstructionPointer -> [Instruction]
patchJump instrs (InstructionPointer addr) newTarget =
  case splitAt addr instrs of
    (before, instr : after) ->
      let patched = case instr of
            IJump _ -> IJump newTarget
            IJumpTrue _ -> IJumpTrue newTarget
            IJumpFalse _ -> IJumpFalse newTarget
            other -> other
       in before ++ [patched] ++ after
    _ -> instrs
