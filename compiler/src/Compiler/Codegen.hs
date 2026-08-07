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
    FFIDecl (..),
    FFIFuncDecl (..),
    ForInit (..),
    FunctionDecl (..),
    ImplDecl (..),
    ImplForDecl (..),
    LValue (..),
    MatchArm (..),
    MatchPattern (..),
    Program (..),
    Stmt (..),
    Visibility (..),
  )
import AST.Types.Common
  ( ErrorName (..),
    FieldName (..),
    FuncName (..),
    Line (..),
    Located (..),
    SourcePos (..),
    SourceSpan (..),
    TypeName (..),
    VarName (..),
    locSpan,
    unLocated,
    unTypeName,
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
import AST.Types.Type (FunctionType (..), PrimitiveType (..), QualifiedType (..), Type (..), paramName, paramVariadic, qualType)
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
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State
  ( State,
    evalState,
    get,
    gets,
    modify,
    put,
  )
import Data.List (find)
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
  | PrivateFunction SourceSpan FuncName
  | UnsupportedConstruct SourceSpan String
  | TypeMismatch SourceSpan String
  deriving stock (Show, Eq)

errSpan :: CompileError -> SourceSpan
errSpan (UndefinedVariable s _) = s
errSpan (UndefinedFunction s _) = s
errSpan (PrivateFunction s _) = s
errSpan (UnsupportedConstruct s _) = s
errSpan (TypeMismatch s _) = s

errorMessage :: CompileError -> String
errorMessage (UndefinedVariable _ v) =
  "undefined variable `" ++ T.unpack (unVarName v) ++ "`"
errorMessage (UndefinedFunction _ f) =
  "undefined function `" ++ T.unpack (unFuncName f) ++ "`"
errorMessage (PrivateFunction _ f) =
  "function `" ++ T.unpack (unFuncName f) ++ "` is private (declared `static`) and cannot be called from outside its module"
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
    || unFuncName fname `elem` ["assert", "assert_eq", "fail"]
    || any (`T.isPrefixOf` unFuncName fname) ["math.", "string.", "sys.", "io.", "file.", "buf.", "dict.", "array.", "json.", "socket.", "regex.", "ptr."]

-- | Map a Quant return type to the C return type tag used in ICallFFI.
-- Only primitive types are supported; anything else is a compile error.
toCRetType :: QualifiedType -> CRetType
toCRetType (QualifiedType _ (TypePrimitive PrimNone)) = CRetVoid
toCRetType (QualifiedType _ (TypePrimitive (PrimInt _))) = CRetInt
toCRetType (QualifiedType _ (TypePrimitive (PrimFloat _))) = CRetFloat
toCRetType (QualifiedType _ (TypePrimitive PrimBool)) = CRetBool
toCRetType (QualifiedType _ (TypePrimitive PrimString)) = CRetStr
toCRetType (QualifiedType _ (TypePrimitive PrimPtr)) = CRetPtr
toCRetType _ = CRetVoid

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
    csFuncTypes :: Map FuncName FunctionType,
    csTempCount :: Int,
    -- visibility
    csCurrentFunc :: FuncName,
    csPrivateFunctions :: Set FuncName,
    -- Map from bare-alias name -> "modname." for bare aliases created by import.
    -- A bare alias inherits its origin module's visibility rights.
    csOriginModules :: Map FuncName Text,
    -- FFI function table: name -> (lib path, C return type)
    csFfiFuncs :: Map FuncName (Text, CRetType),
    -- Bytecodes for lambda functions compiled inline (collected, emitted at end)
    csLambdaBytecodes :: [Bytecode],
    -- Maps method-name span to the resolved function name (from the type checker)
    csMethodCallMap :: Map SourceSpan FuncName,
    -- Receiver spans of enum-variant accesses (Direction.North); emit INewError
    csEnumVariantSpans :: Set SourceSpan,
    -- Maps method-name span to the unqualified method name for dynamic dispatch
    csDynMethodCallMap :: Map SourceSpan Text
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
      csFuncTypes = Map.empty,
      csTempCount = 0,
      csCurrentFunc = FuncName "",
      csPrivateFunctions = Set.empty,
      csOriginModules = Map.empty,
      csFfiFuncs = Map.empty,
      csLambdaBytecodes = [],
      csMethodCallMap = Map.empty,
      csEnumVariantSpans = Set.empty,
      csDynMethodCallMap = Map.empty
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

compileProgram :: Map SourceSpan FuncName -> Set SourceSpan -> Map SourceSpan Text -> Program ann -> Either CompileError [Bytecode]
compileProgram methodCallMap enumVariantSpans dynMethodMap (Program decls) =
  evalState (runExceptT go) (initialState {csMethodCallMap = methodCallMap, csEnumVariantSpans = enumVariantSpans, csDynMethodCallMap = dynMethodMap})
  where
    go :: Compile [Bytecode]
    go = do
      let visDecls =
            [ (vis, fd)
              | decl <- decls,
                let d = unLocated decl,
                DeclFunction vis fd <- [d]
            ]
          implDecls =
            [ (vis, fd)
              | decl <- decls,
                let d = unLocated decl,
                DeclImpl vis idecl <- [d],
                Located _ fd <- implMethods idecl
            ]
          implForDecls =
            [ (vis, fd)
              | decl <- decls,
                let d = unLocated decl,
                DeclImplFor vis ifdecl <- [d],
                Located _ fd <- implForMethods ifdecl
            ]
          funcs = map snd visDecls ++ map snd implDecls ++ map snd implForDecls
          -- Static functions whose names contain '.' were imported from another
          -- module and are private helpers; calls to them from outside that
          -- module are rejected at the ExprCall site.
          privateFuncs =
            Set.fromList
              [ unLocated (funcDeclName fd)
                | (Static, fd) <- visDecls,
                  "." `T.isInfixOf` unFuncName (unLocated (funcDeclName fd))
              ]
          -- Bare aliases are Public functions without a '.' in their name that
          -- were generated by the import resolver from a prefixed version.
          -- They inherit the visibility rights of their origin module so that
          -- internal calls to private helpers still compile correctly.
          originMods =
            Map.fromList
              [ (bareName, modPrefix)
                | (Public, fd) <- visDecls,
                  let bareName = unLocated (funcDeclName fd),
                  let bareText = unFuncName bareName,
                  not ("." `T.isInfixOf` bareText),
                  (_, fd2) <- visDecls,
                  let prefText = unFuncName (unLocated (funcDeclName fd2)),
                  "." `T.isInfixOf` prefText,
                  ("." <> bareText) `T.isSuffixOf` prefText,
                  let modPrefix = fst (T.breakOnEnd "." prefText)
              ]
      -- First pass: register all user-defined function names so forward
      -- references and mutual recursion work.
      let userNames = Set.fromList [unLocated (funcDeclName fd) | fd <- funcs]
          funcTypes =
            Map.fromList
              [ (unLocated (funcDeclName fd), FunctionType (funcDeclParams fd) (funcDeclReturnType fd))
                | fd <- funcs
              ]
          -- Collect FFI declarations: name -> (lib, CRetType)
          ffiFuncMap =
            Map.fromList
              [ (unLocated (ffiFuncName ffd), (ffiLib fd, toCRetType (unLocated (ffiFuncReturnType ffd))))
                | Located _ (DeclFFI fd) <- decls,
                  ffd <- ffiFuncs fd
              ]
          ffiNames = Map.keysSet ffiFuncMap
      modify $ \s ->
        s
          { csKnownFunctions = userNames <> ffiNames,
            csFuncTypes = funcTypes,
            csPrivateFunctions = privateFuncs,
            csOriginModules = originMods,
            csFfiFuncs = ffiFuncMap
          }
      topLevelBcs <- mapM (`compileFunction` Set.empty) funcs
      lambdaBcs <- gets csLambdaBytecodes
      return (topLevelBcs ++ lambdaBcs)

-- ---------------------------------------------------------------------------
-- Function

compileFunction :: FunctionDecl ann -> Set VarName -> Compile Bytecode
compileFunction funcDecl captureNames = do
  let funcName = unLocated (funcDeclName funcDecl)
  oldState <- get
  put
    initialState
      { csKnownFunctions = csKnownFunctions oldState,
        csFuncTypes = csFuncTypes oldState,
        csCurrentFunc = funcName,
        csPrivateFunctions = csPrivateFunctions oldState,
        csOriginModules = csOriginModules oldState,
        csFfiFuncs = csFfiFuncs oldState,
        -- Thread lambda bytecodes and the global lambda counter through
        csLambdaBytecodes = csLambdaBytecodes oldState,
        csTempCount = csTempCount oldState,
        -- Thread the method call map through so ExprMethodCall can resolve names
        csMethodCallMap = csMethodCallMap oldState,
        -- Thread the enum variant span set through so ExprField can detect enum accesses
        csEnumVariantSpans = csEnumVariantSpans oldState,
        -- Thread dynamic dispatch map through
        csDynMethodCallMap = csDynMethodCallMap oldState
      }
  -- Parameters and captured variables are in scope from the start
  let paramNames = [paramName p | Located _ p <- funcDeclParams funcDecl]
  modify $ \s -> s {csScope = Set.fromList paramNames `Set.union` captureNames}
  mapM_ (\(Located _ p) -> void $ emitInstruction (IStore (paramName p))) (funcDeclParams funcDecl)
  compileBlock (funcDeclBody funcDecl)
  void $ emitInstruction (IPush VUnit)
  void $ emitInstruction IRet
  innerState <- get
  -- Restore outer state but keep lambdas and the updated counter
  put oldState {csLambdaBytecodes = csLambdaBytecodes innerState, csTempCount = csTempCount innerState}
  return
    Bytecode
      { bytecodeFunction = funcName,
        bytecodeInstructions = csInstructions innerState,
        bytecodeEntry = InstructionPointer 0,
        bytecodeStrings = csStringList innerState
      }

-- ---------------------------------------------------------------------------
-- Block / Statement

compileBlock :: Block ann -> Compile ()
compileBlock (Block _span stmts) = mapM_ compileLocatedStmt stmts

compileLocatedStmt :: Located (Stmt ann) -> Compile ()
compileLocatedStmt (Located span stmt) = do
  let lineNo = unLine (posLine (spanStart span))
  void $ emitInstruction (ICovMark lineNo)
  case stmt of
    StmtIf condExpr thenBlock mElseBlock ->
      compileBranchIf lineNo condExpr thenBlock mElseBlock
    StmtWhile condExpr body ->
      compileBranchWhile lineNo condExpr body
    _ -> compileStmt stmt

-- | Instrumented @if@ compilation that emits an 'ICovBranch' probe after the
-- condition so that the coverage report can distinguish "condition always true"
-- from "condition always false".
compileBranchIf :: Int -> Located (Expr ann) -> Block ann -> Maybe (Block ann) -> Compile ()
compileBranchIf branchId condExpr thenBlock mElseBlock = do
  compileExpr (unLocated condExpr)
  void $ emitInstruction (ICovBranch branchId)
  jmpFalseAddr <- emitInstruction (IJumpFalse (InstructionPointer 0))
  compileBlock thenBlock
  jmpEndAddr <- emitInstruction (IJump (InstructionPointer 0))
  falseTarget <- gets (InstructionPointer . csInstructionCounter)
  modify $ \s -> s {csInstructions = patchJump (csInstructions s) jmpFalseAddr falseTarget}
  mapM_ compileBlock mElseBlock
  endTarget <- gets (InstructionPointer . csInstructionCounter)
  modify $ \s -> s {csInstructions = patchJump (csInstructions s) jmpEndAddr endTarget}

-- | Instrumented @while@ compilation that emits an 'ICovBranch' probe after
-- the condition on every iteration.
compileBranchWhile :: Int -> Located (Expr ann) -> Block ann -> Compile ()
compileBranchWhile branchId condExpr body = do
  loopStart <- gets (InstructionPointer . csInstructionCounter)
  compileExpr (unLocated condExpr)
  void $ emitInstruction (ICovBranch branchId)
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
          TypeOption _ ->
            void $ emitInstruction (INewError (ErrorName "None") [])
          TypeDict _ _ ->
            void $ emitInstruction INewDict
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
  StmtMatch subj arms -> compileMatch subj arms
  StmtTupleDecl vars _qt initExpr -> do
    n <- gets csTempCount
    modify $ \s -> s {csTempCount = n + 1}
    let tmpVar = VarName (T.pack ("__tup_" ++ show n))
    compileExpr (unLocated initExpr)
    void $ emitInstruction (IStore tmpVar)
    addToScope tmpVar
    forM_ (zip [0 ..] vars) $ \(i, locVar) -> do
      let v = unLocated locVar
          fn = FieldName (T.pack ("_" ++ show (i :: Int)))
      void $ emitInstruction (ILoad tmpVar)
      void $ emitInstruction (IFieldGet fn)
      void $ emitInstruction (IStore v)
      addToScope v
  StmtStructDecl fields _qt initExpr -> do
    n <- gets csTempCount
    modify $ \s -> s {csTempCount = n + 1}
    let tmpVar = VarName (T.pack ("__struct_" ++ show n))
    compileExpr (unLocated initExpr)
    void $ emitInstruction (IStore tmpVar)
    addToScope tmpVar
    forM_ fields $ \(Located _ fn) -> do
      let v = VarName (unFieldName fn)
      void $ emitInstruction (ILoad tmpVar)
      void $ emitInstruction (IFieldGet fn)
      void $ emitInstruction (IStore v)
      addToScope v

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
-- Free-variable analysis (for closure capture)

freeVarsExpr :: Expr ann -> Set VarName
freeVarsExpr = \case
  ExprLiteral lit -> foldMap (freeVarsExpr . unLocated) lit
  ExprVar v -> Set.singleton (unLocated v)
  ExprBinary _ l r -> freeVarsExpr (unLocated l) <> freeVarsExpr (unLocated r)
  ExprUnary _ e -> freeVarsExpr (unLocated e)
  -- Also treat the callee name as a potential variable reference: when the
  -- called name is a local (function-typed variable), it must be captured.
  ExprCall (Located _ fname) args ->
    Set.singleton (VarName (unFuncName fname)) <> foldMap (freeVarsExpr . unLocated) args
  ExprIndex arr idx -> freeVarsExpr (unLocated arr) <> freeVarsExpr (unLocated idx)
  ExprField e _ -> freeVarsExpr (unLocated e)
  ExprStructInit _ fields -> foldMap (freeVarsExpr . unLocated . snd) fields
  ExprEnumVariantInit _ _ fields -> foldMap (freeVarsExpr . unLocated . snd) fields
  ExprArrayInit _ elems -> foldMap (freeVarsExpr . unLocated) elems
  ExprDictLit pairs -> foldMap (\(k, v) -> freeVarsExpr (unLocated k) <> freeVarsExpr (unLocated v)) pairs
  ExprError _ fields -> foldMap (freeVarsExpr . unLocated . snd) fields
  ExprTry e -> freeVarsExpr (unLocated e)
  ExprMust e -> freeVarsExpr (unLocated e)
  ExprSome e -> freeVarsExpr (unLocated e)
  ExprLambda _ _ b -> freeVarsBlock b
  ExprTupleInit elems -> foldMap (freeVarsExpr . unLocated) elems
  ExprMethodCall recv _ args ->
    freeVarsExpr (unLocated recv) <> foldMap (freeVarsExpr . unLocated) args
  ExprParen e -> freeVarsExpr (unLocated e)
  ExprCast e _ -> freeVarsExpr (unLocated e)
  ExprNone -> Set.empty

freeVarsBlock :: Block ann -> Set VarName
freeVarsBlock (Block _ stmts) = foldMap (freeVarsStmt . unLocated) stmts

freeVarsStmt :: Stmt ann -> Set VarName
freeVarsStmt = \case
  StmtVarDecl _ _ me -> maybe Set.empty (freeVarsExpr . unLocated) me
  StmtAssign lv _ e -> freeVarsLVal (unLocated lv) <> freeVarsExpr (unLocated e)
  StmtExpr e -> freeVarsExpr (unLocated e)
  StmtIf c t me -> freeVarsExpr (unLocated c) <> freeVarsBlock t <> maybe Set.empty freeVarsBlock me
  StmtWhile c b -> freeVarsExpr (unLocated c) <> freeVarsBlock b
  StmtFor fi fc fs b ->
    maybe Set.empty freeVarsForInit fi
      <> maybe Set.empty (freeVarsExpr . unLocated) fc
      <> maybe Set.empty (freeVarsStmt . unLocated) fs
      <> freeVarsBlock b
  StmtReturn me -> maybe Set.empty (freeVarsExpr . unLocated) me
  StmtBreak -> Set.empty
  StmtContinue -> Set.empty
  StmtBlock b -> freeVarsBlock b
  StmtMatch e arms -> freeVarsExpr (unLocated e) <> foldMap freeVarsMatchArm arms
  StmtTupleDecl _ _ e -> freeVarsExpr (unLocated e)
  StmtStructDecl _ _ e -> freeVarsExpr (unLocated e)

freeVarsLVal :: LValue ann -> Set VarName
freeVarsLVal = \case
  LVarRef v -> Set.singleton (unLocated v)
  LArrayIndex lv idx -> freeVarsLVal (unLocated lv) <> freeVarsExpr (unLocated idx)
  LFieldAccess lv _ -> freeVarsLVal (unLocated lv)

freeVarsForInit :: ForInit ann -> Set VarName
freeVarsForInit = \case
  ForInitDecl _ _ e -> freeVarsExpr (unLocated e)
  ForInitExpr e -> freeVarsExpr (unLocated e)

freeVarsMatchArm :: MatchArm ann -> Set VarName
freeVarsMatchArm (MatchArm pat body) = freeVarsPat pat <> freeVarsStmt (unLocated body)

freeVarsPat :: MatchPattern ann -> Set VarName
freeVarsPat = \case
  MatchLit e -> freeVarsExpr (unLocated e)
  MatchRange e1 e2 -> freeVarsExpr (unLocated e1) <> freeVarsExpr (unLocated e2)
  MatchTuple _ -> Set.empty
  MatchStruct _ -> Set.empty
  _ -> Set.empty

-- ---------------------------------------------------------------------------
-- Expression

compileExpr :: Expr ann -> Compile ()
compileExpr = \case
  ExprLiteral lit -> compileLiteral lit
  ExprVar var -> do
    scope <- gets csScope
    knownFns <- gets csKnownFunctions
    let vname = unLocated var
    if Set.member vname scope
      then void $ emitInstruction (ILoad vname)
      else do
        -- Check if it's a known function being used as a first-class value
        let fname = FuncName (unVarName vname)
        if isKnownFunction fname knownFns
          then void $ emitInstruction (ILoadFunc fname)
          else throwError $ UndefinedVariable (locSpan var) vname
  ExprBinary binOp left right -> do
    methodMap <- gets csMethodCallMap
    case Map.lookup (locSpan left) methodMap of
      Just qualFname -> do
        compileExpr (unLocated right)
        compileExpr (unLocated left)
        void $ emitInstruction (ICall (FunctionRef qualFname) 2)
      Nothing -> do
        compileExpr (unLocated left)
        compileExpr (unLocated right)
        void $ emitInstruction (IBinary (astBinaryOpToBytecode binOp))
  ExprUnary unaryOp expr ->
    case unaryOp of
      Op.OpPos -> compileExpr (unLocated expr)
      _ -> do
        methodMap <- gets csMethodCallMap
        case Map.lookup (locSpan expr) methodMap of
          Just qualFname -> do
            compileExpr (unLocated expr)
            void $ emitInstruction (ICall (FunctionRef qualFname) 1)
          Nothing -> do
            compileExpr (unLocated expr)
            void $ emitInstruction (IUnary (astUnaryOpToBytecode unaryOp))
  ExprCall funcName args -> do
    knownFns <- gets csKnownFunctions
    funcTypes <- gets csFuncTypes
    ffiMap <- gets csFfiFuncs
    scope <- gets csScope
    let fname = unLocated funcName
        vname = VarName (unFuncName fname)
    if not (isKnownFunction fname knownFns) && Set.member vname scope
      then do
        -- Indirect call through a function-typed variable
        void $ emitInstruction (ILoad vname)
        mapM_ (compileExpr . unLocated) (reverse args)
        void $ emitInstruction (ICallIndirect (length args))
      else case Map.lookup fname ffiMap of
        Just (lib, retTy) -> do
          -- FFI call: push args then emit ICallFFI
          mapM_ (compileExpr . unLocated) (reverse args)
          void $ emitInstruction (ICallFFI lib (unFuncName fname) retTy (length args))
        Nothing -> do
          unless (isKnownFunction fname knownFns) $
            throwError $
              UndefinedFunction (locSpan funcName) fname
          -- Reject cross-module calls to static (private) imported functions.
          -- Bare-alias functions (e.g. `double` as alias of `mymod.double`) inherit
          -- their origin module's visibility rights via csOriginModules.
          privateFns <- gets csPrivateFunctions
          when (Set.member fname privateFns) $ do
            curFunc <- gets csCurrentFunc
            originMods <- gets csOriginModules
            let moduleOf n = fst (T.breakOnEnd "." (unFuncName n))
                callerMod = Map.findWithDefault (moduleOf curFunc) curFunc originMods
                calleeMod = moduleOf fname
            when (callerMod /= calleeMod) $
              throwError $
                PrivateFunction (locSpan funcName) fname
          case Map.lookup fname funcTypes >>= find (paramVariadic . unLocated) . funcParams of
            Nothing -> do
              mapM_ (compileExpr . unLocated) (reverse args)
              void $ emitInstruction (ICall (FunctionRef fname) (length args))
            Just _ -> do
              let ft = funcTypes Map.! fname
                  regularParams = filter (not . paramVariadic . unLocated) (funcParams ft)
                  nRegular = length regularParams
                  regularArgs = take nRegular args
                  varArgs = drop nRegular args
              void $ emitInstruction INewArray
              forM_ (zip [0 ..] varArgs) $ \(i, argExpr) -> do
                void $ emitInstruction IDup
                void $ emitInstruction (IPush (VInt i))
                compileExpr (unLocated argExpr)
                void $ emitInstruction IArraySet
              mapM_ (compileExpr . unLocated) (reverse regularArgs)
              void $ emitInstruction (ICall (FunctionRef fname) (nRegular + 1))
  ExprIndex arrExpr indexExpr -> do
    compileExpr (unLocated arrExpr)
    compileExpr (unLocated indexExpr)
    void $ emitInstruction IArrayGet
  ExprField structExpr locField -> do
    enumSpans <- gets csEnumVariantSpans
    if Set.member (locSpan structExpr) enumSpans
      then void $ emitInstruction (INewError (ErrorName (unFieldName (unLocated locField))) [])
      else do
        compileExpr (unLocated structExpr)
        void $ emitInstruction (IFieldGet (unLocated locField))
  ExprStructInit locTypeName fieldExprs -> do
    void $ emitInstruction (INewStruct (unLocated locTypeName))
    forM_ fieldExprs $ \(locFname, locExpr) -> do
      void $ emitInstruction IDup
      compileExpr (unLocated locExpr)
      void $ emitInstruction (IFieldSet (unLocated locFname))
  ExprEnumVariantInit _ (Located _ variantName) fieldExprs -> do
    mapM_ (compileExpr . unLocated . snd) fieldExprs
    let fnames = map (unLocated . fst) fieldExprs
    void $ emitInstruction (INewError (ErrorName (unTypeName variantName)) fnames)
  ExprDictLit pairs -> do
    void $ emitInstruction INewDict
    mapM_
      ( \(keyExpr, valExpr) -> do
          void $ emitInstruction IDup
          compileExpr (unLocated keyExpr)
          compileExpr (unLocated valExpr)
          void $ emitInstruction IArraySet
      )
      pairs
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
  ExprSome innerExpr -> compileExpr (unLocated innerExpr)
  ExprNone -> void $ emitInstruction (INewError (ErrorName "None") [])
  ExprLambda params retType body -> do
    n <- gets csTempCount
    modify $ \s -> s {csTempCount = n + 1}
    let lambdaName = FuncName (T.pack ("__lambda_" ++ show n))
        paramSet = Set.fromList [paramName p | Located _ p <- params]
        lambdaDecl =
          FunctionDecl
            { funcDeclName = Located (blockSpan body) lambdaName,
              funcDeclTypeParams = [],
              funcDeclTypeBounds = [],
              funcDeclParams = params,
              funcDeclReturnType = retType,
              funcDeclBody = body
            }
    -- Register the lambda so it can be called
    modify $ \s ->
      s
        { csKnownFunctions = Set.insert lambdaName (csKnownFunctions s),
          csFuncTypes = Map.insert lambdaName (FunctionType params retType) (csFuncTypes s)
        }
    -- Compute captures: free vars in body that are not params but are in the current scope
    outerScope <- gets csScope
    let referenced = freeVarsBlock body
        captures = Set.toList (Set.intersection (Set.difference referenced paramSet) outerScope)
    bc <- compileFunction lambdaDecl (Set.fromList captures)
    modify $ \s -> s {csLambdaBytecodes = csLambdaBytecodes s ++ [bc]}
    if null captures
      then void $ emitInstruction (ILoadFunc lambdaName)
      else void $ emitInstruction (IMakeClosure lambdaName captures)
  ExprTupleInit elems -> do
    void $ emitInstruction (INewStruct (TypeName "__tuple__"))
    forM_ (zip [0 ..] elems) $ \(i, elemExpr) -> do
      void $ emitInstruction IDup
      compileExpr (unLocated elemExpr)
      void $ emitInstruction (IFieldSet (FieldName (T.pack ("_" ++ show (i :: Int)))))
  ExprMethodCall receiver (Located methodSp rawMethod) args -> do
    methodMap <- gets csMethodCallMap
    dynMap <- gets csDynMethodCallMap
    case Map.lookup methodSp methodMap of
      Just resolvedFname -> do
        -- Push extra args reversed, then receiver on top (self = first param = top of stack)
        mapM_ (compileExpr . unLocated) (reverse args)
        compileExpr (unLocated receiver)
        void $ emitInstruction (ICall (FunctionRef resolvedFname) (1 + length args))
      Nothing -> case Map.lookup methodSp dynMap of
        Just methodName -> do
          -- Dynamic dispatch: push args reversed, then receiver (TOS used for type lookup)
          mapM_ (compileExpr . unLocated) (reverse args)
          compileExpr (unLocated receiver)
          void $ emitInstruction (IDynMethodCall methodName (1 + length args))
        Nothing -> do
          -- Module/qualified call: receiver text becomes part of the name
          let qualFname = FuncName (qualPrefix (unLocated receiver) <> unFuncName rawMethod)
          mapM_ (compileExpr . unLocated) (reverse args)
          void $ emitInstruction (ICall (FunctionRef qualFname) (length args))
    where
      qualPrefix (ExprVar (Located _ v)) = unVarName v <> "."
      qualPrefix (ExprField (Located _ e) (Located _ f)) =
        qualPrefix e <> unFieldName f <> "."
      qualPrefix _ = ""
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
-- Match statement

compileMatch :: Located (Expr ann) -> [MatchArm ann] -> Compile ()
compileMatch subj arms = do
  compileExpr (unLocated subj)
  tmpN <- gets csTempCount
  modify $ \s -> s {csTempCount = tmpN + 1}
  let subjVar = VarName (T.pack ("__match_subj_" ++ show tmpN))
  void $ emitInstruction (IStore subjVar)
  addToScope subjVar
  exitJumps <- mapM (compileMatchArm subjVar) arms
  exitTarget <- gets (InstructionPointer . csInstructionCounter)
  modify $ \s ->
    s {csInstructions = patchAll (concat exitJumps) exitTarget (csInstructions s)}

-- | Compile one match arm.  Returns the list of IJump-to-exit instruction
-- pointers emitted at the end of the arm body; the caller patches them all
-- to the instruction after the entire match statement once all arms are done.
compileMatchArm :: VarName -> MatchArm ann -> Compile [InstructionPointer]
compileMatchArm subjVar (MatchArm pat body) = case pat of
  MatchWildcard -> do
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    return [exitJmp]
  MatchOk locVar -> do
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction IIsOk
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    let v = unLocated locVar
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction (IStore v)
    addToScope v
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]
  MatchErr locEName locVar -> do
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction (IIsErr (unLocated locEName))
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    let v = unLocated locVar
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction (IStore v)
    addToScope v
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]
  MatchSome locVar -> do
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction IIsOk
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    let v = unLocated locVar
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction (IStore v)
    addToScope v
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]
  MatchNone -> do
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction (IIsErr (ErrorName "None"))
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]
  MatchLit litExpr -> do
    void $ emitInstruction (ILoad subjVar)
    compileExpr (unLocated litExpr)
    void $ emitInstruction (IBinary BOpEq)
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]
  MatchRange loExpr hiExpr -> do
    -- condition: subject >= lo && subject <= hi
    void $ emitInstruction (ILoad subjVar)
    compileExpr (unLocated loExpr)
    void $ emitInstruction (IBinary BOpGte)
    void $ emitInstruction (ILoad subjVar)
    compileExpr (unLocated hiExpr)
    void $ emitInstruction (IBinary BOpLte)
    void $ emitInstruction (IBinary BOpAnd)
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]
  MatchTuple vars -> do
    -- Tuple patterns always match (type-checked statically); bind each element
    forM_ (zip [0 ..] vars) $ \(i, locVar) -> do
      let v = unLocated locVar
          fn = FieldName (T.pack ("_" ++ show (i :: Int)))
      void $ emitInstruction (ILoad subjVar)
      void $ emitInstruction (IFieldGet fn)
      void $ emitInstruction (IStore v)
      addToScope v
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    return [exitJmp]
  MatchStruct fields -> do
    -- Struct patterns always match; bind each named field as a variable
    forM_ fields $ \(Located _ fn) -> do
      let v = VarName (unFieldName fn)
      void $ emitInstruction (ILoad subjVar)
      void $ emitInstruction (IFieldGet fn)
      void $ emitInstruction (IStore v)
      addToScope v
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    return [exitJmp]
  MatchEnumVariant _ (Located _ vname) fieldBindings -> do
    void $ emitInstruction (ILoad subjVar)
    void $ emitInstruction (IIsErr (ErrorName (unTypeName vname)))
    falseJmp <- emitInstruction (IJumpFalse (InstructionPointer 0))
    forM_ fieldBindings $ \(Located _ fname, Located _ var) -> do
      void $ emitInstruction (ILoad subjVar)
      void $ emitInstruction (IFieldGet fname)
      void $ emitInstruction (IStore var)
      addToScope var
    compileLocatedStmt body
    exitJmp <- emitInstruction (IJump (InstructionPointer 0))
    nextArm <- gets (InstructionPointer . csInstructionCounter)
    modify $ \s -> s {csInstructions = patchJump (csInstructions s) falseJmp nextArm}
    return [exitJmp]

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
