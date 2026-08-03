{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AST.Types.AST
  ( Program (..),
    programDecls,
    Visibility (..),
    Decl (..),
    FunctionDecl (..),
    StructDecl (..),
    ImplDecl (..),
    InterfaceMethodSig (..),
    InterfaceDecl (..),
    ImplForDecl (..),
    ModulePath (..),
    ImportTarget (..),
    ImportDecl (..),
    ErrorDecl (..),
    ErrorSetDecl (..),
    FFIFuncDecl (..),
    FFIDecl (..),
    Stmt (..),
    Block (..),
    ForInit (..),
    Expr (..),
    LValue (..),
    MatchPattern (..),
    MatchArm (..),
    LocatedExpr,
    LocatedStmt,
    LocatedDecl,
    LocatedProgram,
    exprSpan,
    stmtSpan,
    declSpan,
  )
where

import AST.Types.Common
  ( ErrorName,
    FieldName,
    FuncName,
    Located (..),
    ModuleName,
    SourceSpan,
    TypeName,
    VarName,
  )
import AST.Types.Literal
  ( Literal,
  )
import AST.Types.Operator
  ( AssignOp,
    BinaryOp,
    UnaryOp,
  )
import AST.Types.Type
  ( ErrorField,
    ErrorSetMember,
    Parameter,
    QualifiedType,
    StructField,
    Type,
  )
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Program ann = Program
  { unProgram :: [Located (Decl ann)]
  }
  deriving stock (Show, Eq, Generic)

programDecls :: Program ann -> [Located (Decl ann)]
programDecls = unProgram

data Visibility
  = Public
  | Static
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable Visibility

data Decl ann
  = DeclFunction Visibility (FunctionDecl ann)
  | DeclStruct Visibility StructDecl
  | DeclImpl Visibility (ImplDecl ann)
  | DeclInterface Visibility InterfaceDecl
  | DeclImplFor Visibility (ImplForDecl ann)
  | DeclImport ImportDecl
  | DeclError Visibility ErrorDecl
  | DeclErrorSet Visibility ErrorSetDecl
  | DeclFFI FFIDecl
  deriving stock (Show, Eq, Generic)

-- | A single function binding declared inside an @ffi@ block.
data FFIFuncDecl = FFIFuncDecl
  { ffiFuncName :: Located FuncName,
    ffiFuncParams :: [Located Parameter],
    ffiFuncReturnType :: Located QualifiedType
  }
  deriving stock (Show, Eq, Generic)

-- | Top-level @ffi "libpath" { fn … }@ declaration.
data FFIDecl = FFIDecl
  { ffiLib :: Text,
    ffiFuncs :: [FFIFuncDecl]
  }
  deriving stock (Show, Eq, Generic)

data FunctionDecl ann = FunctionDecl
  { funcDeclName :: Located FuncName,
    funcDeclTypeParams :: [Located TypeName],
    funcDeclParams :: [Located Parameter],
    funcDeclReturnType :: Located QualifiedType,
    funcDeclBody :: Block ann
  }
  deriving stock (Show, Eq, Generic)

data StructDecl = StructDecl
  { structDeclName :: Located TypeName,
    structDeclTypeParams :: [Located TypeName],
    structDeclFields :: [Located StructField]
  }
  deriving stock (Show, Eq, Generic)

data ImplDecl ann = ImplDecl
  { implTypeName :: Located TypeName,
    implMethods :: [Located (FunctionDecl ann)]
  }
  deriving stock (Show, Eq, Generic)

data InterfaceMethodSig = InterfaceMethodSig
  { ifaceMethodName :: Located FuncName,
    ifaceMethodParams :: [Located Parameter],
    ifaceMethodReturnType :: Located QualifiedType
  }
  deriving stock (Show, Eq, Generic)

data InterfaceDecl = InterfaceDecl
  { ifaceDeclName :: Located TypeName,
    ifaceDeclMethods :: [InterfaceMethodSig]
  }
  deriving stock (Show, Eq, Generic)

data ImplForDecl ann = ImplForDecl
  { implForIfaceName :: Located TypeName,
    implForTypeName :: Located TypeName,
    implForMethods :: [Located (FunctionDecl ann)]
  }
  deriving stock (Show, Eq, Generic)

newtype ModulePath = ModulePath
  { modulePathParts :: [Located ModuleName]
  }
  deriving stock (Show, Eq, Generic)

data ImportTarget
  = -- | Import the module itself: @import math@
    ImportAll
  | -- | Import specific names: @from math import sin, cos@
    ImportNames [Located VarName]
  | -- | Import everything: @from math import *@
    ImportWildcard
  deriving stock (Show, Eq, Generic)

data ImportDecl = ImportDecl
  { importPath :: ModulePath,
    importTarget :: ImportTarget
  }
  deriving stock (Show, Eq, Generic)

data ErrorDecl = ErrorDecl
  { errorDeclName :: Located ErrorName,
    -- | Optional fields
    errorDeclFields :: [Located ErrorField]
  }
  deriving stock (Show, Eq, Generic)

data ErrorSetDecl = ErrorSetDecl
  { errorSetDeclName :: Located ErrorName,
    errorSetDeclMembers :: [Located ErrorSetMember]
  }
  deriving stock (Show, Eq, Generic)

data Block ann = Block
  { -- | Span of entire block including braces
    blockSpan :: SourceSpan,
    -- | Statements in the block
    blockStmts :: [Located (Stmt ann)]
  }
  deriving stock (Show, Eq, Generic)

data Stmt ann
  = StmtVarDecl
      (Located VarName)
      (Located QualifiedType)
      -- | Optional initializer
      (Maybe (Located (Expr ann)))
  | StmtAssign
      (Located (LValue ann))
      AssignOp
      (Located (Expr ann))
  | StmtExpr (Located (Expr ann))
  | StmtIf
      (Located (Expr ann))
      (Block ann)
      (Maybe (Block ann))
  | StmtWhile
      (Located (Expr ann))
      (Block ann)
  | StmtFor
      (Maybe (ForInit ann))
      (Maybe (Located (Expr ann)))
      (Maybe (Located (Stmt ann)))
      (Block ann)
  | StmtReturn (Maybe (Located (Expr ann)))
  | StmtBreak
  | StmtContinue
  | StmtBlock (Block ann)
  | StmtMatch
      (Located (Expr ann))
      [MatchArm ann]
  | -- | Tuple destructuring: @(x, y): (int, str) = expr@
    StmtTupleDecl
      [Located VarName]
      (Located QualifiedType)
      (Located (Expr ann))
  deriving stock (Show, Eq, Generic)

-- | A single pattern in a match arm.
data MatchPattern ann
  = -- | Matches the success branch of an orerror value: @ok(v)@
    MatchOk (Located VarName)
  | -- | Matches a specific error: @err(ErrorName v)@
    MatchErr (Located ErrorName) (Located VarName)
  | -- | Matches the some branch of an option value: @some(v)@
    MatchSome (Located VarName)
  | -- | Matches the none branch of an option value: @none@
    MatchNone
  | -- | Matches a literal value (int, string, bool)
    MatchLit (Located (Expr ann))
  | -- | Matches an inclusive integer range: @lo..hi@
    MatchRange (Located (Expr ann)) (Located (Expr ann))
  | -- | Wildcard; matches anything
    MatchWildcard
  | -- | Tuple destructure pattern: @(x, y)@
    MatchTuple [Located VarName]
  deriving stock (Show, Eq, Generic)

-- | One arm of a match statement.
data MatchArm ann = MatchArm
  { matchArmPat :: MatchPattern ann,
    matchArmBody :: Located (Stmt ann)
  }
  deriving stock (Show, Eq, Generic)

data ForInit ann
  = ForInitDecl
      (Located VarName)
      (Located QualifiedType)
      (Located (Expr ann))
  | ForInitExpr (Located (Expr ann))
  deriving stock (Show, Eq, Generic)

data Expr ann
  = ExprLiteral (Literal (Located (Expr ann)))
  | ExprVar (Located VarName)
  | ExprBinary
      BinaryOp
      (Located (Expr ann))
      (Located (Expr ann))
  | ExprUnary
      UnaryOp
      (Located (Expr ann))
  | ExprCall
      (Located FuncName)
      [Located (Expr ann)]
  | ExprIndex
      (Located (Expr ann))
      (Located (Expr ann))
  | ExprField
      (Located (Expr ann))
      (Located FieldName)
  | ExprStructInit
      (Located TypeName)
      [(Located FieldName, Located (Expr ann))]
  | ExprArrayInit
      (Located Type)
      [Located (Expr ann)]
  | -- | Dict literal: @{ key: val, ... }@ or @{}@
    ExprDictLit
      [(Located (Expr ann), Located (Expr ann))]
  | ExprError
      (Located ErrorName)
      [(Located FieldName, Located (Expr ann))]
  | ExprTry (Located (Expr ann))
  | ExprMust (Located (Expr ann))
  | -- | Wrap a value in an option: @some(expr)@
    ExprSome (Located (Expr ann))
  | -- | The empty option value: @none@
    ExprNone
  | -- Anonymous function expression: fn(params) -> ret { body }
    ExprLambda
      [Located Parameter]
      (Located QualifiedType)
      (Block ann)
  | -- Parenthesized expression (for preserving source structure if needed)
    ExprParen (Located (Expr ann))
  | -- Type cast (explicit)
    ExprCast
      (Located (Expr ann))
      (Located Type)
  | -- | Tuple literal: @(a, b)@
    ExprTupleInit [Located (Expr ann)]
  | -- | Method call: @receiver.method(args)@
    ExprMethodCall
      (Located (Expr ann))
      (Located FuncName)
      [Located (Expr ann)]
  deriving stock (Show, Eq, Generic)

data LValue ann
  = LVarRef (Located VarName)
  | LArrayIndex (Located (LValue ann)) (Located (Expr ann))
  | LFieldAccess (Located (LValue ann)) (Located FieldName)
  deriving stock (Show, Eq, Generic)

type LocatedExpr ann = Located (Expr ann)

type LocatedStmt ann = Located (Stmt ann)

type LocatedDecl ann = Located (Decl ann)

type LocatedProgram ann = Program ann

exprSpan :: Located (Expr ann) -> SourceSpan
exprSpan = locSpan

stmtSpan :: Located (Stmt ann) -> SourceSpan
stmtSpan = locSpan

declSpan :: Located (Decl ann) -> SourceSpan
declSpan = locSpan
