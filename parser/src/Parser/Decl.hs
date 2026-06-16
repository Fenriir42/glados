module Parser.Decl where

import AST.Types.AST
  ( Decl (DeclFunction, DeclImport, DeclStruct),
    FunctionDecl
      ( FunctionDecl,
        funcDeclBody,
        funcDeclName,
        funcDeclParams,
        funcDeclReturnType
      ),
    StructDecl (..),
    Visibility (..),
  )
import AST.Types.Common (FieldName (..), FuncName (..), Located (..), TypeName (..))
import AST.Types.Type (FunctionType (funcParams, funcReturnType), StructField (..))
import Parser.Import (parseImportDecl)
import Parser.Stmt (parseBlock)
import Parser.Type (parseFunctionType, parseQualifiedType)
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    matchKeyword,
    matchSymbol,
    voidSpann,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseVisibility :: TokenParser (Located Visibility)
parseVisibility = do
  maybeStatic <- MP.optional (matchKeyword "static")
  case maybeStatic of
    Just (Located span _) -> return $ Located span Static
    Nothing -> return $ Located voidSpann Public

parseDeclFunction :: TokenParser (Located (Decl ann))
parseDeclFunction = do
  Located visSpan visibility <- parseVisibility
  Located fnSpan _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  Located typeSpan funcType <- parseFunctionType
  Located bodySpan block <- parseBlock

  let combinedSpan = case visibility of
        Static -> visSpan <> fnSpan <> nameSpan <> typeSpan <> bodySpan
        Public -> fnSpan <> nameSpan <> typeSpan <> bodySpan

  let functionDecl =
        FunctionDecl
          { funcDeclName = Located nameSpan (FuncName name),
            funcDeclParams = funcParams funcType,
            funcDeclReturnType = funcReturnType funcType,
            funcDeclBody = block
          }
  return $ Located combinedSpan (DeclFunction visibility functionDecl)

parseStructField :: TokenParser (Located StructField)
parseStructField = do
  Located nameSpan (TokIdentifier fname) <- MP.satisfy isIdentifier
  _ <- matchSymbol ":"
  Located typeSpan qt <- parseQualifiedType
  return $ Located (nameSpan <> typeSpan) (StructField (FieldName fname) qt)

parseDeclStruct :: TokenParser (Located (Decl ann))
parseDeclStruct = do
  Located visSpan visibility <- parseVisibility
  Located structSpan (TokIdentifier _) <- MP.satisfy isStructKw
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol "{"
  fields <- MP.sepEndBy parseStructField (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  let sd = StructDecl (Located nameSpan (TypeName name)) fields
      combinedSpan = case visibility of
        Static -> visSpan <> structSpan <> endSpan
        Public -> structSpan <> endSpan
  return $ Located combinedSpan (DeclStruct visibility sd)
  where
    isStructKw (Located _ (TokIdentifier "struct")) = True
    isStructKw _ = False

-- | Peek to determine if the upcoming tokens start a function or struct, then
-- dispatch to the committed (non-backtracking) parser.  This lets us detect
-- the discriminating keyword without consuming it, so that body-parse errors
-- propagate as hard failures rather than being silently swallowed.
parseDecl :: TokenParser (Located (Decl ann))
parseDecl =
  MP.choice
    [ do
        MP.lookAhead (MP.try functionStart)
        parseDeclFunction,
      do
        MP.lookAhead (MP.try structStart)
        parseDeclStruct,
      do
        Located span importDecl <- parseImportDecl
        return $ Located span (DeclImport importDecl)
    ]
  where
    functionStart = MP.optional (matchKeyword "static") >> matchKeyword "fn"
    structStart = MP.optional (matchKeyword "static") >> MP.satisfy isStructKw
    isStructKw (Located _ (TokIdentifier "struct")) = True
    isStructKw _ = False
