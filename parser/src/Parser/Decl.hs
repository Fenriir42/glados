module Parser.Decl where

import AST.Types.AST
  ( Decl (DeclError, DeclErrorSet, DeclFunction, DeclImport, DeclStruct),
    ErrorDecl (..),
    ErrorSetDecl (..),
    FunctionDecl
      ( FunctionDecl,
        funcDeclBody,
        funcDeclName,
        funcDeclParams,
        funcDeclReturnType,
        funcDeclTypeParams
      ),
    StructDecl (..),
    Visibility (..),
  )
import AST.Types.Common (ErrorName (..), FieldName (..), FuncName (..), Located (..), TypeName (..), unLocated, unTypeName)
import AST.Types.Type
  ( ArrayType (ArrayType),
    ErrorField (..),
    ErrorSetMember (..),
    FunctionType (..),
    Parameter (..),
    QualifiedType (..),
    StructField (..),
    Type (..),
  )
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Parser.Import (parseImportDecl)
import Parser.Stmt (parseBlock)
import Parser.Type (parseFunctionType, parseQualifiedType, parseType)
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
  maybeStatic <- MP.optional (MP.satisfy isStaticId)
  case maybeStatic of
    Just (Located span _) -> return $ Located span Static
    Nothing -> return $ Located voidSpann Public
  where
    isStaticId (Located _ (TokIdentifier "static")) = True
    isStaticId _ = False

parseDeclFunction :: TokenParser (Located (Decl ann))
parseDeclFunction = do
  Located visSpan visibility <- parseVisibility
  Located fnSpan _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  -- Optional generic type parameters: fn foo[T, U](...)
  mTypeParams <- MP.optional $ do
    _ <- matchSymbol "["
    tvs <- MP.sepBy1 parseTypeVar (matchSymbol ",")
    _ <- matchSymbol "]"
    return tvs
  let typeParams = fromMaybe [] mTypeParams
  Located typeSpan funcType <- parseFunctionType
  Located bodySpan block <- parseBlock

  let combinedSpan = case visibility of
        Static -> visSpan <> fnSpan <> nameSpan <> typeSpan <> bodySpan
        Public -> fnSpan <> nameSpan <> typeSpan <> bodySpan
      tvSet = Set.fromList (map (unTypeName . unLocated) typeParams)
      functionDecl =
        FunctionDecl
          { funcDeclName = Located nameSpan (FuncName name),
            funcDeclTypeParams = typeParams,
            funcDeclParams = map (fmap (subParam tvSet)) (funcParams funcType),
            funcDeclReturnType = fmap (subQType tvSet) (funcReturnType funcType),
            funcDeclBody = block
          }
  return $ Located combinedSpan (DeclFunction visibility functionDecl)
  where
    parseTypeVar :: TokenParser (Located TypeName)
    parseTypeVar = do
      Located sp (TokIdentifier tv) <- MP.satisfy isIdentifier
      return (Located sp (TypeName tv))

    subQType tvs (QualifiedType c t) = QualifiedType c (subType tvs t)

    subType tvs (TypeStruct (TypeName n))
      | Set.member n tvs = TypeVar (TypeName n)
    subType tvs (TypeArray (ArrayType qt)) =
      TypeArray (ArrayType (subQType tvs qt))
    subType tvs (TypeFunction ft) =
      TypeFunction
        ft
          { funcParams = map (fmap (subParam tvs)) (funcParams ft),
            funcReturnType = fmap (subQType tvs) (funcReturnType ft)
          }
    subType _ t = t

    subParam tvs p = p {paramType = subQType tvs (paramType p)}

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

parseErrorField :: TokenParser (Located ErrorField)
parseErrorField = do
  Located nameSpan (TokIdentifier fname) <- MP.satisfy isIdentifier
  _ <- matchSymbol ":"
  Located typeSpan typ <- parseType
  return $ Located (nameSpan <> typeSpan) (ErrorField (FieldName fname) typ)

parseDeclError :: TokenParser (Located (Decl ann))
parseDeclError = do
  Located visSpan visibility <- parseVisibility
  Located errSpan _ <- matchKeyword "error"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  maybeFields <- MP.optional $ do
    _ <- matchSymbol "{"
    fields <- MP.sepEndBy parseErrorField (matchSymbol ",")
    Located endSpan _ <- matchSymbol "}"
    return (fields, endSpan)
  Located semiSpan _ <- matchSymbol ";"
  let (fields, lastSpan) = case maybeFields of
        Nothing -> ([], semiSpan)
        Just (fs, es) -> (fs, es <> semiSpan)
      combinedSpan = case visibility of
        Static -> visSpan <> errSpan <> nameSpan <> lastSpan
        Public -> errSpan <> nameSpan <> lastSpan
      ed = ErrorDecl (Located nameSpan (ErrorName name)) fields
  return $ Located combinedSpan (DeclError visibility ed)

parseErrorSetMember :: TokenParser (Located ErrorSetMember)
parseErrorSetMember = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (ErrorMemberSingle (ErrorName name))

parseDeclErrorSet :: TokenParser (Located (Decl ann))
parseDeclErrorSet = do
  Located visSpan visibility <- parseVisibility
  Located errsetSpan _ <- MP.satisfy isErrorsetKw
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol "{"
  members <- MP.sepEndBy parseErrorSetMember (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  _ <- matchSymbol ";"
  let combinedSpan = case visibility of
        Static -> visSpan <> errsetSpan <> nameSpan <> endSpan
        Public -> errsetSpan <> nameSpan <> endSpan
      esd = ErrorSetDecl (Located nameSpan (ErrorName name)) members
  return $ Located combinedSpan (DeclErrorSet visibility esd)
  where
    isErrorsetKw (Located _ (TokIdentifier "errorset")) = True
    isErrorsetKw _ = False

-- | Peek to determine if the upcoming tokens start a function or struct, then
-- dispatch to the committed (non-backtracking) parser.  This lets us detect
-- the discriminating keyword without consuming it, so that body-parse errors
-- propagate as hard failures rather than being silently swallowed.
parseDecl :: TokenParser (Located (Decl ann))
parseDecl =
  MP.choice
    [ do
        _ <- MP.lookAhead (MP.try functionStart)
        parseDeclFunction,
      do
        _ <- MP.lookAhead (MP.try structStart)
        parseDeclStruct,
      do
        _ <- MP.lookAhead (MP.try errorStart)
        parseDeclError,
      do
        _ <- MP.lookAhead (MP.try errorsetStart)
        parseDeclErrorSet,
      do
        Located span importDecl <- parseImportDecl
        return $ Located span (DeclImport importDecl)
    ]
  where
    functionStart = MP.optional (MP.satisfy isStaticId) >> matchKeyword "fn"
    structStart = MP.optional (MP.satisfy isStaticId) >> MP.satisfy isStructKw
    errorStart = MP.optional (MP.satisfy isStaticId) >> matchKeyword "error"
    errorsetStart = MP.optional (MP.satisfy isStaticId) >> MP.satisfy isErrorsetKw
    isStaticId (Located _ (TokIdentifier "static")) = True
    isStaticId _ = False
    isStructKw (Located _ (TokIdentifier "struct")) = True
    isStructKw _ = False
    isErrorsetKw (Located _ (TokIdentifier "errorset")) = True
    isErrorsetKw _ = False
