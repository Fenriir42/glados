module Parser.Type where

import AST.Types.Common (ErrorName (..), Located (..), TypeName (..), VarName (..), getSpan, locSpan, unLocated)
import AST.Types.Literal (IntBase (BaseDec))
import AST.Types.Type
  ( ArrayType (ArrayType),
    Constness (..),
    FloatSize (Float32, Float64),
    FloatType (FloatType),
    FunctionType (FunctionType),
    IntSize (IntSize),
    IntType (IntType),
    Parameter (Parameter),
    PrimitiveType (..),
    QualifiedType (QualifiedType),
    ResultType (ResultType),
    Signedness (..),
    Type (TypeArray, TypeDict, TypeFunction, TypeOption, TypePrimitive, TypeResult, TypeStruct, TypeTuple),
    defaultFloatType,
    defaultIntType,
  )
import qualified Data.Set as Set
import Parser.Utils
  ( TokenParser,
    isIdentifier,
    isIntDec,
    matchKeyword,
    matchSymbol,
    voidSpann,
  )
import qualified Text.Megaparsec as MP
import Tokens (TokenContent (..))
import Prelude hiding (span)

parseSignedness :: TokenParser Signedness
parseSignedness = do
  Located _ (TokIdentifier s) <- MP.satisfy isSignednessId
  case s of
    "s" -> return Signed
    "u" -> return Unsigned
    _ -> MP.failure Nothing Set.empty
  where
    isSignednessId (Located _ (TokIdentifier s)) = s == "s" || s == "u"
    isSignednessId _ = False

parseIntType :: TokenParser (Located IntType)
parseIntType = do
  Located span _ <- matchKeyword "int"
  maybeBracket <- MP.optional (matchSymbol "<")
  case maybeBracket of
    Nothing -> return $ Located span defaultIntType
    Just _ -> do
      Located _ (TokInt size BaseDec) <- MP.satisfy isIntDec
      maybeComma <- MP.optional (matchSymbol ",")
      sign <- case maybeComma of
        Nothing -> return Signed
        Just _ -> parseSignedness
      Located endSpan _ <- matchSymbol ">"
      let combinedSpan = span <> endSpan
      return $ Located combinedSpan (IntType (IntSize (fromInteger size)) sign)

parseFloatType :: TokenParser (Located FloatType)
parseFloatType = do
  Located span _ <- matchKeyword "float"
  maybeBracket <- MP.optional (matchSymbol "<")
  case maybeBracket of
    Nothing -> return $ Located span defaultFloatType
    Just _ -> do
      Located _ (TokInt size BaseDec) <- MP.satisfy isIntDec
      Located endSpan _ <- matchSymbol ">"
      let floatSize = case size of
            32 -> Float32
            64 -> Float64
            _ -> Float64
          combinedSpan = span <> endSpan
      return $ Located combinedSpan (FloatType floatSize)

parsePrimitiveType :: TokenParser (Located PrimitiveType)
parsePrimitiveType =
  MP.choice
    [ fmap PrimInt <$> parseIntType,
      fmap PrimFloat <$> parseFloatType,
      do
        Located span _ <- matchKeyword "bool"
        return $ Located span PrimBool,
      do
        Located span _ <- matchKeyword "str"
        return $ Located span PrimString,
      do
        Located span _ <- matchKeyword "void"
        return $ Located span PrimNone,
      do
        Located span _ <- matchKeyword "ptr"
        return $ Located span PrimPtr
    ]

parseArrayType :: TokenParser (Located ArrayType)
parseArrayType = do
  Located span _ <- matchSymbol "["
  Located elemSpan elemType <- parseQualifiedType
  Located endSpan _ <- matchSymbol "]"
  let combinedSpan = span <> elemSpan <> endSpan
  return $ Located combinedSpan (ArrayType elemType)

parseConstness :: TokenParser (Located Constness)
parseConstness = do
  maybeConst <- MP.optional (matchKeyword "const")
  case maybeConst of
    Just (Located span _) -> return $ Located span Const
    Nothing -> return $ Located voidSpann Mutable

parseQualifiedType :: TokenParser (Located QualifiedType)
parseQualifiedType = do
  Located constSpan constness <- parseConstness
  Located typeSpan typ <- parseType
  let combinedSpan = constSpan <> typeSpan
  return $ Located combinedSpan (QualifiedType constness typ)

parseParameter :: TokenParser (Located Parameter)
parseParameter = do
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol ":"
  mDots <- MP.optional (matchSymbol "...")
  Located typeSpan elemType <- parseType
  let isVariadic = case mDots of Just _ -> True; Nothing -> False
      qt = case mDots of
        Nothing -> QualifiedType Mutable elemType
        Just _ -> QualifiedType Mutable (TypeArray (ArrayType (QualifiedType Mutable elemType)))
      dotsSpan = maybe voidSpann locSpan mDots
      combinedSpan = nameSpan <> dotsSpan <> typeSpan
  return $ Located combinedSpan (Parameter (VarName name) qt isVariadic)

-- | Parse one parameter in a function type: either @name: type@ or bare @type@.
-- Bare types (no name) get the synthetic name @_@ since the name is irrelevant
-- for type-level function references like @(int, str) -> bool@.
parseTypeParam :: TokenParser (Located Parameter)
parseTypeParam = MP.try parseParameter MP.<|> parseAnonParam
  where
    parseAnonParam = do
      mDots <- MP.optional (matchSymbol "...")
      Located typeSpan elemType <- parseType
      let isVariadic = case mDots of Just _ -> True; Nothing -> False
          qt = case mDots of
            Nothing -> QualifiedType Mutable elemType
            Just _ -> QualifiedType Mutable (TypeArray (ArrayType (QualifiedType Mutable elemType)))
          dotsSpan = maybe voidSpann locSpan mDots
      return $ Located (dotsSpan <> typeSpan) (Parameter (VarName "_") qt isVariadic)

-- | Parse a tuple type: @(T1, T2)@ with at least two elements.
-- Must not be followed by @->@ (which would make it a function type instead).
parseTupleType :: TokenParser (Located Type)
parseTupleType = MP.try $ do
  Located startSpan _ <- matchSymbol "("
  types <- MP.sepBy1 parseQualifiedType (matchSymbol ",")
  Located endSpan _ <- matchSymbol ")"
  MP.notFollowedBy (matchSymbol "->")
  case types of
    [] -> fail "empty tuple"
    [_] -> fail "single-element tuple not supported"
    _ -> return $ Located (startSpan <> endSpan) (TypeTuple (map unLocated types))

parseFunctionType :: TokenParser (Located FunctionType)
parseFunctionType = do
  Located span _ <- matchSymbol "("
  params <- MP.sepBy parseTypeParam (matchSymbol ",")
  Located closeSpan _ <- matchSymbol ")"
  Located arrowSpan _ <- matchSymbol "->"
  ret <- parseQualifiedType
  let combinedSpan = span <> closeSpan <> arrowSpan <> getSpan ret
  return $ Located combinedSpan (FunctionType params ret)

parseTypeNamed :: TokenParser (Located TypeName)
parseTypeNamed = do
  Located span (TokIdentifier name) <- MP.satisfy isIdentifier
  return $ Located span (TypeName name)

parseErrorOrType :: TokenParser (Located Type)
parseErrorOrType = do
  Located startSpan _ <- matchKeyword "orerror"
  _ <- matchSymbol "("
  Located _ successType <- parseType
  _ <- matchSymbol ","
  Located errSpan (TokIdentifier errName) <- MP.satisfy isIdentifier
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> errSpan <> endSpan) (TypeResult (ResultType successType (ErrorName errName)))

isOptionIdent :: Located TokenContent -> Bool
isOptionIdent (Located _ (TokIdentifier "option")) = True
isOptionIdent _ = False

parseOptionType :: TokenParser (Located Type)
parseOptionType = MP.try $ do
  Located startSpan _ <- MP.satisfy isOptionIdent
  _ <- matchSymbol "("
  Located _ innerType <- parseType
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> endSpan) (TypeOption innerType)

isDictIdent :: Located TokenContent -> Bool
isDictIdent (Located _ (TokIdentifier "dict")) = True
isDictIdent _ = False

parseDictType :: TokenParser (Located Type)
parseDictType = MP.try $ do
  Located startSpan _ <- MP.satisfy isDictIdent
  _ <- matchSymbol "("
  Located _ keyType <- parseType
  _ <- matchSymbol ","
  Located _ valType <- parseType
  Located endSpan _ <- matchSymbol ")"
  return $ Located (startSpan <> endSpan) (TypeDict keyType valType)

parseType :: TokenParser (Located Type)
parseType =
  MP.choice
    [ parseErrorOrType,
      parseOptionType,
      parseDictType,
      fmap TypePrimitive <$> parsePrimitiveType,
      fmap TypeArray <$> parseArrayType,
      parseTupleType,
      MP.try (fmap TypeFunction <$> parseFunctionType),
      do
        Located span (TokIdentifier name) <- MP.satisfy isIdentifier
        return $ Located span (TypeStruct (TypeName name))
    ]
