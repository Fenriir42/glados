module Parser.Decl where

import AST.Types.AST
  ( Decl (DeclEnum, DeclError, DeclErrorSet, DeclFFI, DeclFunction, DeclImpl, DeclImplFor, DeclImport, DeclInterface, DeclStruct),
    EnumDecl (..),
    EnumVariant (..),
    ErrorDecl (..),
    ErrorSetDecl (..),
    FFIDecl (..),
    FFIFuncDecl (..),
    FunctionDecl
      ( FunctionDecl,
        funcDeclAsync,
        funcDeclBody,
        funcDeclName,
        funcDeclParams,
        funcDeclReturnType,
        funcDeclTypeBounds,
        funcDeclTypeParams
      ),
    ImplDecl (..),
    ImplForDecl (..),
    InterfaceDecl (..),
    InterfaceMethodSig (..),
    StructDecl (..),
    Visibility (..),
  )
import AST.Types.Common (ErrorName (..), FieldName (..), FuncName (..), Located (..), TypeName (..), VarName (..), unLocated, unTypeName, unVarName)
import AST.Types.Type
  ( ArrayType (ArrayType),
    Constness (..),
    ErrorField (..),
    ErrorSetMember (..),
    FunctionType (..),
    Parameter (..),
    QualifiedType (..),
    StructField (..),
    Type (..),
  )
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
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

-- | Parse a single type-parameter name (used in @struct Foo[T]@ and impl methods).
parseTypeVar :: TokenParser (Located TypeName)
parseTypeVar = do
  Located sp (TokIdentifier tv) <- MP.satisfy isIdentifier
  return (Located sp (TypeName tv))

-- | Parse a type parameter with optional interface bounds: @T@ or @T: Iface1 + Iface2@.
parseFuncTypeParam :: TokenParser (Located TypeName, [Located TypeName])
parseFuncTypeParam = do
  Located sp (TokIdentifier tv) <- MP.satisfy isIdentifier
  bounds <- MP.option [] $ do
    _ <- matchSymbol ":"
    MP.sepBy1 parseBound (matchSymbol "+")
  return (Located sp (TypeName tv), bounds)
  where
    parseBound = do
      Located bsp (TokIdentifier b) <- MP.satisfy isIdentifier
      return (Located bsp (TypeName b))

-- | Substitute type-variable names with @TypeVar@ in a qualified type.
subQType :: Set.Set T.Text -> QualifiedType -> QualifiedType
subQType tvs (QualifiedType c t) = QualifiedType c (subType tvs t)

-- | Substitute type-variable names with @TypeVar@ in a type, recursing into
-- compound types so that e.g. @[T]@, @(T, int)@, and @Pair[T, U]@ all work.
subType :: Set.Set T.Text -> Type -> Type
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
subType tvs (TypeTuple ts) = TypeTuple (map (subQType tvs) ts)
subType tvs (TypeGenericApp n args) = TypeGenericApp n (map (subQType tvs) args)
subType tvs (TypeOption t) = TypeOption (subType tvs t)
subType tvs (TypeDict k v) = TypeDict (subType tvs k) (subType tvs v)
subType _ t = t

subParam :: Set.Set T.Text -> Parameter -> Parameter
subParam tvs p = p {paramType = subQType tvs (paramType p)}

parseDeclFunction :: TokenParser (Located (Decl ann))
parseDeclFunction = do
  Located visSpan visibility <- parseVisibility
  mAsync <- MP.optional (matchKeyword "async")
  Located fnSpan _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  -- Optional generic type parameters: fn foo[T, U: Iface](...)
  mTypeParams <- MP.optional $ do
    _ <- matchSymbol "["
    pairs <- MP.sepBy1 parseFuncTypeParam (matchSymbol ",")
    _ <- matchSymbol "]"
    return pairs
  let typeParamPairs = fromMaybe [] mTypeParams
      typeParams = map fst typeParamPairs
      typeBounds = [(unLocated n, map unLocated bs) | (n, bs) <- typeParamPairs, not (null bs)]
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
            funcDeclTypeBounds = typeBounds,
            funcDeclParams = map (fmap (subParam tvSet)) (funcParams funcType),
            funcDeclReturnType = fmap (subQType tvSet) (funcReturnType funcType),
            funcDeclBody = block,
            funcDeclAsync = isJust mAsync
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
  mTypeParams <- MP.optional $ do
    _ <- matchSymbol "["
    tvs <- MP.sepBy1 parseTypeVar (matchSymbol ",")
    _ <- matchSymbol "]"
    return tvs
  let typeParams = fromMaybe [] mTypeParams
  _ <- matchSymbol "{"
  fields <- MP.sepEndBy parseStructField (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  let tvSet = Set.fromList (map (unTypeName . unLocated) typeParams)
      substFields = map (fmap (subStructField tvSet)) fields
      sd =
        StructDecl
          { structDeclName = Located nameSpan (TypeName name),
            structDeclTypeParams = typeParams,
            structDeclFields = substFields
          }
      combinedSpan = case visibility of
        Static -> visSpan <> structSpan <> endSpan
        Public -> structSpan <> endSpan
  return $ Located combinedSpan (DeclStruct visibility sd)
  where
    isStructKw (Located _ (TokIdentifier "struct")) = True
    isStructKw _ = False
    subStructField tvs sf = sf {fieldType = subQType tvs (fieldType sf)}

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

parseDeclEnum :: TokenParser (Located (Decl ann))
parseDeclEnum = do
  Located visSpan visibility <- parseVisibility
  Located enumSpan _ <- MP.satisfy isEnumKw
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  _ <- matchSymbol "{"
  variants <- MP.sepEndBy parseEnumVariant (matchSymbol ",")
  Located endSpan _ <- matchSymbol "}"
  Located semiSpan _ <- matchSymbol ";"
  let combinedSpan = case visibility of
        Static -> visSpan <> enumSpan <> nameSpan <> endSpan <> semiSpan
        Public -> enumSpan <> nameSpan <> endSpan <> semiSpan
      ed = EnumDecl (Located nameSpan (TypeName name)) variants
  return $ Located combinedSpan (DeclEnum visibility ed)
  where
    isEnumKw (Located _ (TokIdentifier "enum")) = True
    isEnumKw _ = False
    parseEnumVariant = do
      Located vspan (TokIdentifier vname) <- MP.satisfy isIdentifier
      fields <- MP.option [] $ do
        _ <- matchSymbol "{"
        fs <- MP.sepEndBy parseVariantField (matchSymbol ",")
        _ <- matchSymbol "}"
        return fs
      return $ Located vspan (EnumVariant (TypeName vname) fields)
    parseVariantField = do
      Located fspan (TokIdentifier fname) <- MP.satisfy isIdentifier
      _ <- matchSymbol ":"
      Located fspan . StructField (FieldName fname) . unLocated <$> parseQualifiedType

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

-- | Parse @impl TypeName { fn method(self, ...) -> R { ... } ... }@.
-- Each method's @self@ parameter is replaced with @TypeName@ at parse time.
parseDeclImpl :: TokenParser (Located (Decl ann))
parseDeclImpl = do
  Located visSpan visibility <- parseVisibility
  Located implSpan _ <- matchKeyword "impl"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  let tname = TypeName name
  _ <- matchSymbol "{"
  methods <- MP.many (parseImplMethod tname)
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = case visibility of
        Static -> visSpan <> implSpan <> endSpan
        Public -> implSpan <> endSpan
      idecl = ImplDecl {implTypeName = Located nameSpan tname, implMethods = methods}
  return $ Located combinedSpan (DeclImpl visibility idecl)

-- | Parse one method inside an @impl@ block; @self@ is substituted with the
-- enclosing struct type.
parseImplMethod :: TypeName -> TokenParser (Located (FunctionDecl ann))
parseImplMethod selfType = do
  Located fnSpan _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier mname) <- MP.satisfy isIdentifier
  mTypeParams <- MP.optional $ do
    _ <- matchSymbol "["
    tvs <- MP.sepBy1 parseTypeVar (matchSymbol ",")
    _ <- matchSymbol "]"
    return tvs
  let typeParams = fromMaybe [] mTypeParams
  Located _ funcType <- parseFunctionType
  Located bodySpan block <- parseBlock
  let tvSet = Set.fromList (map (unTypeName . unLocated) typeParams)
      qualSelf = QualifiedType Mutable (TypeStruct selfType)
      substParam p@(Parameter pname ptype _)
        | unVarName pname == "self" = p {paramType = qualSelf}
        | qualType ptype == TypeStruct (TypeName "self") =
            p {paramName = VarName "self", paramType = qualSelf}
        | otherwise = subParam tvSet p
      params = map (fmap substParam) (funcParams funcType)
      qualFuncName = FuncName (unTypeName selfType <> "." <> mname)
      fd =
        FunctionDecl
          { funcDeclName = Located nameSpan qualFuncName,
            funcDeclTypeParams = typeParams,
            funcDeclTypeBounds = [],
            funcDeclParams = params,
            funcDeclReturnType = fmap (subQType tvSet) (funcReturnType funcType),
            funcDeclBody = block,
            funcDeclAsync = False
          }
  return $ Located (fnSpan <> bodySpan) fd

-- | Parse one method signature inside an @interface@ block (no body, ends with @;@).
parseInterfaceMethodSig :: TokenParser InterfaceMethodSig
parseInterfaceMethodSig = do
  _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier mname) <- MP.satisfy isIdentifier
  Located _ funcType <- parseFunctionType
  _ <- matchSymbol ";"
  return $
    InterfaceMethodSig
      { ifaceMethodName = Located nameSpan (FuncName mname),
        ifaceMethodParams = funcParams funcType,
        ifaceMethodReturnType = funcReturnType funcType
      }

-- | Parse @interface Name { fn method(...) -> R; ... }@.
parseDeclInterface :: TokenParser (Located (Decl ann))
parseDeclInterface = do
  Located visSpan visibility <- parseVisibility
  Located ifaceSpan _ <- MP.satisfy isInterfaceKw
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  mExtends <- MP.optional $ do
    _ <- MP.satisfy isExtendsKw
    MP.sepBy1 parseParentIface (matchSymbol ",")
  _ <- matchSymbol "{"
  methods <- MP.many parseInterfaceMethodSig
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = case visibility of
        Static -> visSpan <> ifaceSpan <> endSpan
        Public -> ifaceSpan <> endSpan
      idecl =
        InterfaceDecl
          { ifaceDeclName = Located nameSpan (TypeName name),
            ifaceDeclExtends = fromMaybe [] mExtends,
            ifaceDeclMethods = methods
          }
  return $ Located combinedSpan (DeclInterface visibility idecl)
  where
    isInterfaceKw (Located _ (TokIdentifier "interface")) = True
    isInterfaceKw _ = False
    isExtendsKw (Located _ (TokIdentifier "extends")) = True
    isExtendsKw _ = False
    parseParentIface = do
      Located pspan (TokIdentifier pname) <- MP.satisfy isIdentifier
      return $ Located pspan (TypeName pname)

-- | Parse @impl InterfaceName for TypeName { fn method(...) -> R { ... } ... }@.
-- Each method's @self@ parameter is replaced with @TypeName@ at parse time (same as inherent impl).
parseDeclImplFor :: TokenParser (Located (Decl ann))
parseDeclImplFor = do
  Located visSpan visibility <- parseVisibility
  Located implSpan _ <- matchKeyword "impl"
  Located ifaceNameSpan (TokIdentifier ifaceName) <- MP.satisfy isIdentifier
  _ <- matchKeyword "for"
  Located typeNameSpan (TokIdentifier typeName) <- MP.satisfy isIdentifier
  let tname = TypeName typeName
  _ <- matchSymbol "{"
  methods <- MP.many (parseImplMethod tname)
  Located endSpan _ <- matchSymbol "}"
  let combinedSpan = case visibility of
        Static -> visSpan <> implSpan <> endSpan
        Public -> implSpan <> endSpan
      ifdecl =
        ImplForDecl
          { implForIfaceName = Located ifaceNameSpan (TypeName ifaceName),
            implForTypeName = Located typeNameSpan tname,
            implForMethods = methods
          }
  return $ Located combinedSpan (DeclImplFor visibility ifdecl)

parseDeclFFI :: TokenParser (Located (Decl ann))
parseDeclFFI = do
  Located ffiSpan _ <- matchKeyword "extern"
  Located _ (TokString lib) <- MP.satisfy isString
  _ <- matchSymbol "{"
  funcs <- MP.many parseFFIFuncDecl
  Located endSpan _ <- matchSymbol "}"
  return $ Located (ffiSpan <> endSpan) (DeclFFI (FFIDecl lib funcs))
  where
    isString (Located _ (TokString _)) = True
    isString _ = False

parseFFIFuncDecl :: TokenParser FFIFuncDecl
parseFFIFuncDecl = do
  _ <- matchKeyword "fn"
  Located nameSpan (TokIdentifier name) <- MP.satisfy isIdentifier
  Located _ funcType <- parseFunctionType
  return
    FFIFuncDecl
      { ffiFuncName = Located nameSpan (FuncName name),
        ffiFuncParams = funcParams funcType,
        ffiFuncReturnType = funcReturnType funcType
      }

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
        _ <- MP.lookAhead (MP.try interfaceStart)
        parseDeclInterface,
      do
        _ <- MP.lookAhead (MP.try implForStart)
        parseDeclImplFor,
      do
        _ <- MP.lookAhead (MP.try implStart)
        parseDeclImpl,
      do
        _ <- MP.lookAhead (MP.try errorStart)
        parseDeclError,
      do
        _ <- MP.lookAhead (MP.try errorsetStart)
        parseDeclErrorSet,
      do
        _ <- MP.lookAhead (MP.try enumStart)
        parseDeclEnum,
      do
        _ <- MP.lookAhead (matchKeyword "extern")
        parseDeclFFI,
      do
        Located span importDecl <- parseImportDecl
        return $ Located span (DeclImport importDecl)
    ]
  where
    functionStart = MP.optional (MP.satisfy isStaticId) >> MP.optional (matchKeyword "async") >> matchKeyword "fn"
    structStart = MP.optional (MP.satisfy isStaticId) >> MP.satisfy isStructKw
    interfaceStart = MP.optional (MP.satisfy isStaticId) >> MP.satisfy isInterfaceKw
    implForStart =
      MP.optional (MP.satisfy isStaticId)
        >> matchKeyword "impl"
        >> MP.satisfy isIdentifier
        >> matchKeyword "for"
    implStart = MP.optional (MP.satisfy isStaticId) >> matchKeyword "impl"
    errorStart = MP.optional (MP.satisfy isStaticId) >> matchKeyword "error"
    errorsetStart = MP.optional (MP.satisfy isStaticId) >> MP.satisfy isErrorsetKw
    enumStart = MP.optional (MP.satisfy isStaticId) >> MP.satisfy isEnumKw
    isEnumKw (Located _ (TokIdentifier "enum")) = True
    isEnumKw _ = False
    isStaticId (Located _ (TokIdentifier "static")) = True
    isStaticId _ = False
    isStructKw (Located _ (TokIdentifier "struct")) = True
    isStructKw _ = False
    isInterfaceKw (Located _ (TokIdentifier "interface")) = True
    isInterfaceKw _ = False
    isErrorsetKw (Located _ (TokIdentifier "errorset")) = True
    isErrorsetKw _ = False
