{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Compiler.Import
  ( resolveImports,
  )
where

import AST.Types.AST
  ( Block (..),
    Decl (..),
    Expr (..),
    ForInit (..),
    FunctionDecl (..),
    ImportDecl (..),
    ImportTarget (..),
    LValue (..),
    MatchArm (..),
    MatchPattern (..),
    ModulePath (..),
    Stmt (..),
    Visibility (..),
  )
import AST.Types.Common
  ( FuncName (..),
    Located (..),
    ModuleName (..),
    VarName (..),
    unLocated,
  )
import Control.Exception (IOException, catch)
import Data.List (intercalate, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lib (lexString)
import Parser.Decl (parseDecl)
import System.FilePath (takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty, many, runParser)

-- ---------------------------------------------------------------------------
-- Public API

-- | Resolve all @DeclImport@ nodes in a program, replacing them with the
-- imported function declarations.
--
-- @searchPaths@ is an ordered list of directories to search: the first match
-- wins.  Typically the caller passes @[sourceFileDir, stdlibDir]@ so that
-- sibling @.qa@ files are found before falling back to the stdlib.
resolveImports ::
  [FilePath] ->
  [Located (Decl ())] ->
  IO (Either String [Located (Decl ())])
resolveImports searchPaths =
  resolveImports' searchPaths Set.empty []

-- Internal: carries the visited-file set and name chain for cycle detection.
resolveImports' ::
  [FilePath] ->
  Set FilePath ->
  [String] ->
  [Located (Decl ())] ->
  IO (Either String [Located (Decl ())])
resolveImports' searchPaths visiting chain decls = do
  results <- mapM resolve decls
  return $ concat <$> sequence results
  where
    resolve (Located _ (DeclImport imp)) =
      resolveOne' searchPaths visiting chain imp
    resolve loc = return (Right [loc])

-- ---------------------------------------------------------------------------
-- Single import resolution

resolveOne' ::
  [FilePath] ->
  Set FilePath ->
  [String] ->
  ImportDecl ->
  IO (Either String [Located (Decl ())])
resolveOne' searchPaths visiting chain (ImportDecl path target) = do
  let modName = moduleText path
  mFound <- findModule searchPaths (T.unpack modName)
  case mFound of
    Nothing ->
      return $ Left $ "cannot find module '" <> T.unpack modName <> "'"
    Just (foundPath, src) ->
      if Set.member foundPath visiting
        then
          let cycleNames = chain ++ [T.unpack modName]
           in return $
                Left $
                  "import cycle detected: " <> intercalate " -> " cycleNames
        else do
          let foundDir = takeDirectory foundPath
              visiting' = Set.insert foundPath visiting
              chain' = chain ++ [T.unpack modName]
          case lexString src of
            Left lexErr ->
              return $ Left $ "lex error in '" <> T.unpack modName <> "': " <> lexErr
            Right tokens ->
              case runParser (many parseDecl) (T.unpack modName <> ".qa") tokens of
                Left bundle ->
                  return $ Left $ errorBundlePretty bundle
                Right rawDecls -> do
                  -- Split module's own declarations from its imports.
                  let isImportDecl (Located _ (DeclImport {})) = True
                      isImportDecl _ = False
                      (importStmts, ownDecls) = partition isImportDecl rawDecls
                  -- Recursively resolve imports declared inside the loaded module.
                  -- Prepend the module's own directory so its sibling files are found.
                  transitiveOrErr <-
                    resolveImports' (foundDir : searchPaths) visiting' chain' importStmts
                  case transitiveOrErr of
                    Left err -> return $ Left err
                    Right transitive ->
                      return $ Right $ transitive ++ pickDecls modName target ownDecls

-- | Try each directory in order; return the first file found together with
-- its contents, or @Nothing@ if no directory contains @modName.qa@.
findModule :: [FilePath] -> String -> IO (Maybe (FilePath, String))
findModule [] _ = return Nothing
findModule (dir : rest) modName = do
  let fp = dir </> modName <> ".qa"
  result <- (Just . (fp,) <$> readFile fp) `catch` (\(_ :: IOException) -> return Nothing)
  case result of
    Just found -> return (Just found)
    Nothing -> findModule rest modName

-- ---------------------------------------------------------------------------
-- Module dispatch

-- | Decide which declarations to add based on the import target.
pickDecls ::
  Text ->
  ImportTarget ->
  [Located (Decl ())] ->
  [Located (Decl ())]
pickDecls modName target rawDecls
  | isWrapperModule modName rawDecls =
      -- Wrapper modules (string, array, sys, io): bare-named functions that
      -- call VM builtins.  Prefix-renaming would cause infinite recursion, so
      -- we only add bare aliases for explicit imports.
      -- Static functions are not exposed (they are module-private helpers).
      case target of
        ImportAll -> [] -- builtins already available as module.fn
        ImportWildcard -> filterPublic rawDecls
        ImportNames names -> filterByNames (Set.fromList (map unLocated names)) rawDecls
  | otherwise =
      -- Real implementation modules (math): compile with module prefix so that
      -- internal cross-calls work correctly.
      -- All prefixed forms are kept (needed for cross-calls), but bare aliases
      -- are only generated for Public functions.
      let prefixed = prefixModule modName rawDecls
       in case target of
            ImportAll -> prefixed
            ImportWildcard -> prefixed <> bareAliases modName prefixed
            ImportNames names ->
              -- Need full prefixed module for cross-calls, plus bare aliases
              -- for the requested names only.
              prefixed <> filterBareAliases modName (Set.fromList (map unLocated names)) prefixed

-- ---------------------------------------------------------------------------
-- Wrapper-module detection

-- | A module is a "wrapper module" if any function body calls a function
-- whose name starts with @modName <> "."@.  This detects thin wrappers that
-- delegate to VM builtins (e.g. @string.qa@, @array.qa@).
isWrapperModule :: Text -> [Located (Decl ())] -> Bool
isWrapperModule modName decls =
  any hasOwnPrefixCall [fd | Located _ (DeclFunction _ fd) <- decls]
  where
    prefix = modName <> "."
    hasOwnPrefixCall fd = blockHasCall prefix (funcDeclBody fd)

blockHasCall :: Text -> Block () -> Bool
blockHasCall prefix (Block _ stmts) =
  any (stmtHasCall prefix . unLocated) stmts

stmtHasCall :: Text -> Stmt () -> Bool
stmtHasCall prefix = \case
  StmtVarDecl _ _ mE -> maybe False (exprHasCall prefix . unLocated) mE
  StmtAssign _ _ e -> exprHasCall prefix (unLocated e)
  StmtExpr e -> exprHasCall prefix (unLocated e)
  StmtIf cond t mE ->
    exprHasCall prefix (unLocated cond)
      || blockHasCall prefix t
      || maybe False (blockHasCall prefix) mE
  StmtWhile cond body ->
    exprHasCall prefix (unLocated cond) || blockHasCall prefix body
  StmtFor mInit mCond mPost body ->
    maybe False (forInitHasCall prefix) mInit
      || maybe False (exprHasCall prefix . unLocated) mCond
      || maybe False (stmtHasCall prefix . unLocated) mPost
      || blockHasCall prefix body
  StmtReturn mE -> maybe False (exprHasCall prefix . unLocated) mE
  StmtBreak -> False
  StmtContinue -> False
  StmtBlock b -> blockHasCall prefix b
  StmtMatch subj arms ->
    exprHasCall prefix (unLocated subj)
      || any armHasCall arms
    where
      armHasCall (MatchArm pat body) =
        patHasCall pat || stmtHasCall prefix (unLocated body)
      patHasCall (MatchLit e) = exprHasCall prefix (unLocated e)
      patHasCall (MatchRange lo hi) =
        exprHasCall prefix (unLocated lo) || exprHasCall prefix (unLocated hi)
      patHasCall _ = False

forInitHasCall :: Text -> ForInit () -> Bool
forInitHasCall prefix = \case
  ForInitDecl _ _ e -> exprHasCall prefix (unLocated e)
  ForInitExpr e -> exprHasCall prefix (unLocated e)

exprHasCall :: Text -> Expr () -> Bool
exprHasCall prefix = \case
  ExprLiteral lit -> any (exprHasCall prefix . unLocated) lit
  ExprVar _ -> False
  ExprBinary _ l r -> exprHasCall prefix (unLocated l) || exprHasCall prefix (unLocated r)
  ExprUnary _ e -> exprHasCall prefix (unLocated e)
  ExprCall (Located _ (FuncName n)) args ->
    prefix `T.isPrefixOf` n || any (exprHasCall prefix . unLocated) args
  ExprIndex arr idx -> exprHasCall prefix (unLocated arr) || exprHasCall prefix (unLocated idx)
  ExprField e _ -> exprHasCall prefix (unLocated e)
  ExprStructInit _ fields -> any (exprHasCall prefix . unLocated . snd) fields
  ExprArrayInit _ elems -> any (exprHasCall prefix . unLocated) elems
  ExprDictLit pairs ->
    any (\(k, v) -> exprHasCall prefix (unLocated k) || exprHasCall prefix (unLocated v)) pairs
  ExprTry e -> exprHasCall prefix (unLocated e)
  ExprMust e -> exprHasCall prefix (unLocated e)
  ExprSome e -> exprHasCall prefix (unLocated e)
  ExprNone -> False
  ExprError _ fields -> any (exprHasCall prefix . unLocated . snd) fields
  ExprLambda _ _ body -> blockHasCall prefix body
  ExprParen e -> exprHasCall prefix (unLocated e)
  ExprCast e _ -> exprHasCall prefix (unLocated e)

-- ---------------------------------------------------------------------------
-- Prefix renaming (for real implementation modules)

-- | Rename every function in the module to @modName.fn@ and rewrite all
-- internal call sites to use the prefixed names.
prefixModule :: Text -> [Located (Decl ())] -> [Located (Decl ())]
prefixModule modName decls =
  [ Located sp (DeclFunction vis (renameFuncDecl modNames modName fd))
    | Located sp (DeclFunction vis fd) <- decls
  ]
  where
    modNames =
      Set.fromList
        [unLocated (funcDeclName fd) | Located _ (DeclFunction _ fd) <- decls]

-- | Rename a single function: add prefix to its own name and rewrite internal calls.
renameFuncDecl ::
  Set FuncName ->
  Text ->
  FunctionDecl () ->
  FunctionDecl ()
renameFuncDecl names prefix fd =
  fd
    { funcDeclName = fmap (addPrefix prefix) (funcDeclName fd),
      funcDeclBody = renameBlock names prefix (funcDeclBody fd)
    }

addPrefix :: Text -> FuncName -> FuncName
addPrefix prefix (FuncName n) = FuncName (prefix <> "." <> n)

-- ---------------------------------------------------------------------------
-- AST rename walk

renameBlock :: Set FuncName -> Text -> Block () -> Block ()
renameBlock names prefix (Block sp stmts) =
  Block sp (fmap (fmap (renameStmt names prefix)) stmts)

renameStmt :: Set FuncName -> Text -> Stmt () -> Stmt ()
renameStmt names prefix = \case
  StmtVarDecl v t mE -> StmtVarDecl v t (fmap (fmap rE) mE)
  StmtAssign lv op e -> StmtAssign (fmap rLV lv) op (fmap rE e)
  StmtExpr e -> StmtExpr (fmap rE e)
  StmtIf cond t mEl -> StmtIf (fmap rE cond) (rB t) (fmap rB mEl)
  StmtWhile cond body -> StmtWhile (fmap rE cond) (rB body)
  StmtFor mInit mCond mPost body ->
    StmtFor
      (fmap (renameForInit names prefix) mInit)
      (fmap (fmap rE) mCond)
      (fmap (fmap (renameStmt names prefix)) mPost)
      (rB body)
  StmtReturn mE -> StmtReturn (fmap (fmap rE) mE)
  StmtBreak -> StmtBreak
  StmtContinue -> StmtContinue
  StmtBlock b -> StmtBlock (rB b)
  StmtMatch subj arms ->
    StmtMatch (fmap rE subj) (map renameArm arms)
    where
      renameArm (MatchArm pat body) =
        MatchArm (renamePat pat) (fmap (renameStmt names prefix) body)
      renamePat (MatchLit e) = MatchLit (fmap rE e)
      renamePat (MatchRange lo hi) = MatchRange (fmap rE lo) (fmap rE hi)
      renamePat p = p
  where
    rE = renameExpr names prefix
    rB = renameBlock names prefix
    rLV = renameLValue names prefix

renameForInit :: Set FuncName -> Text -> ForInit () -> ForInit ()
renameForInit names prefix = \case
  ForInitDecl v t e -> ForInitDecl v t (fmap rE e)
  ForInitExpr e -> ForInitExpr (fmap rE e)
  where
    rE = renameExpr names prefix

renameLValue :: Set FuncName -> Text -> LValue () -> LValue ()
renameLValue names prefix = \case
  LVarRef v -> LVarRef v
  LArrayIndex lv idx -> LArrayIndex (fmap rLV lv) (fmap rE idx)
  LFieldAccess lv f -> LFieldAccess (fmap rLV lv) f
  where
    rE = renameExpr names prefix
    rLV = renameLValue names prefix

renameExpr :: Set FuncName -> Text -> Expr () -> Expr ()
renameExpr names prefix = \case
  ExprLiteral lit -> ExprLiteral (fmap (fmap rE) lit)
  ExprVar v -> ExprVar v
  ExprBinary op l r -> ExprBinary op (fmap rE l) (fmap rE r)
  ExprUnary op e -> ExprUnary op (fmap rE e)
  ExprCall (Located sp fname) args ->
    let fname' =
          if fname `Set.member` names
            then addPrefix prefix fname
            else fname
     in ExprCall (Located sp fname') (map (fmap rE) args)
  ExprIndex arr idx -> ExprIndex (fmap rE arr) (fmap rE idx)
  ExprField e f -> ExprField (fmap rE e) f
  ExprStructInit t fields -> ExprStructInit t [(f, fmap rE e) | (f, e) <- fields]
  ExprArrayInit t elems -> ExprArrayInit t (map (fmap rE) elems)
  ExprDictLit pairs -> ExprDictLit [(fmap rE k, fmap rE v) | (k, v) <- pairs]
  ExprTry e -> ExprTry (fmap rE e)
  ExprMust e -> ExprMust (fmap rE e)
  ExprSome e -> ExprSome (fmap rE e)
  ExprNone -> ExprNone
  ExprError ename fields -> ExprError ename [(f, fmap rE e) | (f, e) <- fields]
  ExprLambda params ret body -> ExprLambda params ret (renameBlock names prefix body)
  ExprParen e -> ExprParen (fmap rE e)
  ExprCast e t -> ExprCast (fmap rE e) t
  where
    rE = renameExpr names prefix

-- ---------------------------------------------------------------------------
-- Bare alias generation

-- | Keep only Public function declarations, dropping Static ones.
filterPublic :: [Located (Decl ())] -> [Located (Decl ())]
filterPublic decls = [loc | loc@(Located _ (DeclFunction Public _)) <- decls]

-- | Create bare-named copies of all Public prefixed functions.
-- Static (private) functions are not re-exported as bare names.
-- e.g. @math.sqrt@ → copy with name @sqrt@ (body unchanged, still calls @math.*@).
bareAliases :: Text -> [Located (Decl ())] -> [Located (Decl ())]
bareAliases modName prefixed =
  [ Located sp (DeclFunction vis (stripPrefixFromName modName fd))
    | Located sp (DeclFunction vis fd) <- filterPublic prefixed
  ]

-- | Create bare-named copies only for the requested Public function names.
-- Static functions are silently omitted; they will produce an undefined-function
-- error at the type-checker stage, which is the correct behaviour.
filterBareAliases ::
  Text ->
  Set VarName ->
  [Located (Decl ())] ->
  [Located (Decl ())]
filterBareAliases modName wantedVars prefixed =
  [ Located sp (DeclFunction vis (stripPrefixFromName modName fd))
    | Located sp (DeclFunction vis fd) <- filterPublic prefixed,
      let bareName = strippedName modName (unLocated (funcDeclName fd)),
      VarName (unFuncName bareName) `Set.member` wantedVars
  ]

-- | Strip @modName.@ from the function's declared name.
stripPrefixFromName :: Text -> FunctionDecl () -> FunctionDecl ()
stripPrefixFromName modName fd =
  fd {funcDeclName = fmap (strippedName modName) (funcDeclName fd)}

strippedName :: Text -> FuncName -> FuncName
strippedName modName (FuncName n) =
  FuncName (T.drop (T.length modName + 1) n)

-- ---------------------------------------------------------------------------
-- Bare-name filtering (for wrapper modules)

-- | Keep only Public functions whose bare name is in the requested set.
-- Static functions are silently omitted even if explicitly named.
filterByNames ::
  Set VarName ->
  [Located (Decl ())] ->
  [Located (Decl ())]
filterByNames wanted decls =
  [ loc
    | loc@(Located _ (DeclFunction Public fd)) <- decls,
      VarName (unFuncName (unLocated (funcDeclName fd))) `Set.member` wanted
  ]

-- ---------------------------------------------------------------------------
-- Utilities

moduleText :: ModulePath -> Text
moduleText (ModulePath parts) =
  T.intercalate "." [unModuleName (unLocated m) | m <- parts]
