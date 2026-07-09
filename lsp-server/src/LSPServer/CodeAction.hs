module LSPServer.CodeAction (makeCodeActions) where

import AST.Types.AST (ImportDecl (..), ImportTarget (..), ModulePath (..))
import AST.Types.Common
  ( FieldName (..),
    FuncName (..),
    Located (..),
    ModuleName (..),
    SourceSpan,
    TypeName (..),
    VarName (..),
    unFieldName,
    unLocated,
    unModuleName,
    unVarName,
  )
import AST.Types.Type
  ( PrimitiveType (..),
    QualifiedType (..),
    StructField (..),
    Type (..),
    qualType,
  )
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import LSPServer.Span (spanToRange)
import qualified Language.LSP.Protocol.Types as LSP
import System.FilePath (dropExtension, takeBaseName)

makeCodeActions ::
  Map SourceSpan (VarName, SourceSpan) ->
  [(SourceSpan, ImportDecl)] ->
  Map FuncName (FilePath, SourceSpan) ->
  Map TypeName [StructField] ->
  Text ->
  FilePath ->
  LSP.Range ->
  [LSP.Diagnostic] ->
  [LSP.CodeAction]
makeCodeActions varDeclSites importDecls stdlibDefs structDefs fileText fp _range =
  concatMap (actionsForDiag varDeclSites importDecls stdlibDefs structDefs fileText fp)

actionsForDiag ::
  Map SourceSpan (VarName, SourceSpan) ->
  [(SourceSpan, ImportDecl)] ->
  Map FuncName (FilePath, SourceSpan) ->
  Map TypeName [StructField] ->
  Text ->
  FilePath ->
  LSP.Diagnostic ->
  [LSP.CodeAction]
actionsForDiag varDeclSites importDecls stdlibDefs structDefs fileText fp diag =
  catMaybes
    [ if isUnnecessary diag then varRemoveAction varDeclSites fp diag else Nothing,
      if isUnnecessary diag then importRemoveAction importDecls fp diag else Nothing,
      addImportAction stdlibDefs importDecls fileText fp diag,
      fillStructAction structDefs fileText fp diag
    ]

-- ---------------------------------------------------------------------------
-- Diagnostic classification

isUnnecessary :: LSP.Diagnostic -> Bool
isUnnecessary (LSP.Diagnostic _ _ _ _ _ _ mTags _ _) =
  case mTags of
    Just tags -> LSP.DiagnosticTag_Unnecessary `elem` tags
    Nothing -> False

diagMessage :: LSP.Diagnostic -> Text
diagMessage (LSP.Diagnostic _ _ _ _ _ msg _ _ _) = msg

-- ---------------------------------------------------------------------------
-- Existing quick fixes: remove unused var / import

varRemoveAction ::
  Map SourceSpan (VarName, SourceSpan) ->
  FilePath ->
  LSP.Diagnostic ->
  Maybe LSP.CodeAction
varRemoveAction varDeclSites fp diag = do
  let LSP.Diagnostic diagRange _ _ _ _ _ _ _ _ = diag
      LSP.Range (LSP.Position nameLine _) _ = diagRange
  (_, (vname, _)) <-
    case [(nameSp, v) | (nameSp, v) <- Map.toList varDeclSites, spanToRange nameSp == diagRange] of
      [x] -> Just x
      _ -> Nothing
  Just (deleteLineAction fp ("Remove unused variable `" <> unVarName vname <> "`") nameLine)

importRemoveAction ::
  [(SourceSpan, ImportDecl)] ->
  FilePath ->
  LSP.Diagnostic ->
  Maybe LSP.CodeAction
importRemoveAction importDecls fp diag = do
  let LSP.Diagnostic diagRange _ _ _ _ _ _ _ _ = diag
  (importSp, importDecl) <-
    case [(sp, d) | (sp, d) <- importDecls, spanToRange sp == diagRange] of
      (x : _) -> Just x
      [] -> Nothing
  let modName = modPathText (importPath importDecl)
      LSP.Range (LSP.Position importLine _) _ = spanToRange importSp
  Just (deleteLineAction fp ("Remove import `" <> modName <> "`") importLine)

-- ---------------------------------------------------------------------------
-- Code action: add import for undefined stdlib function

addImportAction ::
  Map FuncName (FilePath, SourceSpan) ->
  [(SourceSpan, ImportDecl)] ->
  Text ->
  FilePath ->
  LSP.Diagnostic ->
  Maybe LSP.CodeAction
addImportAction stdlibDefs importDecls fileText fp diag = do
  fname <-
    extractUndefinedFunc (diagMessage diag)
      <|> extractImplicitImportFunc (diagMessage diag)
  (libFp, _) <- Map.lookup (FuncName fname) stdlibDefs
  let modName = T.pack (dropExtension (takeBaseName libFp))
      uri = LSP.filePathToUri fp
  case findExistingImport modName importDecls of
    Just (importSp, existingNames) ->
      -- Append new name to the existing "from M import ..." line
      let names = map (unVarName . unLocated) existingNames
          newLine = "from " <> modName <> " import " <> T.intercalate ", " (names ++ [fname])
          LSP.Range (LSP.Position importLineNum _) _ = spanToRange importSp
          replaceRange = LSP.Range (LSP.Position importLineNum 0) (LSP.Position (importLineNum + 1) 0)
          edit = LSP.TextEdit replaceRange (newLine <> "\n")
          wsEdit = LSP.WorkspaceEdit (Just (Map.singleton uri [edit])) Nothing Nothing
       in Just $
            LSP.CodeAction
              ("Add `" <> fname <> "` to `from " <> modName <> " import ...`")
              (Just LSP.CodeActionKind_QuickFix)
              Nothing
              (Just True)
              Nothing
              (Just wsEdit)
              Nothing
              Nothing
    Nothing ->
      -- Insert a new "from M import name" line after the last import
      let importLine = "from " <> modName <> " import " <> fname
          insertAt = lastImportLine fileText + 1
          edit =
            LSP.TextEdit
              (LSP.Range (LSP.Position insertAt 0) (LSP.Position insertAt 0))
              (importLine <> "\n")
          wsEdit = LSP.WorkspaceEdit (Just (Map.singleton uri [edit])) Nothing Nothing
       in Just $
            LSP.CodeAction
              ("Add `" <> importLine <> "`")
              (Just LSP.CodeActionKind_QuickFix)
              Nothing
              (Just True)
              Nothing
              (Just wsEdit)
              Nothing
              Nothing

-- | Extract name from "undefined function `name`".
extractUndefinedFunc :: Text -> Maybe Text
extractUndefinedFunc msg = do
  rest <- T.stripPrefix "undefined function `" msg
  let (name, _) = T.breakOn "`" rest
  if T.null name then Nothing else Just name

-- | Extract name from "function `name` is not explicitly imported from `mod`".
extractImplicitImportFunc :: Text -> Maybe Text
extractImplicitImportFunc msg = do
  rest <- T.stripPrefix "function `" msg
  let (name, rest2) = T.breakOn "`" rest
  _ <- T.stripPrefix "` is not explicitly imported" rest2
  if T.null name then Nothing else Just name

-- | Find an existing "from modName import names" entry in importDecls.
findExistingImport :: Text -> [(SourceSpan, ImportDecl)] -> Maybe (SourceSpan, [Located VarName])
findExistingImport modName importDecls =
  listToMaybe
    [ (sp, names)
      | (sp, ImportDecl path (ImportNames names)) <- importDecls,
        modPathText path == modName
    ]

-- | 0-indexed line number of the last import/from line; 0 if none.
lastImportLine :: Text -> LSP.UInt
lastImportLine fileText =
  let fromFileText =
        [ fromIntegral i
          | (i, l) <- zip [0 :: Int ..] (T.lines fileText),
            "import " `T.isPrefixOf` l || "from " `T.isPrefixOf` l
        ]
   in case fromFileText of
        [] -> 0
        ls -> last ls

-- ---------------------------------------------------------------------------
-- Code action: fill missing struct fields

fillStructAction ::
  Map TypeName [StructField] ->
  Text ->
  FilePath ->
  LSP.Diagnostic ->
  Maybe LSP.CodeAction
fillStructAction structDefs fileText fp diag = do
  let LSP.Diagnostic diagRange _ _ _ _ _ _ _ _ = diag
  (structName, missingNames) <- extractMissingStructInfo (diagMessage diag)
  allFields <- Map.lookup (TypeName structName) structDefs
  let missingFields = [f | f <- allFields, unFieldName (fieldName f) `elem` missingNames]
  if null missingFields
    then Nothing
    else do
      let LSP.Range _ (LSP.Position endLine endChar) = diagRange
          hasExisting = structInitHasFields fileText diagRange
          sep = if hasExisting then ", " else " "
          fieldsText = T.intercalate ", " (map renderField missingFields)
          insertPos = LSP.Position endLine (endChar - 1)
          edit =
            LSP.TextEdit
              (LSP.Range insertPos insertPos)
              (sep <> fieldsText <> " ")
          uri = LSP.filePathToUri fp
          wsEdit = LSP.WorkspaceEdit (Just (Map.singleton uri [edit])) Nothing Nothing
      Just $
        LSP.CodeAction
          ("Fill missing fields of `" <> structName <> "`")
          (Just LSP.CodeActionKind_QuickFix)
          Nothing
          (Just True)
          Nothing
          (Just wsEdit)
          Nothing
          Nothing

-- | Extract (structName, [missingFieldName]) from "struct `X` init missing fields: a, b".
extractMissingStructInfo :: Text -> Maybe (Text, [Text])
extractMissingStructInfo msg = do
  rest <- T.stripPrefix "struct `" msg
  let (structName, rest2) = T.breakOn "`" rest
  rest3 <- T.stripPrefix "` init missing fields: " rest2
  if T.null structName || T.null rest3
    then Nothing
    else Just (structName, T.splitOn ", " rest3)

-- | True if the struct init expression (at diagRange) already has any fields.
structInitHasFields :: Text -> LSP.Range -> Bool
structInitHasFields fileText (LSP.Range (LSP.Position sl sc) (LSP.Position el ec)) =
  let ls = T.lines fileText
      safeGet i = if fromIntegral i < length ls then ls !! fromIntegral i else ""
      initText =
        if sl == el
          then T.take (fromIntegral ec - fromIntegral sc) (T.drop (fromIntegral sc) (safeGet sl))
          else safeGet sl
      afterBrace = T.drop 1 (T.dropWhile (/= '{') initText)
   in T.any (== ':') afterBrace

-- | Render a struct field with a default value for its type.
renderField :: StructField -> Text
renderField sf =
  unFieldName (fieldName sf) <> ": " <> defaultValue (qualType (fieldType sf))

defaultValue :: Type -> Text
defaultValue (TypePrimitive (PrimInt _)) = "0"
defaultValue (TypePrimitive (PrimFloat _)) = "0.0"
defaultValue (TypePrimitive PrimString) = "\"\""
defaultValue (TypePrimitive PrimBool) = "false"
defaultValue (TypePrimitive PrimNone) = "void"
defaultValue (TypeArray _) = "[]"
defaultValue (TypeStruct (TypeName n)) = n <> " { }"
defaultValue _ = "???"

-- ---------------------------------------------------------------------------
-- Helpers

deleteLineAction :: FilePath -> Text -> LSP.UInt -> LSP.CodeAction
deleteLineAction fp title line =
  let deleteRange = LSP.Range (LSP.Position line 0) (LSP.Position (line + 1) 0)
      edit = LSP.TextEdit deleteRange ""
      uri = LSP.filePathToUri fp
      wsEdit = LSP.WorkspaceEdit (Just (Map.singleton uri [edit])) Nothing Nothing
   in LSP.CodeAction
        title
        (Just LSP.CodeActionKind_QuickFix)
        Nothing
        (Just True)
        Nothing
        (Just wsEdit)
        Nothing
        Nothing

modPathText :: ModulePath -> Text
modPathText mp =
  T.intercalate "." [unModuleName n | Located _ n <- modulePathParts mp]
