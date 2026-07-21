module Manifest
  ( Manifest (..),
    loadManifest,
    manifestFileName,
  )
where

import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

data Manifest = Manifest
  { mName :: String,
    mVersion :: String,
    mEntry :: FilePath,
    mStdlib :: Maybe FilePath,
    mTestDir :: FilePath,
    mDocOut :: FilePath,
    mCovIgnore :: [FilePath]
  }

manifestFileName :: FilePath
manifestFileName = "quant.toml"

loadManifest :: IO Manifest
loadManifest = do
  exists <- doesFileExist manifestFileName
  if not exists
    then die ("no " ++ manifestFileName ++ " found; run `glados init NAME` to create a project")
    else do
      content <- readFile manifestFileName
      case parseManifest content of
        Left err -> die (manifestFileName ++ ": " ++ err)
        Right m -> return m

-- ---------------------------------------------------------------------------
-- Parser

type Section = String

type Pairs = [((Section, String), String)]

parseManifest :: String -> Either String Manifest
parseManifest content = do
  let pairs = parsePairs content
  name <- require pairs "project" "name"
  version <- require pairs "project" "version"
  let entry = fromMaybe "src/main.qa" (look pairs "project" "entry")
      stdlib' = look pairs "project" "stdlib"
      testDir = fromMaybe "tests/" (look pairs "test" "dir")
      docOut = fromMaybe "docs/api/" (look pairs "doc" "out")
      covIgnore = lookList pairs "test" "cov_ignore"
  return
    Manifest
      { mName = name,
        mVersion = version,
        mEntry = entry,
        mStdlib = stdlib',
        mTestDir = testDir,
        mDocOut = docOut,
        mCovIgnore = covIgnore
      }

-- | Look up a single string value; strips surrounding quotes.
look :: Pairs -> Section -> String -> Maybe String
look pairs sec key = case lookup (sec, key) pairs of
  Nothing -> Nothing
  Just rawVal ->
    let v = parseValue rawVal
     in if null v then Nothing else Just v

-- | Look up a TOML inline array (@["a", "b"]@) or a single quoted string.
lookList :: Pairs -> Section -> String -> [String]
lookList pairs sec key = maybe [] parseListValue (lookup (sec, key) pairs)

require :: Pairs -> Section -> String -> Either String String
require pairs sec key =
  case look pairs sec key of
    Nothing -> Left ("[" ++ sec ++ "] " ++ key ++ " is required")
    Just v -> Right v

-- | Stores raw (unprocessed) value text so both string and list fields work.
parsePairs :: String -> Pairs
parsePairs content = go "" (lines content) []
  where
    go _ [] acc = acc
    go sec (l : ls) acc =
      let trimmed = dropWhile (== ' ') l
       in case trimmed of
            [] -> go sec ls acc
            '#' : _ -> go sec ls acc
            '[' : rest ->
              let secName = takeWhile (/= ']') rest
               in go secName ls acc
            _ ->
              case parseKV trimmed of
                Nothing -> go sec ls acc
                Just (k, v) -> go sec ls (((sec, k), v) : acc)

-- | Returns (key, raw-value-text) without interpreting the value.
parseKV :: String -> Maybe (String, String)
parseKV s =
  case break (== '=') s of
    (_, []) -> Nothing
    (k, _ : vs) ->
      let key = strip k
          rawVal = dropWhile (== ' ') vs
       in if null key then Nothing else Just (key, rawVal)

parseValue :: String -> String
parseValue ('"' : rest) = takeWhile (/= '"') rest
parseValue s = strip (takeWhile (\c -> c /= '#' && c /= '\n') s)

-- | Parse a TOML inline array or fall back to a single-string value.
parseListValue :: String -> [String]
parseListValue ('[' : rest) = extractQuoted (takeWhile (/= ']') rest)
parseListValue s =
  let v = parseValue s in [v | not (null v)]

-- | Pull every double-quoted string out of a comma-separated fragment.
extractQuoted :: String -> [String]
extractQuoted [] = []
extractQuoted ('"' : rest) =
  let val = takeWhile (/= '"') rest
      after = drop (length val + 1) rest
   in val : extractQuoted (dropWhile (\c -> c == ',' || c == ' ') after)
extractQuoted (_ : rest) = extractQuoted rest

strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
