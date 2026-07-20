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
    mDocOut :: FilePath
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
  return
    Manifest
      { mName = name,
        mVersion = version,
        mEntry = entry,
        mStdlib = stdlib',
        mTestDir = testDir,
        mDocOut = docOut
      }

look :: Pairs -> Section -> String -> Maybe String
look pairs sec key = lookup (sec, key) pairs

require :: Pairs -> Section -> String -> Either String String
require pairs sec key =
  case look pairs sec key of
    Nothing -> Left ("[" ++ sec ++ "] " ++ key ++ " is required")
    Just v -> Right v

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

parseKV :: String -> Maybe (String, String)
parseKV s =
  case break (== '=') s of
    (_, []) -> Nothing
    (k, _ : vs) ->
      let key = strip k
          val = parseValue (dropWhile (== ' ') vs)
       in if null key then Nothing else Just (key, val)

parseValue :: String -> String
parseValue ('"' : rest) = takeWhile (/= '"') rest
parseValue s = strip (takeWhile (\c -> c /= '#' && c /= '\n') s)

strip :: String -> String
strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

die :: String -> IO a
die msg = putStrLn msg >> exitFailure
