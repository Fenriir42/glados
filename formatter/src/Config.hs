module Config
  ( FormatOptions (..),
    defaultOptions,
    loadConfig,
  )
where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data FormatOptions = FormatOptions
  { optIndentSize :: Int,
    optMaxWidth :: Int,
    optHardTabs :: Bool,
    optReorderImports :: Bool,
    optTrailingComma :: Bool
  }
  deriving (Show)

defaultOptions :: FormatOptions
defaultOptions =
  FormatOptions
    { optIndentSize = 4,
      optMaxWidth = 100,
      optHardTabs = False,
      optReorderImports = True,
      optTrailingComma = True
    }

-- | Load options from a simple key = value TOML-style config file.
-- Unknown keys are silently ignored. Missing keys fall back to defaults.
loadConfig :: FilePath -> IO FormatOptions
loadConfig fp = do
  contents <- readFile fp
  let pairs = parseKV contents
  return
    FormatOptions
      { optIndentSize = lookupInt pairs "indent_size" (optIndentSize defaultOptions),
        optMaxWidth = lookupInt pairs "max_width" (optMaxWidth defaultOptions),
        optHardTabs = lookupBool pairs "hard_tabs" (optHardTabs defaultOptions),
        optReorderImports = lookupBool pairs "reorder_imports" (optReorderImports defaultOptions),
        optTrailingComma = lookupBool pairs "trailing_comma" (optTrailingComma defaultOptions)
      }

-- ---------------------------------------------------------------------------

parseKV :: String -> [(String, String)]
parseKV src = foldr parseLine [] (lines src)
  where
    parseLine l acc =
      let s = dropWhile isSpace l
       in case s of
            [] -> acc
            (c : _) | c == '#' || c == '[' -> acc
            _ -> case break (== '=') s of
              (_, []) -> acc
              (k, _ : v) -> (trim k, trim v) : acc
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lookupInt :: [(String, String)] -> String -> Int -> Int
lookupInt pairs key def = fromMaybe def $ do
  v <- lookup key pairs
  readMaybe v

lookupBool :: [(String, String)] -> String -> Bool -> Bool
lookupBool pairs key def = fromMaybe def $ do
  v <- lookup key pairs
  case v of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
