-- | Built-in function implementations.
module VM.Builtins
  ( isBuiltin,
    isHeapBuiltin,
    callBuiltin,
  )
where

import Compiler.Bytecode (Value (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (toLower, toUpper)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HostName (getHostName)
import System.CPUTime (getCPUTime)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv, setEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stdout)
import System.Info (os)
import System.Process (system)

-- | True if the function is a pure builtin (no heap access needed).
isBuiltin :: Text -> Bool
isBuiltin name =
  name `elem` pureBuiltins
    || "math." `T.isPrefixOf` name
    || "string." `T.isPrefixOf` name
    || "sys." `T.isPrefixOf` name
    || "io." `T.isPrefixOf` name

-- | True if the function needs heap access (handled in Interpreter directly).
isHeapBuiltin :: Text -> Bool
isHeapBuiltin name = name `elem` heapBuiltins

pureBuiltins :: [Text]
pureBuiltins = ["print", "println"]

heapBuiltins :: [Text]
heapBuiltins =
  [ "array.len",
    "array.push",
    "array.pop",
    "len",
    "push",
    "pop",
    "string.split",
    "string.join",
    "sys.args"
  ]

-- | Execute a pure built-in. @strings@ is the caller's string pool.
callBuiltin :: Text -> [Text] -> [Value] -> IO Value
callBuiltin "print" strings args = builtinPrint strings args False >> return VUnit
callBuiltin "println" strings args = builtinPrint strings args True >> return VUnit
callBuiltin name strings args
  | "math." `T.isPrefixOf` name = callMath (T.drop 5 name) strings args
  | "string." `T.isPrefixOf` name = callString (T.drop 7 name) strings args
  | "sys." `T.isPrefixOf` name = callSys (T.drop 4 name) strings args
  | "io." `T.isPrefixOf` name = callIO (T.drop 3 name) strings args
callBuiltin name _ _ = ioError $ userError $ "Unknown builtin: " ++ T.unpack name

-- ---------------------------------------------------------------------------
-- Helpers

resolveStr :: [Text] -> Value -> Text
resolveStr strings (VStringRef i) = strings !! i
resolveStr _ (VString t) = t
resolveStr _ (VInt n) = T.pack (show n)
resolveStr _ (VFloat f) = T.pack (formatFloat f)
resolveStr _ (VBool b) = if b then "true" else "false"
resolveStr _ _ = ""

-- ---------------------------------------------------------------------------
-- print / println

builtinPrint :: [Text] -> [Value] -> Bool -> IO ()
builtinPrint _ [] nl = when nl $ TIO.putStrLn ""
builtinPrint strings (first : rest) nl = do
  TIO.putStr (renderValue strings first rest)
  when nl $ TIO.putStrLn ""

renderValue :: [Text] -> Value -> [Value] -> Text
renderValue strings (VStringRef idx) rest =
  let fmt = strings !! idx
   in if null rest then fmt else applyFormat fmt strings rest
renderValue strings (VString t) rest =
  if null rest then t else applyFormat t strings rest
renderValue _ (VInt n) _ = T.pack (show n)
renderValue _ (VFloat f) _ = T.pack (formatFloat f)
renderValue _ (VBool b) _ = if b then "True" else "False"
renderValue _ (VArrayRef i) _ = T.pack ("<array#" ++ show i ++ ">")
renderValue _ VUnit _ = ""

formatFloat :: Double -> String
formatFloat f
  | f == fromIntegral (truncate f :: Integer) = show (truncate f :: Integer) ++ ".0"
  | otherwise = show f

applyFormat :: Text -> [Text] -> [Value] -> Text
applyFormat fmt _ [] = fmt
applyFormat fmt strings (v : vs) =
  case T.breakOn "%" fmt of
    (before, rest)
      | T.null rest -> before
      | "%d" `T.isPrefixOf` rest ->
          before <> renderAsInt strings v <> applyFormat (T.drop 2 rest) strings vs
      | "%s" `T.isPrefixOf` rest ->
          before <> resolveStr strings v <> applyFormat (T.drop 2 rest) strings vs
      | "%f" `T.isPrefixOf` rest ->
          before <> renderAsFlt v <> applyFormat (T.drop 2 rest) strings vs
      | "%%" `T.isPrefixOf` rest ->
          before <> "%" <> applyFormat (T.drop 2 rest) strings (v : vs)
      | otherwise ->
          before <> T.take 2 rest <> applyFormat (T.drop 2 rest) strings (v : vs)

renderAsInt :: [Text] -> Value -> Text
renderAsInt _ (VInt n) = T.pack (show n)
renderAsInt _ (VFloat f) = T.pack (show (round f :: Integer))
renderAsInt _ (VBool b) = if b then "1" else "0"
renderAsInt ss (VStringRef i) = ss !! i
renderAsInt _ (VString t) = t
renderAsInt _ _ = "?"

renderAsFlt :: Value -> Text
renderAsFlt (VFloat f) = T.pack (formatFloat f)
renderAsFlt (VInt n) = T.pack (show n)
renderAsFlt _ = "?"

-- ---------------------------------------------------------------------------
-- math.*

callMath :: Text -> [Text] -> [Value] -> IO Value
callMath "sqrt" _ [VInt n] = return $ VFloat (sqrt (fromIntegral n))
callMath "sqrt" _ [VFloat f] = return $ VFloat (sqrt f)
callMath "abs" _ [VInt n] = return $ VInt (abs n)
callMath "abs" _ [VFloat f] = return $ VFloat (abs f)
callMath "fabs" _ [VFloat f] = return $ VFloat (abs f)
callMath "floor" _ [VFloat f] = return $ VInt (floor f)
callMath "ceil" _ [VFloat f] = return $ VInt (ceiling f)
callMath "round" _ [VFloat f] = return $ VInt (round f)
callMath "pow" _ [VFloat b, VFloat e] = return $ VFloat (b ** e)
callMath "pow" _ [VInt b, VInt e] = return $ VFloat (fromIntegral b ** fromIntegral e)
callMath "exp" _ [VFloat f] = return $ VFloat (exp f)
callMath "log" _ [VFloat f] = return $ VFloat (log f)
callMath "sin" _ [VFloat f] = return $ VFloat (sin f)
callMath "cos" _ [VFloat f] = return $ VFloat (cos f)
callMath "tan" _ [VFloat f] = return $ VFloat (tan f)
callMath "asin" _ [VFloat f] = return $ VFloat (asin f)
callMath "acos" _ [VFloat f] = return $ VFloat (acos f)
callMath "atan" _ [VFloat f] = return $ VFloat (atan f)
callMath "atan2" _ [VFloat y, VFloat x] = return $ VFloat (atan2 y x)
callMath "min" _ [VInt a, VInt b] = return $ VInt (min a b)
callMath "max" _ [VInt a, VInt b] = return $ VInt (max a b)
callMath "fmin" _ [VFloat a, VFloat b] = return $ VFloat (min a b)
callMath "fmax" _ [VFloat a, VFloat b] = return $ VFloat (max a b)
callMath name _ _ = ioError $ userError $ "Unknown math function: math." ++ T.unpack name

-- ---------------------------------------------------------------------------
-- string.*

callString :: Text -> [Text] -> [Value] -> IO Value
callString "len" ss [s] = return $ VInt (fromIntegral (T.length (resolveStr ss s)))
callString "concat" ss [a, b] = return $ VString (resolveStr ss a <> resolveStr ss b)
callString "substring" ss [s, VInt i, VInt j] =
  return $ VString (T.take (fromIntegral (j - i)) (T.drop (fromIntegral i) (resolveStr ss s)))
callString "char_at" ss [s, VInt i] = return $ VString (T.take 1 (T.drop (fromIntegral i) (resolveStr ss s)))
callString "contains" ss [s, sub] = return $ VBool (T.isInfixOf (resolveStr ss sub) (resolveStr ss s))
callString "starts_with" ss [s, pre] = return $ VBool (T.isPrefixOf (resolveStr ss pre) (resolveStr ss s))
callString "ends_with" ss [s, suf] = return $ VBool (T.isSuffixOf (resolveStr ss suf) (resolveStr ss s))
callString "index_of" ss [s, sub] =
  let haystack = T.unpack (resolveStr ss s)
      needle = T.unpack (resolveStr ss sub)
   in return $ VInt (maybe (-1) fromIntegral (findFirst needle haystack))
callString "last_index_of" ss [s, sub] =
  let haystack = T.unpack (resolveStr ss s)
      needle = T.unpack (resolveStr ss sub)
   in return $ VInt (maybe (-1) fromIntegral (findLast needle haystack))
callString "to_upper" ss [s] = return $ VString (T.map toUpper (resolveStr ss s))
callString "to_lower" ss [s] = return $ VString (T.map toLower (resolveStr ss s))
callString "trim" ss [s] = return $ VString (T.strip (resolveStr ss s))
callString "trim_left" ss [s] = return $ VString (T.stripStart (resolveStr ss s))
callString "trim_right" ss [s] = return $ VString (T.stripEnd (resolveStr ss s))
callString "reverse" ss [s] = return $ VString (T.reverse (resolveStr ss s))
callString "replace" ss [s, old, new'] =
  return $ VString (T.replace (resolveStr ss old) (resolveStr ss new') (resolveStr ss s))
callString "replace_first" ss [s, old, new'] =
  let t = resolveStr ss s
      from = resolveStr ss old
      to' = resolveStr ss new'
   in case T.breakOn from t of
        (pre, rest)
          | T.null rest -> return $ VString t
          | otherwise -> return $ VString (pre <> to' <> T.drop (T.length from) rest)
callString "repeat" ss [s, VInt n] =
  return $ VString (T.concat (replicate (fromIntegral n) (resolveStr ss s)))
callString "is_empty" ss [s] = return $ VBool (T.null (resolveStr ss s))
callString "from_int" _ [VInt n] = return $ VString (T.pack (show n))
callString "from_float" _ [VFloat f] = return $ VString (T.pack (formatFloat f))
callString "to_int" ss [s] =
  case reads (T.unpack (resolveStr ss s)) of
    [(n, "")] -> return $ VInt n
    _ -> return $ VInt 0
callString "to_float" ss [s] =
  case reads (T.unpack (resolveStr ss s)) of
    [(f, "")] -> return $ VFloat f
    _ -> return $ VFloat 0.0
callString name _ _ = ioError $ userError $ "Unknown string function: string." ++ T.unpack name

findFirst :: String -> String -> Maybe Int
findFirst needle = go 0
  where
    n = length needle
    go i s
      | length s < n = Nothing
      | needle `isPrefixOf` s = Just i
      | otherwise = go (i + 1) (drop 1 s)

findLast :: String -> String -> Maybe Int
findLast needle = go Nothing 0
  where
    n = length needle
    go acc i s
      | length s < n = acc
      | needle `isPrefixOf` s = go (Just i) (i + 1) (drop 1 s)
      | otherwise = go acc (i + 1) (drop 1 s)

-- ---------------------------------------------------------------------------
-- sys.*

callSys :: Text -> [Text] -> [Value] -> IO Value
callSys "exit" _ [VInt code] =
  exitWith (if code == 0 then ExitSuccess else ExitFailure (fromIntegral code))
callSys "time" _ [] = VInt . floor <$> getPOSIXTime
callSys "time_millis" _ [] = do
  t <- getCPUTime
  return $ VInt (fromIntegral (t `div` 1000000000))
callSys "sleep" _ [VInt ms] = do
  threadDelay (fromIntegral ms * 1000)
  return VUnit
callSys "argc" _ [] = VInt . fromIntegral . length <$> getArgs
callSys "env" ss [name] = do
  val <- lookupEnv (T.unpack (resolveStr ss name))
  return $ VString (maybe "" T.pack val)
callSys "set_env" ss [name, val] = do
  setEnv (T.unpack (resolveStr ss name)) (T.unpack (resolveStr ss val))
  return $ VBool True
callSys "platform" _ [] = return $ VString (T.pack normaliseOs)
callSys "hostname" _ [] = VString . T.pack <$> getHostName
callSys "getcwd" _ [] = VString . T.pack <$> getCurrentDirectory
callSys "chdir" ss [path] = do
  r <- try (setCurrentDirectory (T.unpack (resolveStr ss path))) :: IO (Either SomeException ())
  return $ VBool (either (const False) (const True) r)
callSys "system" ss [cmd] = do
  code <- system (T.unpack (resolveStr ss cmd))
  return $ VInt (case code of ExitSuccess -> 0; ExitFailure n -> fromIntegral n)
callSys name _ _ = ioError $ userError $ "Unknown sys function: sys." ++ T.unpack name

normaliseOs :: String
normaliseOs = case os of
  "mingw32" -> "windows"
  "darwin" -> "macos"
  _ -> "linux"

-- ---------------------------------------------------------------------------
-- io.*

callIO :: Text -> [Text] -> [Value] -> IO Value
callIO "print" strings args = builtinPrint strings args False >> return VUnit
callIO "println" strings args = builtinPrint strings args True >> return VUnit
callIO "read" _ [] = do
  hFlush stdout
  VString . T.pack <$> getLine
callIO name _ _ = ioError $ userError $ "Unknown io function: io." ++ T.unpack name
