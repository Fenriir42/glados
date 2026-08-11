{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | @glados panik@ -- an AFL-style input stress tester.
--
-- Where @native-diff@ and @fuzz@ test /our backend/ by comparing engines,
-- @panik@ tests /the user's own program/: it compiles the project to a native
-- binary and then throws a crapload of generated argument vectors at it,
-- concurrently, hunting for crashes (signals), hangs (timeouts), and -- with
-- the validity oracle -- inputs the program wrongly accepts or rejects.
--
-- The argument grammar lives in the project's @quant.toml@ under a @[panik]@
-- section; see 'loadPanikCfg' for the mini-DSL.
module Panik (runPanik) where

import Compile (buildNative, compileSource, resolveStdlib)
import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, replicateM, unless, void, when)
import Data.Bits (shiftR)
import Data.Char (isAlphaNum)
import Data.IORef
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import Display
  ( bold,
    cyan,
    dim,
    green,
    printColored,
    printOk,
    printStep,
    red,
    reset,
    yellow,
  )
import GHC.Clock (getMonotonicTimeNSec)
import Manifest
  ( Manifest (mEntry, mRaw, mStdlib),
    loadManifest,
    lookBool,
    lookDouble,
    lookInt,
    lookList,
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStr, stderr)
import System.Process
  ( StdStream (NoStream),
    createProcess,
    proc,
    std_err,
    std_in,
    std_out,
    terminateProcess,
    waitForProcess,
  )
import System.Timeout (timeout)

-- ---------------------------------------------------------------------------
-- Argument grammar

-- | One positional argument's specification, parsed from a @[panik] args@
-- token such as @"int:-100..100"@ or @"choice:red,green,blue"@.
data ArgSpec
  = AInt Integer Integer
  | AFloat Double Double
  | -- | minimum and maximum string length
    AStr Int Int
  | AChoice [String]
  | ABool

-- | The whole @[panik]@ configuration, after merging CLI overrides.
data PanikCfg = PanikCfg
  { pArgs :: [ArgSpec],
    pBatch :: Int,
    pJobs :: Int,
    pTimeoutMs :: Int,
    pSwap :: Bool,
    pInvalid :: Double
  }

-- ---------------------------------------------------------------------------
-- A minimal seeded RNG (Knuth MMIX LCG), so runs are reproducible by --seed.

newtype Gen a = Gen {runGen :: Word64 -> (a, Word64)}

instance Functor Gen where
  fmap f (Gen g) = Gen (\s -> let (a, s') = g s in (f a, s'))

instance Applicative Gen where
  pure a = Gen (a,)
  Gen gf <*> Gen gx =
    Gen (\s -> let (h, s1) = gf s; (a, s2) = gx s1 in (h a, s2))

instance Monad Gen where
  Gen g >>= k = Gen (\s -> let (a, s') = g s in runGen (k a) s')

word :: Gen Word64
word =
  Gen (\s -> let s' = s * 6364136223846793005 + 1442695040888963407 in (s' `shiftR` 33, s'))

-- | Inclusive integer in @[lo, hi]@.
intR :: Integer -> Integer -> Gen Integer
intR lo hi
  | hi <= lo = pure lo
  | otherwise = do
      w <- word
      pure (lo + toInteger (w `mod` fromInteger (hi - lo + 1)))

-- | Float in @[lo, hi)@ (or @lo@ when degenerate).
dblR :: Double -> Double -> Gen Double
dblR lo hi
  | hi <= lo = pure lo
  | otherwise = do
      w <- word
      let f = fromIntegral (w `mod` 1000000000) / 1000000000.0
      pure (lo + f * (hi - lo))

pick :: [a] -> Gen a
pick [] = error "panik: pick []" -- never called on empty lists
pick xs = do
  i <- intR 0 (toInteger (length xs - 1))
  pure (xs !! fromInteger i)

-- | True with probability @p@.
chance :: Double -> Gen Bool
chance p = do
  w <- word
  pure (fromIntegral (w `mod` 100000) / 100000.0 < p)

-- | A printable ASCII string of length in @[lo, hi]@.
genStr :: Int -> Int -> Gen String
genStr lo hi = do
  n <- intR (toInteger (max 0 lo)) (toInteger (max 0 hi))
  go (fromInteger n)
  where
    go :: Int -> Gen String
    go 0 = pure ""
    go k = do
      c <- intR 32 126
      rest <- go (k - 1)
      pure (toEnum (fromInteger c) : rest)

-- | Fisher-Yates shuffle.
shuffle :: [a] -> Gen [a]
shuffle [] = pure []
shuffle xs = do
  i <- fromInteger <$> intR 0 (toInteger (length xs - 1))
  rest <- shuffle (dropAt i xs)
  pure (xs !! i : rest)

-- ---------------------------------------------------------------------------
-- Input generation

-- | A single generated run: the argv, whether it is in-spec, and a short
-- human-readable note about how it was mutated (for invalid runs).
data Job = Job
  { jArgs :: [String],
    jValid :: Bool,
    jNote :: String
  }

-- | A valid value drawn from a spec.
genValid :: ArgSpec -> Gen String
genValid (AInt lo hi) = show <$> intR lo hi
genValid (AFloat lo hi) = show <$> dblR lo hi
genValid (AStr lo hi) = genStr lo hi
genValid (AChoice cs) = pick cs
genValid ABool = pick ["true", "false"]

-- | An out-of-spec value for one argument, with a description of the breach.
genInvalidArg :: ArgSpec -> Gen (String, String)
genInvalidArg (AInt lo hi) = do
  k <- intR 0 2
  case k of
    0 -> (,"above range") . show <$> intR (hi + 1) (hi + 1000)
    1 -> (,"below range") . show <$> intR (lo - 1000) (lo - 1)
    _ -> (,"not an int") <$> genStr 1 6
genInvalidArg (AFloat _ hi) = do
  k <- intR 0 1
  case k of
    0 -> (,"above range") . show <$> dblR (hi + 1) (hi + 1000)
    _ -> (,"not a float") <$> pick ["nan", "inf", "1.2.3", "xyz"]
genInvalidArg (AStr lo hi) = do
  k <- intR 0 1
  if k == 0 || lo <= 0
    then (,"too long") <$> genStr (hi + 1) (hi + 64)
    else pure ("", "empty (min " ++ show lo ++ ")")
genInvalidArg (AChoice _) = (,"not in choice set") <$> genStr 3 8
genInvalidArg ABool = (,"not a bool") <$> pick ["yes", "no", "1", "0", "maybe"]

-- | Generate one job honouring the invalid ratio and swap setting.
genJob :: PanikCfg -> Gen Job
genJob cfg = do
  valid <- chance (1.0 - pInvalid cfg)
  base <- mapM genValid (pArgs cfg)
  (args, note) <-
    if valid
      then pure (base, "valid")
      else mutate (pArgs cfg) base
  args' <- if pSwap cfg then shuffle args else pure args
  pure (Job args' valid note)

-- | Turn a valid argv into a deliberately out-of-spec one.
mutate :: [ArgSpec] -> [String] -> Gen ([String], String)
mutate specs base = do
  let n = length base
      kinds = ["value" | n > 0] ++ ["drop" | n > 0] ++ ["extra", "empty"]
  kind <- pick kinds
  case kind of
    "value" -> do
      i <- fromInteger <$> intR 0 (toInteger (n - 1))
      (v, desc) <- genInvalidArg (specs !! i)
      pure (setAt i v base, "arg " ++ show i ++ ": " ++ desc)
    "drop" -> do
      i <- fromInteger <$> intR 0 (toInteger (n - 1))
      pure (dropAt i base, "dropped arg " ++ show i)
    "extra" -> do
      v <- genStr 1 8
      pure (base ++ [v], "extra trailing arg")
    _ -> pure ([], "no arguments")

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

dropAt :: Int -> [a] -> [a]
dropAt i xs = take i xs ++ drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- Running one input

-- | What happened to a single run.
data Outcome
  = OZero -- exited 0
  | ONonZero Int -- exited non-zero (clean error)
  | OSignal Int -- killed by a signal (crash)
  | OHang -- exceeded the timeout
  deriving (Eq)

data Result = Result
  { rJob :: Job,
    rOutcome :: Outcome,
    rNanos :: Word64
  }

-- | Spawn the binary with an argv, discarding all streams, honouring the
-- timeout.  Timing is wall-clock nanoseconds around the spawn+wait.
runOne :: FilePath -> Int -> Job -> IO Result
runOne bin timeoutMs job = do
  t0 <- getMonotonicTimeNSec
  (_, _, _, ph) <-
    createProcess
      (proc bin (jArgs job))
        { std_in = NoStream,
          std_out = NoStream,
          std_err = NoStream
        }
  mec <- timeout (timeoutMs * 1000) (waitForProcess ph)
  t1 <- getMonotonicTimeNSec
  outcome <- case mec of
    Nothing -> do
      terminateProcess ph
      _ <- waitForProcess ph
      pure OHang
    Just ExitSuccess -> pure OZero
    Just (ExitFailure n)
      | n < 0 -> pure (OSignal (negate n)) -- process encodes signals as -sig
      | n >= 128 -> pure (OSignal (n - 128))
      | otherwise -> pure (ONonZero n)
  pure (Result job outcome (t1 - t0))

-- ---------------------------------------------------------------------------
-- Concurrent worker pool

-- | Run every task across @n@ worker threads, bumping @counter@ as each
-- finishes.  Result order is unspecified (it does not matter here).
runPool :: Int -> IORef Int -> [IO a] -> IO [a]
runPool n counter tasks = do
  queue <- newIORef tasks
  results <- newIORef []
  done <- newEmptyMVar
  let worker = do
        mt <-
          atomicModifyIORef' queue $ \case
            [] -> ([], Nothing)
            (x : xs) -> (xs, Just x)
        case mt of
          Nothing -> putMVar done ()
          Just t -> do
            r <- t
            atomicModifyIORef' results (\rs -> (r : rs, ()))
            atomicModifyIORef' counter (\c -> (c + 1, ()))
            worker
  forM_ [1 .. n] (const (forkIO worker))
  forM_ [1 .. n] (const (takeMVar done))
  readIORef results

-- ---------------------------------------------------------------------------
-- Entry point

-- | @runPanik seed batch jobs timeoutMs@ -- the @Maybe@s override the manifest.
runPanik :: Int -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
runPanik seed mBatch mJobs mTimeout = do
  m <- loadManifest
  cfg <- loadPanikCfg m mBatch mJobs mTimeout
  bin <- buildTarget m
  printStep "panik" $
    show (pBatch cfg)
      ++ " runs, "
      ++ show (pJobs cfg)
      ++ " workers, "
      ++ show (round (pInvalid cfg * 100) :: Int)
      ++ "% out-of-spec, timeout "
      ++ show (pTimeoutMs cfg)
      ++ "ms"
  let jobs = fst (runGen (replicateM (pBatch cfg) (genJob cfg)) (fromIntegral seed + 1))
      tasks = map (runOne bin (pTimeoutMs cfg)) jobs
  interactive <- hIsTerminalDevice stderr
  counter <- newIORef 0
  ticking <- newIORef True
  when interactive $ void $ forkIO (progress counter (pBatch cfg) ticking)
  results <- runPool (pJobs cfg) counter tasks
  writeIORef ticking False
  when interactive $ hPutStr stderr "\r\ESC[K" >> hFlush stderr
  report bin cfg results

-- | Compile the project entry point to a native binary under @.build/@.
buildTarget :: Manifest -> IO FilePath
buildTarget m = do
  stdlib <- resolveStdlib (mStdlib m)
  createDirectoryIfMissing True (".build" </> "panik")
  let out = ".build" </> "panik" </> "target"
  printStep "compiling" (mEntry m ++ " -> native")
  r <- try (compileSource stdlib (mEntry m) >>= \bc -> buildNative bc out)
  case r of
    Right () -> pure out
    Left e ->
      die $
        "panik needs a native-translatable program, but the build failed:\n"
          ++ show (e :: SomeException)

-- | Live progress line on stderr, refreshed every 200 ms until stopped.
progress :: IORef Int -> Int -> IORef Bool -> IO ()
progress counter total ticking = do
  live <- readIORef ticking
  when live $ do
    c <- readIORef counter
    hPutStr stderr ("\r  " ++ dim ++ "running " ++ show c ++ "/" ++ show total ++ reset ++ "\ESC[K")
    hFlush stderr
    threadDelay 200000
    progress counter total ticking

-- ---------------------------------------------------------------------------
-- Configuration loading

loadPanikCfg :: Manifest -> Maybe Int -> Maybe Int -> Maybe Int -> IO PanikCfg
loadPanikCfg m mBatch mJobs mTimeout = do
  let pairs = mRaw m
      toks = lookList pairs "panik" "args"
  when (null toks) $
    die "no [panik] section found in quant.toml (need at least an `args = [...]` list)"
  specs <- case mapM parseSpec toks of
    Left err -> die ("panik: bad [panik] args: " ++ err)
    Right ss -> pure ss
  nCaps <- getNumCapabilities
  let firstJust xs = case catMaybes xs of (x : _) -> Just x; [] -> Nothing
      batch = fromMaybe 500 (firstJust [mBatch, lookInt pairs "panik" "batch"])
      jobs = fromMaybe nCaps (firstJust [mJobs, lookInt pairs "panik" "jobs"])
      tmo = fromMaybe 2000 (firstJust [mTimeout, lookInt pairs "panik" "timeout_ms"])
      swap = fromMaybe False (lookBool pairs "panik" "swap")
      invalid = clamp01 (fromMaybe 0.25 (lookDouble pairs "panik" "invalid"))
  pure
    PanikCfg
      { pArgs = specs,
        pBatch = max 1 batch,
        pJobs = max 1 jobs,
        pTimeoutMs = max 1 tmo,
        pSwap = swap,
        pInvalid = invalid
      }
  where
    clamp01 x = max 0.0 (min 1.0 x)

-- | Parse one @[panik] args@ token into an 'ArgSpec'.
parseSpec :: String -> Either String ArgSpec
parseSpec raw =
  let (name, rest) = case break (== ':') raw of
        (n, ':' : r) -> (n, r)
        (n, _) -> (n, "")
   in case name of
        "int" -> let (lo, hi) = intRange rest (-1000000) 1000000 in Right (AInt lo hi)
        "float" -> let (lo, hi) = fltRange rest (-1000000) 1000000 in Right (AFloat lo hi)
        "str" -> let (lo, hi) = intRange rest 0 32 in Right (AStr (fromInteger lo) (fromInteger hi))
        "choice"
          | null rest -> Left "choice needs values, e.g. choice:a,b,c"
          | otherwise -> Right (AChoice (splitComma rest))
        "bool" -> Right ABool
        other -> Left ("unknown arg type `" ++ other ++ "` (int|float|str|choice|bool)")

-- | Parse @"LO..HI"@, a single @"N"@, or an empty string (defaults).
intRange :: String -> Integer -> Integer -> (Integer, Integer)
intRange s dlo dhi
  | null s = (dlo, dhi)
  | otherwise = case splitDots s of
      Just (a, b) -> (readOr dlo a, readOr dhi b)
      Nothing -> let v = readOr dlo s in (v, v)

fltRange :: String -> Double -> Double -> (Double, Double)
fltRange s dlo dhi
  | null s = (dlo, dhi)
  | otherwise = case splitDots s of
      Just (a, b) -> (readOr dlo a, readOr dhi b)
      Nothing -> let v = readOr dlo s in (v, v)

readOr :: (Read a) => a -> String -> a
readOr d s = case reads s of [(v, "")] -> v; _ -> d

-- | Split on the first @".."@.
splitDots :: String -> Maybe (String, String)
splitDots = go ""
  where
    go acc ('.' : '.' : r) = Just (reverse acc, r)
    go acc (c : r) = go (c : acc) r
    go _ [] = Nothing

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (a, ',' : r) -> a : splitComma r
  (a, _) -> [a]

-- ---------------------------------------------------------------------------
-- Reporting

report :: FilePath -> PanikCfg -> [Result] -> IO ()
report bin cfg results = do
  let total = length results
      crashes = [r | r <- results, isCrash (rOutcome r)]
      hangs = [r | r <- results, rOutcome r == OHang]
      rejectedValid = [r | r <- results, jValid (rJob r), isCleanError (rOutcome r)]
      acceptedInvalid = [r | r <- results, not (jValid (rJob r)), rOutcome r == OZero]
      okCount = total - length crashes - length hangs
  putStrLn ""
  printStep "runs" (show total ++ " completed")
  reportBucket red "crashes" bin crashes describeOutcome
  reportBucket red "hangs" bin hangs (const ("timeout > " ++ show (pTimeoutMs cfg) ++ "ms"))
  when (pInvalid cfg > 0.0) $ do
    reportBucket yellow "rejected-valid" bin rejectedValid describeOutcome
    reportBucket yellow "accepted-invalid" bin acceptedInvalid (jNote . rJob)
  reportPerf results
  putStrLn ""
  let hard = length crashes + length hangs
      soft = length rejectedValid + length acceptedInvalid
  if hard == 0 && soft == 0
    then printOk (show okCount ++ "/" ++ show total ++ " runs clean -- no crashes, hangs, or oracle violations")
    else do
      printColored $
        bold
          ++ red
          ++ "  panik"
          ++ reset
          ++ "  "
          ++ show hard
          ++ " hard ("
          ++ show (length crashes)
          ++ " crash / "
          ++ show (length hangs)
          ++ " hang), "
          ++ show soft
          ++ " oracle violation(s)\n"
      exitFailure

isCrash :: Outcome -> Bool
isCrash (OSignal _) = True
isCrash OHang = True
isCrash _ = False

isCleanError :: Outcome -> Bool
isCleanError (ONonZero _) = True
isCleanError _ = False

describeOutcome :: Result -> String
describeOutcome r = case rOutcome r of
  OSignal n -> "signal " ++ show n ++ signalName n
  ONonZero n -> "exit " ++ show n
  OHang -> "timeout"
  OZero -> "exit 0"

signalName :: Int -> String
signalName 11 = " (SIGSEGV)"
signalName 6 = " (SIGABRT)"
signalName 8 = " (SIGFPE)"
signalName 4 = " (SIGILL)"
signalName _ = ""

-- | Print a labelled bucket with a count and up to a few sample repros.
reportBucket :: String -> String -> FilePath -> [Result] -> (Result -> String) -> IO ()
reportBucket colour label bin rs explain = do
  let n = length rs
      swatch = if n == 0 then green else colour
  printColored $ "  " ++ swatch ++ pad 18 label ++ reset ++ "  " ++ show n ++ "\n"
  unless (null rs) $
    forM_ (take 6 rs) $ \r ->
      printColored $
        "    "
          ++ dim
          ++ explain r
          ++ reset
          ++ "\n      "
          ++ cyan
          ++ reproCmd bin (jArgs (rJob r))
          ++ reset
          ++ "\n"

-- | Slowest runs and mean, to surface performance cliffs.
reportPerf :: [Result] -> IO ()
reportPerf [] = pure ()
reportPerf results = do
  let nanos = map rNanos results
      meanMs = fromIntegral (sum nanos) / fromIntegral (length nanos) / 1.0e6 :: Double
      slow = take 3 (sortBy (comparing (Down . rNanos)) results)
  printColored $ "  " ++ dim ++ pad 18 "perf" ++ reset ++ "  mean " ++ showMs meanMs ++ "\n"
  forM_ slow $ \r ->
    printColored $
      "    "
        ++ dim
        ++ showMs (fromIntegral (rNanos r) / 1.0e6)
        ++ reset
        ++ "  "
        ++ cyan
        ++ reproCmd "" (jArgs (rJob r))
        ++ reset
        ++ "\n"

showMs :: Double -> String
showMs ms =
  let scaled = fromIntegral (round (ms * 100) :: Integer) / 100.0 :: Double
   in show scaled ++ "ms"

-- | A copy-pasteable command line reproducing a run.
reproCmd :: FilePath -> [String] -> String
reproCmd bin args = unwords (filter (not . null) (bin : map shellQuote args))

-- | Single-quote an argv element if it contains anything shell-special.
shellQuote :: String -> String
shellQuote "" = "''"
shellQuote s
  | all safe s = s
  | otherwise = "'" ++ concatMap esc s ++ "'"
  where
    safe c = isAlphaNum c || c `elem` "._-/=+,:@"
    esc '\'' = "'\\''"
    esc c = [c]

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

die :: String -> IO a
die msg = printColored (red ++ msg ++ reset ++ "\n") >> exitFailure
