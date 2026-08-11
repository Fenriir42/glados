{-# LANGUAGE TupleSections #-}

-- | Differential fuzzer: generate random, type-correct, terminating Quant
-- programs and run each under both the bytecode VM and a natively compiled
-- binary, reporting any divergence in stdout or exit code.  Programs are
-- deterministic (no time/randomness/IO beyond @println@) so a divergence is
-- always a real bug.  Everything is seeded, so a failing case is reproducible
-- with @glados fuzz --seed N --count 1@.
module Fuzz (runFuzz) where

import Control.Monad (forM, replicateM)
import Data.Bits (shiftR)
import Data.List (intercalate)
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- ---------------------------------------------------------------------------
-- Tiny deterministic generator monad (LCG; no external dependency)

newtype Gen a = Gen {runGen :: Word64 -> (a, Word64)}

instance Functor Gen where
  fmap f (Gen g) = Gen $ \s -> let (a, s') = g s in (f a, s')

instance Applicative Gen where
  pure a = Gen (a,)
  Gen gf <*> Gen ga = Gen $ \s ->
    let (f, s1) = gf s
        (a, s2) = ga s1
     in (f a, s2)

instance Monad Gen where
  Gen ga >>= f = Gen $ \s -> let (a, s1) = ga s in runGen (f a) s1

-- | Advance the LCG (Knuth MMIX constants) and return 31 high bits.
step :: Gen Word64
step = Gen $ \s ->
  let s' = s * 6364136223846793005 + 1442695040888963407
   in (s' `shiftR` 33, s')

-- | Uniform integer in the inclusive range [lo, hi].
rangeI :: Int -> Int -> Gen Int
rangeI lo hi
  | hi <= lo = pure lo
  | otherwise = do
      w <- step
      pure (lo + fromIntegral (w `mod` fromIntegral (hi - lo + 1)))

elements :: [a] -> Gen a
elements xs = do
  i <- rangeI 0 (length xs - 1)
  pure (xs !! i)

-- | Choose and run one of the given generators.
oneOf :: [Gen a] -> Gen a
oneOf gs = do
  i <- rangeI 0 (length gs - 1)
  gs !! i

-- ---------------------------------------------------------------------------
-- Program generation

data Ty = TInt | TFloat | TBool | TStr deriving (Eq)

tyName :: Ty -> String
tyName TInt = "int"
tyName TFloat = "float"
tyName TBool = "bool"
tyName TStr = "str"

-- | A visible function: name, argument types, return type.
type FnSig = (String, [Ty], Ty)

data Ctx = Ctx {ctxScope :: [(String, Ty)], ctxFns :: [FnSig]}

varsOf :: Ctx -> Ty -> [String]
varsOf ctx ty = [n | (n, t) <- ctxScope ctx, t == ty]

fnsOf :: Ctx -> Ty -> [FnSig]
fnsOf ctx ty = [f | f@(_, _, r) <- ctxFns ctx, r == ty]

-- | A literal of the given type.  Integers are kept small and negatives are
-- written as @(0 - k)@ so intermediate products stay well inside int64 (the
-- VM is arbitrary-precision, the native backend is 64-bit) and parse cleanly.
genLit :: Ty -> Gen String
genLit TInt = do
  n <- rangeI (-20) 20
  pure (if n < 0 then "(0 - " ++ show (negate n) ++ ")" else show n)
genLit TFloat = do
  d <- rangeI 0 20
  f <- rangeI 0 9
  pure (show d ++ "." ++ show f)
genLit TBool = elements ["True", "False"]
genLit TStr = do
  k <- rangeI 0 5
  cs <- replicateM k (elements "abcXYZ012 ")
  pure (show cs)

-- | An expression of type @ty@ with a recursion budget of @depth@.
genExpr :: Ctx -> Int -> Ty -> Gen String
genExpr ctx depth ty =
  let leaves = genLit ty : map pure (varsOf ctx ty)
   in if depth <= 0
        then oneOf leaves
        else oneOf (leaves ++ compound ctx depth ty)

-- | Recursive productions for each type.
compound :: Ctx -> Int -> Ty -> [Gen String]
compound ctx depth ty =
  calls ++ case ty of
    TInt ->
      [ bin TInt "+",
        bin TInt "-",
        bin TInt "*",
        divmod "/",
        divmod "%",
        unary TFloat "int"
      ]
    TFloat ->
      [ bin TFloat "+",
        bin TFloat "-",
        bin TFloat "*",
        unary TInt "float"
      ]
    TBool ->
      [ cmp,
        bin TBool "&&",
        bin TBool "||",
        notE,
        strPred "contains",
        strPred "starts_with",
        strPred "ends_with",
        do s <- sub TStr; pure ("string.is_empty(" ++ s ++ ")")
      ]
    TStr ->
      [ do a <- sub TStr; b <- sub TStr; pure ("string.concat(" ++ a ++ ", " ++ b ++ ")"),
        do i <- sub TInt; pure ("string.from_int(" ++ i ++ ")"),
        do s <- sub TStr; pure ("string.to_upper(" ++ s ++ ")"),
        do s <- sub TStr; pure ("string.to_lower(" ++ s ++ ")"),
        do s <- sub TStr; pure ("string.reverse(" ++ s ++ ")"),
        do s <- sub TStr; pure ("string.trim(" ++ s ++ ")"),
        do s <- sub TStr; i <- rangeI 0 6; pure ("string.char_at(" ++ s ++ ", " ++ show i ++ ")"),
        do s <- sub TStr; i <- rangeI 0 3; j <- rangeI 0 6; pure ("string.substring(" ++ s ++ ", " ++ show i ++ ", " ++ show j ++ ")")
      ]
  where
    sub = genExpr ctx (depth - 1)
    bin t op = do a <- sub t; b <- sub t; pure ("(" ++ a ++ " " ++ op ++ " " ++ b ++ ")")
    -- Divide/modulo by a non-zero literal to avoid spurious divide-by-zero.
    divmod op = do a <- sub TInt; d <- rangeI 1 9; pure ("(" ++ a ++ " " ++ op ++ " " ++ show d ++ ")")
    unary t fn = do e <- sub t; pure (fn ++ "(" ++ e ++ ")")
    notE = do e <- sub TBool; pure ("(!" ++ e ++ ")")
    strPred fn = do a <- sub TStr; b <- sub TStr; pure ("string." ++ fn ++ "(" ++ a ++ ", " ++ b ++ ")")
    cmp = do
      ct <- elements [TInt, TFloat, TStr]
      op <- if ct == TStr then elements ["==", "!="] else elements ["==", "!=", "<", ">", "<=", ">="]
      a <- sub ct
      b <- sub ct
      pure ("(" ++ a ++ " " ++ op ++ " " ++ b ++ ")")
    calls = [genCall ctx depth f | f <- fnsOf ctx ty]

genCall :: Ctx -> Int -> FnSig -> Gen String
genCall ctx depth (name, args, _) = do
  as <- mapM (genExpr ctx (depth - 1)) args
  pure (name ++ "(" ++ intercalate ", " as ++ ")")

-- | One statement, plus the (possibly extended) context for what follows.
-- Declarations add a variable; everything else leaves the scope unchanged
-- (nested blocks scope their own declarations).
genStmt :: Ctx -> Int -> Gen (String, Ctx)
genStmt ctx nest = do
  c <- rangeI 0 (if nest > 0 then 11 else 8)
  if c <= 3
    then do
      -- print
      ty <- elements [TInt, TFloat, TBool, TStr]
      e <- genExpr ctx 3 ty
      pure ("    println(" ++ e ++ ");\n", ctx)
    else
      if c <= 8
        then do
          -- declaration
          ty <- elements [TInt, TFloat, TBool, TStr]
          let vn = "v" ++ show (length (ctxScope ctx))
          e <- genExpr ctx 3 ty
          pure ("    " ++ vn ++ ": " ++ tyName ty ++ " = " ++ e ++ ";\n", ctx {ctxScope = (vn, ty) : ctxScope ctx})
        else
          if c <= 10
            then do
              -- if / else
              cond <- genExpr ctx 2 TBool
              (thenB, _) <- genBody ctx (nest - 1) =<< rangeI 1 3
              (elseB, _) <- genBody ctx (nest - 1) =<< rangeI 1 3
              pure ("    if (" ++ cond ++ ") {\n" ++ thenB ++ "    } else {\n" ++ elseB ++ "    };\n", ctx)
            else do
              -- bounded for loop
              n <- rangeI 2 6
              let lv = "fv" ++ show (length (ctxScope ctx))
                  loopCtx = ctx {ctxScope = (lv, TInt) : ctxScope ctx}
              (body, _) <- genBody loopCtx (nest - 1) =<< rangeI 1 3
              pure
                ( "    for ("
                    ++ lv
                    ++ ": int = 0; "
                    ++ lv
                    ++ " < "
                    ++ show n
                    ++ "; "
                    ++ lv
                    ++ " = "
                    ++ lv
                    ++ " + 1) {\n"
                    ++ body
                    ++ "    };\n",
                  ctx
                )

-- | A sequence of @count@ statements, threading declarations through.
genBody :: Ctx -> Int -> Int -> Gen (String, Ctx)
genBody ctx0 nest count = go ctx0 count []
  where
    go ctx 0 acc = pure (concat (reverse acc), ctx)
    go ctx k acc = do
      (s, ctx') <- genStmt ctx nest
      go ctx' (k - 1) (s : acc)

-- | Non-recursive helper functions: function @i@ may only call functions
-- @0..i-1@, which rules out recursion and guarantees termination.
genFns :: Int -> Gen ([String], [FnSig])
genFns total = go 0 [] []
  where
    go i defs sigs
      | i >= total = pure (reverse defs, sigs)
      | otherwise = do
          nargs <- rangeI 1 2
          argTys <- replicateM nargs (elements [TInt, TFloat, TBool, TStr])
          retTy <- elements [TInt, TFloat, TStr]
          let name = "h" ++ show i
              params = [("p" ++ show j, t) | (j, t) <- zip [(0 :: Int) ..] argTys]
          body <- genExpr (Ctx params sigs) 3 retTy
          let def =
                "fn "
                  ++ name
                  ++ "("
                  ++ intercalate ", " [pn ++ ": " ++ tyName t | (pn, t) <- params]
                  ++ ") -> "
                  ++ tyName retTy
                  ++ " {\n"
                  ++ "    return "
                  ++ body
                  ++ ";\n}\n"
          go (i + 1) (def : defs) (sigs ++ [(name, argTys, retTy)])

genProgram :: Gen String
genProgram = do
  nfns <- rangeI 0 3
  (fnDefs, fnSigs) <- genFns nfns
  nstmts <- rangeI 5 12
  (body, ctx) <- genBody (Ctx [] fnSigs) 2 nstmts
  -- Guarantee observable output so the diff is meaningful.
  ty <- elements [TInt, TFloat, TBool, TStr]
  final <- genExpr ctx 3 ty
  pure (concat fnDefs ++ "fn main() -> void {\n" ++ body ++ "    println(" ++ final ++ ");\n}\n")

-- ---------------------------------------------------------------------------
-- Runner

data Outcome
  = Match
  | BuildFail String
  | Diverge String String -- vm out, native out

-- | Build one generated program natively, run it under both engines, and
-- compare stdout + exit code.
checkProgram :: FilePath -> FilePath -> String -> IO Outcome
checkProgram self outDir prog = do
  let qa = outDir </> "case.qa"
      bin = outDir </> "case"
  writeFile qa prog
  built <- run self ["compiler", qa, "--native", bin]
  case built of
    Nothing -> pure (BuildFail "native build timed out")
    Just (ExitFailure _, _, be) -> pure (BuildFail be)
    Just (ExitSuccess, _, _) -> do
      vm <- run self ["compiler", qa]
      nat <- run bin []
      case (vm, nat) of
        (Just (vc, vo, _), Just (nc, no, _))
          | vo == no && vc == nc -> pure Match
          | otherwise -> pure (Diverge (vo ++ exitTag vc) (no ++ exitTag nc))
        _ -> pure (Diverge "<vm timed out>" "<native timed out>")
  where
    exitTag ExitSuccess = ""
    exitTag (ExitFailure n) = "[exit " ++ show n ++ "]"

-- | Run a subprocess with a 5s timeout; Nothing on timeout.
run :: FilePath -> [String] -> IO (Maybe (ExitCode, String, String))
run cmd args = timeout 5000000 (readProcessWithExitCode cmd args "")

-- | Fuzz @count@ programs starting from @startSeed@, reporting divergences.
runFuzz :: Int -> Int -> IO ()
runFuzz startSeed count = do
  self <- getExecutablePath
  let outDir = ".build" </> "fuzz"
  createDirectoryIfMissing True outDir
  putStrLn ("fuzzing " ++ show count ++ " programs from seed " ++ show startSeed ++ " ...")
  results <- forM [startSeed .. startSeed + count - 1] $ \seed -> do
    let (prog, _) = runGen genProgram (fromIntegral seed)
    outcome <- checkProgram self outDir prog
    reportOne seed prog outcome
    pure outcome
  let fails = length [() | r <- results, notMatch r]
  putStrLn ""
  if fails == 0
    then putStrLn ("ok: " ++ show count ++ " programs agree across VM and native")
    else do
      putStrLn ("FAILED: " ++ show fails ++ " of " ++ show count ++ " programs diverged")
      exitFailure
  where
    notMatch Match = False
    notMatch _ = True

reportOne :: Int -> String -> Outcome -> IO ()
reportOne _ _ Match = putStr "."
reportOne seed prog (BuildFail err) = do
  putStrLn ("\n[seed " ++ show seed ++ "] BUILD FAILED")
  putStr prog
  putStrLn ("--- error ---\n" ++ err)
reportOne seed prog (Diverge vo no) = do
  putStrLn ("\n[seed " ++ show seed ++ "] DIVERGENCE (reproduce: glados fuzz --seed " ++ show seed ++ " --count 1)")
  putStr prog
  putStrLn "--- vm output ---"
  putStr vo
  putStrLn "--- native output ---"
  putStr no
