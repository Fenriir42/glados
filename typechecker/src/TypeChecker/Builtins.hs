module TypeChecker.Builtins
  ( builtinReturnType,
    isKnownBuiltin,
  )
where

import AST.Types.Common (FuncName (..))
import AST.Types.Type
  ( PrimitiveType (..),
    Type (..),
    defaultFloatType,
    defaultIntType,
  )
import qualified Data.Text as T

-- | Return type of a known builtin, if statically determinable.
-- Returns Nothing for builtins whose return type depends on their argument
-- (e.g. array.pop returns the element type, which requires generics).
builtinReturnType :: FuncName -> Maybe Type
builtinReturnType (FuncName n)
  -- void
  | n `elem` voidFuncs = voidT
  -- int
  | n `elem` intFuncs = intT
  -- float
  | n == "string.to_float" = floatT
  -- bool
  | n `elem` boolFuncs = boolT
  -- string
  | n `elem` stringFuncs = stringT
  -- math: a handful return int, the rest return float
  | n `elem` mathIntFuncs = intT
  | "math." `T.isPrefixOf` n = floatT
  -- sys: specific return types before the int catch-all
  | n `elem` ["sys.env", "sys.platform", "sys.hostname", "sys.getcwd"] = stringT
  | n == "sys.args" = Nothing -- returns [str]; unknown without generics
  | n == "sys.read" = stringT
  | n `elem` ["sys.close", "sys.isatty"] = boolT
  -- sys.flush already in voidFuncs; remaining sys.* return int
  | "sys." `T.isPrefixOf` n = intT
  -- io.read
  | n == "io.read" = stringT
  -- remaining io.* (io.print, io.println) already in voidFuncs above
  | "io." `T.isPrefixOf` n = voidT
  -- file: read returns str, lines returns [str] (unknown here), bool/int variants explicit
  | n == "file.read" = stringT
  | n `elem` ["file.write", "file.append", "file.exists", "file.delete", "file.rename"] = boolT
  | n == "file.size" = intT
  | n == "file.lines" = Nothing -- returns [str]; unknown without generics
  -- buf.*
  | n == "buf.new" = Nothing -- returns [str]; unknown without generics
  | n `elem` ["buf.write", "buf.writeln", "buf.clear"] = voidT
  | n == "buf.to_str" = stringT
  | n `elem` ["buf.len", "buf.flush"] = intT
  -- dict.*
  | n == "dict.has" = boolT
  | n == "dict.len" = intT
  | n == "dict.delete" = voidT
  | n `elem` ["dict.keys", "dict.values"] = Nothing -- return type depends on dict type
  | otherwise = Nothing
  where
    voidFuncs =
      [ "print",
        "println",
        "io.print",
        "io.println",
        "push",
        "array.push",
        "sys.exit",
        "sys.sleep",
        "sys.flush"
      ]
    intFuncs =
      [ "len",
        "array.len",
        "string.len",
        "string.index_of",
        "string.last_index_of",
        "string.to_int",
        "sys.time",
        "sys.time_millis",
        "sys.argc",
        "sys.system"
      ]
    boolFuncs =
      [ "string.contains",
        "string.starts_with",
        "string.ends_with",
        "string.is_empty",
        "sys.set_env",
        "sys.chdir"
      ]
    stringFuncs =
      [ "string.concat",
        "string.substring",
        "string.char_at",
        "string.to_upper",
        "string.to_lower",
        "string.trim",
        "string.trim_left",
        "string.trim_right",
        "string.reverse",
        "string.replace",
        "string.replace_first",
        "string.repeat",
        "string.from_int",
        "string.from_float"
      ]
    mathIntFuncs =
      [ "math.abs",
        "math.floor",
        "math.ceil",
        "math.round",
        "math.min",
        "math.max"
      ]
    voidT = Just (TypePrimitive PrimNone)
    intT = Just (TypePrimitive (PrimInt defaultIntType))
    floatT = Just (TypePrimitive (PrimFloat defaultFloatType))
    stringT = Just (TypePrimitive PrimString)
    boolT = Just (TypePrimitive PrimBool)

-- | True for any function name that the VM/runtime knows about, including
-- the std-module prefix convention (math.*, string.*, io.*, sys.*, array.*).
isKnownBuiltin :: FuncName -> Bool
isKnownBuiltin (FuncName n) =
  n `elem` standaloneBuiltins
    || any (`T.isPrefixOf` n) modulePrefixes
  where
    standaloneBuiltins = ["print", "println", "len", "push", "pop"]
    modulePrefixes = ["math.", "string.", "io.", "sys.", "array.", "file.", "buf.", "dict."]
