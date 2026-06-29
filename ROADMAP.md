# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-06-29.

---

## What's complete

| Area | Status | Notes |
|------|--------|-------|
| Lexer / Parser | Done | 353 parser tests |
| Type checker | Done | 14 tests; wired into CLI + REPL |
| Compiler / Codegen | Done | All control flow, structs, error handling, compound assignment |
| VM Interpreter | Done | Stack-based; 46 VM tests; OOB and type errors throw correctly |
| Import system | Done | `import M`, `from M import f`, `from M import *`; user `.qa` files resolved relative to the source file; transitive imports; visibility enforced |
| Standard library | Done | 8 modules: `math`, `string`, `array`, `sys`, `io`, `file`, `buf`, `varargs` (~80 functions) |
| REPL | Done | `:load`, `:run`, `:env`, `:reset`, multiline, tab completion |
| CLI | Done | `--stdlib`, `--dump`, `--load`, `--output` flags; 27 integration tests |
| Structs | Done | Declare, init, field access/assignment, nested structs, field compound assignment |
| String interpolation | Done | Backtick strings `` `hello {name}` ``; desugars to `string.concat` + `string.to_str` |
| Error handling | Done | `error`, `orerror`, `try`, `must`; runtime panic on `must`; payload fields on creation and access |
| Visibility enforcement | Done | `static fn` blocked from `from M import` and `import M; M.fn()` |
| Import cycle detection | Done | Circular imports detected and reported with the full cycle path |
| Match expression | Done | `match` statement with `ok(v)`, `err(E v)`, literal, range `lo..hi`, wildcard `_` arms |
| LSP server | Done | 12 protocol features — see table below |
| VS Code extension | Done | Syntax highlighting, snippets, all LSP features wired |
| Docs site | Done | Astro/Starlight; all pages written |
| Tests | Done | 559 total, 0 failures |

### LSP feature coverage

| Feature | Method | Status |
|---------|--------|--------|
| Diagnostics | `publishDiagnostics` | Done |
| Hover | `textDocument/hover` | Done |
| Completion | `textDocument/completion` | Done |
| Go to definition | `textDocument/definition` | Done — functions and variables |
| Signature help | `textDocument/signatureHelp` | Done |
| Document symbols | `textDocument/documentSymbol` | Done |
| Document highlight | `textDocument/documentHighlight` | Done — functions and variables |
| Find references | `textDocument/references` | Done — functions and variables |
| Rename | `textDocument/rename` + `prepareRename` | Done — functions and variables |
| Folding ranges | `textDocument/foldingRange` | Done |
| Inlay hints | `textDocument/inlayHint` | Done |
| Semantic tokens | `textDocument/semanticTokensFull` | Done |

---

## True V1 — what's left

The deadline V1 shipped a working language. True V1 needs a handful of
language-level features that any programmer expects, plus a clean
standard library and reliable tooling.

Out of scope for V1 (later): package manager, graphics/SDL2 library.

---

### 1. First-class functions

Pass functions as arguments, store them in variables, return them from
functions. This unlocks callbacks, higher-order utilities, and most
functional patterns.

```quant
fn apply(f: (int) -> int, x: int) -> int {
    return f(x);
}

fn double(n: int) -> int { return n * 2; }

fn main() -> void {
    result: int = apply(double, 5);   // 10
    println(result);
}
```

**Layers that need work:**

| Layer | Change |
|-------|--------|
| Parser/Type | `(int) -> int` type syntax — positional-typed function types (currently requires named params `(x: int) -> int`) |
| AST/Type | Already has `TypeFunction`; `VFunction FuncName` value is missing from `Bytecode.hs` |
| Codegen | `ExprVar` of a function name → push `VFunction fname`; `ExprCall` on a non-literal callee → `ICallIndirect` |
| VM | New `ICallIndirect` instruction: pop `VFunction`, dispatch like `ICall` |
| Type checker | `TypeFunction` already in the type system; wire it to variable type annotations |

---

### 2. Error payload field access *(done)*

Error types already support payload fields at the declaration and creation
sites. Accessing those fields on a caught error value is the missing half.

```quant
error ParseError { line: int, msg: str };

fn parse(src: str) -> orerror(int, ParseError) {
    return error ParseError { line: 3, msg: "unexpected token" };
}

fn main() -> void {
    result: orerror(int, ParseError) = parse("???");
    // must returns the success value or panics -- field access on the error
    // side is the gap:
    // err: ParseError = ...  (need a way to bind the error branch)
    // println(err.msg);
}
```

Currently `VErrorVal (ErrorName, [(FieldName, Value)])` is stored in the VM
but there is no instruction to project a field out of it.

**Layers that need work:**

| Layer | Change |
|-------|--------|
| AST | Add `ExprErrorField` or reuse `ExprField` for `VErrorVal` |
| VM | `IFieldGet` already exists for structs; extend to handle `VErrorVal` lookup |
| Type checker | Infer field access on `orerror` / error-typed expressions |
| Language design | Decide how to bind the error branch — `match`, or a dedicated `catch err { }` block, or a destructuring `let` |

This is best implemented together with **match** (feature 3) so you have a
natural way to bind the error branch before projecting fields.

---

### 3. Match expression *(done)*

A `match` / `switch` construct for control flow on values, struct fields,
and error variants.

```quant
error IoError { msg: str };
error ParseError { line: int };

fn run(src: str) -> orerror(int, IoError) {
    result: orerror(int, ParseError) = parse(src);
    match result {
        ok(n) => return n * 2;
        err(ParseError e) => return error IoError { msg: e.msg };
    };
}

fn classify(n: int) -> str {
    match n {
        0 => return "zero";
        1..9 => return "single digit";
        _ => return "large";
    };
}
```

**Layers that need work:**

| Layer | Change |
|-------|--------|
| Lexer | `match`, `ok`, `err` keywords (or contextual) |
| AST | `StmtMatch` / `ExprMatch` with arm list; arm patterns: literal, range, wildcard, `ok(v)`, `err(E e)`, struct destructure |
| Parser | `parseMatch` |
| Type checker | Exhaustiveness check (at minimum warn on non-exhaustive); arm body type unification |
| Codegen | Compile arms to conditional jumps; bind pattern variables in arm scope |
| VM | No new instructions needed beyond scope-local stores |

---

### 4. Dict / map type

An associative container keyed by `str` or `int`.

```quant
fn main() -> void {
    counts: dict(str, int) = {};
    counts["hello"] = 1;
    counts["world"] = 2;
    if (dict.has(counts, "hello")) {
        println(counts["hello"]);   // 1
    };
    keys: [str] = dict.keys(counts);
}
```

**Layers that need work:**

| Layer | Change |
|-------|--------|
| AST/Type | `TypeDict KeyType ValueType`; `LitDict []` |
| Parser | `dict(K, V)` type syntax; `{}` literal; subscript assignment (`d[k] = v`) already handled by `LArrayIndex` — reuse or extend |
| VM | `VDictRef Int` (heap-backed like `VArrayRef`); `vmDictHeap :: Map Int (Map Value Value)` in `VMState` |
| Builtins | `dict.has`, `dict.keys`, `dict.values`, `dict.len`, `dict.delete` |
| Type checker | Dict subscript types; builtin return types |
| Codegen | `INewDict`, `IDictGet`, `IDictSet` instructions; or dispatch to builtins |

Stdlib module `dict.qa` for higher-level operations.

---

### 5. Import cycle detection *(done)*

Circular imports currently cause a stack overflow.

```
// a.qa imports b.qa, b.qa imports a.qa -> infinite recursion
```

**Fix:** Thread a `Set FilePath` of in-progress modules through
`resolveImports` / `resolveOne`. If a module is already in the set, return
`Left "import cycle detected: a -> b -> a"`.

**Layers:** `compiler/src/Compiler/Import.hs` only.

---

### 6. Optional / nullable type

A built-in `option(T)` (or `?T`) that makes null-safety explicit.

```quant
fn find(arr: [int], val: int) -> option(int) {
    i: int = 0;
    while (i < len(arr)) {
        if (arr[i] == val) { return some(i); };
        i = i + 1;
    };
    return none;
}

fn main() -> void {
    result: option(int) = find([1, 2, 3], 2);
    match result {
        some(i) => println(i);
        none    => println("not found");
    };
}
```

`option(T)` is a specialisation of the error system (`orerror(T, None)`)
but with dedicated syntax and a `none` literal that reads more naturally.

**Layers:** similar to `orerror` — AST type node, parser, type checker,
VM `VOption`, codegen. Depends on **match** (feature 3) for clean usage.

---

### 7. FFI — call C functions

Bind to C libraries directly from Quant.

```quant
extern fn printf(fmt: str, ...int) -> int;
extern fn malloc(size: int) -> int;

fn main() -> void {
    printf("hello %d\n", 42);
}
```

**Approach:** The VM runs in Haskell. The practical path is:
1. A `DeclExtern` AST node parsed from `extern fn …`
2. At codegen, emit a call to a special `ICallForeign` instruction
3. The VM resolves `ICallForeign` via `dlopen` + `dlsym` at runtime (using
   the `libffi` Haskell binding, or Haskell's `Foreign.Ptr` + `ccall`)

Alternatively, output native code (LLVM or C) and link normally — but that
requires a separate backend.

---

## Stdlib gaps for V1

The current stdlib is comprehensive. Remaining gaps:

| Module | Missing |
|--------|---------|
| `string` | `to_chars` → `[str]`, `from_chars` → `str`, `count` occurrences, `format` (named `{}` holes) |
| `array` | `map(arr, f)` and `filter(arr, f)` — blocked until first-class functions land |
| `math` | `pi` and `tau` constants, `log2`, `log10`, `hypot`, `is_nan`, `is_inf` |
| `json` | `encode(value) -> str`, `decode(s) -> ...` — needs dict and option first |
| `regex` | basic `match`, `find`, `replace` — can wrap Haskell's `regex-compat` |
| `net` | `http.get`, `http.post` — post-V1 in practice |

---

## Implementation order

Dependencies shape the order:

```
cycle detection (5)          — standalone, do first
error field access (2)  ─┐
match (3)               ─┴─ do together, match enables field binding
first-class functions (1)    — independent, can parallelize
dict (4)                     — independent
optional (6)            ─── depends on match for clean usage
stdlib additions             — fill in as language features land
FFI (7)                      — last, needs design decision on backend
```

Realistic V1 sequence:
1. Cycle detection
2. Error field access + match expression (together)
3. First-class functions
4. Dict type
5. Optional type
6. Stdlib additions (json, regex, string.format)
7. FFI

---

## Post-V1 / future

- **Generics** — `fn map[T, U](arr: [T], f: (T) -> U) -> [U]`; requires type-variable inference
- **Tuples** — `(int, str)` for lightweight multiple returns
- **Enum types** — named variants without payload, beyond the error system
- **Closures capturing environment** — lambdas that close over local variables
- **Async / await** — cooperative concurrency
- **Package manager** — resolve external Quant packages from a registry
- **Graphics / game library** — SDL2 or similar via FFI
- **Multi-target codegen** — LLVM or C output instead of the bytecode VM
- **Cycle detection in imports** — if not done in V1
