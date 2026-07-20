# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-07-20.

---

## What's complete

| Area | Status | Notes |
|------|--------|-------|
| Lexer / Parser | Done | 353 parser tests |
| Type checker | Done | 14 tests; wired into CLI + REPL |
| Compiler / Codegen | Done | All control flow, structs, error handling, compound assignment |
| VM Interpreter | Done | Stack-based; 46 VM tests; OOB and type errors throw correctly |
| Import system | Done | `import M`, `from M import f`, `from M import *`; user `.qa` files resolved relative to the source file; transitive imports; visibility enforced; cycle detection |
| Standard library | Done | 13 modules: `math`, `string`, `array`, `sys`, `io`, `file`, `buf`, `varargs`, `dict`, `json`, `socket`, `regex` (~120 functions) |
| REPL | Done | `:load`, `:run`, `:env`, `:reset`, multiline, tab completion |
| CLI | Done | `--stdlib`, `--dump`, `--load`, `--output` flags; 27 integration tests |
| Structs | Done | Declare, init, field access/assignment, nested structs, field compound assignment |
| String interpolation | Done | Backtick strings `` `hello {name}` ``; desugars to `string.concat` + `string.to_str` |
| Error handling | Done | `error`, `orerror`, `try`, `must`; runtime panic on `must`; payload fields on creation and access |
| Visibility enforcement | Done | `static fn` blocked from `from M import` and `import M; M.fn()` |
| Import cycle detection | Done | Circular imports detected and reported with the full cycle path |
| Match expression | Done | `match` statement with `ok(v)`, `err(E v)`, literal, range `lo..hi`, wildcard `_` arms |
| Optional type | Done | `option(T)`, `some(v)`, `none` |
| Generics | Done | Type-erased parametric polymorphism; `fn foo[T, U](...)`; call-site inference |
| First-class functions | Done | `(T) -> R` type syntax; `ILoadFunc` + `ICallIndirect` instructions; lambdas |
| Dict type | Done | `dict(K, V)`, `{}` literal, subscript get/set; `dict.*` builtins |
| FFI | Done | `extern "lib.so" { fn … }` blocks; `dlopen`/`dlsym` + libffi; types: `int`, `float`, `bool`, `str`, `void` |
| FFI variadics | Done | `...T` param syntax in `extern` blocks; uses `ffi_prep_cif_var`; args passed flat past fixed params |
| FFI pointer type | Done | `ptr` keyword type; `VPointer Word64` runtime value; `ptr.null`, `ptr.is_null`, `ptr.to_int`, `ptr.from_int` builtins |
| Formatter | Done | Comment-preserving; idempotent; wired as LSP `textDocument/formatting` |
| LSP server | Done | 14 protocol features — see table below |
| VS Code extension (highlighting) | Done | Syntax highlighting, snippets, language config; `.vsix` in `extension/vscode/quant-highlighter/` |
| VS Code extension (LSP client) | Done | TypeScript client with `vscode-languageclient`; auto-starts `quant-lsp`; `.vsix` in `extension/vscode/quant-lsp/` |
| Docs site | Done | Astro/Starlight; all pages written |
| Packaging: Debian | Done | `make deb` produces installable `.deb`; stdlib in `/usr/local/share/quant/lib/` |
| Packaging: Arch | Done | `packaging/arch/PKGBUILD` for AUR |
| Packaging: Nix | Done | `nix build .` (cli), `.#lsp`, `.#repl`; stdlib bundled via `QUANT_STDLIB` wrapper |

### Stdlib detail

| Module | Status | Notable functions |
|--------|--------|-------------------|
| `math` | Done | `sin/cos/tan/sqrt/abs/floor/ceil/round/min/max/log/exp/pi/tau/log2/log10` |
| `string` | Done | `len/concat/substring/split/join/trim/replace/format/%s%d%f` |
| `array` | Done | `len/push/pop/sort/reverse/slice/map/filter/reduce` (map/filter/reduce via `from array import *`) |
| `sys` | Done | `args/argc/env/exit/sleep/time/platform/hostname/getcwd/system` |
| `io` | Done | `print/println/read` |
| `file` | Done | `read/write/append/lines/exists/delete/rename/size` |
| `buf` | Done | `new/write/writeln/to_str/len/clear/flush` |
| `dict` | Done | `has/keys/values/len/delete` |
| `json` | Done | `parse/encode/decode_str/decode_int/decode_float/decode_bool/has/is_null/keys` |
| `socket` | Done | `connect/listen/accept/send/recv/close/peer_addr` |
| `varargs` | Done | variadic helper utilities |
| `regex` | Done | `match/find/find_all/replace/split` (POSIX ERE via `regex-tdfa`) |
| `net/http` | **In progress** | `get/post/put/delete`; response struct with `status`, `body`, `headers` — colleague's work |

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
| Code lens | `textDocument/codeLens` | Done — "N references" above each `fn` |
| Call hierarchy | `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls` | Done |
| Formatting | `textDocument/formatting` | Done — comment-preserving formatter |
| Selection range | `textDocument/selectionRange` | Done |
| Workspace symbols | `workspace/symbol` | Done |

### LSP: Rust Analyzer parity targets (remaining)

| Feature | LSP method | Priority | Notes |
|---------|-----------|----------|-------|
| Go to type definition | `textDocument/typeDefinition` | High | Jump to struct / error declaration. Needs `structDefSites :: Map TypeName SourceSpan` in `Analyze.hs`. |
| Workspace-wide analysis | cross-file refs/highlight/rename | High | Index ALL `.qa` files in the workspace, not just the open one. Requires a background watcher + per-file `FileState` for every file on disk. |
| Run code lens | `textDocument/codeLens` (extend) | High | `▶ Run` above `fn main()` triggers `glados run <file>` in the integrated terminal. |
| Variable type inlay hints | `textDocument/inlayHint` (extend) | Medium | Show inferred type after variable declarations: `x: int`. Currently only parameter name hints are emitted. |
| Code action: fill struct | `textDocument/codeAction` | Medium | When struct init is missing fields, offer "Fill missing fields" quick fix. |
| Code action: add import | `textDocument/codeAction` | Medium | When an unknown name matches a stdlib function, offer "Import `math`". |
| On-type formatting | `textDocument/onTypeFormatting` | Low | Auto-indent after `{` / `}` / `;`. |
| Diagnostic: dead code | `publishDiagnostics` (extend) | Low | Warn on functions never called from `main` or exported. |
| Status bar indexing indicator | custom notification | Low | Show "Quant: indexing…" while the LSP is analyzing; RA-style. |

---

## V2 Tooling

The language itself is feature-complete for V1. The next layer is the
surrounding toolchain — the things that make the language *pleasant to use at
scale* rather than just *correct*.

---

### 1. Project manager

A `quant` top-level command with `init`, `build`, `run`, `clean` subcommands
backed by a `quant.toml` manifest. No package registry yet — just project
structure and a better UX than knowing the raw CLI flags.

```toml
# quant.toml
name    = "my-project"
version = "0.1.0"
entry   = "src/main.qa"
stdlib  = "/usr/local/share/quant/lib"  # optional override
```

```bash
quant init my-project   # scaffold quant.toml + src/main.qa
quant run               # compile + execute
quant build             # compile to binary without running
quant build --release   # optimised build
quant clean             # remove build artefacts
```

**Implementation:**

| Layer | Change |
|-------|--------|
| CLI (`cli/`) | Add `init`, `run`, `build`, `clean` subcommands to the existing `optparse-applicative` parser |
| Manifest | Parse `quant.toml` with `tomland` or `toml-parser`; resolve `entry` and `stdlib` paths from it |
| Scaffold | `quant init` writes `quant.toml` + `src/main.qa` (hello-world template) |
| Build cache | Optional: track source mtime vs. bytecode mtime, skip recompile if unchanged |

No new language features required — purely CLI and packaging work.

---

### 2. Test runner

Built into the project manager as `quant test`. Discovers test files, runs
every `fn test_*()`, and reports pass/fail with a count.

```bash
quant test              # run all tests/
quant test tests/math_test.qa   # run one file
```

```quant
// tests/math_test.qa
import math

fn test_pi() -> void {
    assert_eq(math.pi(), 3.141592653589793);
}

fn test_log2() -> void {
    assert_eq(math.log2(8.0), 3.0);
}
```

**Implementation:**

| Layer | Change |
|-------|--------|
| Discovery | Scan `tests/` (or configurable `test_dir` in `quant.toml`) for `*_test.qa` files |
| Convention | Any `fn test_*(...)` with no parameters is a test case |
| Assert builtins | Add `assert_eq`, `assert_ne`, `assert_true`, `assert_false`, `assert_panic` as VM builtins that throw a `TestFailure` error on mismatch |
| Runner | Compile each test file; call each `test_*` function; catch `TestFailure`; print TAP-compatible output |
| Exit code | Exit 1 if any test fails — plays nicely with CI |

---

### 3. Linter

A `quant lint` command that reports style and correctness warnings beyond the
type checker. Runs over the parsed AST without executing anything.

```bash
quant lint              # lint all .qa files in the project
quant lint src/main.qa  # lint one file
```

**Checks (initial set):**

| Rule | Example violation | Severity |
|------|------------------|----------|
| Unused variables | `x: int = 5;` never read | Warning |
| Unreachable code | statements after `return` | Warning |
| Unused function parameters | `fn foo(x: int, y: int)` where `y` is never used | Warning |
| Naming: functions snake_case | `fn MyFunc()` | Warning |
| Naming: types PascalCase | `struct myStruct` | Warning |
| Naming: error types PascalCase | `error ioError` | Warning |
| Empty blocks | `if (cond) {};` | Info |
| Shadowed variable | inner `x` hides outer `x` | Warning |
| Missing return on all paths | non-void function can fall off end | Error |

**Implementation:**

| Layer | Change |
|-------|--------|
| New package `linter/` | AST visitor that accumulates `LintDiagnostic` values |
| CLI | `quant lint` subcommand; `--fix` flag for auto-fixable rules |
| LSP integration | Feed lint diagnostics into `publishDiagnostics` alongside type errors |

---

### 4. Doc generator

A `quant doc` command that extracts leading `//` comments from public
functions and emits an HTML or Markdown API reference.

```bash
quant doc               # generate docs/api/ from src/
quant doc --format md   # emit Markdown instead of HTML
```

```quant
// Returns the nth Fibonacci number.
//
// Uses iterative computation — O(n) time, O(1) space.
fn fib(n: int) -> int { ... }
```

**Implementation:**

| Layer | Change |
|-------|--------|
| Comment attachment | The formatter already preserves comments in the AST; attach them to the nearest `DeclFunction` |
| Renderer | Walk public declarations; emit a Markdown/HTML template per module |
| CLI | `quant doc` subcommand; `--out DIR`, `--format html\|md` |

---

## Post-V2 / future

- **Package manager** — resolve external Quant packages from a registry (`quant.toml` `[dependencies]` section)
- **FFI callbacks** — create a C function pointer from a Quant lambda using libffi's closure API (`ffi_closure_alloc` + `ffi_prep_closure_loc`)
- **Tuples** — `(int, str)` for lightweight multiple returns
- **Enum types** — named variants without payload, beyond the error system
- **Closures capturing environment** — lambdas that close over local variables from the enclosing scope
- **Async / await** — cooperative concurrency
- **Generic structs** — `struct Pair[A, B] { first: A, second: B }`
- **Multi-target codegen** — LLVM or C emission instead of the bytecode VM
- **Graphics / game library** — SDL2 or similar via FFI
