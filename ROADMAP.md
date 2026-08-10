# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-07-23.

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
| Closures | Done | Lambdas capture enclosing locals by value; `VClosure` + `IMakeClosure` bytecode; value-capture semantics; `tests/closures.qa` |
| Tuples | Done | `(int, str)` type syntax; `(a, b)` literal; `.0`/`.1` indexed access; destructuring in `let` and `match`; `tests/tuples.qa` |
| Generic structs | Done | `struct Pair[A, B]` syntax; `TypeGenericApp` type node; field type inference at init site; field access with type-var resolution; `tests/generic_structs.qa` |
| Dict type | Done | `dict(K, V)`, `{}` literal, subscript get/set; `dict.*` builtins |
| FFI | Done | `extern "lib.so" { fn … }` blocks; `dlopen`/`dlsym` + libffi; types: `int`, `float`, `bool`, `str`, `void` |
| FFI variadics | Done | `...T` param syntax in `extern` blocks; uses `ffi_prep_cif_var`; args passed flat past fixed params |
| FFI pointer type | Done | `ptr` keyword type; `VPointer Word64` runtime value; `ptr.null`, `ptr.is_null`, `ptr.to_int`, `ptr.from_int` builtins |
| Formatter | Done | Comment-preserving; idempotent; wired as LSP `textDocument/formatting`; `quant-fmt` binary (`glados fmt` dispatches to it) |
| LSP server | Done | 19 protocol features, see table below |
| Project manager | Done | `glados init/build/run/clean/test/doc/fmt/lint` subcommands; `quant.toml` manifest |
| Test runner | Done | `glados test`; discovers `*_test.qa`; `assert_eq/ne/true/false/panic` builtins; TAP output; exit 1 on failure |
| Coverage | Done | `glados test --cov`; fn + line + branch bars per file and total; `--cov-min`, `--cov-out JSON` |
| Doc generator | Done | `glados doc`; scans `//` comments above `fn`/`struct`/`error`; HTML (dark sidebar, scroll-spy) or `--format md` |
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
| `net/http` | **In progress** | `get/post/put/delete`; response struct with `status`, `body`, `headers`, colleague's work |

### LSP feature coverage

| Feature | Method | Status |
|---------|--------|--------|
| Diagnostics | `publishDiagnostics` | Done |
| Hover | `textDocument/hover` | Done |
| Completion | `textDocument/completion` | Done |
| Go to definition | `textDocument/definition` | Done, functions and variables |
| Signature help | `textDocument/signatureHelp` | Done |
| Document symbols | `textDocument/documentSymbol` | Done |
| Document highlight | `textDocument/documentHighlight` | Done, functions and variables |
| Find references | `textDocument/references` | Done, functions and variables |
| Rename | `textDocument/rename` + `prepareRename` | Done, functions and variables |
| Folding ranges | `textDocument/foldingRange` | Done |
| Inlay hints | `textDocument/inlayHint` | Done |
| Semantic tokens | `textDocument/semanticTokensFull` | Done |
| Code lens | `textDocument/codeLens` | Done, "N references" (cross-file) + "Run" above `fn main` |
| Call hierarchy | `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls` | Done |
| Formatting | `textDocument/formatting` | Done, comment-preserving formatter |
| Selection range | `textDocument/selectionRange` | Done |
| Go to type definition | `textDocument/typeDefinition` | Done, jumps to struct / error declaration |
| Workspace symbols | `workspace/symbol` | Done, searches all indexed files |
| Code action: add import | `textDocument/codeAction` | Done, inserts `from M import name` for undefined stdlib functions |
| Code action: fill struct | `textDocument/codeAction` | Done, inserts missing fields with default values |
| Variable type inlay hints | `textDocument/inlayHint` | Done, shows inferred type after `var: type` declarations |

### LSP: parity with Rust Analyzer achieved

All planned LSP features are complete.

---

## V1 Tooling, status

The language core, LSP, and initial toolchain are done. Remaining V1 work is tracked below.

### Completed

| Tool | Command | Notes |
|------|---------|-------|
| Project manager | `glados init/build/run/clean` | `quant.toml` manifest; `glados init NAME` scaffolds project |
| Test runner | `glados test [FILE]` | Discovers `*_test.qa`; `assert_eq/ne/true/false/panic` builtins; TAP output |
| Coverage | `glados test --cov` | fn + line + branch bars; `--cov-min N`, `--cov-out FILE` (JSON) |
| Doc generator | `glados doc [--format html\|md] [--out DIR]` | Scans `//` comments; dark-sidebar HTML or Markdown |
| Formatter | `glados fmt` | Dispatches to `quant-fmt` binary (comment-preserving, idempotent) |
| Linter | `glados lint` | `wheatley` binary; 8 rules: `unused-var`, `unused-param`, `unreachable-code`, `missing-return` (error), `fn-naming`, `type-naming`, `empty-block`, `shadow`; `--deny`/`--allow`/`--rules` flags |
| Workspace-wide LSP indexing |, | Background scan on startup; re-index on `DidChangeWatchedFiles`; cross-file references, rename, and code lens |
| Dead code diagnostic | `publishDiagnostics` | Functions with no callers anywhere in the workspace shown greyed-out (`DiagnosticTag_Unnecessary`) |
| On-type formatting | `textDocument/onTypeFormatting` | Auto-indent after `\n` and `}`; matches opening brace indentation |
| Status bar indexing indicator | custom `$/quant/indexingStatus` | Shows "$(sync~spin) Quant: indexing..." in VS Code status bar while background scan runs |
| DAP debugger | `quant-dap` | Breakpoints, step-over/in/continue, stack frames, locals + heap expansion; stdout capture as DAP output events |

---

## V1, upcoming

### Language features

| Feature | Notes |
|---------|-------|
| ~~Tuples~~ | Done, see "What's complete" table |
| ~~Closures capturing environment~~ | Done, see "What's complete" table |
| ~~Enum types~~ | Done -- `enum Direction { North, South, East, West }` with `Direction.North` access and `match` arm patterns; see `tests/enums.qa` |
| ~~Generic structs~~ | Done, see "What's complete" table |
| ~~Inherent impl blocks~~ | Done -- `impl MyStruct { fn method(self, ...) -> R { ... } }` with `v.method(args)` call syntax; methods compile as regular functions (`TypeName.method`); see `tests/impl_methods.qa` |
| ~~Interfaces~~ | Done -- `interface Printable { fn print(self) -> void; }` + `impl Printable for MyStruct { ... }`; type checker validates all required methods are provided; see `tests/interfaces.qa` |
| ~~Operator overloading~~ | Done -- `impl Add for Vec2 { fn add(self, other: Vec2) -> Vec2 }` desugars `a + b` to `a.add(b)`; traits: `Add Sub Mul Div Rem Eq Ne Lt Gt Le Ge Neg`; see `tests/operator_overload.qa` |
| ~~Destructuring~~ | Done -- `{ x, y }: Point = p` in `let` bindings; `{ x, y } =>` in `match` arms; see `tests/struct_destructuring.qa` |
| ~~Generic bounds~~ | Done -- `fn foo[T: Iface](x: T)` syntax; type checker verifies concrete type implements the interface at every call site; `IDynMethodCall` for runtime dispatch; see `tests/generic_bounds.qa` |
| ~~Data-carrying enum variants~~ | Done -- `enum Shape { Circle { radius: float }, Rect { w: float, h: float }, Point }` with `Shape.Circle { radius: 5.0 }` construction and `Shape.Circle { radius: r } =>` pattern matching with field destructuring; see `tests/data_enum.qa` |
| ~~`impl` on enums~~ | Done -- `impl Direction { fn is_horizontal(self) -> bool { ... } }` with `d.method()` call syntax; `impl Interface for Enum` also supported; see `tests/enum_impl.qa` |
| ~~Interface inheritance~~ | Done -- `interface ReadWrite extends Read, Write { ... }`; impls must provide inherited methods; bounds on a child interface grant its inherited methods, and a child impl satisfies parent bounds; see `tests/interface_inherit.qa` |
| ~~Default interface methods~~ | Done -- interface methods may carry a body; impls that omit the method get it instantiated as `T.method` from the default; overriding and inherited defaults (via `extends`) work; sibling calls on `self` use dynamic dispatch; see `tests/default_methods.qa` |
| ~~Associated types on interfaces~~ | Done -- `type Item;` in interfaces, `type Item = int;` in impl-for blocks; bindings validated for completeness both ways; inherited through `extends`; concrete method calls carry concrete signatures; see `tests/assoc_types.qa` |
| ~~FFI callbacks~~ | Done -- extern fns may take function-typed params: `fn qsort(..., compar: (ptr, ptr) -> int)`; a Quant function passed there becomes a C function pointer (GHC wrapper import) that re-enters the VM; `ptr.add`/`ptr.read_*`/`ptr.write_*` builtins for raw memory; see `tests/ffi_callback.qa` |
| Generalized FFI callback shapes | Lift the fixed `(ptr, ptr) -> int` comparator restriction: thread the extern's declared function type into `ICallFFI` so the VM can pick (or synthesize) a matching C wrapper -- e.g. `(int) -> void` for signal handlers, `(ptr) -> void` for iterators, `(float, float) -> float` for numeric kernels |
| ~~Async / await~~ | Done -- `async fn f() -> T` returns `task(T)` at the call site; `await expr` unwraps it; cooperative green-task scheduler in the VM (`ISpawn`/`IAwait`); tasks advance only at await points; see `tests/async.qa` |
| Multi-target codegen | LLVM IR or C emission as an alternative backend to the bytecode VM; enables AOT compilation and better performance |

### Debugger (DAP) -- done

Full Debug Adapter Protocol implementation, VS Code can set breakpoints, step through Quant code, and inspect variables.

| Component | Status | Notes |
|-----------|--------|-------|
| DAP server (`quant-dap`) | done | `dap-server/` package; speaks DAP over stdio via JSON+Content-Length framing |
| VM debug hooks | done | `vmDebugHook :: Maybe (VMState -> IO ())` in `VMState`; called at every `ICovMark`; blocks on `MVar ResumeCmd` while paused |
| Debug info table | done | `DAP.DebugInfo`: `ICovMark` instructions indexed by function+offset → line and line → [(func,offset)] |
| Variable inspection | done | Locals, array/dict/struct expansion via `variablesReference` ranges; `displayValue` renders all `BC.Value` variants |
| VS Code launch config | done | `"debuggers"` contribution in `quant-lsp` extension; `DebugAdapterDescriptorFactory` launches `quant-dap` |
| Stack frames & stepping | done | `stackTrace`, `scopes`, `variables`, `next`, `stepIn`, `stepOut`, `continue`, `pause` DAP requests |
| Breakpoints | done | `setBreakpoints` resolves source lines to bytecode offsets via debug info; line breakpoints only (no conditional) |
| stdout capture | done | VM stdout redirected through a pipe; capture thread forwards lines as DAP `output` events |
| Man page | done | `man/quant-dap.1` |

### Tooling

| Tool | Command | Notes |
|------|---------|-------|
| File watcher | `glados watch [CMD]` | Re-runs `glados build` (or an arbitrary subcommand) whenever a `.qa` file changes; uses `inotify`/`kqueue`; similar to `cargo watch` |
| Benchmarking | `glados bench [FILE]` | Discovers `*_bench.qa`; `bench_fn` builtin wraps a closure and reports ns/op, iterations, and standard deviation; TAP-compatible output |
| CI template | `glados init --ci github` | Adds `.github/workflows/quant.yml` to the scaffolded project; runs `glados build`, `glados test`, and `glados lint` on push |
| Package manager | `glados add <pkg>`, `glados publish` | `[dependencies]` section in `quant.toml`; resolves packages from a central registry; downloads, caches, and links `.qa` source trees |

### Standard library expansion

| Module | Status | Planned additions |
|--------|--------|-------------------|
| `net/http` | In progress | `get/post/put/delete` client; `serve/handle/response` server-side API |
| `path` | Planned | `join/basename/dirname/ext/absolute/relative/exists/is_dir/is_file` |
| `datetime` | Planned | `now/parse/format/add/diff/unix`; ISO-8601 and RFC-3339 support |
| `crypto` | Planned | `sha256/sha512/md5` (via libcrypto FFI); `rand_bytes/rand_int` (via `/dev/urandom`) |
| `os` | Planned | `spawn/wait/kill` (child processes); `pipe/read/write` (anonymous pipes); `signal` handling |
| `sqlite` | Planned | `open/close/exec/query/bind` via FFI to `libsqlite3`; returns `array(dict(str, str))` |
| `csv` | Planned | `parse/encode/rows/headers`; RFC 4180 compliant |
| `yaml` | Planned | `parse/encode`; maps to the same value tree as `json` |
| `test` | Planned | Property-based testing: `forall(gen, fn)`; built-in generators for `int/str/array`; shrinking on failure |
