# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-08-11.

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
| ~~Generalized FFI callback shapes~~ | Done -- callbacks of any arity 0-4 (pointer args, int-class return) on both engines. The callback's parameter count is recovered from its prologue and used to select the matching trampoline natively and the matching `foreign import "wrapper"` in the VM; `tests/ffi_qsort_r.qa` exercises a 3-argument comparator. Callbacks with float or narrower-than-pointer args remain unsupported (the bytecode does not preserve extern argument types) |
| ~~Async / await~~ | Done -- `async fn f() -> T` returns `task(T)` at the call site; `await expr` unwraps it; cooperative green-task scheduler in the VM (`ISpawn`/`IAwait`); tasks advance only at await points; see `tests/async.qa` |
| ~~Native backend~~ | Done -- transpiles compiled bytecode to C for an optimized standalone binary (60-100x the VM); all ten stages below complete; `glados native-diff` gates it in CI |

### Native backend: transpile to C

Goal: `glados build --target=c` translates the compiled bytecode (not the AST)
to C, links it against a small runtime library, and invokes the system C
compiler to produce an optimized standalone binary. The bytecode VM remains the
reference implementation; every stage is validated by diffing native output
against VM output over the full `tests/*.qa` corpus.

Working bytecode-level keeps the entire existing pipeline (parser, type
checker, codegen, imports, generics erasure) untouched -- the new backend only
replaces the interpreter.

| Stage | Deliverable | Notes |
|-------|-------------|-------|
| ~~1. C runtime foundations~~ | `runtime/quant_runtime.{h,c}` | Done -- `QtValue` tagged union; arrays/dicts/structs/errors as heap objects; never-free arena allocation (`-DQUANT_GC` hooks in Boehm); `print`/`println`/format holes byte-identical to the VM (verified by diff); `make runtime-test` runs 93 C unit checks |
| ~~2. Core translator + driver~~ | `Compiler.CBackend`, `glados build --target=c` | Done -- one bytecode function per C function (stack array, `goto` labels, direct calls, `qt_call_builtin` fallback); `glados compiler FILE --native BIN` / `--emit-c` for single files; unsupported instructions fail at compile time with function+offset; `tests/native_smoke.qa` output is byte-identical to the VM; fib(30) ~50x faster than the VM |
| ~~3. Differential test harness~~ | `glados native-diff` | Done -- runs every `tests/*.qa` under both VM and native binary and diffs stdout + exit codes; the translator statically rejects unsupported instructions *and* builtins, so untranslatable files are clean skips, never silent runtime diffs; wired into CI (`build.yml`) as a gate from this point on -- semantics regressions become impossible to miss |
| ~~4. Builtin coverage~~ | runtime ports of `string.*`, `math.*`, `array.*`, `io.*`, `sys.*`, `file.*`, `buf.*` | Done for every builtin that does not need later-stage heap instructions: all `string.*` (except `hash`, whose VM definition folds over unbounded Integers), all `math.*`, all `sys.*` (fd I/O, open flags, env, process, time; `main` now captures argv), all `file.*`, and all `buf.*`. `tests/native_builtins.qa` and `tests/fd_buf.qa` run byte-identically to the VM. `dict.*` and `json.*` are deferred to stage 5 (they need the dict/struct heap instructions, so any program using them is rejected earlier anyway); `regex.*` needs a regex engine and `socket.*`/`ptr.*` belong to later stages |
| ~~5. Structs, enums, dispatch~~ | struct heap + type tags in C | Done -- `INewStruct`/`IFieldGet`/`IFieldSet` over `QtStruct`; `IDynMethodCall` resolves `<type>.<method>` at runtime from the receiver's type name through a generated `qf_dispatch` if-chain; `INewDict` plus dict-polymorphic `IArrayGet`/`IArraySet` (`qt_index_get`/`qt_index_set`) and the order-independent `dict.has`/`len`/`delete` builtins. Every struct/enum/interface/tuple test in the corpus now diffs byte-identically; `tests/native_structs.qa` is a combined showcase. (`dict.keys`/`values` still need VM `Ord` key ordering; `json.*` needs a C encoder/parser -- both follow-ups) |
| ~~6. Errors and options~~ | `orerror`/`option` semantics | Done alongside stage 5 -- enum/error values are `QtError` (name + fields, exactly the VM's `VErrorVal`); `INewError` builds them, `IFieldGet` reads their fields, `IIsOk`/`IIsErr` peek the tag, `ITryOp` early-returns the error value from the C function, and `IMustOp` (`qt_must`) panics with the `must: unwrapped error` message. `tests/orerror.qa` and `tests/enums.qa` diff byte-identically |
| ~~7. Closures and first-class functions~~ | `IMakeClosure`/`ICallIndirect` | Done -- `ILoadFunc` pushes a `QT_FN` name reference; `IMakeClosure` copies the captured locals into a `QtClosure` (name + captured `QtValue` block); `ICallIndirect` binds the closure's environment and dispatches through `qf_dispatch` by name. Each lambda body reads its captures back from `qt_env` at entry (in `IMakeClosure` order). `tests/closures.qa` (adders, multipliers, composition, multi-capture, value-capture semantics) diffs byte-identically |
| ~~8. FFI passthrough~~ | externs via `dlopen` + trampolines | Done -- `ICallFFI` resolves the symbol with `dlopen`/`dlsym` and calls it through hand-written trampolines (no libffi), marshalling each `QtValue` arg by tag into an integer/pointer or `double` register slot (full float/int mask coverage for 0-3 args, integer-only for 4-6). Function-valued args become a C trampoline that re-enters generated code via a registered dispatcher. Callback shapes are generalized to any arity 0-4 (pointer args, int-class return): the callback's parameter count is recovered from its prologue and used to select the matching trampoline on the native side and the matching `foreign import "wrapper"` on the VM side. All `ptr.*` raw-memory builtins are ported. `tests/ffi_math.qa` (libm), `tests/ffi_callback.qa` (libc `qsort`, 2-arg comparator), and `tests/ffi_qsort_r.qa` (libc `qsort_r`, 3-arg comparator with a direction argument) diff byte-identically. (The bytecode does not preserve extern argument types, so link-time prototypes are not possible; `dlopen` matches the VM's own approach) |
| ~~9. Async/await~~ | cooperative scheduler in C | Done -- each task runs on its own `ucontext` stack; `ISpawn` queues a task without running it, `IAwait` yields to the scheduler when the awaited task is unfinished, and completed tasks re-queue their waiters (FIFO) -- the same advance-at-await, run-ready-in-order semantics as the VM scheduler. `main` runs as task 0 when a program uses async; non-async programs still call `main` directly on the process stack. `tests/async.qa` (spawn-without-run, nested spawn/await, cached re-await, inline await) diffs byte-identically |
| ~~10. Optimization pass~~ | the "optimized" in optimized binary | Done -- the driver compiles with `-O2 -flto` (no `-ffast-math`/`-march=native`, so float results stay bit-identical); `-flto` inlines the tiny runtime value accessors into the generated hot paths. Explicit source-level unboxing was unnecessary: at `-O2` the C compiler already scalar-replaces the 16-byte `QtValue` (SROA), so a bespoke pass would duplicate the backend. A `benchmarks/` suite (`make bench`) times VM vs native after checking their output matches; measured **63x** (fib), **94x** (nested loops), **109x** (prime counting) |

Stages 1-3 form the minimum credible milestone (a native hello world validated
against the VM); each later stage widens the subset of `tests/*.qa` that passes
under `--native` until the corpus is green end-to-end.

**All ten stages are complete.** `glados native-diff` reports 32 of the
showcase programs matching the VM byte-for-byte, with a single skip:
`socket_http.qa`, which is annotated `native-diff: skip` because it talks to
an external host (its response varies between runs and cannot be diffed).
The `socket.*` TCP builtins and the full `dict.*` family (including
`keys`/`values`, iterated in the VM's `Ord` key order via `qt_value_cmp`)
are ported. The only builtins left unported are ones that cannot be made
byte-identical to their Haskell reference implementation: `json.*` (would
have to reproduce aeson's exact serialization and number formatting),
`regex.*` (would have to match regex-tdfa's semantics), and `string.hash`
(folds over unbounded `Integer`s). These are principled exclusions, not
gaps -- a native port would diverge from the VM, which the whole backend is
built not to do.

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
| ~~File watcher~~ | `glados watch [CMD]` | Done -- re-runs `glados build` (or any subcommand, e.g. `glados watch test --cov`, with flags forwarded) whenever a `.qa` file under `src/` or the test dir changes. Polls modification times (~400 ms; portable, dependency-free) and runs once at startup; Ctrl-C stops. `man/glados.1` documents it |
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
