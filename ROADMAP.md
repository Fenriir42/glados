# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-07-22.

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
| Formatter | Done | Comment-preserving; idempotent; wired as LSP `textDocument/formatting`; `quant-fmt` binary (`glados fmt` dispatches to it) |
| LSP server | Done | 19 protocol features — see table below |
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
| Go to type definition | `textDocument/typeDefinition` | Done — jumps to struct / error declaration |
| Workspace symbols | `workspace/symbol` | Done — searches all currently open files |
| Code action: add import | `textDocument/codeAction` | Done — inserts `from M import name` for undefined stdlib functions |
| Code action: fill struct | `textDocument/codeAction` | Done — inserts missing fields with default values |
| Variable type inlay hints | `textDocument/inlayHint` | Done — shows inferred type after `var: type` declarations |

### LSP: remaining gap to Rust Analyzer parity

| Feature | LSP method | Priority | Notes |
|---------|-----------|----------|-------|
| Workspace-wide indexing | `DidChangeWatchedFiles` + background scan | High | Currently only open files are analysed; `DidChangeWatchedFiles` is a no-op. Need to scan all `.qa` files in the workspace root on startup and re-index on save, so cross-file references, highlight, and rename work without opening every file first. |
| Run code lens | `textDocument/codeLens` (extend) | Medium | `▶ Run` above `fn main()` triggers `glados run <file>` in the integrated terminal via `workbench.action.terminal.sendSequence`. |
| On-type formatting | `textDocument/onTypeFormatting` | Low | Auto-indent after `{` / `}` / `;`. |
| Diagnostic: dead code | `publishDiagnostics` (extend) | Low | Warn on functions never called from `main` or re-exported. |
| Status bar indexing indicator | custom notification | Low | Show "Quant: indexing…" while the LSP analyses; RA-style. |

---

## V1 Tooling — status

The language, LSP, and surrounding toolchain are feature-complete for V1.

### Completed

| Tool | Command | Notes |
|------|---------|-------|
| Project manager | `glados init/build/run/clean` | `quant.toml` manifest; `glados init NAME` scaffolds project |
| Test runner | `glados test [FILE]` | Discovers `*_test.qa`; `assert_eq/ne/true/false/panic` builtins; TAP output |
| Coverage | `glados test --cov` | fn + line + branch bars; `--cov-min N`, `--cov-out FILE` (JSON) |
| Doc generator | `glados doc [--format html\|md] [--out DIR]` | Scans `//` comments; dark-sidebar HTML or Markdown |
| Formatter | `glados fmt` | Dispatches to `quant-fmt` binary (comment-preserving, idempotent) |

### Remaining

| Tool | Command | Status | Notes |
|------|---------|--------|-------|
| Linter | `glados lint` | **Not implemented** | `glados lint` delegates to `wheatley` binary; the binary itself does not exist yet. Man page written. Planned: AST visitor for unused vars, unreachable code, naming conventions, shadow warnings, missing-return errors. LSP integration via `publishDiagnostics`. |
| `quant-fmt` install | — | **Done** | Wired into `make install`; builds and installs to `$(BIN_DEST)/quant-fmt`. |

---

## Post-V1 / future

- **Package manager** — resolve external Quant packages from a registry (`quant.toml` `[dependencies]` section)
- **FFI callbacks** — create a C function pointer from a Quant lambda using libffi's closure API (`ffi_closure_alloc` + `ffi_prep_closure_loc`)
- **Tuples** — `(int, str)` for lightweight multiple returns
- **Enum types** — named variants without payload, beyond the error system
- **Closures capturing environment** — lambdas that close over local variables from the enclosing scope
- **Async / await** — cooperative concurrency
- **Generic structs** — `struct Pair[A, B] { first: A, second: B }`
- **Multi-target codegen** — LLVM or C emission instead of the bytecode VM
- **Graphics / game library** — SDL2 or similar via FFI
