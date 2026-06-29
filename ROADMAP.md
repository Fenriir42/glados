# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-06-29.

## What's complete

| Area | Status | Notes |
|------|--------|-------|
| Lexer / Parser | Done | 353 parser tests |
| Type checker | Done | 11 tests; wired into CLI + REPL |
| Compiler / Codegen | Done | All control flow, structs, error handling, compound assignment |
| VM Interpreter | Done | Stack-based; 46 VM tests; OOB and type errors throw correctly |
| Import system | Done | `import M`, `from M import f`, `from M import *`; user `.qa` files resolved relative to the source file; transitive imports supported; visibility enforced |
| Standard library | Done | 8 modules: `math`, `string`, `array`, `sys`, `io`, `file`, `buf`, `varargs` (~80 functions) |
| REPL | Done | `:load`, `:run`, `:env`, `:reset`, multiline, tab completion |
| CLI | Done | `--stdlib`, `--dump`, `--load`, `--output` flags; 27 integration tests |
| Structs | Done | Declare, init, field access/assignment, nested structs, field compound assignment |
| String interpolation | Done | Backtick strings `` `hello {name}` ``; desugars to `string.concat` + `string.to_str` |
| Error handling | Done | `error`, `orerror`, `try`, `must`; runtime panic on `must` over an error value |
| Visibility enforcement | Done | `static fn` blocked from `from M import` and from `import M; M.fn()` |
| LSP server | Done | See LSP feature table below |
| VS Code extension | Done | `extension/vscode/quant-lsp/`; syntax highlighting, snippets, all LSP features wired |
| Docs site | Done | Astro/Starlight; all pages written |
| Tests | Done | 545 total, 0 failures |

### LSP feature coverage

| Feature | Protocol method | Status |
|---------|----------------|--------|
| Diagnostics | `textDocument/publishDiagnostics` | Done |
| Hover | `textDocument/hover` | Done |
| Completion | `textDocument/completion` | Done |
| Go to definition | `textDocument/definition` | Done |
| Signature help | `textDocument/signatureHelp` | Done |
| Document symbols | `textDocument/documentSymbol` | Done |
| Document highlight | `textDocument/documentHighlight` | Done |
| Find references | `textDocument/references` | Done |
| Rename | `textDocument/rename` + `prepareRename` | Done |
| Folding ranges | `textDocument/foldingRange` | Done |
| Inlay hints | `textDocument/inlayHint` | Done |
| Semantic tokens | `textDocument/semanticTokensFull` | Done |

---

## Known gaps

### Variable go-to-definition / highlight / references / rename

The type checker tracks variable *use* sites (`fsVarUseSites` in `FileState`) but
go-to-definition for variables resolves to the declaration span only — highlight,
references, and rename do not yet span across all use sites for variables (only
function names work today). Implementing this requires wiring `fsVarUseSites` into
the relevant LSP modules (`Definition.hs`, `Highlight.hs`, `References.hs`,
`Rename.hs`).

### LSP: variable go-to-definition / highlight / references / rename

`fsVarUseSites` is tracked by the type checker but the LSP modules
(`Definition.hs`, `Highlight.hs`, `References.hs`, `Rename.hs`) only wire it
up partially. Variable highlight and cross-reference navigation do not yet span
all use sites the way function navigation does.

---

## Future / big-picture (not scoped)

- **Package manager** — resolve external Quant packages, fetch from a registry
- **Graphics / game library** — bindings to SDL2 or similar via FFI
- **FFI** — call C functions from Quant
- **Generics** — needed for fully-typed arrays (`[T]`), generic functions
- **Closures / first-class functions** — `fn` as a value, lambdas
- **Cycle detection in imports** — circular imports between user `.qa` files currently stack-overflow rather than report a clean error
- **Error payload fields** — `error Foo { msg: str }` with field access on the error value
