# Quant Language - Roadmap

Current state of the `feat/revival` branch as of 2026-06-10.

## What's complete

| Area | Notes |
|------|-------|
| Lexer / Parser | 342 tests |
| Type checker | Wired into CLI + REPL; 11 tests |
| Compiler / Codegen | All control flow, arrays, casts, compound assignment |
| VM Interpreter | Stack-based; 46 tests; OOB now throws correctly |
| Import system | `import M`, `from M import f`, `from M import *` |
| Standard library | `math`, `string`, `array`, `sys`, `io` (~60 functions) |
| REPL | `:load`, `:run`, `:env`, `:reset`, multiline, tab completion |
| CLI | `--stdlib`, `--dump`, `--load`, `--output` flags |
| LSP server | Diagnostics, hover, completion, go-to-def, signature help, symbols, highlight, references, rename, folding, inlay hints, semantic tokens |
| VS Code extension | `extension/vscode/quant-lsp/` , syntax highlighting, snippets, all LSP features wired |
| Structs | Declare, init, field access/assignment; fully typed and compiled |
| String interpolation | Backtick strings `` `hello {name}` ``; desugars to `string.concat` + `string.to_str` |
| Visibility enforcement | `static fn` hides functions from wildcard/explicit imports |
| Docs site | Astro; all pages written |
| Tests | 545 total, 0 failures |

## Known stubs

These features are parsed and stored in the AST but throw `UnsupportedConstruct` in the compiler:

- **Error handling** , `ExprTry`, `ExprMust`, `DeclError`, `DeclErrorSet`

---

## LSP expansion (in progress)

Expanding toward Rust Analyzer / TypeScript LSP feature parity.

### Phase 1 - No type-checker changes needed
- **Document symbols** (`textDocument/documentSymbol`) - OUTLINE panel showing all `fn` declarations
- **Document highlight** (`textDocument/documentHighlight`) - all occurrences glow on cursor
- **Find references** (`textDocument/references`) - right-click Find All References
- **Rename** (`textDocument/rename`) - F2 rename for user-defined functions (single-file)
- **Folding ranges** (`textDocument/foldingRange`) - collapse function bodies / if / while / for blocks

### Phase 2 - Variable tracking (requires type-checker extension)
- Extend `Env` with variable declaration spans (`envVarDefs`)
- Add `tcsVarUseSites` to TCState to map each `ExprVar` to its declaration
- Unlocks: variable go-to-def, variable highlight, variable references, variable rename

### Phase 3 - Inlay hints
- **Parameter name hints** (`textDocument/inlayHint`) - show `paramName:` before each argument

### Phase 4 - Semantic tokens
- **Semantic highlighting** (`textDocument/semanticTokens`) - function calls, variables, parameters colored by role

---

---

## Larger features

### Error handling (`try` / `must`)
Zig-style error unions. All codegen stubs:

1. Decide representation: a `VResult (Either String Value)` tagged union, or a separate error stack
2. `DeclError` / `DeclErrorSet` , register error types
3. `ExprTry` , propagate error up the call stack (like `?` in Zig)
4. `ExprMust` , assert non-error, panic otherwise
5. Codegen + VM + type checker changes

Effort: ~1 day. Requires design decision on error representation first.

---

## Future / big-picture (not scoped)

- **Package manager** , resolve external Quant packages, fetch from a registry
- **Graphics / game library** , bindings to SDL2 or similar via FFI
- **FFI** , call C functions from Quant
- **Generics** , needed for typed arrays (`[T]`), error sets (`Result(T, E)`)
- **Closures / first-class functions** , `fn` as a value
- **Multi-file compilation** , `import` from user packages, not just stdlib
