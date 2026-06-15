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
| LSP server | Complete; builds and runs via `nix develop` (Nix/zlib issue resolved) |
| VS Code extension | `extension/vscode/quant-lsp/` , syntax highlighting, snippets, diagnostics, hover types |
| Docs site | Astro; all pages written |
| Tests | 546 total, 0 failures |

## Known stubs

These features are parsed and stored in the AST but throw `UnsupportedConstruct` in the compiler:

- **Struct field access / initialization** , `ExprField`, `ExprStructInit`, `LFieldAccess`
- **Error handling** , `ExprTry`, `ExprMust`, `DeclError`, `DeclErrorSet`

`Visibility` (`pub` / `static`) is parsed and stored but never enforced.

---

## Near-term features (medium scope)

### String interpolation
Syntax: `` `Hello {name}, you are {age} years old` ``

- **Lexer** (`parser/src/Lexer.hs`): new token type for interpolated string segments
- **Parser**: emit an `ExprCall "string.concat"` tree or a dedicated `ExprInterp` node
- **Codegen**: flatten into consecutive `string.concat` calls
- No VM changes needed.

Effort: ~2–3h. High UX value.

### `pub` / `static` visibility enforcement
`Visibility` is on every `DeclFunction` and `DeclStruct`. The type checker (`typechecker/src/TypeChecker.hs`) could:

- Reject calls to private (non-`pub`) imported functions
- Warn on `pub` in single-file programs (no effect without a caller)

After import resolution, all imported names are already module-prefixed, so enforcement is mostly an additional check in the type checker's function-call rule.

Effort: ~1–2h. Correctness feature.

---

## Larger features

### Structs
All three codegen cases stub out (`ExprField`, `ExprStructInit`, `LFieldAccess`). Full implementation needs:

1. `VStruct (Map FieldName Value)` added to `Compiler.Bytecode` (`compiler/src/Compiler/Bytecode.hs`)
2. Two new instructions: `IFieldGet FieldName`, `IFieldSet FieldName` in `Instruction`
3. Codegen in `compiler/src/Compiler/Codegen.hs` for the three stubs
4. VM handler in `vm/src/VM/Interpreter.hs` for the new instructions
5. Type checker: track struct field types, validate `ExprField` and `ExprStructInit`

Effort: ~4–6h.

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
