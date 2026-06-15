# CLAUDE.md - Glados Project Guidelines

## Project Overview

Glados is a custom programming language compiler/interpreter toolchain written
in Haskell. It compiles a statically-typed language called Quant (`.qa` files)
through a pipeline: lexer -> parser -> type checker -> bytecode compiler -> VM.
There is also an LSP server (`glados-lsp`) and a VS Code extension.

---

## Codebase Structure

| Package | Role |
|---------|------|
| `ast` | Shared AST types (no logic) |
| `parser` | Lexer + Megaparsec parser |
| `typechecker` | Type inference and error reporting |
| `compiler` | Bytecode codegen + import resolution |
| `vm` | Bytecode interpreter |
| `cli` | `glados` binary; CLI flags, error display |
| `repl` | Interactive REPL |
| `lsp-server` | `glados-lsp` LSP server binary |
| `std/` | Standard library written in Quant |
| `extension/vscode/quant-lsp/` | VS Code extension |

The `lisp` package in this repo is an unrelated project - ignore it completely,
never modify it or reference it.

---

## Build & Test

```bash
nix develop                          # enter dev shell (required)
cabal build                          # build everything
cabal test                           # run all test suites
cabal run cli -- compiler file.qa    # run the compiler
cabal install lsp-server --overwrite-policy=always  # install glados-lsp
```

Formatter: `ormolu` (enforced by pre-commit hook). Run manually with:
```bash
ormolu --mode inplace $(find . -name "*.hs" | grep -v lisp)
```

---

## Coding Rules

### Haskell style

- Prefer pure functions; minimize IO monad usage.
- Use strong ADTs for instruction sets and AST nodes.
- No partial functions (`head`, `tail`, `undefined`, `error`) in new code.
- Error propagation via `Either`, `Maybe`, or custom error types.
- Keep data type definitions separate from evaluation/interpretation logic.

### Bytecode & VM

- Keep the instruction set minimal and decoupled.
- Each VM instruction should have exactly one well-defined semantics.

### Standard library (`std/`)

- Functions are written in Quant (`.qa`), not Haskell.
- Each `fn` declaration must be preceded by `//` doc comment(s).
- All files must be pure ASCII (see Character Encoding below).

---

## Commits

- NEVER add yourself as co-author (no `Co-Authored-By:` line).
- Follow the existing commit style: `type(scope): short imperative title`.
- Title must be under 72 characters.

---

## Character Encoding

**All files in this repo must be pure ASCII (0x00-0x7F). No exceptions.**

The Haskell LSP server reads stdlib `.qa` files with `hGetContents`, which
uses the system locale encoding. On many Linux systems the locale is `C` or
`POSIX`, not UTF-8. Any non-ASCII byte causes an immediate runtime crash:
`hGetContents: invalid argument (cannot decode byte sequence ...)`.

Common offenders and their ASCII replacements:

| Unicode name | Code point | Use instead |
|--------------|------------|-------------|
| EM DASH | U+2014 | `-` or ` - ` |
| EN DASH | U+2013 | `-` |
| RIGHTWARDS ARROW | U+2192 | `->` or `=>` |
| GREEK SMALL LETTER PI | U+03C0 | `PI` |
| SUPERSCRIPT TWO | U+00B2 | `^2` |
| LEFT/RIGHT DOUBLE QUOTATION MARK | U+201C/U+201D | `"` |
| LEFT/RIGHT SINGLE QUOTATION MARK | U+2018/U+2019 | `'` |

This applies to: Haskell source, Quant source, Markdown docs, comments,
string literals - everything.
