# CLAUDE.md - Glados Project Guidelines

## Project Overview

Glados is a custom programming language compiler/interpreter toolchain written
in Haskell. It compiles a statically-typed language called Quant (`.qa` files)
through the pipeline:

lexer -> parser -> type checker -> bytecode compiler -> virtual machine.

The repository also contains:

- `glados-lsp` Language Server
- VS Code extension
- REPL
- standard library

The `lisp` package in this repository is an unrelated project.

**Never modify, reference, or use the `lisp` package unless explicitly asked.**

---

# Agent Behavior

These guidelines exist to minimize unnecessary changes and common LLM mistakes.

When instructions conflict, explicit user instructions take precedence over this
document.

## Think Before Coding

Never silently guess.

Before making changes:

- State important assumptions.
- If multiple interpretations exist, ask instead of picking one.
- Point out simpler alternatives when appropriate.
- If requirements are ambiguous, stop and ask for clarification.

Do not invent requirements.

---

## Simplicity First

Prefer the smallest correct implementation.

- Implement only what was requested.
- Avoid speculative abstractions.
- Avoid future-proofing unless requested.
- Avoid unnecessary configuration.
- Avoid unnecessary helper functions.
- Keep control flow straightforward.

If a solution can reasonably be 50 lines instead of 200, prefer the smaller
solution.

---

## Surgical Changes

Modify only code directly related to the requested task.

Do NOT:

- refactor unrelated code
- rename unrelated identifiers
- reorganize files
- change formatting outside edited regions
- rewrite comments unrelated to the task

Do:

- remove imports made unused by your own changes
- remove dead code introduced by your own changes
- keep existing project style

If unrelated problems are discovered, mention them instead of fixing them.

Every changed line should be traceable to the user's request.

---

## Goal-Driven Work

For non-trivial tasks:

1. Briefly state the implementation plan.
2. Describe how each step will be verified.
3. Finish only after verification succeeds.

Examples:

Bug fix:

- reproduce bug
- fix implementation
- verify bug no longer occurs

Feature:

- implement
- add/update tests
- verify behavior

Refactor:

- preserve behavior
- verify existing tests still pass

---

## Testing Philosophy

When fixing bugs:

- Prefer adding or updating a test that reproduces the issue.
- Verify the test fails before the fix when practical.
- Verify it passes after the fix.

When adding features:

- Add tests whenever the repository's testing approach supports them.

Never claim code works without verification.

If verification cannot be performed, explicitly say so.

---

# Codebase Structure

| Package | Role |
|---------|------|
| `ast` | Shared AST definitions (no compiler logic) |
| `parser` | Lexer and Megaparsec parser |
| `typechecker` | Type inference and diagnostics |
| `compiler` | Bytecode generation and import resolution |
| `vm` | Bytecode interpreter |
| `cli` | `glados` executable |
| `repl` | Interactive REPL |
| `lsp-server` | `glados-lsp` |
| `std/` | Quant standard library |
| `extension/vscode/quant-lsp/` | VS Code extension |

---

# Build & Test

Always work inside the Nix development shell.

```bash
nix develop
```

Build everything:

```bash
cabal build
```

Run tests:

```bash
cabal test
```

Compile a Quant file:

```bash
cabal run cli -- compiler file.qa
```

Install the language server:

```bash
cabal install lsp-server --overwrite-policy=always
```

---

## Formatting

Formatting is enforced with `ormolu`.

Run:

```bash
ormolu --mode inplace $(find . -name "*.hs" | grep -v lisp)
```

Never reformat unrelated files.

---

# Coding Rules

## Haskell

Prefer:

- pure functions
- explicit types
- algebraic data types
- pattern matching

Avoid introducing:

- `head`
- `tail`
- `init`
- `last`
- `undefined`
- `error`
- partial pattern matches

Represent failures with:

- `Either`
- `Maybe`
- dedicated error types

Keep:

- data definitions
- evaluation logic
- parsing
- compilation

clearly separated.

---

## Parser

- Prefer parser combinators already used in the codebase.
- Produce useful source locations in errors.
- Keep grammar changes localized.

---

## Type Checker

- Preserve diagnostic quality.
- Prefer explicit error types over generic failures.
- Do not weaken type safety to make tests pass.

---

## Compiler

- Generate the simplest correct bytecode.
- Avoid duplicate instruction sequences when existing helpers exist.
- Keep import resolution deterministic.

---

## Virtual Machine

Each instruction should have exactly one well-defined semantic.

Avoid instructions that combine multiple responsibilities.

Keep the instruction set minimal.

---

## Standard Library (`std/`)

Quant source files:

- are written in `.qa`
- require `//` documentation immediately before every `fn`
- must remain pure ASCII

---

# Commits

Never add:

```
Co-Authored-By:
```

Follow:

```
type(scope): imperative summary
```

Examples:

```
fix(parser): handle nested tuple patterns

feat(vm): add string concatenation opcode

refactor(typechecker): simplify constraint solving
```

Keep commit titles under 72 characters.

---

# Character Encoding

**Every file in this repository must contain only ASCII characters (0x00-0x7F).**

This includes:

- Haskell
- Quant
- Markdown
- comments
- string literals
- documentation

Reason:

The Haskell LSP reads stdlib files using locale-dependent decoding
(`hGetContents`). Systems using the `C` or `POSIX` locale are not UTF-8 and will
crash when encountering non-ASCII bytes.

Common replacements:

| Unicode | Replace with |
|----------|--------------|
| — | `-` |
| – | `-` |
| → | `->` or `=>` |
| π | `PI` |
| ² | `^2` |
| “ ” | `"` |
| ‘ ’ | `'` |

Never introduce Unicode into this repository.

---

# Before Finishing

Before considering a task complete, verify:

- only necessary files were modified
- no unrelated formatting changes were introduced
- no unused imports remain
- code builds when practical
- relevant tests pass when practical
- limitations or unverified assumptions are clearly stated