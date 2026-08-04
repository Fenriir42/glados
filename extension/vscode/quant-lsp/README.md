# Quant Language Server

Full IDE support for the [Quant](https://github.com/Sigmapitech/glados) programming
language inside Visual Studio Code.

---

## Features

### Diagnostics
Real-time type errors and warnings are published as you type.  Unused
functions are flagged with a dedicated `[dead-code]` diagnostic and greyed
out in the editor via `DiagnosticTag.Unnecessary`.

### Syntax highlighting
TextMate grammar covers keywords, literals, operators, types, and comments.
Semantic token coloring adds a second pass that reflects the type-checked
parse tree (functions, variables, struct fields, type names, …).

### Hover types
Hovering over any identifier shows its inferred type in a markdown popup.

### Go to definition / Go to type definition
`F12` jumps to where the symbol is declared.  `Ctrl+F12` (go to type
definition) jumps to the struct declaration of the type under the cursor.
Both work across imported files.

### Find references
`Shift+F12` lists every use of the symbol across the whole workspace.

### Rename symbol
`F2` renames a symbol and rewrites every reference in every file.

### Document outline
The **Outline** panel and breadcrumb bar show all top-level declarations
(functions, structs, enums, errors, interfaces, impl blocks) in the current
file.

### Workspace symbol search
`Ctrl+T` searches for named symbols across the entire project.

### Completion
`.` triggers dot-completion showing all fields of a struct type or all
functions exported from a module.

### Inlay hints
Parameter-name hints appear inline at each call site so you can read
argument intent without checking the signature.

### Code lens
A `N references` lens appears above every function definition.  `fn main`
also gets a **Run** lens that compiles and runs the file in the terminal.

### Code actions
Quick-fix actions are offered for dead-code warnings (e.g. delete an unused
function).

### Document formatting
`Shift+Alt+F` formats the whole file using `quant-fmt`.  On-type formatting
automatically re-indents after `{` and `}`.

### Test Explorer
Files matching `**/*_test.qa` are discovered automatically.  Any function
whose name starts with `test_` appears as a test case in the **Testing**
sidebar.  Tests can be run individually or in bulk; pass/fail status and
output are reported inline.

### Code coverage
Running tests with the coverage action highlights which lines were exercised
and which were not, using the native VS Code Coverage API.

### Debug Adapter (DAP)
Step through Quant programs with breakpoints, variable inspection, and call
stack via the VS Code debugger.  Add a launch configuration of type `quant`
to `.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "quant",
      "request": "launch",
      "name": "Debug main.qa",
      "program": "${file}"
    }
  ]
}
```

---

## Requirements

The extension delegates all heavy lifting to two native binaries that must
be on your `PATH` (or configured explicitly, see **Settings** below):

| Binary | Role |
|--------|------|
| `quant-lsp` | Language server (diagnostics, hover, navigation, …) |
| `glados` | Compiler, used by the Run lens and test runner |
| `quant-dap` | Debug adapter, required for step debugging |

Build and install all three with:

```sh
make && make install
```

This copies the binaries to `~/.local/bin/` (or the prefix chosen in the
Makefile) and installs man pages.

---

## Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `quant-lsp.enable` | `true` | Enable or disable the extension. |
| `quant-lsp.serverPath` | *(auto)* | Absolute path to the `quant-lsp` binary. Leave empty to locate it on `PATH`. |
| `quant-lsp.gladosPath` | *(auto)* | Absolute path to the `glados` binary. Leave empty to locate it on `PATH`. |
| `quant-lsp.debugAdapterPath` | *(auto)* | Absolute path to the `quant-dap` binary. Leave empty to locate it on `PATH`. |
| `quant-lsp.trace.server` | `"off"` | Log LSP traffic to the **Quant Language Server** output channel. Set to `"messages"` or `"verbose"` to troubleshoot. |

---

## Troubleshooting

**Extension doesn't start / no diagnostics**
Open the **Output** panel, select *Quant Language Server*, and look for
startup errors.  The most common cause is a missing `quant-lsp` binary.
Run `make install` or set `quant-lsp.serverPath` to the full path.

**"quant-lsp binary not found"**
Either `quant-lsp` is not on your shell `PATH` as seen by VS Code, or the
binary wasn't built yet.  Set `quant-lsp.serverPath` to the absolute path,
or add the install directory to `PATH` in your shell profile and restart VS
Code.

**Debug adapter not found**
Build `quant-dap` with `cabal build exe:quant-dap && make install`, or point
`quant-lsp.debugAdapterPath` at the binary directly.

---

## License

BSD 2-Clause, see the repository root for the full text.

## Links

- [Language reference](https://github.com/Sigmapitech/glados)
- [Bug tracker](https://github.com/Sigmapitech/glados/issues)
- [The Quant team](https://github.com/Sigmapitech)
