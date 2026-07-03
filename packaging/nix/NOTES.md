# Nix packaging notes

This file is a briefing for whoever adds the Nix package. The project already
has a working `flake.nix` with a `devShell` and a full GHC toolchain. What is
missing is exposing `glados` and `quant-lsp` as installable Nix packages/apps.

Naming convention:
- `glados`     -- the compiler/runtime CLI (the binary is called `glados`)
- `quant-lsp`  -- the LSP server (the language is Quant, LSP follows the language name)
- stdlib lives at `/usr/local/share/quant/lib/` (language name, not tool name)

---

## Current flake state

The flake (`flake.nix` at the repo root) already:

- Pins `nixpkgs` to `nixos-25.05`
- Exposes `devShells.default` with GHC 9.8.4, cabal-install, HLS, and all
  Haskell dependencies
- Has a `shared` output that gives access to `pkgs` and `haskell` per system
- Supports only `x86_64-linux` right now (`genSystems`)

The `haskell` attribute is `pkgs.haskell.packages.ghc984`.

---

## What needs to be added

### 1. `packages.glados` and `packages.quant-lsp`

Use `callCabal2nix` to build each component from the cabal.project monorepo:

```nix
packages = eachSystem (pkgs: let
  haskell = pkgs.haskell.packages.ghc984;
  src = ./.;
in {
  glados     = haskell.callCabal2nix "cli"        src {};
  quant-lsp  = haskell.callCabal2nix "lsp-server" src {};
});
```

The tricky part is that this is a **multi-package cabal.project**, not a single
cabal file. `callCabal2nix` targets one package at a time. Either:

- Build each sub-package separately pointing `src` at the sub-directory
  (`./cli`, `./lsp-server`), OR
- Use `developPackage` with `returnShellEnv = false` and a
  `cabalProject2nix` overlay (available in `haskellPackages.developPackage`).

The `cabal.project` file already lists all packages; a working approach is:

```nix
glados = (haskell.developPackage {
  root = ./.;
  name = "cli";
  returnShellEnv = false;
  withHoogle = false;
}).overrideAttrs (old: {
  postInstall = ''
    mv $out/bin/cli $out/bin/glados
  '';
});

quant-lsp = (haskell.developPackage {
  root = ./.;
  name = "lsp-server";
  returnShellEnv = false;
  withHoogle = false;
}).overrideAttrs (old: {
  postInstall = ''
    mv $out/bin/glados-lsp $out/bin/quant-lsp
  '';
});
```

### 2. The stdlib `.qa` files

The CLI auto-detects the stdlib at:
1. `--stdlib DIR` flag
2. `$QUANT_STDLIB` env var
3. `/usr/local/share/quant/lib/` (system install path)
4. `./std` (dev fallback)

For the Nix package, the cleanest approach is to set `QUANT_STDLIB` via a
wrapper script so users never have to think about it:

```nix
glados = pkgs.symlinkJoin {
  name = "glados";
  paths = [ glados-unwrapped ];
  buildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/glados \
      --set QUANT_STDLIB ${quant-stdlib}
  '';
};

quant-stdlib = pkgs.runCommand "quant-stdlib" {} ''
  mkdir -p $out
  cp -r ${src}/std/. $out/
'';
```

### 3. `apps` output (for `nix run`)

```nix
apps = eachSystem (pkgs: {
  glados    = { type = "app"; program = "${self.packages.${pkgs.system}.glados}/bin/glados"; };
  quant-lsp = { type = "app"; program = "${self.packages.${pkgs.system}.quant-lsp}/bin/quant-lsp"; };
  default   = self.apps.${pkgs.system}.glados;
});
```

This makes `nix run github:org/glados -- run hello.qa` work.

---

## Runtime dependencies

The binaries link against:

- `gmp` (`libgmp`) -- GHC's bignum library
- `zlib` -- already in `devShells.default.buildInputs`
- `zstd` -- already in `devShells.default.buildInputs`

For the Nix package, add these to `buildInputs` / let Nix track them via
`pkgs.haskell.lib.addBuildDepend`.

---

## Testing the package locally

```bash
# Build
nix build .#glados

# Run
./result/bin/glados run hello.qa

# Install into profile
nix profile install .#glados
glados run hello.qa
```

---

## Multi-platform (aarch64, macOS)

Currently `genSystems` only covers `x86_64-linux`. To add more platforms:

```nix
genSystems = lib.genAttrs ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
```

macOS requires `libiconv` as an extra build dependency for GHC. Darwin
cross-compilation is untested.
