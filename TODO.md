# Open Tasks

## lsp-server: build blocked on `zlib` in Nix/Docker environment

**Status:** Build fails at `zlib-0.7.1.1` (transitive dependency via `lsp` → `websockets` → `streaming-commons` → `zlib`).

**Symptom:**
```
running dist/build/Codec/Compression/Zlib/Stream_hsc_make failed (exit code 127)
error while loading shared libraries: libzstd.so.1: cannot open shared object file
```

**Root cause:** `hsc2hs` (used by `zlib` to process `Stream.hsc`) generates a C helper binary (`Stream_hsc_make`) that is linked against GHC's runtime, which in the Nix GHC toolchain depends on `libzstd.so.1`. In the current Docker/Nix-in-Docker environment the system dynamic linker and the Nix ld.so environment disagree on where to find `libzstd.so.1`.

`libzstd.so.1` physically exists at:
- `/nix/store/2m97xlq3lpfawmvnp9ii2kk1j0yfzy6q-zstd-1.5.7/lib/libzstd.so.1` (Nix store path baked into GHC binaries)
- `/usr/lib/x86_64-linux-gnu/libzstd.so.1` (system)

**Tried:**
- `ldconfig` with Nix zstd paths added to `/etc/ld.so.conf.d/`
- `extra-lib-dirs` in `cabal.project`
- `constraints: zlib installed` (fails, no pre-installed zlib for GHC 9.8.4)

**Likely fix:** Wrap the Nix GCC `cc` command (used by hsc2hs) to inject `-Wl,-rpath,/nix/store/2m97xlq3lpfawmvnp9ii2kk1j0yfzy6q-zstd-1.5.7/lib` so the compiled `Stream_hsc_make` binary has the correct RPATH at link time. Alternatively, set `NIX_LDFLAGS` to include the zstd path before invoking `cabal build`.

Try:
```bash
NIX_LDFLAGS="-rpath /nix/store/2m97xlq3lpfawmvnp9ii2kk1j0yfzy6q-zstd-1.5.7/lib" cabal build glados-lsp
```
