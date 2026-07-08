# Arch Linux packaging notes

## Local build (no AUR)

```bash
cd packaging/arch
makepkg -si
```

`-s` installs missing makedepends automatically via pacman; `-i` installs the
built package when done.  You need `ghc` and `cabal-install` in PATH (both
available in the official `extra` repo).

The first build fetches all Hackage dependencies (~300 MB into `~/.cabal`).
Subsequent builds reuse the cache and are fast.

## pkgver strategy

The PKGBUILD uses a `-git` style `pkgver()` that produces `r<commit-count>.<short-sha>` (e.g. `r142.3f5d10a`).  When a stable release tag exists, replace the function with a plain version string and update `source=` to point to a tarball:

```bash
pkgver=1.0.0
source=("$pkgname-$pkgver.tar.gz::$url/archive/refs/tags/v$pkgver.tar.gz")
sha256sums=('...')  # fill in with sha256sum of the tarball
```

## Publishing to AUR

AUR packages live in their own git repo.  Steps:

```bash
# one-time: clone the (empty) AUR repo for this package
git clone ssh://aur@aur.archlinux.org/glados.git aur-glados

# copy PKGBUILD in and generate .SRCINFO
cp packaging/arch/PKGBUILD aur-glados/
cd aur-glados
makepkg --printsrcinfo > .SRCINFO
git add PKGBUILD .SRCINFO
git commit -m "initial release"
git push
```

You need an AUR account and your SSH key registered at https://aur.archlinux.org/account.

## Runtime dependencies

| Arch package | Why |
|---|---|
| `gmp` | GHC's arbitrary-precision integer support (libgmp.so) |
| `zlib` | GHC RTS compression (libz.so) |
| `zstd` | GHC 9.4+ uses zstd for interface files at runtime |

These are all in the official repos and typically already installed.

## Build dependencies

| Arch package | Why |
|---|---|
| `ghc` | Haskell compiler |
| `cabal-install` | Build tool and dependency resolver |

Both are in the `extra` repo:

```bash
sudo pacman -S ghc cabal-install
```
