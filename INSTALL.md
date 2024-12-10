---
title: Install Spex - Spex
---

# Installation

There are two main ways of installing Spex, either from pre-compiled binaries
or by building the binaries yourself from source.

## From pre-compiled binary

The easiest way to get the right pre-compiled binaries for your platform is to
use the `spexup` installer as follows.

### Automatic

```bash
 curl --proto '=https' --tlsv1.2 -sSf \
   https://raw.githubusercontent.com/spex-lang/spexup/refs/heads/main/spexup \
 | sh
```

What does this do? It
[automates](https://github.com/spex-lang/spexup/blob/main/spexup) the manual
steps below.

### Manual

If you don't like to pipe scripts into a shell (fair enough), you can manually
install the binaries as follows:

1. Go to [releases](https://github.com/spex-lang/spex/releases);
2. Click on "Assets" for the latest release;
3. Download the binaries for your platform and put them into your PATH.

## From source

When you build from source you'll need the GHC Haskell compiler and the Cabal
package system.

### With Nix

If you got the [Nix](https://nixos.org/download/) package manager installed
then the `shell.nix` file provides all dependencies, and you can simply type:

```bash
git clone https://github.com/spex-lang/spex.git
cd spex
nix-shell
cabal build all
cabal install spex spex-demo-petstore
```

### Without Nix

If you don't use Nix, then the easiest way to get the right Haskell
dependencies is to first install
[`ghcup`](https://www.haskell.org/ghcup/install/), the Haskell installer, and
then issue:

```bash
git clone https://github.com/spex-lang/spex.git
cd spex
ghcup install cabal 3.12.1.0 --set
ghcup install ghc 9.8.4 --set
cabal build all
cabal install spex spex-demo-petstore
```

