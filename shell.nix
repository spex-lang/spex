let
  # The last commit of the PR which adds GHC 9.6.6
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/269d73766f777ea37104a1b3b41c1c9575da037d";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    haskell.compiler.ghc966
    cabal-install
    haskellPackages.cabal-fmt
    stylish-haskell
    zlib.dev
    pkg-config
    upx
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
  '';
}
