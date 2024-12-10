let
  # The commit that adds GHC 9.8.4:
  # https://github.com/NixOS/nixpkgs/pull/361586
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/87f98369e0d0e9b25bf7e13387c8d0b935b22217";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    haskell.compiler.ghc984
    cabal-install
    haskellPackages.cabal-fmt
    haskellPackages.fourmolu
    zlib.dev
    pkg-config
    upx
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
  '';
}
