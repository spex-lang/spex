let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell rec {
  buildInputs = with pkgs; [
    haskell.compiler.ghc9101
    cabal-install
    haskellPackages.cabal-fmt
    stylish-haskell
    zlib.dev
    pkg-config
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
  '';
}
