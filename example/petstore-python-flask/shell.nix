let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/24.05.tar.gz";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell {
  packages = with pkgs; [
    (pkgs.python3.withPackages (python-pkgs: with python-pkgs; [
      flask
      flask-jwt-extended
    ]))
  ];
}
