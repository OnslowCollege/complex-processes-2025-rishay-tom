# shell.nix
let
  # Use a specific version of nixpkgs (you can update this as needed)
  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixpkgs-22.11-darwin.tar.gz";
    # Use nixpkgs-22.11; you can also use nixpkgs-23.11 or a specific commit
    sha256 = "1xi53rlslcprybsvrmipm69ypd3g3hr7wkxvzc73ag8296yclyll";
  }) {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.gnumake
    pkgs.zlib
    pkgs.ncurses
    pkgs.libffi
    pkgs.nix

    pkgs.haskell.compiler.ghc8107
    pkgs.haskellPackages.cabal-install

    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-test
  ];

  shellHook = ''
    export STACK_YAML=stack.yaml
    export STACK_IN_NIX_SHELL=1
    export STACK_IN_NIX_EXTRA_ARGS="--system-ghc"
  '';
}
