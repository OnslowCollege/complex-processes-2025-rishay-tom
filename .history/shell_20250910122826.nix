
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      cabal-install
    ]))

    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-test
  ];
}
