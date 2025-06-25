# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      cabal-install
    ]))
  ];
}
