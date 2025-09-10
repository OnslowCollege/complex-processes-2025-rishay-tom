{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      cabal-install
      aeson
      scotty
      text
      bytestring
      directory
      filepath
      transformers
    ]))
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    zlib
    gmp
    pkg-config
  ];
}