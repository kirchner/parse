{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "elm-parse";
  buildInputs = [
    pkgs.elmPackages.elm-make 
    pkgs.elmPackages.elm-package
  ];
}
