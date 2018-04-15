{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "live-queries";
  buildInputs = [
    pkgs.elmPackages.elm-make 
    pkgs.elmPackages.elm-package
    pkgs.mongodb
    pkgs.nodejs-8_x
  ];
}
