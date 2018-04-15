{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "parse-example";

  buildInputs = [ pkgs.mongodb pkgs.nodejs-8_x ];
}
