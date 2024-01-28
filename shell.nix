{ pkgs ? import <nixpkgs> {} }:
with pkgs;

mkShell {
  buildInputs = [
    ocamlPackages.menhir
  ];
}
