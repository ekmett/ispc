{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
  buildInputs = [ ispc zlib ];
}
