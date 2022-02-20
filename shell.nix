{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {

  buildInputs = with pkgs; [
    sbcl
    ccl
    clisp
    ecl
    gcl
    clasp
  ];

}