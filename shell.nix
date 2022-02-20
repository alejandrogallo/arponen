{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {

  CMU_PATH = "${pkgs.cmucl_binary}/bin/lisp";
  buildInputs = with pkgs; [
    sbcl
    ccl
    clisp
    ecl
    gcl
    # clasp-common-lisp
    cmucl_binary
    abcl
  ];

}
