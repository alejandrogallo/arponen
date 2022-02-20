#!/usr/bin/env bash
set -eu

LISP=$1

$LISP <<EOF
(load "t.lisp")
EOF
