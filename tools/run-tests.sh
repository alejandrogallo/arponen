#!/usr/bin/env bash
set -eu

LISP=$1

$LISP <<EOF
(time (load "t.lisp"))
EOF
