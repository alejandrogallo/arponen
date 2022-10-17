#!/usr/bin/env bash
# Copyright (C) 2022 by Alejandro Gallo <aamsgallo@gmail.com>

set -eux
: ${1:?Provide lisp hirata file}
: ${2:?Provide arponen out file}

tmp=`mktemp`

cat <<LISP > $tmp
(in-package :hirata)

(hirata->arponen "$1" "$2")

LISP

sbcl \
    --load $HOME/quicklisp/quicklisp/setup.lisp \
    --load parser.lisp \
    --load $tmp \
    --quit
