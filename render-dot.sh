#!/usr/bin/env bash

set -eu

input_file="${1:?Give an input file}"
output_dir="${2:?Give an output prefix}"

TMP=`mktemp -t new-test.XXXXXX`
trap "rm $TMP* 2>EXIT" 0
cat <<LISP > $TMP
(require :asdf)
(require :uiop)

(defvar *input-file* "${input_file}")
(defvar *output-dir* "${output_dir}")

(asdf:load-system :arponen)

(let ((diagrams (uiop:read-file-form *input-file*)))
  (ensure-directories-exist *output-dir*)
  (loop :for d :in diagrams
        :for id :from 0
        :do (loop :for dot :in (arponen/dot::render-all d)
                  :for idot :from 0
                  :for outname = (format nil "~a/diagram-~a-~a.dot"
                                         *output-dir*
                                         id idot)
                  :do
                     (format t ".")
                     (with-open-file (s outname :if-exists :supersede
                                                :direction :output)
                       (format s "~a" dot)))))
LISP


echo "CREATING DOT"
sbcl --script $TMP

cd $output_dir

echo "CONVERTING DOT TO SVG"
for i in *.dot; do
  printf "."
  dot -Tsvg $i > ${i%.dot}.svg
done
