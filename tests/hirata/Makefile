OUTFILES = $(shell find . -name \*.out)

%.lisp: %.out
	sbcl \
	--load parser.lisp \
	--eval "(format t \"~S\" (hirata::save-parse-outfile \"$<\" \"$@\"))" \
	--quit
