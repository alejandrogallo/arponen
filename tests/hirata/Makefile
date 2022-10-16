OUTFILES = $(shell find . -name \*.out)
LISP_HIRATA_FILES = $(patsubst %.out,%.lisp,$(OUTFILES))

parse: $(LISP_HIRATA_FILES)

%.lisp: %.out
	sbcl \
	--load parser.lisp \
	--eval "(format t \"~S\" (hirata::save-parse-outfile \"$<\" \"$@\"))" \
	--quit

.PHONY: parse
