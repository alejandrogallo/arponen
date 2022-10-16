.PHONY: test-parse
test-parse:
	sbcl \
	--load parser.lisp \
	--load t.lisp \
	--eval "(fiveam:run! 'hirata::hirata-parsing)" \
	--quit

OUTFILES = $(shell find . -name \*.out)
LISP_HIRATA_FILES = $(patsubst %.out,%.lisp,$(OUTFILES))

parse: $(LISP_HIRATA_FILES)

%.lisp: %.out
	sbcl \
	--load parser.lisp \
	--eval "(format t \"~S\" (hirata::save-parse-outfile \"$<\" \"$@\"))" \
	--quit

.PHONY: parse
