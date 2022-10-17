.PHONY: test-parse
test-parse:
	sbcl \
	--load parser.lisp \
	--load t.lisp \
	--eval "(fiveam:run! 'hirata::hirata-parsing)" \
	--quit

OUTFILES = $(shell find . -name \*.out)
LISP_HIRATA_FILES = $(patsubst %.out,%.lisp,$(OUTFILES))
LISP_ARPONEN_FILES = $(patsubst %.out,%.arponen,$(OUTFILES))
SBCL = sbcl --disable-debugger

parse: $(LISP_HIRATA_FILES)
arponen: $(LISP_ARPONEN_FILES)

%.lisp: %.out
	$(SBCL) \
	--load parser.lisp \
	--eval "(hirata::save-parse-outfile \"$<\" \"$@\")" \
	--quit

%.arponen: %.lisp
	$(SBCL) \
	--load parser.lisp \
	--eval "(hirata::hirata->arponen \"$<\" \"$@\")" \
	--quit

.PHONY: parse arponen
