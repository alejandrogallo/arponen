# [[file:README.org::*Parsing test suite][Parsing test suite:2]]
.PHONY: test-parse
test-parse:
	sbcl \
	--load parser.lisp \
	--load t.lisp \
	--eval "(fiveam:run! 'hirata::hirata-parsing)" \
	--quit
# Parsing test suite:2 ends here

# Makefile
# :PROPERTIES:
# :header-args:makefile+: :tangle Makefile
# :END:


# [[file:README.org::*Makefile][Makefile:1]]
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
	./make-arponen.sh $< $@

arponen-clean:
	@rm -v $(LISP_ARPONEN_FILES)

.PHONY: parse arponen
# Makefile:1 ends here
