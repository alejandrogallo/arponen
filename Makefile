%.html: %.org
	emacs -Q --batch \
		$< \
		-f org-html-export-to-html

%.tex: %.org
	emacs -Q --batch \
		$< \
		-f org-latex-export-to-latex
