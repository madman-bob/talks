.PHONY: slides clean distclean

acm-sig-proceedings.csl:
	wget https://raw.githubusercontent.com/citation-style-language/styles/master/acm-sig-proceedings.csl

first-class-algebraic-presentations.pdf: first-class-algebraic-presentations.md template.tex ref.bib acm-sig-proceedings.csl
	pandoc first-class-algebraic-presentations.md --template template.tex --bibliography=ref.bib --citeproc --filter katla-pandoc -o first-class-algebraic-presentations.pdf

first-class-algebraic-presentations-full.pdf: first-class-algebraic-presentations-full.md template.tex ref.bib acm-sig-proceedings.csl
	pandoc first-class-algebraic-presentations-full.md --template template.tex --bibliography=ref.bib --citeproc --filter katla-pandoc -o first-class-algebraic-presentations-full.pdf

slides: slides.pdf

slides.pdf: talk.md
	pandoc talk.md -t beamer --filter katla-pandoc -o slides.pdf

clean:
	$(RM) first-class-algebraic-presentations.pdf slides.pdf

distclean: clean
