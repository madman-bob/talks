.PHONY: slides

slides: talk.pdf

talk.pdf: talk.md
	mddia talk.md | pandoc -t beamer -o slides.pdf --highlight-style=kate
	rm *.dot *.png
