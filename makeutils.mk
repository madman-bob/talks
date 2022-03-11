makeSlides=mddia $(1) | pandoc -t beamer --citeproc --filter pandoc-latex-fontsize -o $(2) --highlight-style=kate; $(RM) *.dot *.png
