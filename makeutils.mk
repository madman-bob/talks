makeSlides=mddia $(1) | pandoc -t beamer -o $(2) --highlight-style=kate; $(RM) *.dot *.png
