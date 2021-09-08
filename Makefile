.PHONY: slides

slides:
	for dir in */; do make -C "$$dir" slides; done
