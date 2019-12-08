# MATLAB-Emacs Tests makefile
#

EMACS=emacs

all: modetests shelltest

.PHONY: modetests
modetests: metest.el
	$(EMACS) -batch -q -l metest.el -e "metest-all-syntax-tests"


.PHONY: shelltest
shelltest: mstest.el
	$(EMACS) -batch -q -l mstest.el -e "mstest-run-all-tests"


#end
