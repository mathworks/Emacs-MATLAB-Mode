# MATLAB-Emacs Tests makefile
#

all: modetests shelltest

.PHONY: modetests
modetests: metest.el
	emacs -batch -l metest.el -e "metest-all-syntax-tests"


.PHONY: shelltest
shelltest: mstest.el
	emacs -batch -l mstest.el -e "mstest-run-all-tests"


#end
