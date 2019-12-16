# Automatically Generated Makefile by EDE.
# For use with: make
# Relative File Name: tests/Makefile
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
#

top=../
ede_FILES=Project.ede Makefile

tests_LISP=mstest.el metest.el
EMACS=emacs
EMACSFLAGS=-batch --no-site-file --eval '(setq debug-on-error t)'
require=$(foreach r,$(1),(require (quote $(r))))
LOADPATH= ../
matlab_MISC=buggy.m dbtester.m expressions.m indents.m mclass.m mpclass.m stringtest.m syntaxerr.m cellscript.m fontlock.m testeeval.m
VERSION=4.0
DISTDIR=$(top)matlab-emacs-$(VERSION)/tests



all: tests matlab modetests shelltests

.PHONY: modetests
modetests: metest.elc
	$(EMACS) -batch -q -l metest.elc -e "metest-all-syntax-tests"

.PHONY: shelltests
shelltests: mstest.elc
	$(EMACS) -batch -q -l mstest.elc -e "mstest-run-all-tests"

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(progn $(call require, $(PRELOADS)))' -f batch-byte-compile $^

.PHONY: tests
tests: $(addsuffix c, $(tests_LISP))

matlab: 
	@

tags: 


clean:
	rm -f *.elc

.PHONY: dist

dist:
	mkdir $(DISTDIR)
	cp $(tests_LISP) $(matlab_MISC) $(ede_FILES) $(DISTDIR)

# End of Makefile
