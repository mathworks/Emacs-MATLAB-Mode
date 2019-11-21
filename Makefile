# Automatically Generated Makefile by EDE.
# For use with: make
# Relative File Name: Makefile
#
# DO NOT MODIFY THIS FILE OR YOUR CHANGES MAY BE LOST.
# EDE is the Emacs Development Environment.
# http://cedet.sourceforge.net/ede.shtml
#

top="$(CURDIR)"/
ede_FILES=Project.ede Makefile

EMACS=emacs
EMACSFLAGS=-batch --no-site-file --eval '(setq debug-on-error t)'
LOADPATH= ./
LOADDEFS=matlab-load.el
LOADDIRS=.
misc_MISC=ChangeLog ChangeLog.old1 ChangeLog.old2 INSTALL README dl_emacs_support.m
lisp_LISP=matlab-compat.el matlab.el matlab-shell.el matlab-netshell.el matlab-complete.el matlab-cgen.el matlab-publish.el matlab-topic.el mlint.el tlc.el linemark.el
cedet_LISP=semantic-matlab.el semanticdb-matlab.el srecode-matlab.el cedet-matlab.el company-matlab-shell.el
VERSION=4.0
DISTDIR=$(top)matlab-emacs-$(VERSION)



all: autoloads misc lisp cedet toolbox Templates

.PHONY: clean-autoloads
clean-autoloads: 
	rm -f $(LOADDEFS)

.PHONY: autoloads
autoloads: 
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(setq generated-autoload-file "$(abspath $(LOADDEFS))")' -f batch-update-autoloads $(abspath $(LOADDIRS))


misc: 
	@

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) --eval '(progn $(call require, $(PRELOADS)))' -f batch-byte-compile $^

.PHONY: lisp
lisp: $(addsuffix c, $(lisp_LISP))

.PHONY: cedet
cedet: $(addsuffix c, $(cedet_LISP))

.PHONY:toolbox
toolbox:
	$(MAKE) -C toolbox

.PHONY:Templates
Templates:
	$(MAKE) -C templates

tags: 
	$(MAKE) -C toolbox/ $(MFLAGS) $@
	$(MAKE) -C templates/ $(MFLAGS) $@


clean:
	rm -f *.elc

.PHONY: dist

dist: autoloads
	rm -rf $(DISTDIR)
	mkdir $(DISTDIR)
	cp matlab-load.el $(misc_MISC) $(lisp_LISP) $(cedet_LISP) $(ede_FILES) $(DISTDIR)
	$(MAKE) -C toolbox $(MFLAGS) DISTDIR=$(DISTDIR)/toolbox dist
	$(MAKE) -C templates $(MFLAGS) DISTDIR=$(DISTDIR)/templates dist
	tar -cvzf $(DISTDIR).tar.gz $(DISTDIR)
	rm -rf $(DISTDIR)

# End of Makefile
