# Copyright 2024 The MathWorks, Inc.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see http://www.gnu.org/licenses/.
#
# Usage
# -----
#   make                             - build and run tests in ./tests
#   make MATLAB_EXE=/path/to/matlab  - build and run tests using specified MATLAB executable

# MATLAB executable to used by tests/Makefile for testing matlab-shell
MATLAB_EXE = matlab
ifeq ($(shell which $(MATLAB_EXE)),)
   $(warning $(MATLAB_EXE) not found. Consider running: make MATLAB_EXE=/path/to/matlab)
endif
ifneq ($(MATLAB_EXE),matlab)
    # This file quoting assumes bash shell syntax
    export MATLAB_PROG_SETUP = --eval='(setq matlab-shell-command "$(MATLAB_EXE)")'
endif

EMACS = emacs
EMACSFLAGS = -batch --no-site-file --eval "(setq debug-on-error t)"

LOADPATH = ./
LOADDEFS = matlab-load.el
LOADDIRS = .

EL_SRCS  = $(filter-out $(LOADDEFS), $(wildcard *.el))
ELC = $(EL_SRCS:.el=.elc)

.PHONY: all
all: lisp tests

.PHONY: lisp
lisp: $(LOADDEFS) $(ELC)

$(LOADDEFS):
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) \
            --eval '(setq generated-autoload-file "$(abspath $(LOADDEFS))")' \
            -f batch-update-autoloads $(abspath $(LOADDIRS))

%.elc: %.el | $(LOADDEFS)
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) -f batch-byte-compile $<

$(ELC): $(MAKEFILE_LIST)

.PHONY: tests
tests: .tests.tstamp

.tests.tstamp: $(LOADDEFS) $(ELC)
	$(MAKE) -C tests
	@touch $@

.PHONY: clean
clean:
	$(RM) $(LOADDEFS)
	$(RM) *.elc
	$(RM) *.tstamp

# [eof] Makefile
