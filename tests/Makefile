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

EMACS = emacs
EMACSFLAGS = -batch --no-site-file --eval '(setq debug-on-error t)'

MATLAB_FILES = $(wildcard *.m)

TEST_TARGETS = modetests

ifndef OS # OS is defined on Windows (e.g. OS=Windows_NT) and there's no MATLAB shell tests on windows.
   TEST_TARGETS += shelltests
endif

.PHONY: all
all: $(TEST_TARGETS)

.PHONY: modetests
modetests: metest.el
	$(EMACS) $(EMACSFLAGS) -l metest.el -e "metest-all-syntax-tests"

.PHONY: shelltests
shelltests: mstest.el
	$(EMACS) $(EMACSFLAGS) -l mstest.el $(MATLAB_PROG_SETUP) -e "mstest-run-all-tests"

# [eof] tests/Makefile

# LocalWords:  EMACSFLAGS setq ifneq modetests shelltests metest mstest eof
