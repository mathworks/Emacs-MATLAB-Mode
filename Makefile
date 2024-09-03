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
#   make NOTESTS=1                   - build without running the tests.

EMACS = emacs
EMACSFLAGS = -batch --no-site-file --eval "(setq debug-on-error t)"

LOADPATH = ./
LOADDEFS = matlab-load.el
LOADDIRS = .

EL_SRCS  = $(filter-out $(LOADDEFS), $(wildcard *.el))
ELC = $(EL_SRCS:.el=.elc)

ALL_TARGETS = lisp
ifeq ($(NOTESTS),)
    ALL_TARGETS += tests
endif

.PHONY: all
all: $(ALL_TARGETS)

.PHONY: lisp
lisp: $(LOADDEFS) $(ELC)

$(LOADDEFS): | .clean.tstamp
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) \
            --eval "(require 'autoload)" \
            --eval '(setq generated-autoload-file "$(abspath $(LOADDEFS))")' \
            -f batch-update-autoloads $(abspath $(LOADDIRS))

%.elc: %.el | $(LOADDEFS)
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) -f batch-byte-compile $<

$(ELC): $(LOADDEFS) $(MAKEFILE_LIST) | .clean.tstamp

ifeq ($(NOTESTS),)
    # MATLAB executable to used by tests/Makefile for testing matlab-shell
    MATLAB_EXE = matlab
    ifeq ($(shell which $(MATLAB_EXE)),)
       $(warning $(MATLAB_EXE) not found. Consider running: make MATLAB_EXE=/path/to/matlab)
    endif
    ifneq ($(MATLAB_EXE),matlab)
        # This file quoting assumes bash shell syntax
        export MATLAB_PROG_SETUP = --eval='(setq matlab-shell-command "$(MATLAB_EXE)")'
    endif
endif

.PHONY: tests
tests: .tests.tstamp

.tests.tstamp: $(LOADDEFS) $(ELC)
	$(MAKE) -C tests
	@touch $@

# When switching emacs versions, we need to clean generated files
.clean.tstamp:
	$(RM) *.elc
	$(RM) $(LOADDEFS)
	@touch $@

.PHONY: clean
clean:
	$(RM) $(LOADDEFS)
	$(RM) *.elc
	$(RM) *.tstamp
	$(RM) -r tstamp

#--------------------------------#
# Test various versions of Emacs #
#--------------------------------#

# make check-emacs-versions
#
# This requires that you define EMACS27, etc. in the environmenbt or specify them when invoking make.

SUPPORTED_EMACS_VERSIONS = 27 28 29

define CHECK_EMACS_VER
  ifeq ($$(shell which $(EMACS${1})),)
    $$(error 'EMACS${1}' is not defined. $$(HELP))
  endif
endef

ifeq ($(MAKECMDGOALS),check-emacs-versions)
    $(foreach V,$(SUPPORTED_EMACS_VERSIONS),$(eval $(call CHECK_EMACS_VER,$(V))))
endif

#---
#  Generate test targets
define TEST_SUPPORTED_VER_TARGET
.tstamp/emacs$(1).tstamp: $(EL_SRCS) $$(MAKEFILE_LIST) | .tstamp
	@echo "Testing build with EMACS$(1)"
	$(MAKE) clean
	$(MAKE) EMACS=$(EMACS$(1))
	@touch $$@

endef

$(foreach V,$(SUPPORTED_EMACS_VERSIONS),$(eval $(call TEST_SUPPORTED_VER_TARGET,$(V))))

#---
# Create dependencies to avoid testing different versions of emacs at the same time. For example:
#    .tstamp/emacs29.tstamp: .tstamp/emacs28.tstamp
define CREATE_VER_DEPEND
.tstamp/emacs$(1).tstamp: .tstamp/emacs$(shell echo $$(($(V) - 1))).tstamp

endef

ALL_BUT_FIRST_VER = $(wordlist 2,$(words $(SUPPORTED_EMACS_VERSIONS)),$(SUPPORTED_EMACS_VERSIONS))

$(foreach V,$(ALL_BUT_FIRST_VER),$(eval $(call CREATE_VER_DEPEND,$(V))))

#---
CHECK_TARGETS = $(foreach V,$(SUPPORTED_EMACS_VERSIONS),.tstamp/emacs$(V).tstamp)

.PHONY: check-emacs-versions
check-emacs-versions: $(CHECK_TARGETS)

.tstamp:
	$(HIDE) mkdir $@

# [eof] Makefile
