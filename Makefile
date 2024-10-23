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
#   make                             - build lisp and run tests in ./tests
#   make MATLAB_EXE=/path/to/matlab  - build lisp and run tests using specified MATLAB executable
#   make lisp                        - build lisp without running the tests.
#   make tests                       - run the tests (will rebuild lisp if needed)
#
# Administration:
#   make list-files-for-release      - show the files that are used by Emacs in "release" mode
#

EMACS = emacs
EMACSFLAGS = --batch -Q --eval "(setq debug-on-error t)"

LOADPATH = ./
LOADDEFS = matlab-autoload.el
LOADDIRS = .

EL_SRCS  = $(filter-out $(LOADDEFS), $(wildcard *.el))
ELC = $(EL_SRCS:.el=.elc)

GOALS := $(if $(MAKECMDGOALS),$(MAKECMDGOALS),all)

.PHONY: all
all: lisp tests

.PHONY: lisp
lisp: $(LOADDEFS) $(ELC)

HAVE_OLD_EMACS = $(shell $(EMACS) --batch -Q --eval \
		   "(when (<= emacs-major-version 28) (message \"have-old-emacs\"))" 2>&1)
ifeq ($(filter have-old-emacs,$(HAVE_OLD_EMACS)),have-old-emacs)
    BATCH_UPDATE = --eval '(setq generated-autoload-file "$(abspath $(LOADDEFS))")' \
		   -f batch-update-autoloads $(abspath $(LOADDIRS))
else
    BATCH_UPDATE = -f loaddefs-generate-batch $(abspath $(LOADDEFS)) $(abspath $(LOADDIRS))
endif

$(LOADDEFS): | .clean.tstamp
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) $(BATCH_UPDATE)

%.elc: %.el | $(LOADDEFS)
	$(EMACS) $(EMACSFLAGS) $(addprefix -L ,$(LOADPATH)) -f batch-byte-compile $<

$(ELC): $(LOADDEFS) $(MAKEFILE_LIST) | .clean.tstamp

ifneq ($(filter tests all, $(GOALS)),)
    # Running tests. Check for existence of MATLAB because it's used by tests/Makefile for testing
    # matlab-shell.
    MATLAB_EXE = matlab
    ifeq ($(shell which $(MATLAB_EXE)),)
        $(warning $(MATLAB_EXE) not found. Consider running: make MATLAB_EXE=/path/to/matlab)
    endif
    export MATLAB_PROG_SETUP = \
        "--eval=(setq matlab-shell-command \"$(MATLAB_EXE)\")" \
	"--eval=(setq matlab-shell-command-switches '(\"-nodesktop\" \"-nosplash\" \"-noFigureWindows\"))"
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
	find . -name .git -prune -o -name "*.tstamp" -exec $(RM) {} \;
	find . -name .git -prune -o -name "*~" -exec $(RM) {} \;

#--------------------------------#
# Test various versions of Emacs #
#--------------------------------#

# make check-emacs-versions
#
# This requires that you define EMACS27, etc. in the environment or specify them when invoking make.

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
.tstamp/emacs$(1).tstamp: $(EL_SRCS) $$(MAKEFILE_LIST)
	@echo "Testing build with EMACS$(1)"
	$(MAKE) clean
	if [ ! -d ".tstamp" ]; then mkdir .tstamp; fi
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

#------------------------------------#
# List files that should be released #
#------------------------------------#

.PHONY: list-files-for-release
list-files-for-release:
	@/usr/bin/ls -1 *.el | grep -v -P '(matlab-autoload.el|matlab-maint.el)'
	@/usr/bin/ls bin/*.sh
	@find toolbox -name '*.m' -print

# [EOF] Makefile

# LocalWords:  EMACSFLAGS setq LOADPATH LOADDEFS LOADDIRS ELC elc MAKECMDGOALS ifeq abspath
# LocalWords:  autoloads loaddefs tstamp addprefix ifneq nodesktop nosplash endef usr
