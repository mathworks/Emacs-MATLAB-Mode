#!/bin/bash
#
# Run a suite of matlab-emacs tests

emacs -batch -l metest.el -e "metest-all-syntax-tests"

#end
