#!/bin/bash
#
# Run a suite of matlab-emacs tests

emacs -batch -l metest.el -e "metest-comment-string-syntax-test"

#end
