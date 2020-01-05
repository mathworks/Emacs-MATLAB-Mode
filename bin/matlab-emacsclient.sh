#!/bin/sh
# File: matlab-emacs/bin/matlab-emacsclient.sh
# Abstract:
#   Use to redirect MATLAB 'edit foo.m' command such that foo.m opens in Emacs.
#
#   When running MATLAB on Mac, openFileOnMac() of edit.m is passed the applicationName which must be
#   a single command without switches:
#      [status, result] = unix(['which ' appInQuotes ]);
#   This file is the applicationName.
#   To enable
#      >> edit foo.m
#   to open foo.m in Emacs via emacsclient, we need to use arguments to emacsclient to open foo.m.
#   Thus, this is a wrapper that bridges the gap. This is used on Linux and Mac. Also, to avoid
#   running an xterm (which doesn't work when ssh'ing into a Mac), we need
#   checkMacApp(applicationName, 'emacs') of edit.m to return false, therefore cannot use
#   emacsclient.sh and therefore use matlab-emacsclient.sh

if [ "$EDITOR_EMACSCLIENT" = "" ]; then
    # This should be setup by matlab-emacs/toolbox/+emacs/set.m
    echo "assert - EDITOR_EMACSCLIENT is not set"
    exit 1
fi

exec $EDITOR_EMACSCLIENT "$@"

# LocalWords:  emacsclient ssh'ing EMACSCLIENT
