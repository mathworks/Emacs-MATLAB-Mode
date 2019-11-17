% This file contains sample text from a MATLAB shell buffer.
% Use this for parsing tests for error trackers.

% Newer MATLABs.  This text copied from MATLAB R2019b

% Errors:
Error using ls (line 49)
ls: cannot access 'blarg': No such file or directory


Error in buggy (line 12)
        ls blarg

% Errors:
Error using buggy (line 7)
You encounered an error in buggy.m

% Syntax
Error: File: /home/eludlam/WORK/matlab-emacs-src/toolbox/syntaxerr.m Line: 8 Column: 12
Invalid expression. When calling a function or indexing a variable, use parentheses. Otherwise, check for mismatched delimiters.

% Syntax
Error: File: /home/eludlam/WORK/matlab-emacs-src/toolbox/syntaxerr.m Line: 4 Column: 9
Character vector is not terminated properly.
    

% Warning:
Warning: You enountered a warning in buggy.m 
> In buggy (line 15) 


% Oldest MATLABs.  This text taken from comments in MATLAB.el from
% a long time ago.

% Errors:
>> ls
Error in ==> buggy
On line 12 ==> ls blarg

% Errors:

Error using ==> buggy at 7

% Syntax:

Syntax error in ==> syntaxerr.m
On line 8 ==> B = A(1

% Warning: 
In buggy at line 15 You encountered a warning


% End