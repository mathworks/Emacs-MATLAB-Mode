% Copyright (C) 2023  Eric Ludlam (and others)

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
Error: File: /home/eludlam/WORK/matlab-emacs-src/tests/syntaxerr.m Line: 8 Column: 12
Invalid expression. When calling a function or indexing a variable, use parentheses. Otherwise, check for mismatched delimiters.

% Syntax
Error: File: /home/eludlam/WORK/matlab-emacs-src/tests/syntaxerr.m Line: 4 Column: 9
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
