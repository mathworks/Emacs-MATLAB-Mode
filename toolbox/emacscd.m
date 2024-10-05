% Copyright (C) 2024  Eric Ludlam (and others)

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
function emacscd(dir)
% CD to DIRECTORY in a way that Emacs Shell won't see via dirtrack.
% Instead, show example of how to tell Emacs what the new directory is.
    
    if nargin == 1
        cd(dir);
    end

    emacs = getenv('INSIDE_EMACS');
    
    if isempty(emacs)
        disp('Not inside Emacs')
    else
        % disp('Inside Emacs - Sending directory cookie.')

        % Set matlab-shell `default-directory' to the current MATLAB pwd. Note, default-directory
        % requires a trailing slash so things like `find-file' C-x C-f work as expect.
        disp('<EMACSCAP>(eval)')
        disp(['(setq default-directory "' pwd '/")'])
        disp('</EMACSCAP>')
    end
end

% LocalWords:  dirtrack EMACSCAP setq
