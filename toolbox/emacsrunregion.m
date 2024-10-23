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
function emacsrunregion(file, startchar, endchar)
% Run code from FILE between STARTCHAR and ENDCHAR.
% Command sent by Emacs for run code sections and run-region functionality.

    % Filter out emacs tramp file path prefix
    trampMatch = regexp(file, {'/*:',':/'});
    if (~isempty(trampMatch{1}))
        file = file((trampMatch{2}+1):end);
    end

    if ~exist(file,'file')
        error('You must save your region into a file accessible by MATLAB process.');
    end

    % Now figure out if shortFileName is on the path.
    [ fullFilePath, shortFileName ] = fileparts(file);
    onpath = ~isempty(which(shortFileName));

    % If not on the path, temporarily switch to that directory so it and an files it references are
    % accessible
    if ~onpath
        oldpath = pwd;
        cd(fullFilePath);
        cleanup = onCleanup(@()cd(oldpath));
    end

    txt = fileread(file);
    evalTxt = txt(startchar:min(endchar,length(txt)));
    evalin('base',evalTxt);
end

% LocalWords:  Ludlam STARTCHAR ENDCHAR
