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
function opentoline(file, line, column)
%OPENTOLINE Open to specified line in function file in Emacs.
%   This is a hack to override the built-in opentoline program in MATLAB.
%
%   Remove this M file from your path to get the old behavior.

    editor = system_dependent('getpref', 'EditorOtherEditor');
    editor = editor(2:end);
    
    if nargin==3
        linecol = sprintf('+%d:%d',line,column);
    else
        linecol = sprintf('+%d',line);
    end
    
    f = which(file);
    if ~isempty(f)
        file=f;
    end    
    
    if ispc
        % On Windows, we need to wrap the editor command in double quotes
        % in case it contains spaces
        system(['"' editor '" "' linecol '" "' file '"&']);
    else
        % On UNIX, we don't want to use quotes in case the user's editor
        % command contains arguments (like "xterm -e vi")
        system([editor ' "' linecol '" "' file '" &']);
    end
end
