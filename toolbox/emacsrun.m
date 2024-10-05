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
function emacsrun(mfile, varargin)
% Run code from MFILE.
% Assumes MFILE was recently edited, and proactively clears that function.
%
% Command sent by Emacs for save-and-go functionality

    % Now figure out if shortFileName is on the path.
    [ fullFilePath, shortFileName ] = fileparts(mfile);
    onpath = ~isempty(which(shortFileName));

    if ~exist(fullFilePath,'file')
        error('You must save your file into a location accessible by MATLAB process.');
    end

    
    % If not on the path, temporarilly switch to that directory so it and an files it references are
    % accessible
    if ~onpath
        oldpath = pwd;
        cd(fullFilePath);
        cleanup = onCleanup(@()cd(oldpath));
    end
    clear(shortFileName);

    cmd = [ shortFileName varargin{:} ];
    
    evalin('base',cmd);
end
