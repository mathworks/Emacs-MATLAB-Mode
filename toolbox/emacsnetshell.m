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
function nso = emacsnetshell(cmd, data)
% Create a connection to an EMACS editor server.
%
% emacsnetshell('init') - Initialize the connection with Emacs.
%    emacs will send commands to MATLAB with additional connectivity
%    information.
%
% emacsnetshell('ack') - Send ack to Emacs, it should echo back.
%
% emacsnetshell('output',data) - Send DATA as output to display in Emacs.
% emacsnetshell('error',data) - Send DATA as error description to Emacs.
% emacsnetshell('eval',data) - Send DATA - a string containing an Emacs
%    lisp form which will be evaluated in Emacs.
    

    EMACSSERVER = getappdata(groot, 'EmacsNetShell');

    if nargin == 0
        if isempty(EMACSSERVER)
            cmd = 'init';
        else
            cmd = 'fetch';
        end
    end

    if strcmp(cmd, 'fetch')
        % Fetch means to get the server and return it.  Do not create
        % the server on fetch - otherwise no way to know if a server was started
        % or not.
        nso = EMACSSERVER;
        return;
        
    elseif strcmp(cmd, 'shutdown')
        % Shutdown our connection to emacs.
        setappdata(groot, 'EmacsNetShell', []);

        if ~isempty(EMACSSERVER)
            delete(EMACSSERVER);
        end
    else
        if ~isempty(EMACSSERVER) && ~isvalid(EMACSSERVER)
            EMACSSERVER = [];
            setappdata(groot, 'EmacsNetShell', EMACSSERVER);
        end

        if isempty(EMACSSERVER)
            EMACSSERVER = emacs.EmacsServer();
            setappdata(groot, 'EmacsNetShell', EMACSSERVER);
            sendinit = false;
        else
            sendinit = true;
        end

        if ~ischar(cmd)
            error('Command must be a char vector.');
        end

        if ~strcmp(cmd,'init') || sendinit
            if nargin == 2
                EMACSSERVER.SendCommand(cmd, data);
            else
                EMACSSERVER.SendCommand(cmd);
            end
        end
    end
    
    if nargout == 1
        nso = EMACSSERVER;
    end

end
