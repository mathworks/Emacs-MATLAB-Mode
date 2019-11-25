function emacsnetshell(cmd, data)
% Create a connection to an EMACS editor server.
% If we succeed then setup a timer to watch what comes in from the connection.

    EMACSSERVER = getappdata(groot, 'EmacsNetShell');

    if nargin == 0
        cmd = 'init';
    end

    if strcmp(cmd, 'shutdown')
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
            sendinit = true;
        else
            sendinit = false;
        end

        if ~ischar(cmd)
            error('Command must be a char vector.');
        end

        if ~strcmp(cmd,'init') && sendinit
            if nargin == 2
                EMACSSERVER.SendCommand(cmd, data);
            else
                EMACSSERVER.SendCommand(cmd);
            end
        end
    end

end
