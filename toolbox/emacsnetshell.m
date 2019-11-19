function emacsnetshell(sendmsg)
% Create a connection to an EMACS editor server.
% If we succeed then setup a timer to watch what comes in from the connection.
    
    NETSHELL = getappdata(groot, 'EmacsNetShell');

    if nargin == 0
        sendmsg = 'init';
    end
   
    if strcmp(sendmsg, 'shutdown')
        % Shutdown our connection to emacs.
        if ~isempty(NETSHELL)
            stop(NETSHELL.timer);

            delete(NETSHELL.tcpclient);
            delete(NETSHELL.timer);
            setappdata(groot, 'EmacsNetShell', []);
        end
    else
        if isempty(NETSHELL)
    
            NETSHELL.tcpclient = tcpclient('localhost', 32475);
            
            if isempty(NETSHELL.tcpclient)
                error('Unable to connect to Emacs.');
            end
            
            NETSHELL.timer = timer('Name','Emacs NetShell timer', ...
                                   'TimerFcn', @watch_emacs, ...
                                   'Period', 3,...
                                   'BusyMode', 'drop',...
                                   'ErrorFcn', @drop_emacs,...
                                   'ExecutionMode', 'fixedSpacing');
            
            setappdata(groot, 'EmacsNetShell', NETSHELL);

            start(NETSHELL.timer);
        end

        if isstring(sendmsg)
            sendmsg = char(sendmsg);
        end
        
        % We have a client, so lets send our msg.
        write(NETSHELL.tcpclient, uint8([ sendmsg '\n']));
    end


end

function watch_emacs(~, ~)
% Timer Callback Function:
% Watch for bytes available from the Emacs network connection, and act on any events.
    
    NETSHELL = getappdata(groot, 'EmacsNetShell');

    ba = NETSHELL.tcpclient.BytesAvailable;
    
    if ba > 0
        msg = char(read(NETSHELL.tcpclient));
        
        OUT = evalc(msg);
        
        write(NETSHELL.tcpclient, uint8(OUT));
    else
        % Nothing recieved
        % disp('No Luv from Emacs');
    end
    
end

function drop_emacs(~, ~)
% If the timer throws an error, then shutdown.
    
    emacsnetshell('shutdown');
    
    disp('Error in timer, dropping connection to Emacs.');
    
end