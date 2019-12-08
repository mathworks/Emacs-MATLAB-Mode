classdef Breakpoints < handle
% Class BREAKPOINTS - manages breakpoints that are shared with Emacs.

    properties
        % Breakpoints we have sent to Emacs already.
        EmacsBreakpoints = struct([]);
    end

    methods
	function bp = Breakpoints()
            bp.resetEmacs();
        end
        
        function updateEmacs(bp)
        % update Emacs with just the breakpoint deltas.
        % if there is a delta, display prefix text before sending
        % our update.
            currpts = unwindBreakpoints(builtin('dbstatus'));
            
            oldpts = bp.EmacsBreakpoints;
            
            addpts = [];
            delpts = [];
            
            if isempty(oldpts)
                addpts = currpts;
            else
                % Did we add any new breakpoints?
                for i=1:length(currpts)
                    if ~isempty(currpts(i).name)
                        if ~ptInList(currpts(i).name, currpts(i).line, oldpts)
                            if isempty(addpts)
                                addpts = currpts(i);
                            else
                                addpts(end+1) = currpts(i); %#ok
                            end
                        end
                    else
                        % This is for dbstop if error and friends.
                    end
                end
                % Did we remove any old breakpoints;
                for i=1:length(oldpts)
                    if ~ptInList(oldpts(i).name, oldpts(i).line, currpts)
                        if isempty(delpts)
                            delpts = oldpts(i);
                        else
                            delpts(end+1) = oldpts(i); %#ok
                        end
                    end
                end
            end

            if ~isempty(delpts) || ~isempty(addpts)
                disp(['<EMACSCAP>(eval)' newline  '(progn']);
                % Send the sequence of Emacs commands to update breakpoints
                sendPtList('del', delpts);
                sendPtList('add', addpts);
            
                % Wrap up the request
                disp([')' newline '</EMACSCAP>'])

                bp.EmacsBreakpoints = currpts;
            end
        end
        
        function resetEmacs(bp)
            disp(['<EMACSCAP>(eval)' newline  '(progn']);
            disp('(mlg-reset-breakpoints)');

            currpts = unwindBreakpoints(builtin('dbstatus'));
            sendPtList('add', currpts);
            bp.EmacsBreakpoints = currpts;
            
            disp([')' newline '</EMACSCAP>'])
        end
    end
end

function sendPtList(ad, bpstructlist)
    for i=1:length(bpstructlist)
        disp(['(mlg-' ad '-breakpoint "' bpstructlist(i).file '" ' ...
              num2str(bpstructlist(i).line) ')']);    
    end
end

function newlist = unwindBreakpoints(bplist)
% Unwind the breakpoints in BPLIST so there is one struct per line per file.
    
    newlist = [];
    
    for f = 1:length(bplist)
        for l = 1:length(bplist(f).line)
            
            news.name = bplist(f).name;
            news.file = bplist(f).file;
            news.line = bplist(f).line(l);
            
            if isempty(newlist)
                newlist = news;
            else
                newlist(end+1) = news; %#ok
            end
        end
    end
    
end

function tf = ptInList(name, line, lst)
% Return true if a breakpoint at NAME/LINE exists in LST.
    
    tf = false;
    
    for i=1:length(lst)

        tf = strcmp(name, lst(i).name) && line==lst(i).line;
        
        if tf, return; end
    end
    
end

