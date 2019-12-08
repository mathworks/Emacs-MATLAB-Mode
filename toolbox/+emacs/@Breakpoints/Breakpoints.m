classdef Breakpoints < handle
% Class BREAKPOINTS - manages breakpoints that are shared with Emacs.

    properties
        % Breakpoints we have sent to Emacs already.
        EmacsBreakpoints = [];
        % Netshell object if we should direct that way instead.
        NetShellObject = [];
    end

    methods
	function bp = Breakpoints(nso)
            if nargin == 1
                bp.NetShellObject = nso;
            end

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
                % Send the sequence of Emacs commands to update breakpoints
                str = [ '(progn' newline ...
                        sendPtList('del', delpts) ...
                        sendPtList('add', addpts)  ...
                        ')' ];
                if isempty(bp.NetShellObject)
                    disp(['<EMACSCAP>(eval)' newline]);
                    disp(str)
                    disp([newline '</EMACSCAP>'])
                else
                    bp.NetShellObject.SendEval(str);
                end
                

                bp.EmacsBreakpoints = currpts;
            end
        end
        
        function resetEmacs(bp)

            currpts = unwindBreakpoints(builtin('dbstatus'));
            bp.EmacsBreakpoints = currpts;
            
            str = ['(progn (mlg-reset-breakpoints)' newline ...
                   sendPtList('add', currpts) ')'];       
            
            if isempty(bp.NetShellObject)
                disp(['<EMACSCAP>(eval)' newline ]);
                disp(str);
                disp([newline '</EMACSCAP>'])
            else
                bp.NetShellObject.SendEval(str);
            end
        end
    end
end

function str = sendPtList(ad, bpstructlist)
    str = '';
    for i=1:length(bpstructlist)
        str = [ str '(mlg-' ad '-breakpoint "' bpstructlist(i).file '" ' ...
                num2str(bpstructlist(i).line) ')' newline];     %#ok
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

