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
        
        function str = updateString(bp, show)
        % update Emacs with just the breakpoint deltas.
        % if there is a delta, display prefix text before sending
        % our update.
            
            if nargin < 2
                show = false;
            end
            
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

            if show
                showcmd = [ newline '(mlg-show-breakpoints)'];
            elseif ~isempty(delpts) || ~isempty(addpts)
                showcmd = [ newline '(mlg-refresh-breakpoint-buffer)'];
            else
                showcmd = '';
            end

            bp.EmacsBreakpoints = currpts;
            
            if ~isempty(delpts) || ~isempty(addpts) || ~isempty(showcmd)
                str = [ '(progn ;;breakpoint' newline ...
                        sendPtList('del', delpts) ...
                        sendPtList('add', addpts)  ...
                        showcmd ...
                        ')' ];
            else
                str = '';
            end
        end
        
        function updateEmacs(bp, show)
        % update Emacs with just the breakpoint deltas.
        % if there is a delta, display prefix text before sending
        % our update.

            if nargin < 2
                show = false;
            end
            
            str = bp.updateString(show);
            
            if ~isempty(str)
                % Send the sequence of Emacs commands to update breakpoints
                if isempty(bp.NetShellObject)
                    disp(['<EMACSCAP>(eval)' newline]);
                    disp(str)
                    disp([newline '</EMACSCAP>'])
                else
                    bp.NetShellObject.SendEval(str);
                end
            end

        end
        
        function updateForHotLinks(bp)
            
            str = bp.updateString(false);
            if ~isempty(str)
                disp(str)
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
        str = [ str '(mlg-' ad '-breakpoint "' fixFile(bpstructlist(i).file) '" "' ...
                bpstructlist(i).name '" ' ...
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

function nf = fixFile(filename)
% Fix FILENAME so it has no escape chars, that way we can send to Emacs.
   
    nf = regexprep(filename,"\", "/");
    
end
