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
classdef Stack < handle
% Class STACK - Manage Emacs' stack state.

    properties
        % Stack last sent to Emacs
        EmacsStack = [];
        EmacsFrame = 1;
        % Netshell object if we should direct that way instead.
        NetShellObject = [];
        % Flag - do we need to tell Emacs what changed?
        StackPending = false;
        FramePending = false;
    end

    methods
	function es = Stack(nso)
            if nargin == 1
                es.NetShellObject = nso;
            end
            es.resetEmacs();
        end
        
        function captureStack(es, newstack, newframe)
            if ~isempty(newstack) && ...
                    ( strcmp(newstack(1).name, 'ebstack') ||...
                      strcmp(newstack(1).name, 'dbhotlink') )
                newstack = newstack(2:end);
            end

            idx = 1;
            cutbelow = 0;
            while idx < length(newstack)
                if contains(newstack(idx).name, 'timercb')
                    % As soon as we see Emacs Server, then we went too far.
                    cutbelow = idx;
                end
                idx = idx+1;
            end

            if cutbelow
                newstack = newstack(cutbelow+1:end);
            end
            
            if ~stackEqual(es.EmacsStack, newstack)
                es.EmacsStack = newstack;
                es.StackPending = true;
            end
            
            if newframe ~= es.EmacsFrame
                es.EmacsFrame = newframe;
                es.FramePending = true;
            end
        end
        
        function updateEmacs(es, newstack, newframe)
        % Update Emacs' view of the stack

            es.captureStack(newstack, newframe);

            str = [ '(progn ;;Stack' newline ];
                
            if es.StackPending
                str = [ str ...
                        stackFrames(es.EmacsStack) ...
                        newline ];
            end
            str = [ str ...
                    '  (mlg-set-stack-frame ' num2str(es.EmacsFrame) ')' ];
                
            str = [ str ')'];
                
            es.StackPending = false;
            es.FramePending = false;
                
            if isempty(es.NetShellObject)
                disp('<EMACSCAP>(eval)');
                disp(str);
                disp('</EMACSCAP>')
            else
                es.NetShellObject.SendEval(str);
            end
            
        end
        
        function resetEmacs(es)
        end
   
        function updateForHotLinks(es, newstack, newframe)
 
            es.captureStack(newstack, newframe);
            
            % updateEmacs(es, newstack, newframe);
            
            if es.StackPending || es.FramePending
                str = [ '(progn ;;Stack' newline ];
                
                if es.StackPending
                    str = [ str ...
                            stackFrames(es.EmacsStack) ...
                            newline ];
                end
                str = [ str ...
                        '  (mlg-set-stack-frame-via-gud ' num2str(es.EmacsFrame) ')' ];
                
                str = [ str ')'];
                
            else
                str = '';
            end
                
            es.StackPending = false;
            es.FramePending = false;

            if ~isempty(str)
                if isempty(es.NetShellObject)
                    disp(str);
                else
                    es.NetShellObject.SendEval(str);
                end
            end
        end
        
    end
end

function str=stackFrames(ST)
% Return Emacs Lisp form representing the current stack.
    
    str = '  (mlg-set-stack (quote (';
    for i=1:length(ST)
        str = [str  newline '    ("' fixFile(ST(i).file) '" "' ...
               ST(i).name '" ' num2str(ST(i).line) ')' ]; %#ok
    end
    str = [ str ')))' ];

end

function nf = fixFile(filename)
% Fix FILENAME so it has no escape chars, that way we can send to Emacs.
   
    nf = regexprep(filename,"\", "/");
    
end

function thesame = stackEqual(stack1, stack2)
   thesame = true;
   
   if length(stack1) ~= length(stack2)
       thesame=false;
       return;
   end
   
   for i=1:length(stack1)
       if ~strcmp(stack1(i).name, stack2(i).name) || ...
               stack1(i).line ~= stack2(i).line
           thesame = false;
           return
       end       
   end
   
end
