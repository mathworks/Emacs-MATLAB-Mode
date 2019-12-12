classdef Stack < handle
% Class STACK - Manage Emacs' stack state.

    properties
        % Stack last sent to Emacs
        EmacsStack = [];
        EmacsFrame = [];
        % Netshell object if we should direct that way instead.
        NetShellObject = [];        
    end

    methods
	function es = Stack(nso)
            if nargin == 1
                es.NetShellObject = nso;
            end
            es.resetEmacs();
        end
        
        function updateEmacs(es, newstack, newframe)
        % Update Emacs' view of the stack
            
            if ~isempty(newstack) && strcmp(newstack(1).name, 'ebstack')
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
            
                str = [ '(progn ' newline ...
                        stackFrames(newstack) ...
                        newline ...
                        '  (mlg-set-stack-frame ' num2str(newframe) ')' ...
                        ')'];

                if isempty(es.NetShellObject)
                    disp('<EMACSCAP>(eval)');
                    disp(str);
                    disp('</EMACSCAP>')
                else
                    es.NetShellObject.SendEval(str);
                end
            end
            
        end
        
        function resetEmacs(es)
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

function changed = stackEqual(stack1, stack2)
   changed = true;
   
   if length(stack1) ~= length(stack2)
       changed=false;
       return;
   end
   
   for i=1:length(stack1)
       if ~strcmp(stack1(i).name, stack2(i).name) || ...
               stack1(i).line ~= stack2(i).line
           changed = false;
           return
       end       
   end
   
end