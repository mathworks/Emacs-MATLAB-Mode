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
        
        function resetEmacs(es)
            
        end
        
    end
end

function str=stackFrames(ST)
% Return Emacs Lisp form representing the current stack.
    
    str = '  (mlg-set-stack (quote (';
    for i=1:length(ST)
        str = [str  newline '    ("' ST(i).file '" "' ST(i).name '" ' num2str(ST(i).line) ')' ]; %#ok
    end
    str = [ str ')))' ];

end