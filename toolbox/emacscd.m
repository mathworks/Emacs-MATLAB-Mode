function emacscd(dir)
% CD to DIRECTORY in a way that Emacs Shell won't see via dirtrack.
% Instead, show example of how to tell Emacs what the new directory is.
    
    if nargin == 1
        cd(dir);
    end

    emacs = getenv('INSIDE_EMACS');
    
    if isempty(emacs)
        disp('Not inside Emacs')
    else
        % disp('Inside Emacs - Sending directory cookie.')
        disp('<EMACSCAP>(eval)')
        disp(['(setq default-directory "' pwd '")'])
        disp('</EMACSCAP>')
    end
end
