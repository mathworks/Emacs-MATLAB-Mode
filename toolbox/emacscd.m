function emacscd(dir)
% CD to DIRECTORY in a way that Emacs Shell won't see via dirtrack.
% Instead, show example of how to tell Emacs what the new directory is.
    
    cd(dir);
    
    disp('<EMACSCAP>(eval)')
    disp(['(setq default-directory "' pwd '")'])
    disp('</EMACSCAP>')
end
