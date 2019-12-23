function testeeval(style)
% Test that we can tell Emacs what to do.  

    if nargin == 0
        style = 'eval';
    end
    
    switch style
      case 'eval'
        disp('<EMACSCAP>(eval)')
        disp('(setq mstest-EVAL-TEST "evaluate this")')
        disp('</EMACSCAP>')
        
      case 'buffer'
        disp('<EMACSCAP>(*MATLAB TEST*)')
        pause(2)
        disp('Random text to display in a buffer')
        disp('Random text to display in a buffer')
        disp('Random text to display in a buffer')
        disp('Random text to display in a buffer')
        disp('</EMACSCAP>')
      case 'noend'
        disp('<EMACSCAP>(*MATLAB TEST*)')
        disp('Random text to display but can''t');
    end

end

