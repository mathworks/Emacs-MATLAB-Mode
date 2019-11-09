function [a, b, c] = fontlock(input1, ...
                              input2, ...
                              input3)
% FONTLOCK testing function for Emacs & MATLAB.
    
    nested(input1);
    
    q = input2;
    r = input3;
    
    function nested(ni1)
    % Nest function comment
        
        b = ni1;
        a = q;
        c = r;
        
    end        

    
end
