function mfuncends( )  
% Test function that has ends for each function. !!0
%  !!0
% Used with test harness to validate indentation and end detection.  !!0
%   !!0
% %%%function function function
    
    fcn_call(); %!!4
    
    if condition %!!4
        fcn_call(); %!!8
        fcn_call ... !!8
            (); %!!12
    end  %!!4
    
end%!!0   - also no space after end

function a=fcn_call(inp) %!!0
    
    while inp > 0 %!!4
        fcn_call(inp-1); %!!8
        a = [ 1 2 ... !!8
              3 4 ];  %!!14
    end %!!4
    
end  %!!0
