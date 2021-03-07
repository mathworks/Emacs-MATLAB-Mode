function mfuncends()
% Test function that has ends for each function.
% 
% Used with test harness to validate indentation and end detection.
%
% %%%function function
 
    fcn_call(); %!!4
    
    if condition %!!4
        fcn_call(); %!!8
    end  %!!4
    
end%!!0   - also no space after end
    
function fcn_call(inp) %!!0
    
    while inp > 0 %!!4
        fcn_call(inp-1); %!!8
    end %!!4
    
end  %!!0
