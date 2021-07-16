function mfuncnofuncindent( )  
% Test function that has ends for each function. !!0
%  !!0
% Used with test harness to validate indentation and end detection.  !!0
%   !!0
% %%%function function nil
    
fcn_call(); %!!0
    
if condition %!!0
    fcn_call(); %!!4
    fcn_call ... !!4
        (); %!!8
end  %!!0

end%!!0   - also no space after end

function a=fcn_call(inp) %!!0

while inp > 0 %!!0
    fcn_call(inp-1); %!!4
    a = [ 1 2 ... !!4
          3 4 ];  %!!10
end %!!0

end  %!!0

%{
%  Local Variables:
%  matlab-indent-function-body: nil
%  End:
%}