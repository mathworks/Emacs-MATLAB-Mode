%{
  Add a block comment at the beginning to skip over.
  x % !!2
% !! 0
%}
function mfuncnoendblock 
% A function file that does not have ends at the end of functions.
% !!0
% %%% function nil nil

fcn_call(1) %!!0


function fcn_call(idx) %!!0

if idx > 0 %!!0
    fcn_call(ix-1) %!!4
    goo(3);
end  %!!0

function c=goo(p3) %!!0
if p3 < 3 %!!0
    if p3 < 0  %!!4
        c = p3 * -3;  %!!8
    else %!!4
        c = p3 * 3;  %!!8
        for i=1:10  %!!8
            c = c + i;  %!!12
        end  %!!8
    end  %!!4
else  %!!0
    c=p3*4; %!!4
end %!!0
