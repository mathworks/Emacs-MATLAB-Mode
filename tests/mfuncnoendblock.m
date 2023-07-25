% Copyright (C) 2023  Eric Ludlam (and others)

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
