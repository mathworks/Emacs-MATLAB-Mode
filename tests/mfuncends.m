% Copyright (C) 2024  Eric Ludlam (and others)

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
