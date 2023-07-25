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
function OUT = dbtester()
% An M file for exercising MATLAB's debugger APIs
    
    B = 1:.2:pi;
    
    OUT = localfunc_1(B);
    
    OUT = OUT + 1;

end


function OUT = localfunc_1(IN)
% A local function

    if isempty(IN)
        IN = 1:.5:pi;
    end
    
    et = eltest.EmacsTest;
    
    OUT_TMP = et.dbtest(IN);
    
    OUT = localfunc_2(OUT_TMP(1:2:end));
    
end

function A = localfunc_2(B)
    A = localfunc_3(B);
end

function A = localfunc_3(B)
    A = localfunc_4(B);
end

function A = localfunc_4(B)
    A = localfunc_5(B);
end

function A = localfunc_5(B)
    A = B;
end
