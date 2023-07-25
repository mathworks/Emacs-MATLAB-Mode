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
function complete (a_arg1, b_arg2)
% This function is for testing completion tools
    
    arguments
        a_arg1 (1,1) double
        b_arg2 (2,1) int
    end
    
    global a_global
    global b_global

    a_localvar = 1;
    b_localvar = 1;

    for a_for=1:10
    end
    
    if bool_var
    end
    
    switch a_switch
    end
    
    a_
    % @@(solo var "a_localvar" "a_switch" "a_for" "a_global" "a_arg1")
    % @@(solo fcn )

    % Note: For b, there are other test files the completion
    %       engine will find, so they are included.
    b
    % @@(solo fcn "blocal_fcn" "blazy_fcn" "buggy" "blocks")
    % @@(solo var "b_localvar" "bool_var" "b_global" "b_arg2")
    
   
    % quiet mlint
    blocal_fcn(a_arg1, b_arg2, a_localvar, b_localvar, a_global, b_global);
end

function blocal_fcn(varargin)
    
    blazy_fcn(varargin{:});
    
end

function blazy_fcn(varargin)
end

