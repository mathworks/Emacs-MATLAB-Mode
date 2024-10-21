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
classdef (abstract) mpclass < handle & matlab.mixin.SetGetExactNames
    
    properties
        X
        Y
    end
    
    properties (Access='private')
        A
        B
    end
    
    methods
        function obj = mpclass(x, y)
        % Parsetest constructor
            obj.X = x;
            obj.Y = y;
        end
    end
    
    methods (Access='protected')
        function do_thing(obj, a, b)
        % Do a thing for the parse test
            obj.A = a;
            obj.B = b;
            
            localfunc('hello');
        end
    end
    
end

function localfunc(T)
% Local functions are handy.
    disp(T);
end

%% End
