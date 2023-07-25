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
classdef EmacsTest < handle
% Class EMACSTEST
%
% Class that produces errors and file names we can test.

    properties
        prop1;
    end

    methods
	function h = EmacsTest()
	% Constructor
	   
        end

        function throwerr(~)
            error('Error thrown from Emacs Test');
        end
        
        function throwprop(et)
            et.prop1 = 1;
            et.prop2 = 2; % should error
        end
    
        function OUT = dbtest(h, IN)
            
            h.prop1 = IN;
            
            OUT = sin(h.prop1);
            
        end
        
    end
end
