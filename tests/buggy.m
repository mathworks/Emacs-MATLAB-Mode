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
function buggy(input,n)
% The purpose of this function is to have bugs for creating errors
% tha matlab-shell needs to detect.
    
    switch input
      case 'err'
        error('You encounered an error in buggy.m');
        
        
      case 'cmderr'
        % there is no blarg, so this should error.
        ls blarg
    
      case 'warn'
        warning('You enountered a warning in buggy.m');

      case 'stack'
        
        if nargin == 1
            buggy('stack',3);
        elseif n == 1
            buggy('err');
        else
            buggy('stack',n-1);
        end

      case 'indented'
        % Test errors that are captured, but then reported up
        % but indented.
        disp('	In buggy.m (TestPoint_foo) at 36')
        
    end
    
end

function TestPoint_foo
    
    message('A test point');
    
end
