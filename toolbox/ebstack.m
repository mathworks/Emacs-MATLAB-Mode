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
function ebstack(FRAMEIDX)
% Emacs version of dbstack.  Updates Emacs for where we are in the stack.
    
    [ST, I] = dbstack('-completenames');
    
   % Send emacs our updated stack
   es = getappdata(groot, 'EmacsStack');

   if nargin == 1
       I = FRAMEIDX;
   end
       
   es.updateEmacs(ST, I);

end
