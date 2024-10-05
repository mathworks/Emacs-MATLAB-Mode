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
function dbhotlink()
% Display text that EMACS can interpret as a hotlink
% so the debugger can auto move to the right spot.
% Input L is the stack frame to specify.
% If L is not provided, then use the current stack frame.
    
   [ST, I] = dbstack('-completenames');
   
   disp('(progn ');
   es = getappdata(groot, 'EmacsStack');
   es.updateForHotLinks(ST, I);
   bp = getappdata(groot, 'EmacsBreakpoints');
   bp.updateForHotLinks();
   disp(')');

end
