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
function testeeval(style)
% Test that we can tell Emacs what to do.  

    if nargin == 0
        style = 'eval';
    end
    
    switch style
      case 'eval'
        disp('<EMACSCAP>(eval)')
        disp('(setq mstest-EVAL-TEST "evaluate this")')
        disp('</EMACSCAP>')
        
      case 'buffer'
        disp('<EMACSCAP>(*MATLAB TEST*)')
        pause(2)
        disp('Random text to display in a buffer')
        disp('Random text to display in a buffer')
        disp('Random text to display in a buffer')
        disp('Random text to display in a buffer')
        disp('</EMACSCAP>')
      case 'noend'
        disp('<EMACSCAP>(*MATLAB TEST*)')
        disp('Random text to display but can''t');
    end

end

