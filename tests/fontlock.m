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

% TEST FILE FOR FONT LOCK SPECIAL WORDS
function fontlock()
%^ ^kw     ^fn    ^df
%^  ^ig

% $$$ ignored comment
%^    ^ig
    
    persistent var1 % !!4
%^     ^kw      ^vn    ^co
    
    global     var2 % !!4
%^     ^kw      ^vn    ^co
    
 end
%^ ^kw

function [ var1, var2 ] = local(input1)
%^ ^kw      ^vn             ^fn   ^vn ^df
    
end

function [a, b] = local2(input1,...
                         input2)
%^                        ^vn
end

% TODO - these are cross function variables, but we turn off mlint in
%        tests, so these aren't tested.
function [a, b, c] = localvars(input1, input2, input3)
%^ ^kw       ^vn       ^fn       ^vn     ^vn     ^vn ^df
   
    nested(input1);
    
    q = input2;
    r = input3;
    
    function nested(ni1)
    % Nest function comment
        
        b = ni1;
        a = q;
        c = r;
        
    end        
    
end

function keywordstuff()
   
    while true
%^    ^kw  ^ma
        
        for varname=1:10
%^       ^kw  ^vn    ^cn
            break
%^           ^kw
        end
        
        if varname ==  2
%^      ^kw   ^df  ^bi ^df
            disp(1)
        elseif varname==3
%^        ^kw     ^df  ^bi
            disp(2)
        else
%^        ^kw
            disp(3)
        end
%^       ^kw
        
        switch varname
%^       ^kw     ^cn
          case 1
%^        ^kw  ^cn
            disp(1)
            continue
%^           ^kw
          case 2
%^        ^kw  ^cn
            disp(2)
          otherwise
%^        ^kw
            disp('other');
%^          ^df    ^st
            return
%^           ^kw
        end
%^        ^kw
        
        try
%^        ^kw
            disp(1)
        catch
%^        ^kw
            disp(2)
        end
%^        ^kw
        
        
    end
end

function dographics(value)
    
    f = figure;
%^        ^bi
    ax = axes(f);
%^        ^bi
 
    set ( ax, 'property',value)
%^    ^bi  ^vn    ^st
    
    s = open_system('foo.mdl');
%^         ^si        ^st
    
    set_param(s, 'param', value);
%^      ^si   ^vn  ^st      ^df
end

function dodebug()

    dbstop in dodebug
%^      ^bo     ^cd
    
    dbclear
%^      ^bo
    
end
 
function mathstuff()
    
   myvar = eps +   pi  +   nan +   ans +   i   +   NaT + true  ;
%^     ^df ^ma ^bi ^ma ^bi ^ma ^bi ^ma ^bi ^ma ^bi ^ma ^bi ^ma ^df
end

function helptest()
% HELPTEXT has fancy fonts in it.
%^  ^cn
        
end

function name_no_args
%^  ^kw    ^fn
end

function retarg = ret_and_name_no_args % comment
%^  ^kw  ^vn    ^df ^fn                  ^co
end

function retarg = args_have_cont (arg_1, ...
                                  arg_2)
%^                                ^vn
end

function [ retarg1, ...
           retarg2, ...
           retarg3 ] ...
           = name_of_fcn (arg1)
%^         ^df  ^fn       ^vn ^df
    
end

classdef (Abstract) myclass < handle
%^ ^kw      ^ty       ^fn   ^bi ^cn
end

classdef (Abstract)myclass<handle
%^ ^kw      ^ty       ^fn ^bi ^ty
end

%{
%  Local Variables:
%  matlab-show-mlint-warnings: nil
%  End:
%}

