
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
%^    ^kw  ^cn
        
        for varname=1:10
%^       ^kw  ^vn    ^cn
            break
%^           ^kw
        end
        
        if varname ==  2
%^      ^kw   ^df  ^ty ^df
            disp(1)
        elseif varname==3
%^        ^kw     ^df  ^ty
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
%^        ^ty
    ax = axes(f);
%^        ^ty
 
    set ( ax, 'property',value)
%^    ^ty  ^vn    ^st
    
end

function dodebug()

    dbstop in dodebug
%^      ^bo     ^cd
    
    dbclear
%^      ^bo
    
end
 
function mathstuff()
    
   myvar = eps +   pi  +   nan +   ans +   i   +   NaT + true  ;
%^     ^df ^cn ^ty ^cn ^ty ^cn ^ty ^cn ^ty ^cn ^ty ^cn ^ty ^cn ^df
end

function helptest()
% HELPTEST has fancy fonts in it.
%^  ^cn
        
end

classdef (Abstract) myclass < handle
%^ ^kw      ^ty       ^fn   ^ty ^cn
end

classdef (Abstract)myclass<handle
%^ ^kw      ^ty       ^fn ^ty ^ty
end

%{
%  Local Variables:
%  matlab-show-mlint-warnings: nil
%  End:
%}

