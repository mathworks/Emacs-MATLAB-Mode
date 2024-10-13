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
function continuations(a,b) %!!0
% !!0 Help Comment

    arguments (Repeating)
        a (1,1) ... % !!8
            double  % !!12
        b (1,1) double { mustBeSomething ...   %!!8
                         mustBeSomethingElse } %!!25
    end

    global var1, ...  % !!4
        var2          % !!8
    
    localfcn(a,b);  % !!4
    
    localfcn(var1, ...  % !!4
             var2);     % !!13
    
    localfcn(a, localfcn(b, localfcn(var1,... %!!4
                                     var2)... %!37
                        )...  %!!24
            ); %!!12
    
    
    code1(), ...
        code2(); %!!8
    
    % NOTE: Blank space below should cancel the indent effect of ellipsis.
    code1() ...
        
    var1 = 1 + ...  %!!4
           ...      %!!11
           2 + ...  %!!11
           3;       %!!11
    
    medvar = 1 + ... %!!4
             2;      %!!13
    
    long_var_name = 1 + ...  %!!4
        2; %!!8
    
    localfcn      (medvar, ... %!!4
                   long_var_name); %!!19
    
    localfcn( ... %!!4
        a,b);     %!!8

    
    if true, if true, if true  %#ok  %!!4
                localfcn(a,b); ... %!!16
    end; ... %!!4
    end; ... %!!4
    end ... %!!4
        
    odd_if_location(); %!!4   -  this after those continued ends
    
    
    ... % !!4  A continuation with a ctrl block after it
        for g=1:10    %#ok  % !!8 b/c continuation
        localfcn(a,g)  % !!8 b/c not continued, and +4 from comment continuation
    end            % !!4 to match
    
    % !!4 to undo continuation.
    
    % Indent past 1st arg for special functions
    set(myhandle, 'Prop1', value, ... %!!4
                  'Prop2', value, ... %!!18
                  'Prop3', value);    %!!18
    
    % Indent past = sign for assignments.
    A = 1 + ... % !!4
        2;      % !!8
    
    medvar = 1 + ... % !!4
             2;      % !!13
    
    alongvariablename = 1 +...  % !!4
        2;   % !!8
    
    
    fancyfunctionname(arg1, ...  %!!4
                      innerfcn(arg2, ... %!!22
                               arg3), ... %!!31
                      { cell1; %!!22
                        cell2; %!!24
                        [ 1 2 ;  %!!24
                          3 4 ] ; %!!26
                        cell 4 },... %!!24
                      arg5);  %!!22
    
    
    ...   % Continuation by itself just before an end.
end  %!!0

function [ a, ... !!0
           b, ... !!11
           c, ... !!11
           d ] ... !!11
           = continuations_in_return(opt)
% H1 Comment Line !!0
    
    code(); %!! 4
    
end %!! 0

function [ a, b ] = continuation_in_args(a,...
                                         b) %!!41
% H1 comment line !!0
    
end

function c=expression_cont(a,b)
% H1 line  !!0
    
    if (a > 0 && ... %!!4
        b < 0)       %!!8

        % comment one !!8
        c=1;         %!!8
    elseif (a < 0 && ... %!!4
            b > 0)   %!!12
        
        % comment two !!8
        c=2;         %!!8
    end              %!!4
    
    switch a             %!!4
      case 'ert'         %!!6
        b = 1;           %!!8
      case {'sim', ...   %!!6
            'normalsim'} %!!12
        b=2;             %!!8
    end                  %!!4
    
end

function a=odd_if_location(n) %!!0
    
    i=1; while i<10, if xfcn(i,n)==0, i=i+1;  %!!4
                     else, i=20; end; end  %!!21
    
    if i<20   %!!4
        a=1;  %!!8
    else      %!!4
        a=0;  %!!8
    end       %!!4
    
    foo();
end %!!0

function val = localfcn(c,d)   %!!0
% !!0 Help Comment
    
    try fclose( fid );  catch, end %!!4
    
    val = c+d; % !!4
    
    odd_end_location_from_dspfwiz_load(1);
    
end  %!!0

function [z,o,n]=odd_end_location_from_dspfwiz_load(opts)  %!!0
    if opts.zeros, z = 'On';        %!!4
    else,          z = 'Off'; end   %!!4
    
    if opts.ones, o = 'On';        %!!4
    else,         o = 'Off'; end   %!!4
    
    if opts.neg_ones, n = 'On';        %!!4
    else,             n = 'Off'; end   %!!4
end    %!!0

function a=foo
%{
  for !!2
  for !!2
%}
    if true  %#ok  %!!4
        if true  %!!8
            a = 1;  %!!12
        end end   %#ok  %!!8
end %!!0


