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


