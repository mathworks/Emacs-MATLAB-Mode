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

    
    if true, if true, if true %!!4
                localfcn(a,b) ... %!!16
    end; ... %!!4
    end; ... %!!4
    end ... %!!4
        
    code1(); %!!4   -  this after those continued ends
    
    
    ... % !!4  A continuation with a ctrl block after it
        for g=1:10   % !!8 b/c continuation
        localfcn(a,g)  % !!8 b/c not continued, and +4 from comment continuation
        end            % !!4 to match
    
    % !!4 to undo continuation.
    
    ...   % Continuation by itself just before an end.
end  %!!0


function val = localfcn(c,d)   %!!0
% !!0 Help Comment
    
    
    
    val = c+d; % !!4
    
end  %!!0