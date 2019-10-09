function indents()
% !!0

    ends_in_comments_and_strings(); % has end in name
    
    % !!4

    block_starts_in_comments_and_strings();
    
end % Comment with end in it

function B = ends_in_comments_and_strings()
% !!0
    
% >>6
    if foo
        A = 1;
    end % <<6 end in comment after end
    
    % !!4

    symbol_with_end_in_it;
    
    B = A(1:end); %#ok
    
    if foo
        C = "this is the end of the line";
        % !!8
        
    end;  D = "string end string";
    % !!4
    
    E = [ D C];

    if bar
        
        A = E;
        
    end; B = A(1:end);
    % !!4

    E = B;

    if baz
        
        A = C;
        
    end; B = [ 1 2 ... is this the end?
               3 4 ];
    % !!4

    if foo
        
        A = E;
        
    end ... the other end
    % !!4

    B = [ B A ];

    % Multi-ends
    % >>8
    if foo %#ok
        if bar %#ok
            if baz
                
                A = B;
                
            else
                
            end; end; end % <<8 comment end thing

    % !!4
    B = A;

end

function C = block_starts_in_comments_and_strings()
% !!0
    
    C = 0;
    
    if true % if true
        
    end % if true
    
    
    % see previous function
    % !!4
    for x=1:length(C) % !!4
    
        % !!8
    end

end
