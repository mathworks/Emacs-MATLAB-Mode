function indents()
% Help text
% !!0
% of many lines
% !!0
    
% including a gap
% !!0

    ends_in_comments_and_strings(); % has end in name
    
    % !!4

    block_starts_in_comments_and_strings();
    
    % !!4
    
    continuations_and_block_comments();
    
% $$$ !!0
% $$$ special ignore comments

    has_nested_fcn();
    
    % !!4  - after ignore comments
    
end % Comment with end in it

%!!0

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
    else %!!4
        
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
        
    end; B = [ 1 2 ...  % is this the end?
               3 4 ];   % !!15

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

    switch foo()  %!!4
      case 1  %!!6
        
        %!!8
      otherwise %!!6
        
        %!!8
    end %!!4
    
    
    try
        % !!8
    catch %!!4    
    
        % !!8
    end
    
end

function B = continuations_and_block_comments
% !!0
% !!0
% !!0
    
    %{  !!4
      !!6
      !!6
    %}  !!4
    
    foo(arg1, ... %!!4
        arg2);  %!!8
    
    foo_long_fcn(arg1, ... %!!4
                 arg2); %!!17
    
    A = [ 1 2  % !!4
          3 4 ]; % !!10
    
    foo(['this is a very long string' ... %!!4
         'with a continution to do something very exciting'])%!!9
    
    set(gcf,'Position',[ 1 2 3 4],... !!4
            'Color', 'red');  % !!12
    
    B = A + 1 + 4 ...
        + 6; % !!8
    
    foo_code();  % eol-comment !!4
                 % continuation-comment !!17
    
    % !!4 -blank between this & continuation comment
    
    
end

function has_nested_fcn

    plot(1:10); %!!4
    
    function am_nested_fcn %!!4
    % help
    % !!4
        code();
        %!!8
    end
    
    %!!4
    am_nested_fcn();
    
    
end
