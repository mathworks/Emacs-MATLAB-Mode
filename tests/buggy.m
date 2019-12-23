function buggy(input,n)
% The purpose of this function is to have bugs for creating errors
% tha matlab-shell needs to detect.
    
    switch input
      case 'err'
        error('You encounered an error in buggy.m');
        
        
      case 'cmderr'
        % there is no blarg, so this should error.
        ls blarg
    
      case 'warn'
        warning('You enountered a warning in buggy.m');

      case 'stack'
        
        if nargin == 1
            buggy('stack',3);
        elseif n == 1
            buggy('err');
        else
            buggy('stack',n-1);
        end

      case 'indented'
        % Test errors that are captured, but then reported up
        % but indented.
        disp('	In buggy.m (TestPoint_foo) at 36')
        
    end
    
end

function TestPoint_foo
    
    message('A test point');
    
end