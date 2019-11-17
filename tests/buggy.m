function buggy(input)
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
        
    end
    
end
