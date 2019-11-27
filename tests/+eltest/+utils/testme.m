function testme(belocal)
% A Function that throws an error.
    
    if nargin == 1 && belocal
        localfcn();
    else
        error('A problem in a package function.');
    end
    
end

function localfcn()
   
    error('A problem in a package local fcn');
    
end