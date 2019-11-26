function OUT = dbtester()
% An M file for exercising MATLAB's debugger APIs
    
    B = 1:.25:pi;
    
    OUT = localfunc_1(B);
    
end


function OUT = localfunc_1(IN)
% A local function
    
    
    OUT = sin(IN);
    
end