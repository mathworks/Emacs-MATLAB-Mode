function OUT = dbtester()
% An M file for exercising MATLAB's debugger APIs
    
    B = 1:.25:pi;
    
    OUT = localfunc_1(B);
    
    OUT = OUT + 1;

end


function OUT = localfunc_1(IN)
% A local function

    if isempty(IN)
        IN = 1:.5:pi;
    end
    
    OUT = sin(IN);
    
end