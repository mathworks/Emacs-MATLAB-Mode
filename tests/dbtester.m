function OUT = dbtester()
% An M file for exercising MATLAB's debugger APIs
    
    B = 1:.2:pi;
    
    OUT = localfunc_1(B);
    
    OUT = OUT + 1;

end


function OUT = localfunc_1(IN)
% A local function

    if isempty(IN)
        IN = 1:.5:pi;
    end
    
    et = eltest.EmacsTest;
    
    OUT_TMP = et.dbtest(IN);
    
    OUT = OUT_TMP(1:2:end);
    
end