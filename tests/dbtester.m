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
    
    OUT = localfunc_2(OUT_TMP(1:2:end));
    
end

function A = localfunc_2(B)
    A = localfunc_3(B);
end

function A = localfunc_3(B)
    A = localfunc_4(B);
end

function A = localfunc_4(B)
    A = localfunc_5(B);
end

function A = localfunc_5(B)
    A = B;
end