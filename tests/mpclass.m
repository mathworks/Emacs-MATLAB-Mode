classdef (abstract) mpclass < handle & matlab.mixin.SetGetExactNames
    
    properties
        X
        Y
    end
    
    properties (Access='private')
        A
        B
    end
    
    methods
        function obj = mpclass(x, y)
        % Parsetest constructor
            obj.X = x;
            obj.Y = y;
        end
        
        function do_thing(obj, a, b)
        % Do a thing for the parse test
            obj.A = a;
            obj.B = b;
            
            localfunc('hello');
        end
        
    end
    
end

function localfunc(T)
% Local functions are handy.
    disp(T);
end

%% >> SEMANTIC TEST EXPECTED OUTPUT

%{
(( "mpclass" type
             ( :type "class" 
               :members (
               ("mpclass" function (
                   :return ("obj")
                   :arguments ("x" "y")))
               ("do_thing" function (
                   :arguments ("obj" "a" "b"))))))
    ("localfunc" function (
        :arguments ("T"))))
%}