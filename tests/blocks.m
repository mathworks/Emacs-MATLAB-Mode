classdef blocks < handle
% !!0

    properties
        normalprop = 1;
    end

    properties(Access='public')
        % See if we can create properties using keywords
        %properties = 1;
        %methods = 1;
        %events = 1;
        arguments
        prop = 1;
    end
    
    events (Access='private')
        %properties
        %events
        %methods
        arguments
        misc
    end
    
    methods
        
        function simple_method(obj)
            arguments
                obj
            end
            
            disp(obj.normalprop);
        end
        
        function obj = blocks(arguments,events,properties,methods,enumeration,normal)
            
            arguments
                arguments
                events
                properties
                methods
                enumeration
                normal
            end
            
            obj.prop = arguments;
            obj.prop = events;
            obj.prop = properties;
            obj.prop = methods;
            obj.prop = enumeration;
            obj.prop = normal;
        end
        
        function properties(~)
        end

        function methods(~)
        end

        function events(~)
        end

        function arguments(~)
        end

        function enumeration(~)
        end
    
        function usestuff(obj)
        % Try using the methods of this object
            obj.properties();
            obj.methods();
            obj.events();
            obj.arguments();
            obj.enumeration();
        end
    
    end
    
end