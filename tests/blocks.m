% >>1
classdef blocks < handle
% !!0

% >>11
    properties  %!!4
        normalprop = 1;  %!!8
    end % <<11

    %>>12
    properties(Access='public') %!!4

        % See if we can create properties using keywords
        %properties = 1;
        %methods = 1;
        %events = 1;
        arguments %!!8
        prop = 1;
        
    end %<<12
    
    % >> 13
    events (Access='private') %!!4
        
        %properties
        %events
        %methods
        arguments %!!8
        misc %!!8
        
    end % <<13
    
    %>>14
    methods %!!4
        
        %>>15
        function simple_method(obj) %!!8
        %>>151
            arguments %!!12
                obj %!!16
            end %<<151
            
            disp(obj.normalprop);
        end %<<15
        
        %>>16
        function obj = blocks(arguments,events,properties,methods,enumeration,normal)%!!8
        %>>161
            arguments %!!12
                arguments%!!16
                events%!!16
                properties%!!16
                methods%!!16
                enumeration%!!16
                normal%!!16
            end %<<161
            
            obj.prop = arguments;%!!12
            obj.prop = events;%!!12
            obj.prop = properties;%!!12
            obj.prop = methods;%!!12
            obj.prop = enumeration;%!!12
            obj.prop = normal;%!!12
        end %<<16
        
        %>>17
        function properties(~)%!!8
        end %<<17

        %>>18
        function methods(~)%!!8
        end %<<18

        %>>19
        function events(~)%!!8
        end %<<19

        %>>20
        function events=arguments(arguments)%!!8
            arguments, arguments(:,:) {mustBeNumeric}, end %!!12
            
            enumeration ... %!!12
                = arguments;
            
            if enumeration > 0  %!!12
                arguments = -enumeration;  %!!16
            end  %!!12
            
            events ...  %!!12
                = arguments + 1;
        end %<<20
        
        %>>21
        function enumeration(~)%!!8
        end %<<21

        %>>22
        function usestuff(obj)%!!8
        % Try using the methods of this object
            obj.properties();%!!12
            obj.methods();%!!12
            obj.events();%!!12
            obj.arguments();%!!12
            obj.enumeration();%!!12
            normal();%!!12
        end %<<22
        
        %>>23
        function [m,s] = blarg(~, arguments)%!!8
        % Starter comments.
            
        %>>231
            arguments%!!12
                ~%!!16
                arguments(:,:) {mustBeNumeric}%!!16
            end %<<231
            
            m = mean( ...%!!12
                arguments, 'all');  %!!16
            
            arguments = 1;%!!12
            events = arguments;%!!12
            methods = events;%!!12
            properties = methods;%!!12
            enumeration = properties;%!!12
            
            s = enumeration;%!!12
        end %<<23
        
        %>>24
        function s = simple(~,arguments)%!!8
        % Simple function
            
            events = arguments;%!!12
            methods = events;%!!12
            properties = methods;%!!12
            enumeration = properties;%!!12
            
            s = enumeration; %!!12
        end %<<24

        function methods=foo3(obj,properties) %!!8
            methods=obj.arguments(properties); %!!12
        end %!!8

        %!!8
    end %<<14
    
    %!!4
end % <<1

%!!0
