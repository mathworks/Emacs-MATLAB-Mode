% >>1
classdef (abstract) mclass < handle & matlab.mixin.SetGetExactNames % #7#
%^  ^kw     ^ty       ^fn     ^cn   ^ty  ^cn                          ^co
% !!0
% %%% class class
    
% >>11
    properties (Access='public') % #2#
% $$$^ ^kw       ^ty     ^st       ^co

        % !!8
        AP = []; % #2#
% $$$^  ^vn  ^df   ^co
        AB = 'charvec with space'; % #2#
        AC = "string with space and ( ";  % #2#
        AD = fun_call(1,2); % #3#
        AE (1,:) double {mustBePositive} = 1; % #5#
% $$$^  ^vn      ^ty      ^df                   ^co
    end % <<11
    
    % >> 111
    properties (AbortSet=true, NonCopyable=true) % #2#
% $$$^  ^kw       ^ty     ^cn   ^ty         ^cn    ^co

        % !!8
        AF (1,1) char {mustBeMember(AF, {'High','Medium','Low'})} = 'Low'; % #5#
% $$$^  ^vn   ^ty       ^df               ^st                   ^df  ^st     ^co
        AG (1,1) matlab.lang.OnOffSwitchState = 'on'; % #6#
    end % <<111

    % >> 112
    events
        % !!8
        Event1
        %^ ^vn
        Event2
    end % <<112
    
    % >>12
    methods
        % !!8
        
        % >>16
        function obj = mclass()
        %^ ^kw   ^vn    ^fn   ^df
        % !!8
            
            obj.AB = obj.AP(1:end);
            % !!12
            
            disp('charvect with if and for words [ in it'); % #2#

            % !!12
            
            notify(obj,'Event1',...
                       'indent test');
            
            notify(obj, 'Event1', 'indent test');
            %^ ^df   ^vn   ^st      ^st         ^df

            
            % >>17
            while obj.AB % #3#
                
                disp("while loop going on here ("); % #2#
                                                    % !!52
                
                % !!16
                
            end % <<17
            
            error('function mclass in charvec }'); % #2#
            
            % !!12
            
        end % <<16
    end % <<12
    
    methods (Access='public')
        % >>13
        function meth(obj) % #3#
            
        % >>14
            if obj.AP % #3#
                
                disp('display the word end here'); % #2#
                
            else
                
                % >>15
                try
                    
                    % comment with if, while, parfor words in it.
                    
                catch 
                    
                    % comment with end in it.

                end % <<15
            end % <<14

        end % <<13

    end % <<12
    
    methods (Abstract, Hidden=true) % #2#
        
        result = abs_func(a,b) % #3#

        result = other_abs_fun(a,b) % #3#
        
    end

    methods %!!4
        
        function end_separate_line(~) %!!8
        end  %!!8
        
        function end_same_line(~), end  %!!8
        
        function after_end_same_line(~), end  %!!8
        
    end %!!4
    
    methods %!!4
        function properties(~)   %!!8
        end  %!!8

        function methods(~)  %!!8
        end   %!!8

        function events(~)  %!!8
        end  %!!8

        function arguments(~)  %!!8
        end  %!!8

        function enumeration(~)  %!!8
        end  %!!8
        
        function usestuff(obj)  %!!8
        % Try using the methods of this object
            obj.properties();  %!!12
            obj.methods();  %!!12
            obj.events();  %!!12
            obj.arguments();  %!!12
            obj.enumeration();  %!!12
        end  %!!8
    
    end %!!4
    
end % <<1

% End