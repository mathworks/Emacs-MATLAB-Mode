% >>1
classdef (abstract) mclass < handle & matlab.mixin.SetGetExactNames % #7#
% !!0
    
% >>11
    properties (Access='public') % #2#
        
        % !!8
        AP = []; % #2#
        AB = 'charvec with space'; % #2#
        AC = "string with space and ( ";  % #2#
        AD = fun_call(1,2); % #3#
        AE (1,:) double {mustBePositive} = 1; % #5#
    end % <<11
    
    % >> 111
    properties (AbortSet=true, NonCopyable=true) % #2#

        % !!8
        AF (1,1) char {mustBeMember(AF, {'High','Medium','Low'})} = 'Low'; % #5#
        AG (1,1) matlab.lang.OnOffSwitchState = 'on'; % #6#
    end % <<111

    % >> 112
    events
        % !!8
        Event1
        Event2
    end % <<112
    
    % >>12
    methods
        % !!8
        
        % >>16
        function obj = mclass()
        % !!8
            
            obj.AB = obj.AP(1:end);
            % !!12
            
            disp('charvect with if and for words [ in it'); % #2#

            % !!12
            
            notify(obj,'Event1',...
                       'indent test');
            
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

end % <<1

% End