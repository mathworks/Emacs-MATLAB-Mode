% >>1
classdef mclass % #2#
    
% >>11
    properties
        AP = []; % #2#
        AB = []; % #2#
    end % <<11
    
    % >>12
    methods
        
        % >>16
        function obj = mclass()
           
            obj.AB = obj.AP(1:end);

            disp('charvect with if and for words [ in it'); % #2#
            
            % >>17
            while obj.AB % #3#
                
                disp("while loop going on here ("); % #2#
                
            end % <<17
            
            error('function mclass in charvec }'); % #2#
            
        end % <<16
        
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
    
end % <<1

% End