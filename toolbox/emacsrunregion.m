function emacsrunregion(file, startchar, endchar)
% Run code from FILE between STARTCHAR and ENDCHAR.
% Uses internal Editor API to enable support of local functions and breakpoints.
    
    fullFileName = which(file); % get full expansion

    if ~exist(fullFileName,'file')
        error('You must save your region into an accessible file.');
    end
        
    [ fullFilePath, shortFileName ] = fileparts(fullFileName);  %#ok  % Get the file parts
    
    fullFileText = fileread(fullFileName); % Get the text

    % TODO - find out if this is available.  If it isn't, we can extract the text and
    % then use evalin or something.
    
    % Call into the evaluator.
    feature('DisablePrelineEvents', true);
    
    % TODO - Info I got says fullFilePath, not the shortFileName for
    % below.  Need to try in more versions of MATLAB.
    
    builtin('_LiveEvaluate', 'caller', shortFileName, startchar, endchar-startchar, [], [], [], [], ...
            true, fullFileText)
    feature('DisablePrelineEvents', false);

end
