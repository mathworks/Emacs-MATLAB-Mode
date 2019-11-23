function emacsrunregion(file, startchar, endchar)
% Run code from FILE between STARTCHAR and ENDCHAR.
% Uses internal Editor API to enable support of local functions and breakpoints.

    if exist(file,'file')
        fullFileName = file;
    else
        error('You must save your region into a file accessible by MATLAB process.');
    end

    % Now figure out if shortFileName is on the path.  We can use a special eval
    % if it is.
    [ fullFilePath, shortFileName ] = fileparts(fullFileName);
    revlookup = which(shortFileName);
    if ~isempty(revlookup) && strcmp(revlookup, fullFileName)
        onpath = true;
    else
        onpath = false;
    end

    % Of not on the path, temporarilly switch to that directory.
    if ~onpath
        oldpath = pwd;
        cd(fullFilePath);
    end
    
    if true % TODO remove true eventually
        
        % When other options aren't a vailable, use eval on extracted text. 
        % disp('Evaluating with eval');
        
        TXT = fileread(fullFileName);
        ETXT = TXT(startchar:endchar);
        
        eval(ETXT);

    else
        % Newer versions of MATLAB with Live Editor can call into the evaluator.
        % disp('Evaluating with LXE');
        
        % TODO - get permission for using internal API for running regions of a file.
        % builtin('_xxxx', 'base', shortFileName, startchar, endchar-startchar)
    end
    
    
    if ~onpath % Return to previous directory
        cd(oldpath);
    end
end
