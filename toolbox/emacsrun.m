function emacsrun(mfile)
% Run code from MFILE.
% Assumes MFILE was recently edited, and proactively clears that function.
%
% Command sent by Emacs for save-and-go functionality

    if ~exist(mfile,'file')
        error('You must save your file into a location accessible by MATLAB process.');
    end

    % Now figure out if shortFileName is on the path.
    [ fullFilePath, shortFileName ] = fileparts(mfile);
    onpath = ~isempty(which(shortFileName));

    % If not on the path, temporarilly switch to that directory so it and an files it references are
    % accessible
    if ~onpath
        oldpath = pwd;
        cd(fullFilePath);
        cleanup = onCleanup(@()cd(oldpath));
    end

    clear(shortFileName);
    evalin('base',shortFileName);
end