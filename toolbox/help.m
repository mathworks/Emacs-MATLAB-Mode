function [out, docTopic] = help(varargin)
% Provide help, augmented so Emacs picks it up to display in a special buffer.
% See the help for the built-in help command by asking for "help help" in
% MATLAB, which will redirect to the correct location.

    origPath = path;
    cleanup = onCleanup(@()path(origPath));
    me = mfilename('fullpath');
    myDir = fileparts(me);
    rmpath(myDir);

    builtinHelp = which('help');
    clear cleanup;

    helpPath = fileparts(builtinHelp);

    oldCWD = pwd;
    cd(helpPath);
    cleanup = onCleanup(@()cd(oldCWD));

    args = varargin;

    cookie = true;
    if nargin > 0 && strcmp(args{1}, '-emacs')
        cookie=false;
        args = args(2:end);
    end

    switch nargout
      case 0
        if cookie
            disp('<EMACSCAP>(*MATLAB Help*)');
        end
        help(args{:});
        if cookie
            disp('</EMACSCAP>');
        end
      case 1
        [out] = help(args{:});
      case 2
        [out, docTopic] = help(args{:});
    end
end