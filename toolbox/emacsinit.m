function emacsinit(clientcommand)
% EMACSINIT Initialize the current MATLAB session for matlab-shell-mode

    if usejava('jvm')

        v = ver('MATLAB');
        if str2double(v.Version) < 8.5
            % In 8.5 (R2015b) the MATLAB removed the ability to display hot link's when
            % debugging in -nodesktop mode. In R2015a (8.4) MATLAB would display in -nodesktop
            % mode something like:
            %   >> dbstop in testit
            %   >> testit
            %   >> <a href="matlab: opentoline('/path/to/testit.m',3,1)">3   </a>    a = 10 * 10;
            %   K>>
            % and emacs then would use the hot link to drive debugging from within emacs.
            % Given that R2015b and later does not have hot links, we use the graphical debugging
            % by leaving EditorGraphicalDebugging set to true.

            % Use the desktop hotlinking system in MATLAB Shell.  matlab-shell
            % will interpret them, and provide clickable areas.
            % NOTE: This doesn't work in all cases where HotLinks are used.
            feature('HotLinks','on');

            % Disable built-in editor showing up for debugging
            com.mathworks.services.Prefs.setBooleanPref('EditorGraphicalDebugging', false);
        end

        % Disable the built-in editor for normal editing
        com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', false);

        % Use emacsclient no-wait to send edit requests to a running emacs.
        if nargin == 0
            clientcommand = 'emacsclient -n';
        end
        com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', clientcommand);
    end
end
