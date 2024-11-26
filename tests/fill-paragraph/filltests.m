function C = filltests()
% Test filling of comments to make sure they work in various contexts.  Including help text fill
% with both short
% and very long lines that need to be filled.

    A=1;

    % Regular comment with short
    % and some very very long lines that need to be filled and mixed together over the whole entire extra long
    % comment

    B=2;    % end of line comment with follow on long comment that should be filled across lines but also then a
            % short comment

    C=A+B;
end
