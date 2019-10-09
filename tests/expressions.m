%% Tests for syntactic expressions in the Emacs sense
%

%% In comments - just words
%
%  word1 ( list two ) word #4#
%
%

A = 'one charvec'; % #2#
B = "one string"; % #2#

% >>1
if expr1
    a = 'charvec'; % #2#
    b = "string";
    % comment with a few words
    c = { 1 2 'cell array' };  % #2#
end % <<1

cmC = { 'a b } #v#', 1}; % #2#
cmS = { "a b } #s#", 2}; % #2#

cmCr = { 'a b { #v#', 1}; % #2#
cmSr = { "a b { #s#", 2}; % #2#

func_A('charvec with ) in it'); % #2#
func_A('charvec with ( in it'); % #2#
func_B("string with ) in it"); % #2#
func_V("string with ( in it"); % #2#

mA = [ 'concat charvec' 'with [ in it' ]; % #2#
mB = [ 'concat charvec' 'with ] in it' ]; % #2#
msA = [ "concat strings" "with [ in it" ]; % #2#
msB = [ "concat strings" "with ] in it" ]; % #2#

% >>2
if expr2
    ifcmC = { 'a b } #v#', 1}; % #2#
    ficmS = { "a b } #s#", 2}; % #2#
else
    ifcmCr = { 'a b { #v#', 1}; % #2#
    ifcmSr = { "a b { #s#", 2}; % #2#
end % <<2


BM = [ 1 2 3 ... comment 
       4 5 6 ... comment #4#
       7 8 9 ];

CC = { 1 2 { 3 4 } 5 6 { 7 { 8 { 9 }} 10 }}; % #2#
CM = { [ 1 2 ] { [ 3 4 ] 5 } [ 6 7 ] A(1:4) }; % #2#

AA = [];

% >>4
for flv=1:10 
    if i == 1
        AA = [ AA 1 2 3 ]; %#ok  - pragma test
    else
        AB = AA(2:end);
    end
end
% <<4

% >>5
switch AA
  case 1
    % >>51
    while false
        if false
        end
    end % <<51
    
  case 2
    % >>52
    for i=1:10
        AC = AA(end:-1:1);
        AD = pi + nan + true;
    end % <<52
    
  case 3
    % >>53
    try
        error('Moose');
    catch E
    end % <<53
    
  otherwise
end % <<5

% end

