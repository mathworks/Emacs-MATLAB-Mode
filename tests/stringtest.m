%% Tests for char vector and string handling.
%
%  #c#
%
% %%%script script

%% Basic strings

'char vector #v#'
"string #s#"

charvec = 'character vector #v#';
stringscalar = "string scalar #s#";

% Comment with 'character vector #c#' in it.
% Comment with "string scalar #c#" in it.

charvi = 'char vector incomplete #V#
stringi = "string scalar incomplete #S#

% Comment with 'char vector incomplete #c#
% Comment with "string scalar incomplete #c#

%% Strings in Strings

charvs = 'char vector with "string #v#" in it';
stringcv = "string scalar with 'char vec #s#' in it";

chard = 'char vector with '' in it #v#';
stringd = "string scalar with "" in it #s#";

chardi = 'incomplete char vector with '' in it #V#
stringdi = "incomplete string scalar with "" in it #S#

%% Strings with Comments

charvc = 'char vector with % comment char #v#';
stringc = "string scalar with % comment char #s#";

charvci = 'incomplete char vector with % comment char #V#
stringci = "incomplete string scalar with % comment char #S#

charvbc = 'char vector with %{ comment char #v# %} ';
stringbc = "string scalar with %{ comment char #s# %} ";

charvel = 'char vector with elipsis ... #v# ';
stringel = "string scalar with elipsis ...  #s#";

%% Mixed String Char on the same line

charv2str = { 'char vec #v#' "string #s#" };
str2charv = { "string #s#" 'char vec #v#' };

cv2s_quoted = { 'char vec and ''quote #v#' "string and "" quote #s#" };
s2cv_quoted = { "string and "" quote #s#" 'char vec and '' quote #v#' };

cv2s_nested = { 'char vec and " quote #v#' "string and ' quote #s#" };
s2cv_nested = { "string and ' quote #s#" 'char vec and " quote #v#' };

cv2s_transp = { 'char vec and t" quote #v#' "string and t' quote #s#" };
s2cv_transp = { "string and t' quote #s#" 'char vec and t" quote #v#' };

cell_in_strs = { "strinc { innercel #s# }" 'charv {innercel #v#}' };

cell_in_strs1_nested = { "strinc { innercel ' #s# }" 'charv { innercell " #v# }' };
cell_in_strs2_nested = { 'charv { innercell " #v# }' "strinc { innercel ' #s# }" };

icell_in_strs1_nested = { "strinc  innercel ' #s# }" 'charv  innercell " #v# }' };
icell_in_strs2_nested = { 'charv  innercell " #v# }' "strinc  innercel ' #s# }" };

%% Elipsis as comment

fun_call(); ...  This is a comment after an elipsis #e#

fun_call(); ...  'charvec in elipsis comment #e#'

fun_call(); ...  "string in elipsis comment #e#"

fun_call(); ...  % comment after an elipsis is still elipsis #e#
    
%% Elipsis and strings and other comments

Ecv = 'string with ... in #v# it';
Es = "string with ... in #s# it";
% Comment with ... in it #c#
eecv = '...'; % string with only ellipsis in it #c#
ees = "..."; % string with only ellipsis in it #c#

x = [ 'foo bar', newline, ... #e#
      '  ''-goo'', ... #v#', newline, ... #e#
      '  ''-bar'', ... #v#', newline ];

func_call1('function with charvec', ... #e#
           'after ellipsis charvec with ellipsis ... #v#');
func_call2('test indendation here. Should not indent');

%% Indentation protection & confusing cell/string mixing

icC = 'charv with { in it #v#';
icS = "strings with { in it #s#";

imC = 'charv with [ in it #v#';
imS = "strings with [ in it #s#";

cmC = { 'a b } #v#', 1};
cmS = { "a b } #s#", 2};

%% Concatenation

CA = [ 'char' 'vector' 'concat #v#' ];
CAE = [ 'char'
        'vect #v#'
        'conc' ];

SA = [ "array" "of" "scalar" "strings" "#s#" ];
SAE = [ "vert"
        "array #s#"
        "of"
        "strings" ];

%% Tests for transpose

A = [1 2 3]';
B = A';
C = A'';
D = { 'cell' 'transpose' '#v#' }';
E = [ 1 2 3 ]'';
F = { 'cell' 'trnspose' }'';
G = [ "string" "array" "transpose" "#s#" ]';
H = A.';
I = A.'';
J = A(B')';
K = 12';
L = "string transpose #s#"';

% Comment with transpose' in it. #c#
% Comment with something" in it. #c#


%% Unreachable

if 0
    % Note: unreachable code doesn't break char vectors or strings.
    Ac = 'char vector #v#';
    Bs = "string scalar #s#";
else
    
    Cs = "not unreachable #s#";
end

%% Block Comments #C#

%{
  
  Block Comment: #b#
  
  'char vector #b#'
  "string scalar #b#"
  
%}

not_commented();

%{ just a regular comment #c# %} should_be_comment #c#

% Normal comment #c#

%% Ignored Comments #C#

% $$$ This comment is ignored by indentation engine.  #i#

%^ This comment is igored too, used in tests for font lock.  #i#


%% Command line dual #C#
% Note: stuff after a symbol<space> treated as string

disp  _this is string input to function #d#_
disp  _this is also string input to a function #d#_

regularcode;  #r#

% Note: Case sensitivity of cmd dual functions
DISP _regular code even though ML would treat as cmd dual #r#_

%{
%  Local Variables:
%  matlab-syntax-support-command-dual: t
%  matlab-show-mlint-warnings: nil
%  End:
%}


%% END
