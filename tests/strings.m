%% Tests for char vector and string handling.
%
%  #c#

%% Basic strings

'char vector #v#'
"string #s#"

charvec = 'character vector #v#';
stringscalar = "string scalar #s#";

% Comment with 'character vector #c#' in it.
% Comment with "string scalar #c#" in it.

charvi = 'char vector incomplete #v#
stringi = "string scalar incomplete #s#

% Comment with 'char vector incomplete #c#
% Comment with "string scalar incomplete #c#

%% Strings in Strings

charvs = 'char vector with "string #v#" in it';
stringcv = "string scalar with 'char vec #s#' in it";

chard = 'char vector with '' in it #v#';
stringd = "string scalar with "" in it #s#";

chardi = 'incomplete char vector with '' in it #v#
stringdi = "incomplete string scalar with "" in it #s#

%% Strings with Comments

charvc = 'char vector with % comment char #v#';
stringc = "string scalar with % comment char #s#";

charvci = 'incomplete char vector with % comment char #v#
stringci = "incomplete string scalar with % comment char #s#

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

%% Indentation protection

icC = 'charv with { in it #v#';
icS = "strings with { in it #s#";

imC = 'charv with [ in it #v#';
imS = "strings with [ in it #s#";


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
   
    Ac = 'char vector';
    Bs = "string scalar";
    
end

%% Block Comments #c#

%{

   Block Comment:
 
   'char vector'
   "string scalar"
 
 %}
  
 