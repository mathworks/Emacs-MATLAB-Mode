%% Tests for char vector and string handling.
%

%% Basic strings

charvec = 'character vector';
stringscalar = "string scalar";

% Comment with 'character vector' in it.
% Comment with "string scalar" in it.

charvi = 'char vector incomplete
stringi = "string scalar incomplete

% Comment with 'char vector incomplete
% Comment with "string scalar incomplete

%% Strings in Strings

charvs = 'char vector with "string" in it';
stringcv = "string scalar with 'char vec' in it";

chard = 'char vector with '' in it';
stringd = "string scalar with "" in it";

chardi = 'incomplete char vector with '' in it
stringdi = "incomplete string scalar with "" in it

%% Strings with Comments

charvc = 'char vector with % comment char';
stringc = "string scalar with % comment char";

charvci = 'incomplete char vector with % comment char
stringci = "incomplete string scalar with % comment char

charvbc = 'char vector with %{ comment char %} ';
stringbc = "string scalar with %{ comment char %} ";

charvel = 'char vector with elipsis ... ';
stringel = "string scalar with elipsis ... ";

%% Mixed String Char on the same line

charv2str = { 'char vec' "string" };
str2charv = { "string" 'char vec' };

cv2s_quoted = { 'char vec and ''quote' "string and "" quote" };
s2cv_quoted = { "string and "" quote" 'char vec and '' quote' };

cv2s_nested = { 'char vec and " quote' "string and ' quote" };
s2cv_nested = { "string and ' quote" 'char vec and " quote' };

cv2s_transp = { 'char vec and t" quote' "string and t' quote" };
s2cv_transp = { "string and t' quote" 'char vec and t" quote' };

cell_in_strs = { "strinc { innercel }" 'charv {innercel}' };

cell_in_strs1_nested = { "strinc { innercel ' }" 'charv { innercell " }' };
cell_in_strs2_nested = { 'charv { innercell " }' "strinc { innercel ' }" };

icell_in_strs1_nested = { "strinc  innercel ' }" 'charv  innercell " }' }; % TODO - bad cell match
icell_in_strs2_nested = { 'charv  innercell " }' "strinc  innercel ' }" };


%% String scalar special cases

SA = [ "array" "of" "scalar" "strings" ];
SAE = [ "vert"
        "array"
        "of"
        "strings" ];

%% Tests for transpose

A = [1 2 3]';
B = A';
C = A'';
D = { 'cell' 'transpose' }';
E = [ 1 2 3 ]'';
F = { 'cell' 'trnspose' }'';
G = [ "string" "array" "transpose" ]';
H = A.';
I = A.'';

% Comment with transpose' in it.
% Comment with something" in it.


%% Unreachable

if 0
   
    Ac = 'char vector';
    Bs = "string scalar";
    
end

%% Block Comments

%{

   Block Comment:
 
   'char vector'
   "string scalar"
 
 %}
  
 