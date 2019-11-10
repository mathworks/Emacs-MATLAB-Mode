function dbhotlink(L)
% Display text that EMACS can interpret as a hotlink
% so the debugger can auto move to the right spot.
% Input L is the stack frame to specify.
% If L is not provided, then use the current stack frame.
    
   [ST, I] = dbstack('-completenames');
   
   if nargin == 0
       L = I;
       LINESTR = '';
   else
       LINESTR = num2str(L);
   end

   if L+1 <=numel(ST)
      fprintf('<a href="matlab: opentoline(''%s'',%i,1)">%i</a>\n', ST(L+1).file, ...
              ST(L+1).line, LINESTR);
   end
end
