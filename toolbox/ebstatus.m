function ebstatus
% Send emacs some breakpoints

   bp = getappdata(groot, 'EmacsBreakpoints');
   bp.updateEmacs;

end
