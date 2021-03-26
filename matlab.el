;;; matlab.el --- major mode for MATLAB(R) dot-m files

;; Author: Matt Wette <mwette@alumni.caltech.edu>,
;;         Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>
;; Created: 04 Jan 91
;; Keywords: MATLAB(R)
;; Version:

(defconst matlab-mode-version "5.0"
  "Current version of MATLAB(R) mode.")

;;
;; Copyright (C) 1997-2020 Eric M. Ludlam
;; Copyright (C) 1991-1997 Matthew R. Wette
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; This major mode for GNU Emacs provides support for editing MATLAB(R) dot-m
;; files.  It automatically indents for block structures (including nested
;; functions), line continuations (e.g., ...), and comments.
;;
;; Additional features include auto-fill including auto-additions of
;; ellipsis for commands, and even strings.  Block/end construct
;; highlighting as you edit.  Primitive code-verification and
;; identification.  Templates and other code editing functions.
;; Advanced symbol completion.  Code highlighting via font-lock.
;; There are many navigation commands that let you move across blocks
;; of code at different levels.
;;
;; Lastly, there is support for running MATLAB(R) in an Emacs buffer,
;; with full shell history and debugger support (when used with the db
;; commands.)  The shell can be used as an online help while editing
;; code, providing help on functions, variables, or running arbitrary
;; blocks of code from the buffer you are editing.

;;; Code:

(require 'matlab-compat)
(require 'matlab-syntax)
(require 'matlab-scan)
(require 'easymenu)
(require 'derived)


;;; User-changeable variables =================================================
;;

;; Variables which the user can change
(defgroup matlab nil
  "MATLAB(R) mode."
  :prefix "matlab-"
  :group 'languages)

(defcustom matlab-mode-for-new-mfiles 'maybe
  "*Enter `matlab-mode' for new *.m files.
The `matlab' package will automatically enter `matlab-mode' when
the first part of a *.m file is doesn't contain Objective-C
comments or '#' characters. If you want new (empty) files to
automatically enter `matlab-mode', specify this item as
t (always). If you specify 'maybe, new files will enter
`matlab-mode' when you have an existing MATLAB buffer. Specifying
nil (never) means that new *.m files will not enter
`matlab-mode', and with default Emacs settings they will enter
`objc-mode'"
  :group 'matlab
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Maybe" maybe)))

(defcustom matlab-indent-level 4
  "*The basic indentation amount in `matlab-mode'."
  :group 'matlab
  :type 'integer)

(defcustom matlab-continuation-indent-level 4
  "*Basic indentation after continuation if no other methods are found."
  :group 'matlab
  :type 'integer)

(defcustom matlab-array-continuation-indent-level 2
  "*Basic indentation after continuation within an array if no other methods are found."
  :group 'matlab
  :type 'integer)

(defcustom matlab-cont-requires-ellipsis t
  "*Specify if ellipses are required at the end of a line for continuation.
Future versions of Matlab may not require ellipses ... , so a heuristic
determining if there is to be continuation is used instead."
  :group 'matlab
  :type 'integer)

(defcustom matlab-case-indent-level '(2 . 2)
  "*How far to indent case/otherwise statements in a switch.
This can be an integer, which is the distance to indent the CASE and
OTHERWISE commands, and how far to indent commands appearing in CASE
and OTHERWISE blocks.  It can also be a cons cell which is of form
  (CASEINDENT . COMMANDINDENT)
where CASEINDENT is the indentation of the CASE and OTHERWISE
statements, and COMMANDINDENT is the indentation of commands appearing
after the CASE or OTHERWISE command.

Note: Currently a bug exists if:
  CASEINDENT+COMMANDINDENT != `matlab-indent-level'
so if you customize these variables, follow the above rule, and you
should be ok."
  :group 'matlab
  :type 'sexp)

(defcustom matlab-indent-past-arg1-functions
  "[sg]et\\(_param\\)?\\|waitfor\\|notify"
  "*Regex describing functions whose first arg is special.
This specialness means that all following parameters which appear on
continued lines should appear indented to line up with the second
argument, not the first argument."
  :group 'matlab
  :type 'string)

(defcustom matlab-arg1-max-indent-length 15
  "*The maximum length to indent when indenting past arg1.
If arg1 is exceptionally long, then only this number of characters
will be indented beyond the open paren starting the parameter list."
  :group 'matlab
  :type 'integer)

(defcustom matlab-maximum-indents '(;; = is a convenience. Don't go too far
				    (?= . (10 . 4))
				    ;; Fns should provide hard limits
				    (?\( . 50)
				    ;; Matrix/Cell arrays
				    (?\[ . 20)
				    (?\{ . 20))
  "Alist of maximum indentations when lining up code.
Each element is of the form (CHAR . INDENT) where char is a character
the indent engine is using, and INDENT is the maximum indentation
allowed.  Indent could be of the form (MAXIMUM . INDENT), where
MAXIMUM is the maximum allowed calculated indent, and INDENT is the
amount to use if MAXIMUM is reached."
  :group 'matlab
  :type '(repeat (cons (character :tag "Open List Character")
		       (sexp :tag "Number (max) or cons (max indent)"))))

(defcustom matlab-align-to-paren t
  "*Whether continuation lines should be aligned to the opening parenthesis.
When non-nil, continuation lines are aligned to the opening parenthesis if the
opening is not followed by only spaces and ellipses.  When nil, continued lines
are simply indented by `matlab-continuation-indent-level'."
  :group 'matlab
  :type 'boolean
  )

(defcustom matlab-indent-function-body 'MathWorks-Standard
  "*If non-nil, indent body of function.
If the global value is nil, do not indent function bodies.
If the global value is t, always indent function bodies.
If the global value is 'guess, then the local value will be set to
either nil or t when the MATLAB mode is started in a buffer based on the
file's current indentation.
If the global value is 'MathWorks-Standard, then the local value is not
changed, and functions are indented based on `matlab-functions-have-end'."
  :group 'matlab
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Guess" guess)
                 (const :tag "MathWorks Standard"
                        MathWorks-Standard))
  )

(make-variable-buffer-local 'matlab-indent-function-body)

(defcustom matlab-functions-have-end 'guess
  "*If non-nil, functions-have-end minor mode is on by default.
If the value is 'guess, then we guess if a file has end when
`matlab-mode' is initialized."
  :group 'matlab
  :type 'boolean)

(make-variable-buffer-local 'matlab-functions-have-end)

(defun matlab-toggle-functions-have-end ()
  "Toggle `matlab-functions-have-end-minor-mode'."
  (interactive)
  (matlab-toggle-functions-have-end-minor-mode))

;; The following minor mode is on if and only if the above variable is true;
(easy-mmode-define-minor-mode matlab-functions-have-end-minor-mode
  "Toggle functions-have-end minor mode, indicating function/end pairing."
  nil
  (:eval (cond ((eq matlab-functions-have-end 'guess)
		" function... ?")
	       ((eq matlab-functions-have-end 'class)
		" classdef...end")
	       (matlab-functions-have-end
		" function...end")
	       (t
		" function...")))
  nil ; empty mode-map
  ;; body of matlab-functions-have-end-minor-mode
  (let ((type (matlab-guess-script-type)))
    (if matlab-functions-have-end-minor-mode
	(if (eq type 'empty)
	    (setq matlab-functions-have-end 'guess)
	  (setq matlab-functions-have-end type))
      (setq matlab-functions-have-end nil)
      ))
  )

(defvar matlab-defun-regex) ;; Quiet compiler warning (is defined below)
(defvar matlab-last-script-type-guess nil
  "The last time we guessed the script type, what was it?")
(defun matlab-last-guess-decl-p ()
  "Return non-nil if our last guess at a script type was function or class."
  (memq matlab-last-script-type-guess '(function class)))

(defun matlab-guess-script-type ()
  "Guess the type of script this `matlab-mode' file contains.
Returns one of 'empty, 'script, 'function, 'class."
  (setq matlab-last-script-type-guess
	(save-excursion
	  (goto-char (point-min))
	  (if (matlab-find-code-line)
	      ;; We found some code, what is it?
	      (if (save-excursion
		    (beginning-of-line)
		    (looking-at matlab-defun-regex))
		  ;; A match - figure out the type of thing.
		  (let ((str (match-string-no-properties 1)))
		    (cond ((string= str "function")
			   'function)
			  ((string= str "classdef")
			   'class)))
		;; No function or class - just a script.
		'script)
	    ;; No lines of code, we are empty, so undecided.
	    'empty))))
	
(defun matlab-do-functions-have-end-p (&optional no-navigate)
  "Look at the contents of the current buffer and decide if functions have end.
If the current value of `matlab-functions-have-end' is 'guess, look @ the buffer.
If the value is t, then return that."
  (if (eq matlab-functions-have-end 'guess)
      ;; Lets guess what we think the answer is.
      (let ((type (matlab-guess-script-type)))
	(cond ((eq type 'empty)
	       'guess) ;; Keep guessing until we get some code.
	      ((eq type 'script)
	       'script) ;; modern scripts can have functions, and they are required to have an end.
	      ((eq type 'class)
	       'class)  ;; classes always have ends.
	      (no-navigate
	       ;; Functions, but don't navigate ... stay in guess mode.
	       'guess)
	      (t
	       ;; functions but do navigate - we need to see if there is an end.
	       (save-excursion
		 (goto-char (point-min))
		 (matlab-find-code-line)
		 (let ((matlab-functions-have-end t)) ;; pretend we have ends
		   (beginning-of-line)
		   (condition-case nil
		       ;; Try to navigate.  If success, then t
		       (progn (matlab-forward-sexp) t)
		     ;; On failure, then no ends.
		     (error nil))
		   ))))
	)
    ;; Else, just return the default.
    matlab-functions-have-end))

(defun matlab-toggle-functions-have-end-minor-mode ()
  "Toggle `matlab-functions-have-end-minor-mode' only for `matlab-mode' buffers."
  (matlab-functions-have-end-minor-mode)
  (if (and matlab-functions-have-end-minor-mode (not (eq major-mode 'matlab-mode)))
      (progn
	(matlab-functions-have-end-minor-mode -1)
	(error "Mode `matlab-functions-have-end' minor mode is only for MATLAB Major mode")))
  )

(defun matlab-indent-function-body-p ()
  "Non-nil if functions bodies are indented.
See `matlab-indent-function-body' variable."
  (if (eq matlab-indent-function-body 'MathWorks-Standard)
      ;; Dec '09
      ;; The MathWorks standard is the same as if functions have end.
      matlab-functions-have-end
    ;; Else, just return the variable.
    matlab-indent-function-body))

(defcustom matlab-fill-fudge 10
  "Number of characters around `fill-column' we can fudge filling.
Basically, there are places that are very convenient to fill at, but
might not be the closest fill spot, or occur after `fill-column'.
If they occur within this fudge factor, we will use them.
Also, if none of the above occur, and we find a symbol to break at,
but an open paren (group) starts or ends within this fudge factor,
move there to boost the amount of fill leverage we can get."
  :group 'matlab
  :type 'integer)

(defcustom matlab-fill-fudge-hard-maximum 79
  "The longest line allowed when auto-filling code.
This overcomes situations where the `fill-column' plus the
`matlab-fill-fudge' is greater than some hard desired limit."
  :group 'matlab
  :type 'integer)

(defcustom matlab-elipsis-string "..."
  "Text used to perform continuation on code lines.
This is used to generate and identify continuation lines."
  :group 'matlab
  :type 'string)

(defcustom matlab-fill-code nil
  "*If true, `auto-fill-mode' causes code lines to be automatically continued."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-fill-count-ellipsis-flag t
  "*Non-nil means to count the ellipsis when auto filling.
This effectively shortens the `fill-column' by the length of
`matlab-elipsis-string'."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-fill-strings-flag t
  "*Non-nil means that when auto-fill is on, strings are broken across lines.
If `matlab-fill-count-ellipsis-flag' is non nil, this shortens the
`fill-column' by the length of `matlab-elipsis-string'."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-comment-column 40
  "*The goal comment column in `matlab-mode' buffers."
  :group 'matlab
  :type 'integer)

(defcustom matlab-comment-anti-indent 0
  "*Amount of anti-indentation to use for comments in relation to code."
  :group 'matlab
  :type 'integer)

(defcustom matlab-comment-line-s "% "
  "*String to start comment on line by itself."
  :group 'matlab
  :type 'string)

(defcustom matlab-comment-on-line-s "% "
  "*String to start comment on line with code."
  :group 'matlab
  :type 'string)

(defcustom matlab-comment-region-s "% $$$ "
  "*String inserted by \\[matlab-comment-region] at start of each line in \
region."
  :group 'matlab
  :type 'string)

(defcustom matlab-verify-on-save-flag t
  "*Non-nil means to verify M whenever we save a file."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-mode-verify-fix-functions
  '(matlab-mode-vf-functionname matlab-mode-vf-classname matlab-mode-vf-add-ends)
  "List of function symbols which perform a verification and fix to M code.
Each function gets no arguments, and returns nothing.  They can move
point, but it will be restored for them."
  :group 'matlab
  :type '(repeat (choice :tag "Function: "
			 '(matlab-mode-vf-functionname
			   matlab-mode-vf-classname
			   matlab-mode-vf-add-ends
			   matlab-mode-vf-block-matches-forward
			   matlab-mode-vf-block-matches-backward
			   matlab-mode-vf-quiesce-buffer
			   ))))

(defcustom matlab-block-verify-max-buffer-size 50000
  "*Largest buffer size allowed for block verification during save."
  :group 'matlab
  :type 'integer)

(defcustom matlab-mode-hook nil
  "*List of functions to call on entry to MATLAB mode."
  :group 'matlab
  :type 'hook)

(defcustom matlab-show-mlint-warnings nil
  "*If non-nil, show mlint warnings."
  :group 'matlab
  :type 'boolean)
(make-variable-buffer-local 'matlab-show-mlint-warnings)
(put 'matlab-show-mlint-warnings 'safe-local-variable #'booleanp)

(defcustom matlab-highlight-cross-function-variables nil
  "*If non-nil, highlight cross-function variables."
  :group 'matlab
  :type 'boolean)
(make-variable-buffer-local 'matlab-highlight-cross-function-variables)
(put 'matlab-highlight-cross-function-variables 'safe-local-variable #'booleanp)

(defcustom matlab-return-add-semicolon nil
  "*If non nil, check to see a semicolon is needed when RET is pressed."
  :group 'matlab
  :type 'boolean)

(make-variable-buffer-local 'matlab-return-add-semicolon)

(defcustom matlab-change-current-directory nil
  "*If non nil, make file's directory the current directory when evaluating it."
  :group 'matlab
  :type 'boolean)

(make-variable-buffer-local 'matlab-change-current-directory)

(defvar matlab-mode-abbrev-table nil
  "The abbrev table used in `matlab-mode' buffers.")
(define-abbrev-table 'matlab-mode-abbrev-table ())


;;; Keybindings ===============================================================

(defvar matlab-help-map
  (let ((km (make-sparse-keymap)))
    (define-key km "r" 'matlab-shell-run-command)
    (define-key km "f" 'matlab-shell-describe-command)
    (define-key km "a" 'matlab-shell-apropos)
    (define-key km "v" 'matlab-shell-describe-variable)
    km)
  "The help key map for `matlab-mode' and `matlab-shell-mode'.")

;; mode map
(defvar matlab-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km [return] 'matlab-return)
    (define-key km "%" 'matlab-electric-comment)
    (define-key km "}" 'matlab-electric-block-comment)
    (define-key km "{" 'matlab-electric-block-comment)
    (define-key km "\C-c;" 'matlab-comment-region)
    (define-key km "\C-c:" 'matlab-uncomment-region)
    (define-key km [(control c) return] 'matlab-comment-return)
    (define-key km [(control c) (control c)] 'matlab-insert-map-fcn)
    (define-key km [(control c) (control f)] 'matlab-fill-comment-line)
    (define-key km [(control c) (control j)] 'matlab-justify-line)
    (define-key km [(control c) (control q)] 'matlab-fill-region)
    (define-key km [(control c) (control s)] 'matlab-shell-save-and-go)
    (define-key km [(control c) (control r)] 'matlab-shell-run-region)
    (define-key km [(meta control return)] 'matlab-shell-run-cell)
    (define-key km [(control return)] 'matlab-shell-run-region-or-line)
    (define-key km [(control c) (control t)] 'matlab-show-line-info)
    (define-key km [(control c) ?. ] 'matlab-shell-locate-fcn)
    (define-key km [(control h) (control m)] matlab-help-map)
    (define-key km [(control j)] 'matlab-linefeed)
    (define-key km "\M-\r" 'newline)
    (define-key km [(meta \;)] 'matlab-comment)
    (define-key km [(meta a)] 'matlab-beginning-of-command)
    (define-key km [(meta e)] 'matlab-end-of-command)
    (define-key km [(meta j)] 'matlab-comment-line-break-function)
    (define-key km [(meta s)] 'matlab-show-matlab-shell-buffer)
    (define-key km "\M-\t" 'matlab-complete-symbol)
    (define-key km [(meta control f)] 'matlab-forward-sexp)
    (define-key km [(meta control b)] 'matlab-backward-sexp)
    (define-key km [(meta control q)] 'matlab-indent-sexp)
    (define-key km [(meta control a)] 'matlab-beginning-of-defun)
    (define-key km [(meta control e)] 'matlab-end-of-defun)
    (if (string-match "XEmacs" emacs-version)
	(define-key km [(control meta button1)] 'matlab-find-file-click)
      (define-key km [(control meta mouse-2)] 'matlab-find-file-click))

    (substitute-key-definition 'read-only-mode 'matlab-toggle-read-only
			       km global-map)

    (substitute-key-definition 'comment-region 'matlab-comment-region
			       km global-map) ;torkel
    km)
  "The keymap used in `matlab-mode'.")

;;; TODO - this menu was all about when emacs didn't always have windows (e18 ?)
;;  turn this into a regular menu definition.
(defvar matlab-mode-menu-keymap nil
  "Keymap used in MATLAB mode to provide a menu.")

(defun matlab-frame-init ()
  "Initialize Emacs menu system."
  (interactive)
  ;; make a menu keymap
  (easy-menu-define
   matlab-mode-menu
   matlab-mode-map
   "MATLAB menu"
   '("MATLAB"
     ["Start MATLAB" matlab-shell
      :active (not (matlab-shell-active-p))
      :visible (not (matlab-shell-active-p)) ]
     ["Switch to MATLAB" matlab-shell
      :active (matlab-any-shell-active-p)
      :visible (matlab-any-shell-active-p)]
     ["Save and go" matlab-shell-save-and-go
      :active (matlab-any-shell-active-p) ]
     ["Run Region" matlab-shell-run-region
      :active (matlab-any-shell-active-p) ]
     ["Run Cell" matlab-shell-run-cell
      :active (matlab-any-shell-active-p) ]
     ["Version" matlab-show-version t]
     "----"
     ["Locate MATLAB function" matlab-shell-locate-fcn
       :active (matlab-shell-active-p)
       :help "Run 'which FCN' in matlab-shell, then open the file in Emacs"]
     ["Show M-Lint Warnings" matlab-toggle-show-mlint-warnings
      :active (and (locate-library "mlint") (fboundp 'mlint-minor-mode))
      :style toggle :selected  matlab-show-mlint-warnings
      ]
     ("Auto Fix"
      ["Verify/Fix source" matlab-mode-verify-fix-file t]
      ["Spell check strings and comments" matlab-ispell-strings-and-comments t]
      ["Quiesce source" matlab-mode-vf-quiesce-buffer t]
      )
     ("Navigate"
      ["Beginning of Command" matlab-beginning-of-command t]
      ["End of Command" matlab-end-of-command t]
      ["Forward Block" matlab-forward-sexp t]
      ["Backward Block" matlab-backward-sexp t]
      ["Beginning of Function" matlab-beginning-of-defun t]
      ["End of Function" matlab-end-of-defun t])
     ("Format"
      ["Justify Line" matlab-justify-line t]
      ["Fill Region" matlab-fill-region t]
      ["Fill Comment Paragraph" matlab-fill-paragraph
       (save-excursion (matlab-comment-on-line))]
      ["Join Comment" matlab-join-comment-lines
       (save-excursion (matlab-comment-on-line))]
      ["Comment Region" matlab-comment-region t]
      ["Uncomment Region" matlab-uncomment-region t]
      ["Indent Syntactic Block" matlab-indent-sexp])
     ("Debug"
      ["Edit File (toggle read-only)" matlab-shell-gud-mode-edit
       :help "Exit MATLAB debug minor mode to edit without exiting MATLAB's K>> prompt."
       :visible gud-matlab-debug-active ]
      ["Add Breakpoint (ebstop in FILE at point)" gud-break
       :active (matlab-shell-active-p)
       :help "When MATLAB debugger is active, set break point at current M-file point"]
      ["Remove Breakpoint (ebclear in FILE at point)" gud-remove
       :active (matlab-shell-active-p)
       :help "Show all active breakpoints in a separate buffer." ]
      ["List Breakpoints (ebstatus)" gud-list-breakpoints
       :active (matlab-shell-active-p)
       :help "List active breakpoints."]
      ["Step (dbstep in)" gud-step
       :active gud-matlab-debug-active
       :help "When MATLAB debugger is active, step into line"]
      ["Next (dbstep)" gud-next
       :active gud-matlab-debug-active
       :help "When MATLAB debugger is active, step one line"]
      ["Finish function  (dbstep out)" gud-finish
       :active gud-matlab-debug-active
       :help "When MATLAB debugger is active, run to end of function"]
      ["Continue (dbcont)" gud-cont
       :active gud-matlab-debug-active
       :help "When MATLAB debugger is active, run to next break point or finish"]
      ["Evaluate Expression" matlab-shell-gud-show-symbol-value
       :active (matlab-any-shell-active-p)
       :help "When MATLAB is active, show value of the symbol under point."]
      ["Show Stack" mlg-show-stack
       :active gud-matlab-debug-active
       :help "When MATLAB debugger is active, show the stack in a buffer."]
;;;  Advertise these more if we can get them working w/ gud's frame show.
;;;      ["Up Call Stack (dbup)" gud-up
;;;       :active gud-matlab-debug-active
;;;       :help "When MATLAB debugger is active and at break point, go up a frame"]
;;;      ["Down Call Stack (dbdown)" gud-down
;;;       :active gud-matlab-debug-active
;;;       :help "When MATLAB debugger is active and at break point, go down a frame"]
      ["Quit debugging (dbquit)" gud-stop-subjob
       :active gud-matlab-debug-active
       :help "When MATLAB debugger is active, stop debugging"]
      )

;; TODO - how to autoload these?  Do we want this menu?
;;     ("Insert"
;;      ["Complete Symbol" matlab-complete-symbol t]
;;      ["Comment" matlab-comment t]
;;      ["if end" tempo-template-matlab-if t]
;;      ["if else end" tempo-template-matlab-if-else t]
;;      ["for end" tempo-template-matlab-for t]
;;      ["switch otherwise end" tempo-template-matlab-switch t]
;;      ["Next case" matlab-insert-next-case t]
;;      ["try catch end" tempo-template-matlab-try t]
;;      ["while end" tempo-template-matlab-while t]
;;      ["End of block" matlab-insert-end-block t]
;;      ["Function" tempo-template-matlab-function t]
;;      ["Stringify Region" matlab-stringify-region t]
;;      )
     ("Customize"
      ["Indent Function Body"
       (setq matlab-indent-function-body (not (matlab-indent-function-body-p)))
       :style toggle :selected matlab-indent-function-body]
      ["Functions Have end"
       matlab-toggle-functions-have-end
       :style toggle :selected matlab-functions-have-end]
      ["Verify File on Save"
       (setq matlab-verify-on-save-flag (not matlab-verify-on-save-flag))
       :style toggle :selected matlab-verify-on-save-flag]
      ["Auto Fill does Code"
       (setq matlab-fill-code (not matlab-fill-code))
       :style toggle :selected matlab-fill-code ]
      ["Highlight Cross-Function Variables"
       matlab-toggle-highlight-cross-function-variables
       :active (locate-library "mlint")
       :style toggle :selected  matlab-highlight-cross-function-variables
       ]
      ["Add Needed Semicolon on RET"
       (setq matlab-return-add-semicolon (not matlab-return-add-semicolon))
       :style toggle :selected  matlab-return-add-semicolon
       ]
      ["Customize" (customize-group 'matlab)
       (and (featurep 'custom) (fboundp 'custom-declare-variable))
       ]
      )
     "----"
     ["Run M Command" matlab-shell-run-command (matlab-shell-active-p)]
     ["Describe Command" matlab-shell-describe-command (matlab-shell-active-p)]
     ["Describe Variable" matlab-shell-describe-variable (matlab-shell-active-p)]
     ["Command Apropos" matlab-shell-apropos (matlab-shell-active-p)]
     ))
  (easy-menu-add matlab-mode-menu matlab-mode-map))


;;; Font Lock : Character Vectors, Strings and Comments ================================
;;
;; Combine these, but do all the matching internally instead of using regexp
;; because it's just too complex for a regular expression.
(defface matlab-region-face
  '((t :inherit region))
  "*Face used to highlight a matlab region."
  :group 'matlab)

(defvar matlab-unterminated-string-face 'matlab-unterminated-string-face
  "Self reference for unterminated string face.")

(defvar matlab-commanddual-string-face 'matlab-commanddual-string-face
  "Self reference for command dual string face.")

(defvar matlab-simulink-keyword-face 'matlab-simulink-keyword-face
  "Self reference for simulink keywords.")

(defvar matlab-nested-function-keyword-face 'matlab-nested-function-keyword-face
  "Self reference for nested function/end keywords.")

(defvar matlab-cross-function-variable-face 'matlab-cross-function-variable-face
  "Self reference for cross-function variables.")

(defvar matlab-cellbreak-face 'matlab-cellbreak-face
  "Self reference for cellbreaks.")

(defface matlab-unterminated-string-face
  '((t :inherit font-lock-string-face
       :underline t))
  "*Face used to highlight unterminated strings."
  :group 'matlab)

(defface matlab-commanddual-string-face
  '((t :inherit font-lock-string-face
       :slant italic))
  "*Face used to highlight command dual string equivalent."
  :group 'matlab)

(defface matlab-simulink-keyword-face
  '((t :inherit font-lock-type-face
       :underline t))
  "*Face used to highlight simulink specific functions."
  :group 'matlab)

(defface matlab-nested-function-keyword-face
  '((t :inherit font-lock-keyword-face
       :slant  italic))
  "*Face to use for cross-function variables.")

(defface matlab-cross-function-variable-face
  '((t :weight bold
       :slant  italic))
  "*Face to use for cross-function variables."
  :group 'matlab)

(defface matlab-cellbreak-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "*Face to use for cellbreak %% lines.")

(defface matlab-ignored-comment-face
  '((t :inherit font-lock-comment-face
       :height  .8
       :slant italic))
  "*Face to use for ignored comments.
Ignored comments are lines that start with '% $$$'  or '%^'.")

(defface matlab-pragma-face
  '((t :inherit font-lock-comment-face
       :bold t))
  "*Face to use for cellbreak %% lines.")

;;; Font Lock MLINT data highlighting

;; TODO - THE BELOW LOOKS BROKEN TO ME.
;;    - these are used in font lock, but are hand adding overlays
;;    - and returning no matches - but the font lock keywords try to add
;;    - a font.    NEEDS FIX
(defun matlab-font-lock-nested-function-keyword-match (limit)
  "Find next nested function/end keyword for font-lock.
Argument LIMIT is the maximum distance to search."
; Because of the way overlays are setup, the cursor will be sitting
; on either a "function" or "end" keyword.
  (catch 'result
    (let ((pos (point))
          overlays)
      (while (< pos limit)
        (setq overlays (matlab-overlays-at pos))
        (while overlays
          (let ((overlay (car overlays)))
	    (when (matlab-overlay-get overlay 'nested-function)
	      (when (= pos (matlab-overlay-start overlay))
		(goto-char pos)
		;; The following line presumably returns true.
		(throw 'result (re-search-forward "function" (+ pos 8) t)))
	      (let ((end-of-overlay (- (matlab-overlay-end overlay) 3)))
		(when (<= pos end-of-overlay)
		  (goto-char end-of-overlay)
		  (throw 'result
			 (re-search-forward "end" (+ end-of-overlay 3) t))))))
          (setq overlays (cdr overlays)))
        (setq pos (matlab-next-overlay-change pos)))
      nil ;; no matches, stop
      )))

(defun matlab-font-lock-cross-function-variables-match (limit)
  "Find next cross-function variable for font-lock.
Argument LIMIT is the maximum distance to search."
  (catch 'result
    (let ((pos (point))
          overlays variables)
      (while (< pos limit)
        (let ((overlays (matlab-overlays-at pos)))
	  (while overlays
	    (let ((overlay (car overlays)))
	      (setq variables (matlab-overlay-get
			       overlay 'cross-function-variables))
	      (if variables
		  (progn
		    (goto-char pos)
		    (setq pos (min limit (matlab-overlay-end overlay)))
		    (if (re-search-forward variables pos t)
			(progn
			  (throw 'result t))))))
	    (setq overlays (cdr overlays))))
        (setq pos (matlab-next-overlay-change pos)))
      nil ;; no matches, stop
      )))

(defcustom matlab-keyword-list '("global" "persistent" "for" "parfor" "while"
				 "spmd" "if" "elseif" "else"
				 "return" "break" "continue"
				 "switch" "case" "otherwise" "try"
				 "catch" "tic" "toc"
				 )
  "List of keywords for MATLAB used in highlighting.
Customizing this variable is only useful if `regexp-opt' is available."
  :group 'matlab
  :type '(repeat (string :tag "Keyword: ")))

(defcustom matlab-keyword-first-on-line-list '( "properties" "methods" "enumeration" "events"
						"arguments"
						)
  "List of keywords for MATLAB that should be highilghted only if the first word on a line.
Customizing this variable is only useful if `regexp-opt' is available."
  :group 'matlab
  :type '(repeat (string :tag "Keyword: ")))

(defcustom matlab-handle-graphics-list '("figure" "axes" "axis" "line"
					"surface" "patch" "text" "light"
					"image" "set" "get" "uicontrol"
					"uimenu" "uitoolbar"
					"uitoggletool" "uipushtool"
					"uicontext" "uicontextmenu"
					"setfont" "setcolor")
  "List of handle graphics functions used in highlighting.
Customizing this variable is only useful if `regexp-opt' is available."
  :group 'matlab
  :type '(repeat (string :tag "HG Keyword: ")))

(defcustom matlab-debug-list '("dbstop" "dbclear" "dbcont" "dbdown" "dbmex"
			      "dbstack" "dbstatus" "dbstep" "dbtype" "dbup"
			      "dbquit")
  "List of debug commands used in highlighting.
Customizing this variable is only useful if `regexp-opt' is available."
  :group 'matlab
  :type '(repeat (string :tag "Debug Keyword: ")))

(defcustom matlab-simulink-keywords
  '("simulink" "get_param" "set_param" "simget" "simset" "sim"
    "new_system" "open_system" "close_system" "save_system" "find_system"
    "add_block" "delete_block" "replace_block"
    "add_line" "delete_line" "replace_line"
    "bdroot" "bdclose" )
  ;; Missing this regex "\\(mld\\|ss\\)[A-Z]\\w+\\)"
  "List of keywords to highlight for simulink."
  :group 'matlab
  :type '(repeat (string :tag "Debug Keyword: ")))


(defcustom matlab-constants-keyword-list
  '("eps" "pi" "inf" "Inf" "nan" "NaN" "ans" "i" "j" "NaT" "true" "false")
  "List of constants and special variables in MATLAB."
  :group 'matlab
  :type '(repeat (string :tag "Debug Keyword: ")))

(defun matlab-font-lock-regexp-opt (keywordlist)
  "Create a font-lock usable KEYWORDLIST matching regular expression.
Uses `regex-opt' if available.  Otherwise creates a 'dumb' expression."
  (concat "\\<\\("
	  (if (fboundp 'regexp-opt)
	      (regexp-opt keywordlist)
	    (mapconcat (lambda (s) s) keywordlist "\\|"))
	  "\\)\\>"))

;;; Font lock keyword handlers
;;
(defun matlab-font-lock-basic-keyword-match (limit)
  "Font lock matcher for basic keywords.
Fails to match when keywords show up as variables, etc."
  (matlab--scan-next-keyword 'fl-simple limit))

(defun matlab-font-lock-vardecl-keyword-match (limit)
  "Font lock matcher for mcos keywords.
Fails to match when keywords show up as variables, etc."
  (matlab--scan-next-keyword 'vardecl limit))

(defvar matlab-fl-anchor-keyword nil)
(defun matlab-font-lock-mcos-keyword-match (limit)
  "Font lock matcher for mcos keywords.
Fails to match when keywords show up as variables, etc."
  (and (eq matlab-functions-have-end 'class)
       (setq matlab-fl-anchor-keyword
	     (matlab--scan-next-keyword 'mcos limit))))

(defun matlab-font-lock-args-keyword-match (limit)
  "Font lock matcher for mcos keywords.
Fails to match when keywords show up as variables, etc."
  (setq matlab-fl-anchor-keyword
	(matlab--scan-next-keyword 'args limit)))

(defvar font-lock-beg) (defvar font-lock-end) ; quiet compiler.
(defun matlab-font-lock-extend-region ()
  "Called by font-lock to extend the region for multiline expressions.
Supports expressions like arguments and property blocks with anchored
color support."
  (save-excursion
    (let* ((flb font-lock-beg)
	   (fle font-lock-end)
	   (tmp (matlab--scan-block-backward-up (window-start)))
	   (blockmatch (when (not tmp) (matlab--mk-keyword-node))))
      (when (and (member (nth 1 blockmatch) '("properties" "events" "arguments"))
		 (matlab--valid-keyword-node blockmatch))
	(setq font-lock-beg (min font-lock-beg (point-at-bol)))
	(when (not (matlab--scan-next-keyword 'all (window-end)))
	  (setq font-lock-end (max font-lock-end (point-at-eol)))))

      (if (and (eq font-lock-beg flb)
	       (eq font-lock-end fle))
	  ;; We didn't change anything.
	  nil

	;; We made a change
	t))))

(defvar ml-fl-anchor-limit nil)
(defun matlab-font-lock-anchor-set-end-limit ()
  "Set the end limit for anchored matchers."
  (save-excursion
    ;; next keyword is faster, plus if someone is in the middle of typing
    ;; a new block, prevents going too far into the distance.
    (matlab--scan-next-keyword 'all (point-max))
    (forward-word -1)
    (setq ml-fl-anchor-limit (point))))

(defun matlab-font-lock-anchor-clear-end-limit ()
  "Clear the end limit for anchored matchers."
    (setq ml-fl-anchor-limit nil))

(defun matlab-font-lock-anchor-variable-match (limit)
  "After finding a keyword like PROPERTIES or ARGUMENTS, match vars.
This matcher will handle a range of variable features."
  (when (member (nth 1 matlab-fl-anchor-keyword)
		'("properties" "events" "arguments"))
    (let* ((match (re-search-forward "^\\s-+\\(\\w+\\)\\_>" ml-fl-anchor-limit t))
	   ;; Save this match so we can do a 2nd anchored search for a data type.
	   (md1 (list (match-beginning 1) (match-end 1)))
	   (tm (looking-at
		"\\s-*\\(\\(?:([^\n)]+)\\)?\\s-*\\(?:\\w+\\|\\.\\)*\\)\\s-*\\($\\|[.%{=]\\)"))
	   (tm1 (if tm (list (match-beginning 1) (match-end 1))
		  ;; The below is a cheat to not highlight anything but
		  ;; still supply the match data for this optional piece.
		  (list (nth 1 md1) (nth 1 md1))))
	   (newmdata (append md1 md1 tm1)))
      (when match
	(goto-char (apply 'max newmdata))
	(set-match-data newmdata)
	t))))

;;; Font Lock keyword handling
;;
;; Many parts of the keyword handling are shared with matlab-shell.
;; The matlab based variables here are divided up between generic keywords
;; and keywords only for M files.  This means the M shell won't highlight
;; some syntaxes like classdef stuff even though someone might paste them in.
;;
;; matlab-*-keywords      -- MATLAB Files or Shell
;; matlab-file-*-keywords -- MATLAB Files only

(defconst matlab-basic-font-lock-keywords
  (list
   ;; General keywords
   '(matlab-font-lock-basic-keyword-match
     (0 font-lock-keyword-face))
   ;; Handle graphics stuff
   (list
    (matlab-font-lock-regexp-opt matlab-handle-graphics-list)
    '(0 font-lock-type-face))
   (list
    ;; How about a few matlab constants such as pi, infinity, and sqrt(-1)?
    (matlab-font-lock-regexp-opt matlab-constants-keyword-list)
    1 font-lock-constant-face)
   ;; Imaginary number support
   '("\\<[0-9]\\.?\\(i\\|j\\)\\_>" 1 font-lock-reference-face)
   )
  "Basic Expressions to highlight in MATLAB mode or shell.")

(defconst matlab-file-basic-font-lock-keywords
  (append
   matlab-basic-font-lock-keywords
   (list
    ;; MCOS keywords like properties, methods, events
    '(matlab-font-lock-mcos-keyword-match
      (1 font-lock-keyword-face))
    ;; ARGUMENTS keyword
    '(matlab-font-lock-args-keyword-match
      (1 font-lock-keyword-face))
    ;; Highlight cross function variables
    '(matlab-font-lock-cross-function-variables-match
      (1 matlab-cross-function-variable-face prepend))
    ;; Highlight nested function/end keywords
    '(matlab-font-lock-nested-function-keyword-match
      (0 matlab-nested-function-keyword-face prepend))
    ))
  "Basic Expressions to highlight in MATLAB Files.")

(defconst matlab-function-arguments
  "\\(([^)]*)\\)?\\s-*\\([,;\n%]\\|$\\)")

(defconst matlab-function-font-lock-keywords
  (list
   ;; defining a function, a (possibly empty) list of assigned variables,
   ;; function name, and an optional (possibly empty) list of input variables
   (list (concat "^\\s-*\\(function\\)\\_>[ \t\n.]*"
		 "\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*"
		 "=[ \t\n.]*\\(\\(?:[sg]et\\.\\)?\\sw+\\)[ \t\n.]*"
		 matlab-function-arguments)
	 ;;'(1 font-lock-keyword-face append) - handled as keyword
	 '(2 font-lock-variable-name-face append)
	 '(3 font-lock-function-name-face prepend))
   ;; defining a function, a function name, and an optional (possibly
   ;; empty) list of input variables
   (list (concat "^\\s-*\\(function\\)[ \t\n.]+"
		 "\\(\\(?:[sg]et\\.\\)?\\sw+\\)[ \t\n.]*"
		 matlab-function-arguments)
	 ;; '(1 font-lock-keyword-face append) - handled as keyword
	 '(2 font-lock-function-name-face prepend))
   ;; Anchor on the function keyword, highlight params
   (list (concat "^\\s-*function\\>[ \t\n.]*"
		 "\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*=[ \t\n.]*\\)?"
		 "\\(?:[sg]et\\.\\)?\\sw+\\s-*(")
	 '("\\s-*\\(\\sw+\\)\\s-*[,)]"
	   (save-excursion
	     (condition-case nil
		 (matlab-scan-end-of-command)
	       (error (point-at-eol))))
	   nil
	   (1 font-lock-variable-name-face)))
   ;; ARGUMENTS have variables to highlight
   '(matlab-font-lock-args-keyword-match
     (matlab-font-lock-anchor-variable-match	 ;; matcher fcn
      (matlab-font-lock-anchor-set-end-limit)	 ;; pre forms
      (matlab-font-lock-anchor-clear-end-limit)	 ;; post forms
      (1 font-lock-variable-name-face t)
      (2 font-lock-type-face t)
      ))
   ;; VARDECL keywords
    '(matlab-font-lock-vardecl-keyword-match
      ("\\(\\w+\\)\\(\\s-*=[^,; \t\n]+\\|[, \t;]+\\|$\\)"
       nil  nil (1 font-lock-variable-name-face)))
   ;; I like variables for FOR loops
   '("\\<\\(\\(?:par\\)?for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\
\\(\\([^\n,;%(]+\\|([^\n%)]+)\\)+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face append)
     (3 font-lock-reference-face append))
   ;; Items after a switch statements are cool
   '("\\_<\\(case\\|switch\\)\\_>\\s-+\\({[^}\n]+}\\|[^,%\n]+\\)"
     (2 font-lock-reference-face))
    ;; set_param and waitfor have input variables that can be highlighted.
    (list (concat "\\<" matlab-indent-past-arg1-functions "\\s-*")
	  '("(\\s-*\\(\\w+\\)\\s-*\\(,\\|)\\)" nil  nil
	    (1 font-lock-variable-name-face)))
   )
  "List of font lock keywords for stuff in functions.")

(defconst matlab-class-attributes-list-re
  "\\s-*\\(?2:(\\([^)]+\\))\\|\\)"
  "Regular expression for matching an attributes block.")

(defconst matlab-file-class-font-lock-keywords
  (list
   ;; Classdefs keyword and the class name
   (list (concat "^\\s-*\\(classdef\\)\\_>"
		 matlab-class-attributes-list-re
		 "\\s-*\\(?3:\\sw+\\)")
	 ;; '(1 font-lock-keyword-face append) - handled as keyword
	 '(3 font-lock-function-name-face)
	 )
   ;; Classdef anchor for highlighting all the base classes in inherits from
   (list (concat "^\\s-*\\(classdef\\)"
    		 matlab-class-attributes-list-re
    		 "\\s-+\\(\\sw+\\)")
    	 '("\\s-*[<&]\\s-*\\(\\(\\sw\\|\\.\\)+\\)" nil  nil
   	   (1 font-lock-constant-face)))
   ;; Property and Method blocks have attributes to highlight
   (list "^\\s-*\\(classdef\\|properties\\|methods\\|events\\|arguments\\)\\s-*("
	 '("\\(\\sw+\\)\\s-*\\(=\\s-*[^,)]+\\)?" nil  nil
	   (1 font-lock-type-face)
	   ))
   ;; PROPERY, EVENTS, etc have variables to highlight
   '(matlab-font-lock-mcos-keyword-match
     (matlab-font-lock-anchor-variable-match	 ;; matcher fcn
      (matlab-font-lock-anchor-set-end-limit)	 ;; pre forms
      (matlab-font-lock-anchor-clear-end-limit)	 ;; post forms
      (1 font-lock-variable-name-face t)
      (2 font-lock-type-face t)
      ))
   
   ;; Properties can have a type syntax after them
   ;;'("^\\s-*\\w+\\s-*\\(([:0-9,]+)\\s-*[^{=\n]+\\)"
   ;;  (1 font-lock-type-face nil nil))
   ;; Properties blocks are full of variables
   ;;'("^\\s-*\\(properties\\|events\\|arguments\\)\\s-*[(,;%\n]"
   ;;  ("^\\s-*\\(\\sw+\\)\\>" ;; This part matches the variable
   ;;   ;; extend region to match in
   ;;   (save-excursion (matlab-forward-sexp t) (beginning-of-line) (point))
   ;;   nil
   ;;   (1 font-lock-variable-name-face t))
   ;;  )
   )
  "List of font-lock keywords used when an MATLAB file contains a class.")

(defconst matlab-file-gaudy-font-lock-keywords
  (append
   matlab-basic-font-lock-keywords
   matlab-file-basic-font-lock-keywords
   matlab-function-font-lock-keywords
   matlab-file-class-font-lock-keywords
   )
  "Expressions to highlight in MATLAB mode.")

(defconst matlab-really-gaudy-font-lock-keywords
  (append
   (list
    ;; Since it's a math language, how bout dem symbols?
    '("\\([<>~]=?\\|\\.[/*^']\\|==\\|\\<xor\\>\\|[-!^&|*+\\/~:]\\)"
      1 font-lock-type-face)
    '("[]A-Za-z0-9_\"})']\\('+\\)" 1 font-lock-type-face)
    ;; How about references in the HELP text.
    (list (concat "^" matlab-comment-line-s "\\s-*"
		  "\\(\\([A-Z]+\\s-*=\\s-+\\|\\[[^]]+]\\s-*=\\s-+\\|\\)"
		  "\\([A-Z][0-9A-Z]+\\)\\(([^)\n]+)\\| \\)\\)")
	  '(1 font-lock-reference-face prepend))
    (list (concat "^" matlab-comment-line-s "\\s-*"
		  "See also\\s-+")
	  '("\\([A-Z][A-Z0-9]+\\)\\([,.]\\| and\\|$\\) *" nil  nil
	    (1 font-lock-reference-face prepend)))
    (list (concat "^" matlab-comment-line-s "\\s-*"
		  "\\(\\$" "Revision" "[^\n$]+\\$\\)")
	  '(1 font-lock-reference-face prepend))
    ;; Debugging Keywords
    (list (matlab-font-lock-regexp-opt matlab-debug-list)
	  '(0 'bold))
    ;; Simulink functions
    (list (matlab-font-lock-regexp-opt matlab-simulink-keywords)
	;;(list (list (concat "\\<\\(\\([sg]et_param\\|sim\\([gs]et\\)?\\|"
	;;		    "\\(mld\\|ss\\)[A-Z]\\w+\\)\\|"
	;;		    "\\(new\\|open\\|close\\|save\\|find\\)_system\\|"
	;;		    "\\(add\\|delete\\|replace\\)_\\(block\\|line\\)\\|"
	;;		    "simulink\\|bd\\(root\\|close\\)"
	;;		    "\\)\\>")
	      1 matlab-simulink-keyword-face)
    ))
  "Expressions to highlight in MATLAB mode.")

(defconst matlab-file-really-gaudy-font-lock-keywords
  (append
   matlab-file-gaudy-font-lock-keywords
   matlab-really-gaudy-font-lock-keywords
   )
  "Expressions to highlight in MATLAB mode.")

;; Imenu support.
(defvar matlab-imenu-generic-expression
  '((nil "^\\s-*function\\>[ \t\n.]*\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*\
< =[ \t\n.]*\\)?\\([a-zA-Z0-9_]+\\)" 3))
  "Expressions which find function headings in MATLAB M files.")


;;; MATLAB mode entry point ==================================================

;; Choose matlab-mode if when loading MATLAB *.m files
;; See "How Emacs Chooses a Major Mode"
;;    https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html

;;;###autoload
(defun matlab-is-matlab-file ()
  "Enter `matlab-mode' when file content looks like a MATLAB *.m
file or for empty files *.m files when `matlab-mode-for-new-mfiles'
indicates as such."
  (and buffer-file-name ;; have a file?
       ;; AND a valid MATLAB file name
       (string-match
        "^\\(?:.*/\\)?[a-zA-Z][a-zA-Z0-9_]*\\.m\\'"  ;; /path/to/file.m ?
        (file-name-sans-versions
         (if (and (boundp 'archive-subfile-mode) archive-subfile-mode)
             (aref archive-subfile-mode 0)   ;; Will just be file.m without the directory
           buffer-file-name)))
       ;; AND (have MATLAB code OR an empty file that should enter matlab-mode)
       (or
        ;; Is content MATLAB code? We can definitely identify *some* MATLAB content using
        ;;    (looking-at "^[[:space:]\n]*\\(%\\|function\\|classdef\\)")
        ;; i.e. '%', '%{' comments, or function/classdef start, but this fails to find MATLAB
        ;; scripts. Thus, if buffer is NOT Objective-C and has something in it, we assume MATLAB.
        ;; Objective-c is identified by
        ;;   - comment start chars: // or /*,
        ;;   - # char (as in #import)
        ;;   - @ char (as in @interface)
        ;; MATLAB scripts are identified by the start of a valid identifier, i.e. a letter or
        ;; some math operation, e.g. [1,2,3]*[1,2,3]', thus all we really need to look for
        ;; is a non-whitespace character which could be a MATLAB comment, generic MATLAB commands,
        ;; function/classdef, etc.
        (and (not (looking-at "^[[:space:]\n]*\\(//\\|/\\*\\|#\\|@\\)"))
             (looking-at "^[[:space:]\n]*[^[:space:]\n]"))
        ;; Empty file - enter matlab-mode based on `matlab-mode-for-new-mfiles' setting
        (and (= (buffer-size) 0)
             (or (equal matlab-mode-for-new-mfiles t)
                 (and (equal matlab-mode-for-new-mfiles 'maybe)
                      ;; Enter matlab-mode if we already have a buffer in matlab-mode
                      (let ((buffers (buffer-list))
                            enter-matlab-mode)
                        (while buffers
                          (with-current-buffer (car buffers)
                            (when (or (eq major-mode 'matlab-mode)
                                      (eq major-mode 'matlab-shell-mode))
                              (setq enter-matlab-mode t)
                              (setq buffers nil)))
                          (setq buffers (cdr buffers)))
                        enter-matlab-mode)))))))

;;;###autoload
(add-to-list 'magic-mode-alist '(matlab-is-matlab-file . matlab-mode))

(defvar mlint-minor-mode)
(declare-function mlint-minor-mode "mlint.el")
(declare-function mlint-buffer "mlint.el")
(declare-function mlint-clear-warnings "mlint.el")
(declare-function mlint-clear-cross-function-variable-highlighting "mlint.el")
(defvar show-paren-data-function)

(defun matlab-mode-leave ()
  "When leaving `matlab-mode', turn off `mlint-minor-mode'"
  (when (eq major-mode 'matlab-mode)
    (mlint-minor-mode -1)
    (matlab-scan-disable)
    ))

;;;###autoload
(define-derived-mode matlab-mode prog-mode "MATLAB"
  "MATLAB(R) mode is a major mode for editing MATLAB dot-m files.
\\<matlab-mode-map>
Convenient editing commands are:
 \\[matlab-comment-region]   - Comment/Uncomment out a region of code.
 \\[matlab-fill-comment-line] - Fill the current comment line.
 \\[matlab-fill-region] - Fill code and comments in region.
 \\[matlab-complete-symbol]   - Symbol completion of matlab symbols\
based on the local syntax.
 \\[matlab-indent-sexp] - Indent syntactic block of code.

Convenient navigation commands are:
 \\[matlab-beginning-of-command]   - Move to the beginning of a command.
 \\[matlab-end-of-command]   - Move to the end of a command.
 \\[matlab-beginning-of-defun] - Move to the beginning of a function.
 \\[matlab-end-of-defun] - Move do the end of a function.
 \\[matlab-forward-sexp] - Move forward over a syntactic block of code.
 \\[matlab-backward-sexp] - Move backwards over a syntactic block of code.

Convenient template insertion commands:
 \\[tempo-template-matlab-function] - Insert a function definition.
 \\[tempo-template-matlab-if] - Insert an IF END block.
 \\[tempo-template-matlab-for] - Insert a FOR END block.
 \\[tempo-template-matlab-switch] - Insert a SWITCH END statement.
 \\[matlab-insert-next-case] - Insert the next CASE condition in a SWITCH.
 \\[matlab-insert-end-block] - Insert a matched END statement.  With \
optional ARG, reindent.
 \\[matlab-stringify-region] - Convert plain text in region to a string \
with correctly quoted chars.

Variables:
  `matlab-indent-level'		Level to indent blocks.
  `matlab-continuation-indent-level' Level to indent after ... continuation
  `matlab-case-indent-level'		Level to unindent case statements.
  `matlab-indent-past-arg1-functions'
                                Regexp of functions to indent past the first
                                  argument on continuation lines.
  `matlab-maximum-indents'      List of maximum indents during lineups.
  `matlab-comment-column'       Goal column for on-line comments.
  `fill-column'			Column used in auto-fill.
  `matlab-indent-function-body' If non-nil, indents body of MATLAB functions.
  `matlab-functions-have-end'	If non-nil, MATLAB functions terminate with end.
  `matlab-return-function'	Customize RET handling with this function.
  `matlab-fill-code'            Non-nil, auto-fill code in auto-fill-mode.
  `matlab-fill-strings'         Non-nil, auto-fill strings in auto-fill-mode.
  `matlab-verify-on-save-flag'  Non-nil, enable code checks on save.
  `matlab-vers-on-startup'	If t, show version on start-up.
  `matlab-handle-simulink'      If t, enable simulink keyword highlighting.

All Key Bindings:
\\{matlab-mode-map}"
  :after-hook (matlab-mode-init-mlint-if-needed)

  (kill-all-local-variables)
  (use-local-map matlab-mode-map)
  (setq major-mode 'matlab-mode)
  (setq mode-name "MATLAB")
  (if (boundp 'whitespace-modes)
      (add-to-list 'whitespace-modes 'matlab-mode))
  (setq local-abbrev-table matlab-mode-abbrev-table)

  ;; Syntax tables and related features are in matlab-syntax.el
  ;; This includes syntax table definitions, misc syntax regexps
  ;; and font-lock for comments/strings.
  (matlab-syntax-setup)
  (matlab-scan-setup)
  
  ;; Indentation setup.
  (setq indent-tabs-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'matlab-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'matlab-indent-region)
  (make-local-variable 'comment-column)
  (setq comment-column matlab-comment-column)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'matlab-comment-indent)

  (make-local-variable 'add-log-current-defun-function)
  (setq add-log-current-defun-function 'matlab-current-defun)
  ;; Emacs 20 supports this variable.
  ;; This lets users turn auto-fill on and off and still get the right
  ;; fill function.
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'matlab-auto-fill)
  (make-local-variable 'fill-column)
  (setq fill-column matlab-fill-column)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'matlab-fill-paragraph)
  (make-local-variable 'fill-prefix)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression matlab-imenu-generic-expression)

  ;; Save hook for verifying src.  This lets us change the name of
  ;; the function in `write-file' and have the change be saved.
  ;; It also lets us fix mistakes before a `save-and-go'.
  (make-local-variable 'write-contents-functions)
  (add-hook 'write-contents-functions 'matlab-mode-verify-fix-file-fn)

  ;; give each file it's own parameter history
  (make-local-variable 'matlab-shell-save-and-go-history)

  ;; Font lock support:
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-file-font-lock-keywords
			      matlab-file-gaudy-font-lock-keywords
			      matlab-file-really-gaudy-font-lock-keywords
			      )
			     nil ; matlab-syntax supports comments and strings.
			     nil ; keywords are case sensitive.
			     ;; This puts _ as a word constituent,
			     ;; simplifying our keywords significantly
			     ((?_ . "w"))))
  (setq font-lock-multiline 'undecided)
  (add-to-list 'font-lock-extend-region-functions #'matlab-font-lock-extend-region t)
  
  (if window-system (matlab-frame-init))

  ;; If first function is terminated with an end statement, then functions have
  ;; ends.
  (if (matlab-do-functions-have-end-p)
      ;; minor mode now treat's 'guess' as true when passing in 1.
      (matlab-functions-have-end-minor-mode 1)
    (matlab-functions-have-end-minor-mode -1))

  ;; When matlab-indent-function-body is set to 'MathWorks-Standard,
  ;;    - we indent all functions that terminate with an end statement
  ;;    - old style functions (those without end statements) are not
  ;;      indented.
  ;; It is desired that all code be terminate with an end statement.
  ;;
  ;; When matlab-indent-function-body is set to 'guess,
  ;;    - look at the first line of code and if indented, keep indentation
  ;;      otherwise use MathWorks-Standard
  ;;
  (cond
   ((eq matlab-indent-function-body 'MathWorks-Standard)
    )

   ((eq matlab-indent-function-body 'guess)
    (save-excursion
      (goto-char (point-max))

      (if (re-search-backward matlab-defun-regex nil t)
	  (let ((beg (point))
		end			; filled in later
		(cc (current-column))
		)
	    (setq end (if matlab-functions-have-end
			  (progn (forward-line 0) (point))
			(point-max)))
	    (goto-char beg)
	    (catch 'done
	      (while (progn (forward-line 1) (< (point) end))
		(if (looking-at "\\s-*\\(%\\|$\\)")
		    nil			; go on to next line
		  (looking-at "\\s-*")
		  (goto-char (match-end 0))
		  (setq matlab-indent-function-body (> (current-column) cc))
		  (throw 'done nil))))
	    )
	(setq matlab-indent-function-body 'MathWorks-Standard)
	))
    )

   (t)
   )

  ;; When leaving matlab-mode, turn off mlint
  (add-hook 'change-major-mode-hook #'matlab-mode-leave)

  )

(defun matlab-mode-init-mlint-if-needed ()
  "Check if we should start `mlint-minor-mode' for this buffer."
  ;; Check to see if the user asked for any features that need mlint.
  (if (and (or (not (boundp 'mlint-minor-mode))
	       (not mlint-minor-mode))	; prevent double init
	   (or matlab-show-mlint-warnings
	       matlab-highlight-cross-function-variables)) ; check settings for need
      ;; Some users may not feel like getting all the extra stuff
      ;; needed for mlint working.  Do this only if we can get
      ;; mlint loaded ok.
      (condition-case nil
	  (mlint-minor-mode
	   (if (or matlab-show-mlint-warnings matlab-highlight-cross-function-variables)
	       1
	     0))

	;; If there is an error loading the stuff, don't
	;; continue.
	(error nil))))


;; Support debug mode and read only toggling.
(defvar gud-matlab-debug-active nil)
(declare-function matlab-shell-gud-minor-mode "matlab-shell-gud")

(defun matlab-toggle-read-only (&optional arg interactive)
  "Toggle read-only bit in MATLAB mode.
This looks to see if we are currently debugging, and if so re-enable
our debugging feature.
Optional argument ARG specifies if the read-only mode should be set.
INTERACTIVE is ignored."
  (interactive "P")
  (if (and (featurep 'matlab-shell-gud)
	   gud-matlab-debug-active)
      ;; The debugging is active, just re-enable debugging read-only-mode
      (matlab-shell-gud-minor-mode 1)
    ;; Else - it is not - probably doing something else.
    (call-interactively 'read-only-mode)
    ))


;;; Utilities =================================================================

(defun matlab-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "matlab-mode, version %s" matlab-mode-version))

(defun matlab-find-prev-code-line ()
  "Navigate backward until a code line is found.
Navigate across continuations until we are at the beginning of
that command.
Return t on success, nil if we couldn't navigate backwards."
  (let ((ans (matlab-find-prev-line 'ignore-comments)))
    (when ans
      (matlab-scan-beginning-of-command)
      ans)))

(defun matlab-find-prev-line (&optional ignorecomments)
  "Recurse backwards until a code line is found."
  (if ignorecomments
      ;; This version is now super easy, as this built-in
      ;; skips comments and whitespace.  Nil on bobp.
      (progn
	(beginning-of-line)
	(forward-comment -100000)
	(not (bobp)))
    ;; Else, scan backward at least 1 step.  nil if bob
    (if (= -1 (forward-line -1)) nil
      ;; Now scan backwards iteratively
      (catch 'moose
	(let ((lvl1 (matlab-compute-line-context 1)))
	  (while (or (matlab-line-empty-p lvl1)
		     (matlab-line-comment-ignore-p lvl1)
		     )
	    (when (= -1 (forward-line -1))
	      (throw 'moose nil))))
	t))))

(defun matlab-prev-line ()
  "Go to the previous line of code or comment.  Return nil if not found."
  (interactive)
  (let ((old-point (point)))
    (if (matlab-find-prev-line) t (goto-char old-point) nil)))

(defun matlab-find-code-line ()
  "Walk forwards until we are on a line of code return t on success.
If the currnet line is code, return immediately.
Ignore comments and whitespace."
  (forward-comment 100000)
  (not (eobp)))

(defun matlab-valid-end-construct-p ()
  "Return non-nil if the end after point terminates a block.
Return nil if it is being used to dereference an array."
  (if (eq (preceding-char) ?.)
      ;; This is a struct field, not valid.
      nil
    (let ((pps (syntax-ppss (point))))
      ;; If we are in a set of parenthisis, then not valid b/c it is
      ;; likely an array reference.  Valid == 0 paren depth.
      (= (nth 0 pps) 0))))


;;; Regexps for MATLAB language ===============================================

;; "-pre" means "partial regular expression"
;; "-if" and "-no-if" means "[no] Indent Function"

(defconst matlab-defun-regex "^\\s-*\\(function\\|classdef\\)[ \t.[]"
  "Regular expression defining the beginning of a MATLAB function.")

(defconst matlab-mcos-innerblock-regexp "properties\\|methods\\|events\\|enumeration\\|arguments"
  "Keywords which mark the beginning of mcos blocks.
These keywords can be overriden as variables or functions in other contexts
asside from that which they declare their content.")

(defconst matlab-mcos-regexp (concat "\\|classdef\\|" matlab-mcos-innerblock-regexp)
  "Keywords which mark the beginning of mcos blocks.")

(defconst matlab-block-beg-pre-if
  (concat "function\\|parfor\\|spmd\\|for\\|while\\|if\\|switch\\|try"
	  matlab-mcos-regexp)
  "Keywords which mark the beginning of an indented block.
Includes function.")

(defconst matlab-block-beg-pre-no-if
  (concat "parfor\\|for\\|spmd\\|while\\|if\\|switch\\|try"
	  matlab-mcos-regexp)
  "Keywords which mark the beginning of an indented block.
Excludes function.")

(defun matlab-block-beg-pre ()
  "Partial regular expression to recognize MATLAB block-begin keywords."
  (if matlab-functions-have-end
      matlab-block-beg-pre-if
    matlab-block-beg-pre-no-if))

(defun matlab-match-function-re ()
  "Expression to match a function start line.
There are no reliable numeric matches in this expression.
Know that `match-end' of 0 is the end of the function name."
  ;; old function was too unstable.
  ;;"\\(^function\\s-+\\)\\([^=\n]+=[ \t\n.]*\\)?\\(\\sw+\\)"
  (concat "\\(^\\s-*function\\b[ \t\n.]*\\)\\(\\(\\[[^]]*\\]\\|\\sw+\\)"
	  "[ \t\n.]*=[ \t\n.]*\\|\\(\\)\\)\\(\\sw+\\)"))

(defun matlab-match-classdef-re ()
  "Expression to match a classdef start line.
The class name is match 2."
  "\\(^\\s-*classdef\\b[ \t\n]*\\)\\(\\sw+\\)\\(\\s-*<\\)?")

;;; Navigation ===============================================================

(defvar matlab-scan-on-screen-only nil
  "When this is set to non-nil, then forward/backward sexp stops off screen.
This is so the block highlighter doesn't gobble up lots of time when
a block is not terminated.")

(defun matlab-backward-sexp (&optional autoend noerror)
  "Go backwards one balanced set of MATLAB expressions.
If optional AUTOEND, then pretend we are at an end.
If optional NOERROR, then we return t on success, and nil on failure.
This assumes that expressions do not cross \"function\" at the left margin."
  (interactive "P")
  (let ((p (point))
	(returnme t)
	keyword)
    (save-excursion
      (skip-syntax-backward " .><")
      (cond
       ;; Auto end - Just go!
       (autoend
	(when (matlab--scan-block-backward-up nil)
	  (if noerror
	      (setq returnme nil)
	    (error "Unstarted END")))
	)

       ;; No auto-end ....
       
       ;; End of a block comment
       ((matlab-ltype-block-comment-end)
	(beginning-of-line)
	(matlab-beginning-of-string-or-comment))

       ((or (not (setq keyword (matlab-on-keyword-p)))
	    (memq keyword '(decl ctrl mcos arg vardecl keyword)))
	;; Just walk over block starts and other random stuff.
	(matlab-move-simple-sexp-internal -1))

       ((memq keyword '(mid case))
	;; If we're on a middle, then assume we're in the middle
	;; of something and keep going.
	(when (matlab--scan-block-backward-up nil)
	  (if noerror
	      (setq returnme nil)
	    (error "Unstarted END")))
	)

       (t
	(when (matlab--scan-block-backward nil)
	  (if noerror
	      (setq returnme nil)
	    (error "Unstarted END")))
	)
       )

      (when returnme
	(setq p (point))))
    (goto-char p)
    returnme))

(defun matlab-forward-sexp (&optional autostart parentblock)
  "Go forward one balanced set of MATLAB expressions.
If AUTOSTART is non-nil, assume we are already inside a block, and navigate
forward until we exit that block.
PARENTBLOCK is used when recursing to validate block starts as being in
a valid context."
  (interactive "P")
  (let (p keyword)  ;; go to here if no error.
    (save-excursion ;; Don't move if there is an error
      ;; skip over preceding whitespace
      (skip-syntax-forward " .><")
      (cond
       ;; Auto start - just go!
       (autostart
	(when (matlab--scan-block-forward-up nil)
	  (error "Unterminated Block")
	  ))

	;; No Autostart ....

	;; Looking at a block comment.
	((and (not autostart)
	      (looking-at "%"))
	 (goto-char (match-end 0))
	 (matlab-end-of-string-or-comment))

	((or (not (setq keyword (matlab-on-keyword-p)))
	     (memq keyword '(end vardecl keyword)))
	 ;; Just walk over ends and other random stuff.
	 (matlab-move-simple-sexp-internal 1))

	((memq keyword '(mid case))
	 ;; If we're on a middle, then assume we're in the middle
	 ;; of something and keep going.
	 (when (matlab--scan-block-forward-up nil)
	   (error "Unterminated Block"))
	 )
	 
	(t
	 (when (matlab--scan-block-forward nil nil)
	   (error "Unterminated Block"))
	 )
	)
      (setq p (point)))
    (goto-char p)))

(defun matlab-indent-sexp ()
  "Indent the syntactic block starting at point."
  (interactive)
  (indent-region (point) (save-excursion (matlab-forward-sexp) (point)) nil))

(defun matlab-beginning-of-defun ()
  "Go to the beginning of the current function."
  (interactive)
  (if (eq (matlab-on-keyword-p) 'end)
      (progn
	;; If we are ON an end an it matches a decl,
	;; that is probably what was indented.
	(matlab--scan-block-backward)
	(if (eq (matlab-on-keyword-p) 'decl)
	    nil ; done
	  (matlab--scan-block-backward-up-until 'decl)))
    ;; else, just do a scan.
    (matlab--scan-block-backward-up-until 'decl)))

(defun matlab-end-of-defun ()
  "Go to the end of the current function."
  (interactive)
  (when (not (eq (matlab-on-keyword-p) 'decl))
    (matlab--scan-block-backward-up-until 'decl))
  (matlab--scan-block-forward))

(defun matlab-current-defun ()
  "Return the name of the current function."
  (save-excursion
    (matlab-beginning-of-defun)
    (if (looking-at (matlab-match-function-re))
	(progn
	  (goto-char (match-end 0))
	  (current-word)))))

(defun matlab-beginning-of-command ()
  "Go to the beginning of an M command.
Travels across continuations."
  (interactive "P")
  (matlab-scan-beginning-of-command))

(defun matlab-end-of-command ()
  "Go to the end of an M command.
Travells a cross continuations"
  (interactive)
  (matlab-scan-end-of-command))


;;; Line types, attributes, and string/comment context ====================================

(defun matlab-line-continued-comment ()
  "Return column of previous line's comment start, or nil."
  (save-excursion
    (beginning-of-line)
    (let* ((lvl (matlab-compute-line-context 1)))
      (if (or (null (matlab-line-regular-comment-p lvl))
	      (bobp))
	  nil
	;; We use forward-line and not matlab-prev-line because
	;; we want blank lines to terminate this indentation method.
	(forward-line -1)
	(matlab-line-end-comment-column (matlab-compute-line-context 1))))))

(defun matlab-line-count-open-blocks (&optional eol)
  "Return a the number of unterminated block constructs.
This is any block, such as if or for, that doesn't have an END on this line.
Optional EOL indicates a virtual end of line."
  (save-excursion
    (let* ((v 0)
	   (lvl (matlab-compute-line-context 1))
	   (bound (or eol (save-excursion (matlab-line-end-of-code lvl))))
	   (keyword nil))
      
      (if (or (matlab-line-comment-p lvl)
	      (matlab-line-empty-p lvl))
	  ;; If this line is comments or empty, no code to scan
	  0
	(back-to-indentation)
	(while (setq keyword
		     (matlab-re-search-keyword-forward (matlab-keyword-regex 'indent) bound t))
	  (if (or (and (eq (car keyword) 'mcos)
		       (not (matlab--valid-mcos-keyword-point nil)))
		  (and (eq (car keyword) 'args)
		       (not (matlab--valid-arguments-keyword-point nil))))
	      ;; Not valid, skip it.
	      nil
	    ;; Increment counter, move to end.
	    (setq v (1+ v))
	    (let ((p (point)))
	      (forward-word -1)
	      (if (matlab--scan-block-forward bound)
		  (goto-char p)
		(setq v (1- v))))))
	v))))

(defun matlab-line-count-closed-blocks (&optional start)
  "Return the number of closing block constructs on this line.
Argument START is where to start searching from."
  (save-excursion
    (when start (goto-char start))
    (let ((v 0)
	  (lvl1 (matlab-compute-line-context 1))
	  (bound (save-excursion (matlab-scan-beginning-of-command))))
      
      (if (matlab-line-comment-p lvl1)
	  ;; If this is even vagely a comment line, then there is no
	  ;; need to do any scanning.
	  0
	;; Else, lets scan.
	;; lets only scan from the beginning of the comment
	(goto-char start)
	(matlab-line-end-of-code lvl1)

	;; Count every END from our starting point till the beginning of the
	;; command.  That count indicates unindent from the beginning of the
	;; command which anchors the starting indent.
	(while (matlab-re-search-keyword-backward (matlab-keyword-regex 'end) bound t)
	  (let ((startmove (match-end 0))
		(nomove (point)))
	    ;; Lets count these end constructs.
	    (setq v (1+ v))
	    (if (matlab--scan-block-backward bound) ;;(matlab-backward-sexp t t)
		(goto-char nomove)
	      (setq v (1- v)))
	    ))
	(if (<= v 0) 0 v)))))

(defun matlab-function-called-at-point ()
  "Return a string representing the function called nearby point."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "\\s-*\\([a-zA-Z]\\w+\\)[^=][^=]")
	   (match-string 1))
	  ((and (re-search-forward "=" (matlab-point-at-eol) t)
		(looking-at "\\s-*\\([a-zA-Z]\\w+\\)\\s-*[^=]"))
	   (match-string 1))
	  (t nil))))


(defun matlab-comment-on-line ()
  "Place the cursor on the beginning of a valid comment on this line.
If there isn't one, then return nil, point otherwise."
  (interactive)
  (let ((lvl1 (matlab-compute-line-context 1)))
    (goto-char (or (matlab-line-end-comment-point lvl1)
		   (point)))))

;;; Indent functions ==========================================================
;;

(defun matlab-indent-region (start end &optional column noprogress)
  "Indent the region between START And END for MATLAB mode.
Unlike `indent-region-line-by-line', this function captures
parsing state and re-uses that state along the way."
  (interactive)
  (matlab-navigation-syntax
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (let ((pr (when (and (not (minibufferp)) (not noprogress))
                  (make-progress-reporter "MATLAB Indenting region..." (point) end)))
	    (lvl2 nil)
	    (lvl1 nil)
	    )
	(while (< (point) end)
          (unless (and (bolp) (eolp))
	    ;; This is where we indent each line
	    (setq lvl1 (matlab-compute-line-context 1)
		  lvl2 (matlab-compute-line-context 2 lvl1));; lvl2))
	    (when (matlab--indent-line lvl2)
	      ;; If the indent changed something, refresh this
	      ;; context obj.
	      ;;(matlab-refresh-line-context-lvl2 lvl2)
	      ))
          (forward-line 1)
          (and pr (progress-reporter-update pr (point))))
	(and pr (progress-reporter-done pr))
	(move-marker end nil)))))


(defun matlab-indent-line ()
  "Indent a line in `matlab-mode'."
  (interactive)
  (matlab-navigation-syntax
    (let ((lvl2 (matlab-compute-line-context 2)))
      (matlab--indent-line lvl2))))

(defvar matlab--change-indentation-override #'matlab--change-indentation
  "Tests to override this to validate indent-region.")

(defun matlab--indent-line (lvl2)
  "Indent the current line according to MATLAB mode.
Input LVL2 is a pre-scanned context from `matlab-compute-line-context' lvl2.
Used internally by `matlab-indent-line', and `matlab-indent-region'."
  (let* ((i (matlab--calc-indent lvl2)))
    (funcall matlab--change-indentation-override i)))

(defun matlab--change-indentation (new-indentation)
  "Change the indentation on line to NEW-INDENTATION.
This function exists so the test harness can override it."
  (let* ((i (max new-indentation 0))
	 (ci (current-indentation))
	 (cc (current-column))
	 (diff (- ci i)))
    (save-excursion
      (back-to-indentation)
      (cond ((= diff 0) ;; Already a match - do nothing.
	     nil)
	    ((< diff 0) ;; Too short - Add stuff
	     (indent-to i))
	    ((<= diff ci) ;; Too much, delete some.
	     (delete-region (save-excursion (move-to-column i t) (point)) (point))
	     )
	    (t ;; some sort of bug that wants to delete too much. Ignore.
	     nil)
	    ))
    (if (<= cc ci) (move-to-column (max 0 i)))
    (/= 0 diff)))

(defun matlab--calc-indent (&optional lvl2 debug-sym)
  "Return the appropriate indentation for this line as an integer."
  ;; In case it wasn't provided.
  (unless lvl2 (setq lvl2 (matlab-compute-line-context 2)))
  ;; The first step is to find the current indentation.
  ;; This is defined to be zero if all previous lines are empty.
  (let* ((sem (matlab-calculate-indentation lvl2)))
    (when debug-sym
      (set debug-sym sem))
    ;; simplistic
    (nth 1 sem)))

(defun matlab--previous-line-indent-recommendation (lvl2)
  "Return the indentation recommendation from the previous line of CODE.
Uses the lvl2 context of the current line of code it scans backward from.
This function scans backward over blank lines and comments to find a
line of code.  It then scans that line and recommends either:
  - same indentation - if just boring old code.
  - indent more - if has control block openings on it."
  (cond
   ((save-excursion (beginning-of-line) (bobp))
    ;; Beginning of buffer - do no work, just return 0.
    0)
   ((matlab-line-in-array lvl2)
    ;; If we are inside an array continuation, then we shouldn't
    ;; need to do anything complicated here b/c we'll just ignore
    ;; the returned value in the next step.  Return current indentation
    ;; of the previous non-empty line.
    (matlab-line-indentation (matlab-previous-nonempty-line lvl2)))
   (t
    ;; Else, the previous line might recommend an indentation based
    ;; on it's own context, like being a block open or continuation.
    (let ((prevcmd (or (matlab-previous-code-line lvl2)
		       (matlab-previous-line lvl2))))
      (matlab-with-context-line prevcmd
	(matlab-next-line-indentation prevcmd))))))
  

(defconst matlab-functions-have-end-should-be-true
  "This end closes a function definition.\nDo you want functions to have ends? "
  "Prompt the user about whether to change `matlab-functions-have-end'.")


(defun matlab--maybe-yes-or-no-p (prompt noninteractive-default)
  "When in non-interactive mode run (yes-or-no-p prompt),
otherwise return NONINTERACTIVE-DEFAULT"
  (if noninteractive
      noninteractive-default
    (yes-or-no-p prompt)))


(defun matlab-calculate-indentation (&optional lvl2)
  "Calculate out the indentation of the current line.
Return a list of descriptions for this line.  Return format is:
 '(TYPE DEPTHNUMBER)
where TYPE is one of (comment, code, function, blockstart, blockmid,
blockendless, blockend) DEPTHNUMBER is how many characters to indent
this line."
  (unless lvl2 (setq lvl2 (matlab-compute-line-context 2)))
  
  (let ((lvl1 (matlab-get-lvl1-from-lvl2 lvl2))
	(tmp nil))
    (cond
     ;; COMMENTS
     ((matlab-line-comment-p lvl1)
      (let ((comment-style (matlab-line-comment-style lvl1)))
	(cond
	 ;; BLOCK END undoes body indent
	 ((or (eq comment-style 'block-end)
	      (eq comment-style 'block-body-prefix)) ; body prefix has same lineup rule
	  (list 'comment (matlab-line-end-comment-column lvl1)))
	 ;; BLOCK BODY is indented slightly from the start.
	 ((eq comment-style 'block-body)
	  (list 'comment (+ 2 (matlab-line-end-comment-column lvl1))))
	 ;; HELP COMMENT and COMMENT REGION
	 ((and (matlab-last-guess-decl-p)
	       (setq tmp (matlab-scan-comment-help-p lvl2)))
	  (list 'comment-help tmp))
	 ;; BLOCK START is like regular comment
	 ((eq comment-style 'block-start)
	  ;; indent like code, but some users like anti-indent
	  (list 'comment (+ (matlab--previous-line-indent-recommendation lvl2) matlab-comment-anti-indent))
	  )
	 ;; COMMENT REGION comments
	 ((matlab-line-comment-ignore-p lvl1)
	  (list 'comment-ignore 0))
	 ;; COMMENT Continued From Previous Line
	 ((setq tmp (matlab-line-continued-comment)) ;;; TODO : REPLACE
	  (list 'comment tmp))
	 (t
	  (list 'comment (+ (matlab--previous-line-indent-recommendation lvl2) matlab-comment-anti-indent))))))
     
     ;; FUNCTION DEFINITION
     ((matlab-line-declaration-p lvl1)
      (cond ((not matlab-functions-have-end)
	     (setq tmp 0))
	    ;; If we do have ends, check if we are nexted
            ;; A function line has intrinsic indentation iff function bodies are
            ;; not indented and the function line is nested within another function.
	    ((and (not (matlab-indent-function-body-p))
                  (save-excursion
                    (beginning-of-line)
		    ;; TODO - maybe replace this? Not usually used.
                    (matlab--scan-block-backward-up-until 'decl)))
             (setq tmp (+ (matlab--previous-line-indent-recommendation lvl2) matlab-indent-level)))
	    (t
             ;; If no intrinsic indentation, do not change from current-indentation.
	     (setq tmp (matlab--previous-line-indent-recommendation lvl2)))
            )
      (list 'function tmp))
     
     ;; END keyword
     ((matlab-line-end-p lvl1)
      (let* ((CTXT (matlab-with-context-line lvl1
		     (matlab-scan-block-start-context))))

        (if (eq (car CTXT) 'decl) ;; declarations (ie - function) is treated special.
            (if (or matlab-functions-have-end
                    (if (matlab--maybe-yes-or-no-p matlab-functions-have-end-should-be-true t)
                        (matlab-functions-have-end-minor-mode 1)
                      (error "Unmatched end")))
		
                (if (matlab-indent-function-body-p)
		    ;; Match indentation of the function regardless of any other
		    ;; state that might have gotten messed up.
		    (setq tmp (matlab-line-indentation (nth 3 CTXT)))
		  
		  ;; Else, no change
		  (setq tmp (matlab--previous-line-indent-recommendation lvl2))
		  ))
	  
	  ;; Not a declaration.  In that case, just match up with the
	  ;; line that the block stat is on.
	  (setq tmp (matlab-line-indentation (nth 3 CTXT)))))
      (list 'blockend tmp))
     
     ;; ELSE/CATCH keywords
     ((matlab-line-block-middle-p lvl1)
      (list 'blockmid
	    (save-excursion
	      (back-to-indentation)
	      (if (matlab--scan-block-backward-up)
		  (error "Missing start block")
		(if (not (eq (matlab-on-keyword-p) 'ctrl))
		    (error "Does not match opening block type"))
		(current-column)))))
     
     ;; CASE/OTHERWISE keywords
     ((matlab-line-block-case-p lvl1)
      (list 'blockendless
	    (save-excursion
	      (back-to-indentation)
	      (if (matlab--scan-block-backward-up)
		  (error "Missing switch")
		(if (not (eq (matlab-on-keyword-p) 'ctrl))
		    (error "Wrong type of start block")
		  (+ (current-column)
		     (if (listp matlab-case-indent-level)
			 (car matlab-case-indent-level)
		       matlab-case-indent-level)))))))
     
     ;; END of a MATRIX
     ((matlab-line-close-paren-p lvl1)
      (list 'array-end (let* ((fc (matlab-line-close-paren-inner-char lvl1))
			      (pc (matlab-line-close-paren-inner-col lvl1))
			      (mi (assoc fc matlab-maximum-indents))
			      (max (if mi (if (listp (cdr mi))
					      (car (cdr mi)) (cdr mi))
				     nil))
			      (ind (if mi (if (listp (cdr mi))
					      (cdr (cdr mi)) (cdr mi))
				     nil)))
			 ;; apply the maximum limits.
			 (if (and ind (> (- pc (matlab--previous-line-indent-recommendation lvl2)) max))
			     (1- ind)	; decor
			   pc))))
     
     ;; CODE LINES
     ((and (not (matlab-line-close-paren-outer-point lvl1))
	   (not (matlab-scan-previous-line-ellipsis-p)))
      ;; Code always matches up against the previous line.
      (list 'code (matlab--previous-line-indent-recommendation lvl2)))
     
     ;; CONTINUATION : A group of cases for continuation
     (t
      (let* ((boc-lvl1 (save-excursion
			 (matlab-scan-beginning-of-command)
			 (matlab-compute-line-context 1)))
	     (ci-boc (matlab-line-indentation boc-lvl1))
	     (boc (matlab-line-point boc-lvl1))

	     ;; Scratch vars for paren stuff
	     (parencol (matlab-line-close-paren-inner-col lvl1))
	     (parenchar (matlab-line-close-paren-inner-char lvl1))
	     (parenpt (matlab-line-close-paren-inner-point lvl1))
	     (parenindent (when parenpt
			    (save-excursion (goto-char parenpt)
					    (current-indentation))))
	     (parenopt (matlab-line-close-paren-outer-point lvl1))

	     
	     ;; What shall we use to describe this for debugging?
	     (indent-type (cond ((matlab-line-empty-p lvl1) 'empty)
				((and parenchar (= parenchar ?\()) 'function-call-cont)
				((and parencol (= parenindent parencol)) 'array-solo-cont)
				((and parenpt (/= parenpt parenopt)) 'nested-array-cont)
				(parenpt 'array-cont)
				(t 'code-cont)))
	     
	     (found-column nil)
	     )
	
	(save-excursion
	  (cond
	   ((and
	     ;; CONTINUATION with FUNCTIONs that indent past arg1
	     (eq indent-type 'function-call-cont)
	     ;; This checks for our special set of functions.
	     (save-excursion
	       (goto-char parenpt)
	       (forward-symbol -1)
	       (looking-at
		matlab-indent-past-arg1-functions))
	     ;; We are in a fcn call, AND the fcn wants to
	     ;; indent past the first argument.  Only do so
	     ;; if first arg is a SIMPLE EXPR.
	     (matlab-navigation-syntax
	       (goto-char parenpt)
	       (looking-at "(\\s-*\\w+\\s-*,")
	       (setq found-column (match-end 0)))
	     (save-excursion
	       (goto-char found-column) ; move to comma
	       ;; Don't bother if we hit the EOL.
	       (not (looking-at "\\s-*\\(\\.\\.\\.\\|$\\|)\\)"))))
	    ;; We are in the right kind of place.  Lets
	    ;; start indenting
	    (goto-char found-column)
	    (skip-chars-forward " \t")
	    (if (> (- (current-column) parencol)
		   matlab-arg1-max-indent-length)
		(setq tmp (+ parencol matlab-arg1-max-indent-length))
	      (setq tmp (current-column))))

	   ;; CONTINUATION with PARENS
	   (parenchar ;; a str if in parens
	    (let* ((mi (assoc parenchar matlab-maximum-indents))
		   (max (if mi (if (listp (cdr mi)) (car (cdr mi)) (cdr mi)) nil))
		   (ind (if mi (if (listp (cdr mi)) (cdr (cdr mi)) (cdr mi)) nil)))
	      (goto-char parenpt)
	      (forward-char 1)
	      (skip-chars-forward " \t")
	      ;; If we are at the end of a line and this
	      ;; open paren is there, then we DON'T want
	      ;; to indent to it.  Use the standard
	      ;; indent.
	      (if (or (not matlab-align-to-paren)
		      (looking-at "\\.\\.\\.\\|$"))
		  (if (or (eq indent-type 'function-call-cont)
			  (and (not (eq indent-type 'array-solo-cont))
			       (not (eq indent-type 'nested-array-cont))))
		      ;; functions or an array ending on a EOL should
		      ;; do normal code indentation from beginning of cmd
		      (setq tmp (+ ci-boc matlab-continuation-indent-level))
		    ;; If in an array in an array ending on EOL should
		    ;; indent a wee bit
		    (setq tmp (+ parencol matlab-array-continuation-indent-level)))
		;; current column is location on original line where
		;; first bit of text is, so line up with that.
		(setq tmp (current-column))
		;; TODO - this disables indentation MAXs
		;;        if we really want to be rid of this
		;;        we can dump a bunch of logic above too.
		;; apply the maximum limits.
		;;(if (and ind (> (- (current-column) ci-boc) max))
		;;    (+ ci-boc ind)
		;;  (current-column))
		)))

	   ;; CONTINUATION with EQUALS
	   ((save-excursion
	      (goto-char boc)
	      (while (and (re-search-forward "=" (matlab-point-at-eol) t)
			  (matlab-cursor-in-string-or-comment)))
	      (when (= (preceding-char) ?=)
		(skip-chars-forward " \t")
		(setq found-column (point)))
	      )
	    (save-excursion
	      (goto-char found-column)
	      (let ((cc (current-column))
		    (mi (assoc ?= matlab-maximum-indents)))
		      
		(if (looking-at "\\.\\.\\.\\|$")
		    ;; In this case, the user obviously wants the
		    ;; indentation to be somewhere else.
		    (setq tmp (+ (matlab--previous-line-indent-recommendation lvl2) (cdr (cdr mi))))
		  ;; If the indent delta is greater than the max,
		  ;; use the max + current
		  (if (and mi (> (- cc (matlab--previous-line-indent-recommendation lvl2)) (if (listp (cdr mi))
					       (car (cdr mi))
					     (cdr mi))))
		      (setq tmp (+ (matlab--previous-line-indent-recommendation lvl2) (if (listp (cdr mi))
					  (cdr (cdr mi))
					(cdr mi))))
		    (setq tmp cc))))))

	   ;; CONTINUATION with nothing special about it.
	   (t
	    (setq tmp (+ (matlab--previous-line-indent-recommendation lvl2) matlab-continuation-indent-level)))
	   ))
	
	(list indent-type tmp)
	))
     )))

(defun matlab-next-line-indentation (lvl1)
  "Calculate the indentation for lines following this command line.
See `matlab-calculate-indentation' for how the output of this fcn is used."
  (let ((startpnt (point-at-eol))
	) 
    (save-excursion
      (matlab-scan-beginning-of-command lvl1)
	  
      (back-to-indentation)
      (setq lvl1 (matlab-compute-line-context 1))
      (let ((cc (matlab-line-count-closed-blocks startpnt))
	    (bc (matlab-line-count-open-blocks startpnt))
	    (end (matlab-line-end-p lvl1))
	    (mc (and (matlab-line-block-middle-p lvl1) 1))
	    (ec (and (matlab-line-block-case-p lvl1) 1))
	    (ci (current-indentation)))

	;; When CC is positive, and END is false, or CC > 1, then the NEXT
	;; line should have an indent that matches the context at the beginning
	;; of the block of the last end.
	(if (or (> cc 1) (and (= cc 1) end))
	    (let* ((CTXT (matlab-with-context-line lvl1
			   (matlab-line-end-of-code lvl1)
			   (matlab-re-search-keyword-backward
			    (matlab-keyword-regex 'end) (point-at-bol) t)
			   (matlab-scan-block-start-context))))
	      (matlab-line-indentation (nth 3 CTXT)))

	  ;; Old technique.
	
	  ;; When the current point is on a line with a function, the value of bc will
	  ;; reflect the function in a block count iff if matlab-functions-have-end is
	  ;; true.  However, if matlab-indent-function-body-p is false, there should be
	  ;; no actual indentation, so bc needs to be decremented by 1.  Similarly, if
	  ;; on a line with an end that closes a function, bc needs to be decremented
	  ;; by 1 if matlab-functions-have-end is true and matlab-indent-function-body-p
	  ;; is false.  However, just to be safe, indentation is not allowed to go
	  ;; negative.  Thus:
	  (if matlab-functions-have-end
	      (if (and
		   (not (matlab-indent-function-body-p))
		   (or (matlab-line-declaration-p lvl1)
		       (and (matlab-line-end-p lvl1)
			    (save-excursion
			      (matlab-backward-sexp t)
			      (looking-at "function\\b")))))
		  (if (> bc 0)
		      (setq bc (1- bc))
		    (if (>= ci matlab-indent-level)
			(setq bc -1))))
	    ;; Else, funtions don't have ends in this file.
	    (if (and (matlab-indent-function-body-p)
		     (matlab-line-declaration-p lvl1))
		(setq bc (1+ bc))))
	  ;; Remove 1 from the close count if there is an END on the beginning
	  ;; of this line, since in that case, the unindent has already happened.
	  (when end (setq cc (1- cc)))
	  ;; Calculate the suggested indentation.
	  (+ ci
	     (* matlab-indent-level bc)
	     (* matlab-indent-level (or mc 0))
	     (* matlab-indent-level (- cc))
	     (* (if (listp matlab-case-indent-level)
		    (cdr matlab-case-indent-level) matlab-case-indent-level)
		(or ec 0))
	     ))))))

;;; The return key ============================================================

(defcustom matlab-return-function 'matlab-indent-end-before-ret
  "Function to handle return key.
Must be one of:
    'matlab-plain-ret
    'matlab-indent-after-ret
    'matlab-indent-end-before-ret
    'matlab-indent-before-ret"
  :group 'matlab
  :type '(choice (function-item matlab-plain-ret)
		 (function-item matlab-indent-after-ret)
		 (function-item matlab-indent-end-before-ret)
		 (function-item matlab-indent-before-ret)))

(defun matlab-return ()
  "Handle carriage return in `matlab-mode'."
  (interactive)
  (matlab-semicolon-on-return)
  (funcall matlab-return-function))

(defun matlab-plain-ret ()
  "Vanilla new line."
  (interactive)
  (newline))

(defun matlab-indent-after-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (matlab-indent-line))

(defun matlab-indent-end-before-ret ()
  "Indent line if block end, start new line, and indent again."
  (interactive)
  (let ((lvl1 (matlab-compute-line-context 1)))
    (when (or (matlab-line-end-p lvl1)
	      (matlab-line-block-case-p lvl1)
	      (matlab-line-block-middle-p lvl1)
	      (matlab-line-declaration-p lvl1))
      (matlab-indent-line)))
  (newline)
  (matlab-indent-line))

(defvar matlab-quiesce-nosemi-regexp) ;; quiet compiler warning (var is defined below)
(defun matlab-semicolon-on-return ()
  "If needed, add a semicolon at point automatically."
  (if matlab-return-add-semicolon
      (when (matlab-line-end-of-code-needs-semicolon-p)
	(insert ";")
	(end-of-line))))

(defun matlab-indent-before-ret ()
  "Indent line, start new line, and indent again."
  (interactive)
  (matlab-indent-line)
  (newline)
  (matlab-indent-line))

(defun matlab-linefeed ()
  "Handle line feed in `matlab-mode'.
Has effect of `matlab-return' with (not matlab-indent-before-return)."
  (interactive)
  (matlab-indent-line)
  (newline)
  (matlab-indent-line))


;;; Comment management========================================================

(defun matlab-comment-return ()
  "Handle carriage return for MATLAB comment line."
  (interactive)
  (newline)
  (matlab-comment))

(defun matlab-electric-comment (arg)
  "Indent line and insert comment character.
Argument ARG specifies how many %s to insert."
  (interactive "P")
  (self-insert-command (or arg 1))
  (matlab-indent-line)
  (skip-chars-forward "%"))

(defun matlab-electric-block-comment (arg)
  "Indent line and insert block comment end character.
Argument ARG specifies how many %s to insert."
  (interactive "P")
  (self-insert-command (or arg 1))
  (let ((bc (save-excursion (beginning-of-line) (matlab-block-comment-bounds))))

    (cond ((matlab-ltype-block-comment-start)

	 ;; Starting block comment.  Check if we are already in a block
	 ;; comment, and blink it if a problem.
	 (let ((bcwrapped (save-excursion
			    (beginning-of-line)
			    (matlab-block-comment-bounds))))

	   ;; Regardless, indent our line
	   (matlab-indent-line)

	   (when bcwrapped
	     (save-excursion
	       (goto-char (car bcwrapped))
	       (skip-chars-forward "%{")
	       (message "Nested block comment start %%{")
	       (pulse-momentary-highlight-region (car bcwrapped) (point))))
	   ))

	  ;;ELSE, maybe end of block comment
	  ((and bc (matlab-ltype-block-comment-end))
	   (progn
	     (matlab-indent-line)
	     ;; The above sometimes puts the cursor on the %, not after it.
	     (skip-chars-forward "%}")
	     (pulse-momentary-highlight-region (car bc) (cdr bc)))
	   )

	  ;; Else, not in a comment - which means we don't have
	  ((and (not bc) (save-excursion
			   (skip-chars-backward "%{")
			   (looking-at "%{")))
	   (message "Block comment end has no matching %%{")
	   (save-excursion
	     (beginning-of-line)
	     (when (re-search-backward matlab-block-comment-end-re nil t)
	       (pulse-momentary-highlight-region (match-beginning 0) (match-end 0))))
	   )
	  )))

(defun matlab-comment ()
  "Add a comment to the current line."
  (interactive)
  (let ((lvl1 (matlab-compute-line-context 1)))
    (cond ((region-active-p)
	   (call-interactively #'comment-or-uncomment-region))

	  ((matlab-line-empty-p lvl1)	; empty line
	   (insert matlab-comment-line-s)
	   (back-to-indentation)
	   (matlab-indent-line)
	   (skip-chars-forward " \t%"))
	  
	  ((matlab-line-comment-p lvl1) ; comment line
	   (back-to-indentation)
	   (matlab-indent-line)
	   (skip-chars-forward " \t%"))
	  
	  ((matlab-line-end-comment-column lvl1)  ; code line w/ comment
	   (goto-char (matlab-line-end-comment-point lvl1))
	   (if (> (current-column) comment-column) (delete-horizontal-space))
	   (if (< (current-column) comment-column) (indent-to comment-column))
           ;; Now see if the current line is too long to fit.  Can we back indent?
           (let ((eol-col (- (point-at-eol) (point-at-bol))))
             (when (> eol-col fill-column)
               (delete-horizontal-space)
               (indent-to (- comment-column (- eol-col fill-column)))))
           (skip-chars-forward "% \t"))

	  (t				; code line w/o comment
	   (end-of-line)
	   (re-search-backward "[^ \t\n^]" 0 t)
	   (forward-char)
	   (delete-horizontal-space)
	   (if (< (current-column) comment-column)
	       (indent-to comment-column)
	     (insert " "))
	   (insert matlab-comment-on-line-s)))))

(defun matlab-comment-line-break-function (&optional soft)
  "Break the current line, and if in a comment, continue it.
Optional argument SOFT indicates that the newline is soft, and not hard."
  (interactive)
  (if (not (matlab-cursor-in-comment))
      (matlab-return)
    ;; Will the below fn work in old emacsen?
    (if soft (insert-and-inherit ?\n) (newline 1))
    (insert "% ")
    (matlab-indent-line)
    (end-of-line)))

(defun matlab-comment-indent ()
  "Indent a comment line in `matlab-mode'."
  (matlab--calc-indent))

(defun matlab-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts `matlab-comment-region-s' at the beginning of every line in the region.
BEG-REGION and END-REGION are arguments which specify the region boundaries.
With non-nil ARG, uncomment the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert matlab-comment-region-s)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert matlab-comment-region-s)))
      (let ((com (regexp-quote matlab-comment-region-s))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun matlab-uncomment-region (beg end)
  "Uncomment the current region if it is commented out.
Argument BEG and END indicate the region to uncomment."
  (interactive "*r")
  (matlab-comment-region beg end t))

;;; Filling ===================================================================

(defun matlab-set-comm-fill-prefix ()
  "Set the `fill-prefix' for the current (comment) line."
  (interactive)
  (if (matlab-line-comment-p (matlab-compute-line-context 1))
      (setq fill-prefix
	    (save-excursion
	      (beginning-of-line)
	      (let ((e (matlab-point-at-eol))
		    (pf nil))
		(while (and (re-search-forward "%+[ \t]*\\($$$ \\|\\* \\)?" e t)
			    (matlab-cursor-in-string)))
		(setq pf (match-string 0))
		(when (string-match "%\\s-*\\* " pf)
		  (setq pf (concat "%" (make-string (1- (length pf)) ?  ))))
		(concat (make-string (- (current-column) (length pf)) ? )
			pf))))))

(defun matlab-set-comm-fill-prefix-post-code ()
  "Set the `fill-prefix' for the current post-code comment line."
  (interactive)
  (matlab-set-comm-fill-prefix))

(defun matlab-reset-fill-prefix ()
  "Reset the `fill-prefix'."
  (setq fill-prefix nil))

(defun matlab-find-convenient-line-break ()
  "For the current line, position the cursor where we want to break the line.
Basically, spaces are best, then operators.  Always less than `fill-column'
unless we decide we can fudge the numbers.  Return nil if this line should
not be broken.  This function will ONLY work on code."
  ;; First of all, if this is a continuation, then the user is
  ;; requesting that we don't mess with his stuff.
  (let ((lvl1 (matlab-compute-line-context 1)))
    (if (matlab-line-ellipsis-p lvl1)
	nil
      (save-restriction
	(narrow-to-region (matlab-point-at-bol) (matlab-point-at-eol))
	;; get ourselves onto the fill-column.
	(move-to-column fill-column)
	(let ((pos nil)
	      (orig (point)))
	  (or
	   ;; Next, if we have a trailing comment, use that.
	   (progn (setq pos (or (matlab-line-comment-p lvl1)
				(matlab-point-at-bol)))
		  (goto-char pos)
		  (if (and (> (current-column) (- fill-column matlab-fill-fudge))
			   (< (current-column) (+ fill-column matlab-fill-fudge)))
		      t
		    (goto-char orig)
		    nil))
	   ;; Now, lets find the nearest space (after or before fill column)
	   (let* ((after (save-excursion
			   (re-search-forward "[ \t]" nil t)))
		  (before (save-excursion
			    (re-search-backward "[ \t]" nil t)))
		  (afterd (- (or after (matlab-point-at-eol)) (point)))
		  (befored (- (point) (or before (matlab-point-at-bol)))))
	     ;; Here, if "before" is actually the beginning of our
	     ;; indentation, then this is most obviously a bad place to
	     ;; break our lines.
	     (if before
		 (save-excursion
		   (goto-char before)
		   (if (<= (point) (save-excursion
				     (back-to-indentation)
				     (point)))
		       (setq before nil))))
	     (cond ((and after
			 (< afterd matlab-fill-fudge)
			 (< afterd befored))
		    (goto-char after)
		    t)
		   ((and before
			 (< befored matlab-fill-fudge)
			 (< befored afterd))
		    (goto-char before)
		    t)
		   (t (goto-char orig)
		      nil)))
	   ;; Now, lets find the nearest backwards
	   (progn
	     (re-search-backward "\\(\\s-\\|\\s.\\)+" nil t)
	     (while (and (looking-at "\\^\\|\\.\\|'")
			 (re-search-backward "\\(\\s-\\|\\s.\\)+" nil t)))
	     (if (or (not (looking-at "\\(\\s-\\|\\s.\\)+"))
		     (<= (point) (save-excursion
				   (back-to-indentation)
				   (point))))
		 (progn
		   ;; We failed in our mission to find anything, or fell
		   ;; of the edge of the earth.  If we are out of
		   ;; bounds, lets try again.
		   (goto-char orig)
		   (if (re-search-backward "\\s.+" nil t)
		       t
		     nil))
	       ;; Ok, we have a good location to break.  Check for column
	       ;; and ref against nearest list ending to predict a possibly
	       ;; better break point.
	       (forward-char 1)
	       (let ((okpos (current-column))
		     (startlst (save-excursion
				 (condition-case nil
				     (matlab-up-list -1)
				   (error nil))
				 (if (save-excursion
				       (forward-char -1)
				       (looking-at "\\w"))
				     (forward-word -1))
				 (current-column)))
		     (endlst (save-excursion
			       (condition-case nil
				   (matlab-up-list 1)
				 (error nil))
			       (current-column))))
		 ;; When evaluating list fudge factors, breaking on the
		 ;; edge of a list, or at the beginning of a function
		 ;; call can be more valuable than breaking on a symbol
		 ;; of a mid-sized list.  As such, allow double-fudge
		 ;; for lists.
		 (cond
		  ;; First, pick the end of a list.
		  ((and (< endlst matlab-fill-fudge-hard-maximum)
			(<= endlst (+ fill-column matlab-fill-fudge))
			(or (<= (* matlab-fill-fudge 2) (- endlst okpos))
			    (<= endlst fill-column))
			(save-excursion
			  (move-to-column endlst)
			  (not (looking-at "\\^"))))
		   (move-to-column endlst)
		   t)
		  ;; Else, back up over this list and poke around
		  ((>= (* 2 matlab-fill-fudge) (- okpos startlst))
		   (move-to-column startlst)
		   t)
		  ;; Oh well, just do this symbol.
		  (t (move-to-column okpos)
		     t)))))
	   ;; Well, this just sucks
	   (progn (goto-char orig)
		  nil)))))))

(defun matlab-auto-fill ()
  "Do auto filling.
Set variable `auto-fill-function' to this symbol to enable MATLAB style auto
filling which will automatically insert `...' and the end of a line."
  (interactive)
  (let ((fill-prefix fill-prefix) ;; safe way of modifying fill-prefix.
	(fill-column (- fill-column
			(if matlab-fill-count-ellipsis-flag
			    (save-excursion
			      (move-to-column fill-column)
			      (if (not (bobp))
				  (forward-char -1))
			      (if (matlab-cursor-in-string)
				  4 3))
			  0)))
	(lvl1 (matlab-compute-line-context 1)))
    (if (> (current-column) fill-column)
	(cond
	 ((matlab-line-comment-ignore-p lvl1)
	  nil)
	 ((or (matlab-line-comment-p lvl1)
	      (and (save-excursion (move-to-column fill-column)
				   (matlab-cursor-in-comment))
		   (matlab-line-comment-p lvl1)))
	  ;; If the whole line is a comment, do this.
	  (matlab-set-comm-fill-prefix) (do-auto-fill)
	  (matlab-reset-fill-prefix))
	 ((and (not (or (matlab-line-comment-p lvl1) (matlab-line-empty-p lvl1)))
	       (not (matlab-line-ellipsis-p lvl1))
	       matlab-fill-code)
	  ;; If we are on a code line, we ellipsify before we fill.
	  (let ((m (make-marker)))
	    (move-marker m (point))
	    (set-marker-insertion-type m t)
	    (if (not (matlab-find-convenient-line-break))
		nil
	      (if (not (save-excursion
			 (forward-char -1)
			 (matlab-cursor-in-string)))
		  (progn
		    (delete-horizontal-space)
		    (insert " " matlab-elipsis-string "\n")
		    (matlab-indent-line))
		(if matlab-fill-strings-flag
		    (let ((pos (point))
			  (pos2 nil))
		      (while (and (re-search-backward "'" (point-at-bol) t)
				  (progn (forward-char -1)
					 (looking-at "''"))))
		      (setq pos2 (point))
		      ;; Check if there is already an opening bracket or if string is continued
		      (if (or (looking-at "\\[")
			      (save-excursion (skip-chars-backward " \t")
				     (forward-char -1)
				     (looking-at "\\["))
			      (progn
				(beginning-of-line)
				     (skip-chars-backward (concat " \t\n" matlab-elipsis-string))
				     (if (> (point) (point-min))
					 (progn
					   (forward-char -1)
					   (looking-at (concat "'\\s-*" matlab-elipsis-string))))))
			  (goto-char pos)
			(goto-char pos2)
			(forward-char 1)
			(insert "[")
			(goto-char pos)
			(forward-char 1))
		      ;(delete-horizontal-space)
		      (skip-chars-forward " \t")
		      (insert "' " matlab-elipsis-string "\n")
		      (matlab-indent-line)
		      (insert "'")
			;; Re scan forward for the end of the string. Add an end bracket
			;; if there isn't one already. Also add an apostrophe if necessary.
		      (if (not (looking-at "'\\s-*]"))
			  (save-excursion
			    (if (not (re-search-forward "[^']'\\([^']\\|$\\)" (line-end-position) t))
				(progn
				  (end-of-line)
				  (insert "']")
				  (move-marker m (- (point) 2)))
			      (re-search-backward "'")
                              (cond ((looking-at "'\\s-*]")
                                     nil ; already in an array.
                                     )
                                    ((or (looking-at "'\\s-*$") (looking-at "'\\s-*[^]]"))
                                     ;; in a string, add an array end.
                                     (forward-char 1)
                                     (insert "]"))
                                    ((looking-at "'\\s-*\\.\\.\\.")
                                     ;; Already extended to next line ... leave it alone.
                                     nil)
                                    ))))
                      ))))
            (goto-char m)))
         ))))

(defun matlab-join-comment-lines ()
  "Join current comment line to the next comment line."
  ;; New w/ V2.0: This used to join the previous line, but I could find
  ;; no editors that had a "join" that did that.  I modified join to have
  ;; a behaviour I thought more inline with other editors.
  (interactive)
  (end-of-line)
  (if (looking-at "\n[ \t]*%")
      (replace-match " " t t nil)
    (error "No following comment to join with")))

(defun matlab-fill-region (beg-region end-region &optional justify-flag)
  "Fill the region between BEG-REGION and END-REGION.
Non-nil JUSTIFY-FLAG means justify comment lines as well."
  (interactive "*r\nP")
  (let ((end-reg-mk (make-marker)))
    (set-marker end-reg-mk end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (while (< (point) end-reg-mk)
      ;; This function must also leave the point at the end of the
      ;; justified line.
      (matlab-fill-paragraph justify-flag)
      (forward-line 1)
      (beginning-of-line))))

(defconst matlab-cline-start-skip "[ \t]*%[ \t]*"
  "*The regular expression for skipping comment start.")

(defun matlab-fill-comment-line (&optional justify)
  "Fill the current comment line.
With optional argument, JUSTIFY the comment as well."
  (interactive)
  (if (not (matlab-comment-on-line))
      (error "No comment to fill"))
  (beginning-of-line)
  ;; First, find the beginning of this comment...
  (while (and (looking-at matlab-cline-start-skip)
	      (not (bobp)))
    (forward-line -1)
    (beginning-of-line))
  (if (not (looking-at matlab-cline-start-skip))
      (forward-line 1))
  ;; Now scan to the end of this comment so we have our outer bounds,
  ;; and narrow to that region.
  (save-restriction
    (narrow-to-region (point)
		      (save-excursion
			(while (and (looking-at matlab-cline-start-skip)
				    (not (save-excursion (end-of-line) (eobp))))
			  (forward-line 1)
			  (beginning-of-line))
			(if (not (looking-at matlab-cline-start-skip))
			    (forward-line -1))
			(end-of-line)
			(point)))
    ;; Find the fill prefix...
    (matlab-comment-on-line)
    (looking-at "%[ \t]*")
    (let ((fill-prefix (concat (make-string (current-column) ? )
			       (match-string 0))))
      (fill-region (point-min) (point-max) justify))))

(defun matlab-justify-line ()
  "Delete space on end of line and justify."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (justify-current-line)))

(defun matlab-fill-paragraph (arg)
  "When in a comment, fill the current paragraph.
Paragraphs are always assumed to be in a comment.
ARG is passed to `fill-paragraph' and will justify the text."
  (interactive "P")
  (cond ((or (matlab-line-comment-p (matlab-compute-line-context 1))
	     (and (matlab-cursor-in-comment)
		  (not (matlab-line-ellipsis-p (matlab-compute-line-context 1)))))
	 ;; We are in a comment, lets fill the paragraph with some
	 ;; nice regular expressions.
	 ;; Cell start/end markers of %% also separate paragraphs
	 (let ((paragraph-separate "%%\\|%[a-zA-Z]\\|%[ \t]*$\\|[ \t]*$")
	       (paragraph-start "%[a-zA-Z]\\|%[ \t]*$\\|[ \t]*$\\|%\\s-*\\*")
	       (paragraph-ignore-fill-prefix nil)
	       (start (save-excursion (matlab-scan-beginning-of-command)
				      (if (looking-at "%%")
					  (progn (end-of-line)
						 (forward-char 1)))
				      (point-at-bol)))
	       (end (save-excursion (matlab-scan-end-of-command)))
	       (fill-prefix nil))
	   (matlab-set-comm-fill-prefix)
	   (save-restriction
	     ;; Ben North fixed to handle comment at the end of
	     ;; a buffer.
	     (narrow-to-region start (min (point-max) (+ end 1)))
	     (fill-paragraph arg))))
	((let ((lvl  (matlab-compute-line-context 1)))
	   (not (or (matlab-line-comment-p lvl) (matlab-line-empty-p lvl))))
	 ;; Ok, lets get the outer bounds of this command, then
	 ;; completely refill it using the smart line breaking code.
	 (save-restriction
	   (narrow-to-region (save-excursion
			       (matlab-scan-beginning-of-command)
			       (beginning-of-line)
			       (point))
			     (save-excursion
			       (matlab-scan-end-of-command)))
	   ;; Remove all line breaks
	   (goto-char (point-min))
	   (while (and (re-search-forward "$" nil t)
		       (not (eobp)))
	     (delete-horizontal-space)
	     ;; Blow away continuation marks
	     (if (matlab-line-ellipsis-p (matlab-compute-line-context 1))
		 (progn
		   (goto-char (match-beginning 0))
		   (forward-char 1)
		   (delete-region (point) (matlab-point-at-eol))))
	     ;; Zap the CR
	     (if (not (eobp)) (delete-char 1))
	     ;; Clean up whitespace
	     (delete-horizontal-space)
	     ;; Clean up trailing comments
	     (if (and (looking-at "% *")
		      (matlab-cursor-in-comment))
		 (progn
		   (delete-char 1)
		   (delete-horizontal-space)))
	     (insert " "))
	   ;; Now fill till we are done
	   (goto-char (point-max))
	   (while (or (> (current-column) (+ fill-column matlab-fill-fudge))
		      (> (current-column) matlab-fill-fudge-hard-maximum))
	     (if (= (point)
		    (progn
		      (matlab-auto-fill)
		      (point)))
		 (error "Fill algorithm failed!"))
	     (if arg (save-excursion
		       (forward-line -1)
		       (matlab-justify-line))))
	   (if arg (save-excursion
		     (forward-line -1)
		     (matlab-justify-line)))))
	(t
	 (message "Paragraph Fill not supported in this context."))))


;;; Show Paren Mode support ==================================================

(defun matlab-show-paren-or-block ()
  "Function to assign to `show-paren-data-function'.
Highlights parens and if/end type blocks.
Returns a list: \(HERE-BEG HERE-END THERE-BEG THERE-END MISMATCH)"
  (unless (matlab-cursor-in-string-or-comment) ; Only do this if not in a string.

    (save-match-data
      (save-excursion
	(let ((here-beg nil)
	      (here-end nil)
	      (there-beg nil)
	      (there-end nil)
	      (mismatch nil)
	      (noreturn nil)
	      (here-syntax (syntax-after (point)))
	      (here-prev-syntax (syntax-after (1- (point))))
	      (there-syntax nil)
	      (here-char (char-after))
	      (here-prev-char (preceding-char))
	      (there-char nil)
	      )

	  ;; Notes about fcns used here:
	  ;; (syntax-after ) returns ( 4 c ) or ( 5 c )
	  ;; where 4 == open paren and 5 == close paren
	  ;; and c is the char that closes the open or close paren
	  ;; These checks are much faster than regexp

	  ;; Step one - check for parens
	  (cond ((and here-syntax (= (syntax-class here-syntax) 4)) ; open paren
		 (setq here-beg (point)
		       here-end (1+ (point)))
		 (condition-case err
		     (progn
		       (matlab-move-simple-sexp-internal 1)
		       (setq there-beg (- (point) 1)
			     there-end (point)
			     there-syntax (syntax-after there-beg)
			     there-char (char-after there-beg))
		       (when (or (/= (syntax-class there-syntax) 5)
				 (/= (cdr there-syntax) here-char)
				 (/= (cdr here-syntax) there-char)) ; this part seems optional
					;(message "ts = %S  hs=%S tc = %d hc = %d" there-syntax here-syntax there-char here-char)
			 (setq mismatch t))
		       )
		   (error (setq mismatch t))))
		((and here-prev-syntax (= (syntax-class here-prev-syntax) 5))
		 (setq here-beg (1- (point))
		       here-end (point))
		 (condition-case err
		     (progn
		       (matlab-move-simple-sexp-backward-internal 1)
		       (setq there-end (+ (point) 1)
			     there-beg (point)
			     there-syntax (syntax-after there-beg)
			     there-char (char-after there-beg))
		       (when (or (/= (syntax-class there-syntax) 4)
				 (/= (cdr there-syntax) here-prev-char)
				 (/= (cdr here-prev-syntax) there-char)) ; this part seems optional
			 (setq mismatch t))
		       )
		   (error (setq mismatch t))))
		(t
		 ;; Part 2: Are we looking at a block start/end, such as if end;

		 ;; If we are on a word character, or just after a
		 ;; word character move back one symbol. This will let
		 ;; us use the block begin / end matchers to figure
		 ;; out where we are.
		 (let ((startsym (matlab--mk-keyword-node))
		       (endsym nil)
		       )
		   (when (matlab--valid-keyword-node startsym)
		     (goto-char (nth 2 startsym))

		     (condition-case err
			 (cond
			  ((eq (car startsym) 'decl)
			   ;; We are looking at a 'function' start.
			   ;; Since functions may not have an end, we need
			   ;; to handle this case special.
			   (setq here-beg (nth 2 startsym)
				 here-end (nth 3 startsym))
			   (if (matlab--scan-block-forward)
			       ;; if context is returned, failed to find something
			       ;; this is a missmatch if fcns don't have end.
			       (setq mismatch t)
			     ;; Otherwise, we found something good
			     (setq endsym (matlab--mk-keyword-node))
			     (if (not endsym)
				 ;; End of buffer on function w/ no end?
				 (if matlab-functions-have-end
				     (setq mismatch t)
				   (setq mismatch nil))
			       ;; Found a symbol
			       (setq there-beg (nth 2 endsym)
				     there-end (nth 3 endsym)
				     mismatch nil)
			       ) ))

			  ;; Misc block starts
			  ((memq (car startsym) '(ctrl args mcos))
			   ;; We are at the beginning of a block.  Navigate forward to the end
			   ;; statement.
			   (setq here-beg (nth 2 startsym)
				 here-end (nth 3 startsym))
			   (if (matlab--scan-block-forward)
			       (setq mismatch t)
			     (setq endsym (matlab--mk-keyword-node))
			     (if (not endsym)
				 (setq mismatch t)
			       ;; Found a symbol
			       (setq there-beg (nth 2 endsym)
				     there-end (nth 3 endsym)
				     mismatch nil))))

			  ;; Misc block middles an ends
			  ((memq (car startsym) '(end mid case))
			   ;; We are at the end of a block or on a middle keyword
			   ;; like else, catch, or case.  In all these go to beginning.
			   (setq here-beg (nth 2 startsym)
				 here-end (nth 3 startsym))
			   (goto-char here-beg)
			   (if (matlab--scan-block-backward-up)
			       (setq mismatch t)
			     (setq endsym (matlab--mk-keyword-node))
			     (if (not endsym)
				 (setq mismatch t)
			       ;; Found a symbol
			       (setq there-beg (nth 2 endsym)
				     there-end (nth 3 endsym)
				     mismatch nil))))

			  ;; No block matches, just return nothing.
			  (t (setq noreturn t))
			  )
		       ;; An error occurred.  Assume 'here-*' is set, and setup mismatch.
		       (error (setq mismatch t))))
		   )))

	  (if noreturn
	      nil
	    (list here-beg here-end there-beg there-end mismatch) ))))))


;;; M Code verification & Auto-fix ============================================

(defun matlab-mode-verify-fix-file-fn ()
  "Verify the current buffer from `write-contents-hooks'."
  (if matlab-verify-on-save-flag
      (matlab-mode-verify-fix-file (> (point-max)
				      matlab-block-verify-max-buffer-size)))
  ;; Always return nil.
  nil)

(defun matlab-mode-verify-fix-file (&optional fast)
  "Verify the current buffer satisfies all M things that might be useful.
We will merely loop across a list of verifiers/fixers in
`matlab-mode-verify-fix-functions'.
If optional FAST is non-nil, do not perform usually lengthy checks."
  (interactive)
  (let ((p (point))
	(l matlab-mode-verify-fix-functions))
    (while l
      (funcall (car l) fast)
      (setq l (cdr l)))
    (goto-char p))
  (if (matlab-called-interactively-p)
      (message "Done.")))

(defun matlab-toggle-show-mlint-warnings ()
  "Toggle `matlab-show-mlint-warnings'."
  (interactive)
  (setq matlab-show-mlint-warnings (not matlab-show-mlint-warnings))
  (if matlab-highlight-cross-function-variables
      (if matlab-show-mlint-warnings
          (mlint-buffer)        ; became true, recompute mlint info
        (mlint-clear-warnings))) ; became false, just remove highlighting
  ;; change mlint mode altogether
  (mlint-minor-mode
   (if (or matlab-highlight-cross-function-variables
           matlab-show-mlint-warnings)
       1 -1)))

(defun matlab-toggle-highlight-cross-function-variables ()
  "Toggle `matlab-highlight-cross-function-variables'."
  (interactive)
  (setq matlab-highlight-cross-function-variables
        (not matlab-highlight-cross-function-variables))
  (if matlab-show-mlint-warnings
      (if matlab-highlight-cross-function-variables
          (mlint-buffer)        ; became true, recompute mlint info
                                ; became false, just remove highlighting ...
        (mlint-clear-cross-function-variable-highlighting)))
  (mlint-minor-mode
   (if (or matlab-highlight-cross-function-variables
           matlab-show-mlint-warnings)
       1 -1)))        ; change mlint mode altogether

;;
;; Add more auto verify/fix functions here!
;;
(defun matlab-mode-vf-functionname (&optional fast)
  "Verify/Fix the function name of this file.
Optional argument FAST is ignored."
  (matlab-navigation-syntax
    (goto-char (point-min))
    (matlab-find-code-line)
    (let ((func nil)
	  (bn (file-name-sans-extension
	       (file-name-nondirectory (buffer-file-name)))))
    (if (looking-at (matlab-match-function-re))
	;; The expression above creates too many numeric matches
	;; to apply a known one to our function.  We cheat by knowing that
	;; match-end 0 is at the end of the function name.  We can then go
	;; backwards, and get the extents we need.  Navigation syntax
	;; lets us know that backward-word really covers the word.
	(let ((end (match-end 0))
	      (begin (progn (goto-char (match-end 0))
			    (forward-word -1)
			    (point))))
	  (setq func (buffer-substring-no-properties begin end))
	  (if (not (string= func bn))
	      (if (not (matlab-mode-highlight-ask
			begin end
			"Function and file names are different. Fix function name?"))
		  nil
		(goto-char begin)
		(delete-region begin end)
		(insert bn))))))))

(defun matlab-mode-vf-classname (&optional fast)
  "Verify/Fix the class name of this file.
Optional argument FAST is ignored."
  (matlab-navigation-syntax
    (goto-char (point-min))
    (matlab-find-code-line)
    (let ((class nil)
	  (bn (file-name-sans-extension
	       (file-name-nondirectory (buffer-file-name)))))
    (if (looking-at (matlab-match-classdef-re))
	;; The name of this class is match 2.
	(let ((end (match-end 2))
	      (begin (match-beginning 2)))
	  (setq class (buffer-substring-no-properties begin end))
	  (if (not (string= class bn))
	      (if (not (matlab-mode-highlight-ask
			begin end
			"Class name and file names are different. Fix class name?"))
		  nil
		(goto-char begin)
		(delete-region begin end)
		(insert bn))))))))

(defun matlab-mode-vf-add-ends (&optional fast)
  "Verify/Fix adding ENDS to functions.
Optional argument FAST skips this test in fast mode."
  ;; We used to do extra checking here, but now we do
  ;; checking in the verifier
  (when (not fast)
    (matlab-mode-vf-block-matches-forward nil t)
    ))

(defun matlab-mode-vf-block-matches-forward (&optional fast addend)
  "Verify/Fix unterminated (or un-ended) blocks.
This only checks block regions like if/end.
If `matlab-mode-vf-add-ends' is part of your verify list, this will
not be needed.

Optional argument FAST causes this check to be skipped.
Optional argument ADDEND asks to add ends to functions, and is used
by `matlab-mode-vf-add-ends'"
  (let ((go t)
	(expr nil)
	;; lets avoid asking questions based on id of this file
	;; and if ends are optional in the first place.
	(filetype (matlab-guess-script-type))
	)
    
    ;; Before checking syntax, lets re-look at the file if we were in
    ;; guess mode and re-assert what we should do.
    (cond
     ;; If the file is empty of code (from before, or just now)
     ;; then optimize out this step.
     ((eq filetype 'empty)
      ;; No code, no need to loop.
      (setq fast t)
      ;; If user deleted content, go back into guess mode.
      (setq matlab-functions-have-end 'guess)
      (matlab-functions-have-end-minor-mode 1)
      )

     ;; If there is just bad syntax somewhere, skip it with a notice.
     ((save-excursion (goto-char (point-max)) (matlab-in-list-p))
      (setq fast t)
      ;; Let user fix it later
      (setq matlab-functions-have-end 'guess)
      (matlab-functions-have-end-minor-mode 1)
      (message "Unterminated list - skipping block check"))

     
     ;; If we are in guess mode, but user added content, we can
     ;; not have a fresh new guess.
     ((eq matlab-functions-have-end 'guess)
      (let ((guess (matlab-do-functions-have-end-p 'no-navigate)))
	(if guess (matlab-functions-have-end-minor-mode 1)
	  (matlab-functions-have-end-minor-mode -1)))
      )

     ;; If we are in no-end mode, BUT the filetype is wrong, say something.
     ((and (not matlab-functions-have-end) (or (eq filetype 'script) (eq filetype 'class)))
      (message "Type of file detected no longer matches `matlab-functions-have-end' of nil, assume t.")
      (matlab-functions-have-end-minor-mode 1)
      (sit-for 1)
      )

     ;; If functions have end but the style changes, re-up the lighter on the minor mode.
     ;; note, we can ignore that 'empty == 'guess b/c handled earlier.
     ((and matlab-functions-have-end (not (eq matlab-functions-have-end filetype)))
      (matlab-functions-have-end-minor-mode 1))

     ;; If the variable was specified and file is not empty, then do nothing.
     ;; TODO - maybe we should force to t for scripts and classes?

     ) ;; end cond

    ;; compute expression after changing state of funtions have end above.
    (setq expr (concat "\\<\\(" (matlab-block-beg-pre) "\\)\\>"))
    
    ;; Navigate our sexp's and make sure we're all good.
    (matlab-navigation-syntax
      (goto-char (point-min))
      (while (and (not fast) go (re-search-forward expr nil t))
	(forward-word -1)		;back over the special word
	(let ((s (point))
	      e)
	  (condition-case nil
	      (if (and (not (matlab-cursor-in-string-or-comment))
		       (not (matlab-block-comment-bounds))
		       (or matlab-functions-have-end (not (eq (matlab-on-keyword-p) 'decl))))
		  (progn
		    (matlab-forward-sexp)
		    (forward-word -1)
		    (when (not (eq (matlab-on-keyword-p) 'end))
		      ;; TODO - is this possible ?
		      (setq go nil)))
		(forward-word 1))
	    (error (setq go nil)))

	  (cond
	   ;; If we are still in guess mode and file is good, then we now have our answer.
	   ((and go (eq matlab-functions-have-end 'guess))
	    (matlab-functions-have-end-minor-mode 1))
	  
	   ;; If we had an error and still in guess mode we can look at the latest
	   ;; content and decide if we should have ends anyway.
	   ((and (not go) (eq matlab-functions-have-end 'guess))
	    (if (matlab-do-functions-have-end-p 'no-navigate)
		;; minor mode now looks at file type when passing in 1
		(matlab-functions-have-end-minor-mode 1)
	      ;; Turn off for this file.  No questions.
	      (matlab-functions-have-end-minor-mode -1)))

	   ;; If we had an error, but none of the above, try to fix?
	   ((not go)
	    (goto-char s)
	    (setq e (save-excursion (forward-word 1) (point)))
	    ;; Try to add an end to the broken block
	    (if addend
		(if (matlab-mode-highlight-ask
		     s e "Unterminated block.  Try to add end?")
		    (progn
		      (matlab-mode-vf-add-end-to-this-block)
		      (setq go t))
		  ;; Else, mark this buffer as not needing ends,
		  ;; but ONLY if a function buffer
		  (when (eq filetype 'function)
		    (if (matlab-mode-highlight-ask
			 s e "Should funtions have end in this file?")
			(matlab-functions-have-end-minor-mode 1)
		      (matlab-functions-have-end-minor-mode -1)
		      (message "Marking buffer as not needing END for this session.")
		      (sit-for 1))))
	      ;; We aren't in addend mode then we are in plain verify
	      ;; mode
	      (if (matlab-mode-highlight-ask
		   s e
		   "Unterminated block.  Continue anyway?")
		  nil ;; continue anyway.
		(error "Unterminated Block found!"))))
	   )) ;; cond, let
	(message "Block-check: %d%%" (/ (/ (* 100 (point)) (point-max)) 2))))))

(defun matlab-mode-vf-add-end-to-this-block ()
  "Add an end to the current block the cursor is on."
  ;; Our best guess is just in front of a 'function' block, or at the end
  ;; of the current buffer.
  (save-excursion
    (end-of-line)
    (if (re-search-forward "^function " nil t)
	(progn
	  (beginning-of-line)
	  (save-excursion (insert "end\n\n"))
	  (matlab-indent-line))
      (goto-char (point-max))
      (save-excursion (insert "\nend\n\n"))
      (matlab-indent-line))))

(defun matlab-mode-vf-block-matches-backward (&optional fast)
  "Verify/fix unstarted (or dangling end) blocks.
Optional argument FAST causes this check to be skipped."
  (goto-char (point-max))
  (let ((go t) (expr (matlab-keyword-regex 'end)))
    (matlab-navigation-syntax
      (while (and (not fast) go (matlab-re-search-keyword-backward expr nil t))
	(forward-word 1)
	(let ((s (point)))
	  (condition-case nil
	      (if (and (not (matlab-cursor-in-string-or-comment))
		       (matlab-valid-end-construct-p))
		  (matlab-backward-sexp)
		(backward-word 1))
	    (error (setq go nil)))
	  (if (and (not go) (goto-char s)
		   (not (matlab-mode-highlight-ask
			 (point) (save-excursion (backward-word 1) (point))
			 "Unstarted block.  Continue anyway?")))
	      (error "Unstarted Block found!")))
	(message "Block-check: %d%%"
		 (+ (/ (/ (* 100 (- (point-max) (point))) (point-max)) 2) 50))))))

;;; Utility for verify/fix actions if you need to highlight
;;  a section of the buffer for the user's approval.
(defun matlab-mode-highlight-ask (begin end prompt)
  "Highlight from BEGIN to END while asking PROMPT as a yes-no question."
  (let ((mo (matlab-make-overlay begin end (current-buffer)))
	(show-paren-mode nil) ;; this will highlight things we often ask about.  disable.
	(ans nil))
    (condition-case nil
	(progn
	  (matlab-overlay-put mo 'face 'matlab-region-face)
	  (setq ans (y-or-n-p prompt))
	  (matlab-delete-overlay mo))
      (quit (matlab-delete-overlay mo) (error "Quit")))
    ans))

;;; Quiesce an M file to remove accidental display of ANS during a run.
;;  Useful if you have random outputs and you don't know where they are from,
;;  or before compiling to standalone where some functions now have outputs
;;  that did not have outputs earlier.
;;
;;  You probably don't want this as a default verify function
(defvar matlab-quiesce-nosemi-regexp "\\s-*\\(function\\|parfor\\|for\\|spmd\\|while\\|try\\|catch\\|\
switch\\|otherwise\\|case\\|break\\|if\\|else\\|end\\|return\\|disp\\|\
$\\|%\\)"
  "Regular expression used to detect if a semicolon is needed at the end of a line.")

(defun matlab-mode-vf-quiesce-buffer (&optional fast)
  "Find all commands that do not end in ;, and add one.
This has the effect of removing any extraneous output that may not be
desired.  Optional argument FAST is not used."
  (interactive)
  (save-excursion
    (push-mark)
    (goto-char (point-min))
    (let ((msgpos 0) (dir .2))
      (while (not (save-excursion (end-of-line) (eobp)))
	(message (aref [ "Scanning o...." "Scanning .o..." "Scanning ..o.."
			 "Scanning ...o." "Scanning ....o" ] (floor msgpos)))
	(setq msgpos (+ msgpos dir))
	(if (or (> msgpos 5) (< msgpos 0)) (setq dir (- dir)
						 msgpos (+ (* 2 dir) msgpos)))
	(matlab-scan-end-of-command)
	(when (and (matlab-line-end-of-code-needs-semicolon-p)
		   (matlab-mode-highlight-ask (point) (+ 1 (point))
					      "Add Semi colon here? "))
	  (insert ";"))
	(forward-line 1))))
  (message "Scanning .... done"))



;;; matlab-mode debugging =====================================================

(defun matlab-show-line-info ()
  "Display type and attributes of current line.  Used in debugging."
  (interactive)
  (matlab-navigation-syntax
    (let* ((msg "")
	   (lvl2 (matlab-compute-line-context 2))
	   (lvl1 (matlab-get-lvl1-from-lvl2 lvl2))
	   (lvl1msg (matlab-describe-line-indent-context lvl1 t))
	   (indent nil)
	   (fullindent (matlab--calc-indent lvl2 'indent))
	   (nexti (matlab-next-line-indentation (matlab-previous-line lvl2))))
      (setq msg (concat msg
			"Line Syntax: " lvl1msg
			"  | Preferred Indents: This: " (int-to-string (nth 1 indent))
			"  Next: " (int-to-string nexti)
			"  Indent Style: " (symbol-name (car indent))
			))
      
      (when (matlab-line-ellipsis-p lvl1)
	(setq msg (concat msg " w/cont")))
      (when (matlab-line-end-comment-point lvl1)
	(setq msg (concat msg " w/comm")))
      (message "%s" msg))))


(provide 'matlab)

;;; matlab.el ends here

;; LocalWords:  el Wette mwette caltech edu Ludlam eludlam defconst online mfiles ebstop ebclear
;; LocalWords:  compat easymenu defcustom CASEINDENT COMMANDINDENT sexp defun ebstatus mlg gud's
;; LocalWords:  mmode setq progn sg Fns Alist elipsis vf functionname vers subjob flb fle elisp
;; LocalWords:  minibuffer featurep fboundp facep zmacs defface cellbreak bcend lastcompute noblock
;; LocalWords:  cellbreaks overline keymap torkel ispell gud allstring strchar decl lcbounds setcar
;; LocalWords:  bs eu bc ec searchlim eol charvec Matchers ltype cdr if'd setcdr bcwrapped
;; LocalWords:  uicontext setcolor mld keywordlist mapconcat pragmas Classdefs
;; LocalWords:  dem Za Imenu imenu alist prog reindent unindent boundp fn
;; LocalWords:  symbolp prev lst nlst nreverse Aki Vehtari backquote
;; LocalWords:  defmacro oldsyntax edebug cline ctxt eobp bobp sc fc
;; LocalWords:  udir funcall sexps skipnav eolp autoend noerror returnme
;; LocalWords:  Unstarted includeelse autostart lattr zerop cellstart
;; LocalWords:  linebounds bol commtype startmove nomove charvector sregex
;; LocalWords:  insregex laststart bolp calc ci sem DEPTHNUMBER blockstart
;; LocalWords:  blockmid blockendless blockend unstarted listp boc parencol
;; LocalWords:  cci startpnt hc rc nosemi emacsen afterd befored okpos startlst
;; LocalWords:  endlst ellipsify noreturn hs tc matchers hideshow func PUSHNEW
;; LocalWords:  pushnew bn nondirectory un msgpos nexti
