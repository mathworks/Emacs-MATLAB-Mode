;;; matlab.el --- major mode for MATLAB(R) dot-m files

;; Author: Matt Wette <mwette@alumni.caltech.edu>,
;;         Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>
;; Created: 04 Jan 91
;; Keywords: MATLAB(R)
;; Version:

(defconst matlab-mode-version "4.0"
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

(defcustom matlab-cont-level 4
  "*Basic indentation after continuation if no other methods are found."
  :group 'matlab
  :type 'integer)

(defcustom matlab-cont-requires-ellipsis t
  "*Specify if ellipses are required at the end of a line for continuation.
Future versions of Matlab may not require ellipses ... , so a heuristic
determining if there is to be continuation is used instead."
  :group 'matlab
  :type 'integer)

(defcustom matlab-case-level '(2 . 2)
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

(defcustom matlab-align-to-paren t
  "*Whether continuation lines should be aligned to the opening parenthesis.
When non-nil, continuation lines are aligned to the opening parenthesis if the
opening is not followed by only spaces and ellipses.  When nil, continued lines
are simply indented by `matlab-cont-level'."
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
		" function...?")
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
(defun matlab-guess-script-type ()
  "Guess the type of script this `matlab-mode' file contains.
Returns one of 'empty, 'script, 'function, 'class."
  (save-excursion
    (goto-char (point-min))
    (if (matlab-find-code-line)
	;; We found some code, what is it?
	(if (looking-at matlab-defun-regex)
	    ;; A match - figure out the type of thing.
	    (let ((str (match-string-no-properties 1)))
	      (cond ((string= str "function")
		     'function)
		    ((string= str "classdef")
		     'class)))
	  ;; No function or class - just a script.
	  'script)
      ;; No lines of code, we are empty, so undecided.
      'empty)))
	
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

;; It is time to disable this.
(defcustom matlab-vers-on-startup nil
  "*If non-nil, show the version number on startup."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-highlight-block-match-flag t
  "*Non-nil means to highlight the matching if/end/whatever.
The highlighting only occurs when the cursor is on a block start or end
keyword."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-show-periodic-code-details-flag nil
  "*Non-nil means to show code details in the minibuffer.
This will only work if `matlab-highlight-block-match-flag' is non-nil."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-mode-hook nil
  "*List of functions to call on entry to MATLAB mode."
  :group 'matlab
  :type 'hook)


(defcustom matlab-show-mlint-warnings nil
  "*If non-nil, show mlint warnings."
  :group 'matlab
  :type 'boolean)

(make-variable-buffer-local 'matlab-show-mlint-warnings)

(defcustom matlab-highlight-cross-function-variables nil
  "*If non-nil, highlight cross-function variables."
  :group 'matlab
  :type 'boolean)

(make-variable-buffer-local 'matlab-highlight-cross-function-variables)

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

;; Load in the region we use for highlighting stuff.
(if (and (featurep 'custom) (fboundp 'custom-declare-variable))

    (let ((l-region-face (if (facep 'region) 'region 'zmacs-region)))
      ;; If we have custom, we can make our own special face like this
      (defface matlab-region-face
	(list
	 (list t
	       (list :background (face-background l-region-face)
		     :foreground (face-foreground l-region-face))))
	"*Face used to highlight a matlab region."
	:group 'matlab))

  ;; If we do not, then we can fake it by copying 'region.
  (cond ((facep 'region)
	 (copy-face 'region 'matlab-region-face))
	(t
	 (copy-face 'zmacs-region 'matlab-region-face))))

(defvar matlab-unterminated-string-face 'matlab-unterminated-string-face
  "Self reference for unterminated string face.")

(defvar matlab-simulink-keyword-face 'matlab-simulink-keyword-face
  "Self reference for simulink keywords.")

(defvar matlab-nested-function-keyword-face 'matlab-nested-function-keyword-face
  "Self reference for nested function/end keywords.")

(defvar matlab-cross-function-variable-face 'matlab-cross-function-variable-face
  "Self reference for cross-function variables.")

(defvar matlab-cellbreak-face 'matlab-cellbreak-face
  "Self reference for cellbreaks.")

(defun matlab-font-lock-adjustments ()
  "Make adjustments for font lock.
If font lock is not loaded, lay in wait."
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))

      (progn
	(defface matlab-unterminated-string-face
	  (list
	   (list t
		 (list :background (face-background font-lock-string-face)
		       :foreground (face-foreground font-lock-string-face)
		       :underline t)))
	  "*Face used to highlight unterminated strings."
	  :group 'matlab)
	(defface matlab-simulink-keyword-face
	  (list
	   (list t
		 (list :background (face-background font-lock-type-face)
		       :foreground (face-foreground font-lock-type-face)
		       :underline t)))
	  "*Face used to highlight simulink specific functions."
	  :group 'matlab)
        (defface matlab-nested-function-keyword-face
	  (list
	   (list t
		 (list :slant  'italic)))
          "*Face to use for cross-function variables.")
        (defface matlab-cross-function-variable-face
	  (list
	   (list t
		 (list :weight 'bold
                       :slant  'italic)))
          "*Face to use for cross-function variables."
	  :group 'matlab)
	(defface matlab-cellbreak-face
	  (list
	   (list t
		 (list :background (face-background font-lock-comment-face)
		       :foreground (face-foreground font-lock-comment-face)
		       :overline t
		       :bold t)))
	  "*Face to use for cellbreak %% lines.")
	)

    ;; Now, lets make the unterminated string face
    (cond ((facep 'font-lock-string-face)
	   (copy-face 'font-lock-string-face
		      'matlab-unterminated-string-face))
	  (t
	   (make-face 'matlab-unterminated-string-face)))
    (matlab-set-face-underline 'matlab-unterminated-string-face t)

    ;; Now make some simulink faces
    (cond ((facep 'font-lock-type-face)
	   (copy-face 'font-lock-type-face 'matlab-simulink-keyword-face))
	  (t
	   (make-face 'matlab-simulink-keyword-face)))
    (matlab-set-face-underline 'matlab-simulink-keyword-face t)

    ;; Now make some nested function/end keyword faces
    (cond ((facep 'font-lock-type-face)
	   (copy-face 'font-lock-type-face 'matlab-nested-function-keyword-face))
	  (t
	   (make-face 'matlab-nested-function-keyword-face)))

    ;; Now make some cross-function variable faces
    (cond ((facep 'font-lock-type-face)
	   (copy-face 'font-lock-type-face 'matlab-cross-function-variable-face))
	  (t
	   (make-face 'matlab-cross-function-variable-face)))
    (matlab-set-face-bold 'matlab-cross-function-variable-face t)

    ;; Now make some cellbreak variable faces
    (cond ((facep 'font-comment-face)
	   (copy-face 'font-lock-comment-face 'matlab-cellbreak-face))
	  (t
	   (make-face 'matlab-cellbreak-face)))
    (matlab-set-face-bold 'matlab-cellbreak-face t)
    (condition-case nil
	(set-face-attribute 'matlab-cellbreak-face nil :overline t)
      (error nil))
    )
  (remove-hook 'font-lock-mode-hook 'matlab-font-lock-adjustments))

;; Make the adjustments for font lock after it's loaded.
;; I found that eval-after-load was unreliable.
(if (featurep 'font-lock)
    (matlab-font-lock-adjustments)
  (add-hook 'font-lock-mode-hook 'matlab-font-lock-adjustments))


;;; MATLAB mode variables =====================================================

;; abbrev table
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
    ;;(define-key km [(meta q)] 'matlab-fill-paragraph) ; replace w/ fill-paragraph-function setting.
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
;      ["Auto Fill Counts Elipsis"
;       (lambda () (setq matlab-fill-count-ellipsis-flag
;			(not matlab-fill-count-ellipsis-flag)))
;       :style toggle :selected 'matlab-fill-count-ellipsis-flag]
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
      ["Periodic Code Details"
       (setq matlab-show-periodic-code-details-flag
	     (not matlab-show-periodic-code-details-flag))
       :style toggle :selected matlab-show-periodic-code-details-flag ]
      ;; ["Highlight Matching Blocks"
      ;;  (matlab-enable-block-highlighting)
      ;;  :style toggle :selected (member 'matlab-start-block-highlight-timer
      ;; 				       post-command-hook) ]
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

(defvar matlab-string-start-regexp "\\(^\\|[^]})a-zA-Z0-9_.'\"]\\)"
  "Regexp used to represent the character before the char vector or string scalars.
The ' character has restrictions on what starts a string which is needed
when attempting to understand the current context.")

;;; Font Lock Comment and Unreachable Code Matchers
;;
(defvar font-lock-beg) (defvar font-lock-end) ; quiet compiler.

(defun matlab-font-lock-extend-region ()
  "Called by font-lock to extend the region if we are in a multi-line block."
  ;; Only deal with block comments for now.

  (let* ((pos (matlab-block-comment-bounds t))
	 (flb font-lock-beg)
	 (fle font-lock-end))
    (when pos
      (setq font-lock-beg (min font-lock-beg (car pos))
	    font-lock-end (max font-lock-end (cdr pos))))

    (if (and (eq font-lock-beg flb)
	     (eq font-lock-end fle))
	;; We didn't change anything.
	nil

      ;; We made a change
      t)))


(defun matlab-find-unreachable-code (limit)
  "Find code that is if'd out with if(0) or if(false), and mark it as a comment.
The if(0) and else/end construct should be highlighted differently.
Argument LIMIT is the maximum distance to search."
  (if (and (< (point) limit)
	   (re-search-forward
	    "\\<\\(if\\>\\s-*(?\\s-*\\(0\\|false\\)\\s-*)?$\\)"
	    limit t))
      (let ((b1 (match-beginning 1))
	    (e1 (match-end 1))
	    (b2 nil) (e2 nil)
	    (b3 nil) (e3 nil))
	(goto-char b1)
	(condition-case nil
	    (progn
	      ;; Go forward over the matlab sexp.  Include scanning
	      ;; for ELSE since parts of the ELSE block are not
	      ;; `commented out'.
	      (matlab-forward-sexp t)
	      (forward-word -1)
	      ;; Is there an ELSE in this block?
	      (if (looking-at (matlab-block-mid-re))
		  (progn
		    (setq b3 (match-beginning 0)
			  e3 (match-end 0))
		    ;; Now find the REAL end.
		    (matlab-forward-sexp)
		    (forward-word -1)))
	      ;; End of block stuff
	      (if (looking-at (matlab-block-end-re))
		  (progn
		    (setq b2 (match-beginning 0)
			  e2 (match-end 0))
		    ;; make sure something exists...
		    (if (not b3) (setq b3 b2 e3 e2)))
		(error "Eh?"))
	      ;; Ok, build up some match data.
	      (set-match-data
	       (list b1 e2		;the real deal.
		     b1 e1		;if (0)
		     b2 e2		;end
		     b3 e3		;else (if applicable.)
		     b1 e3))		;body commented out.
	      t)
	  (error nil)))))


;;; Font Lock MLINT data highlighting

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
   ;; Various pragmas should be in different colors.
   ;; I think pragmas are always lower case?
   '("%#\\([a-z]+\\)" (1 'bold prepend))
   ;; General keywords
   (list (matlab-font-lock-regexp-opt matlab-keyword-list)
	 '(0 font-lock-keyword-face))
   ;; The end keyword is only a keyword when not used as an array
   ;; dereferencing part.
   '("\\(^\\|[;,]\\)[ \t]*\\(end\\)\\b"
     2 (if (matlab-valid-end-construct-p) font-lock-keyword-face nil))
   ;; The global keyword defines some variables.  Mark them.
   '("^\\s-*global\\s-+"
     ("\\(\\w+\\)\\(\\s-*=[^,; \t\n]+\\|[, \t;]+\\|$\\)"
      nil  nil (1 font-lock-variable-name-face)))
   ;; Handle graphics stuff
   (list
    (matlab-font-lock-regexp-opt matlab-handle-graphics-list)
    '(0 font-lock-type-face))
   (list
    ;; How about a few matlab constants such as pi, infinity, and sqrt(-1)?
    (matlab-font-lock-regexp-opt matlab-constants-keyword-list)
    1 font-lock-constant-face)
   ;; Imaginary number support
   '("\\<[0-9]\\.?\\(i\\|j\\)\\>" 1 font-lock-reference-face)
   )
  "Basic Expressions to highlight in MATLAB mode or shell.")

(defconst matlab-file-basic-font-lock-keywords
  (append
   matlab-basic-font-lock-keywords
   (list
    ;; Keywords that should be the first word on a line AND
    ;; also be alone, or have parameters after it.
    (list (concat "^\\s-*\\("
		  (matlab-font-lock-regexp-opt matlab-keyword-first-on-line-list)
		  "\\)\\s-*[(,;%\n]")
	  '(1 font-lock-keyword-face))
    ;; How about unreachable code?  MUST BE AFTER KEYWORDS in order to
    ;; get double-highlighting.
    '(matlab-find-unreachable-code
      (1 'underline prepend)		;if part
      (2 'underline prepend)		;end part
      (3 'underline prepend)		;else part (if applicable)
      (4 font-lock-comment-face prepend) ;commented out part.
      )
    ;; Cell mode breaks get special treatment
    '("^\\s-*\\(%%[^\n]*\n\\)" (1 matlab-cellbreak-face append))
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
   (list (concat "^\\s-*\\(function\\)\\>[ \t\n.]*"
		 "\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*"
		 "=[ \t\n.]*\\(\\(?:[sg]et\\.\\)?\\sw+\\)[ \t\n.]*"
		 matlab-function-arguments)
	 '(1 font-lock-keyword-face append)
	 '(2 font-lock-variable-name-face append)
	 '(3 font-lock-function-name-face prepend))
   ;; defining a function, a function name, and an optional (possibly
   ;; empty) list of input variables
   (list (concat "^\\s-*\\(function\\)[ \t\n.]+"
		 "\\(\\(?:[sg]et\\.\\)?\\sw+\\)[ \t\n.]*"
		 matlab-function-arguments)
	 '(1 font-lock-keyword-face append)
	 '(2 font-lock-function-name-face prepend))
   ;; Anchor on the function keyword, highlight params
   (list (concat "^\\s-*function\\>[ \t\n.]*"
		 "\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*=[ \t\n.]*\\)?"
		 "\\(?:[sg]et\\.\\)?\\sw+\\s-*(")
	 '("\\s-*\\(\\sw+\\)\\s-*[,)]"
	   (save-excursion (matlab-end-of-command) (point))
	   nil
	   (1 font-lock-variable-name-face)))
   ;; I like variables for FOR loops
   '("\\<\\(\\(?:par\\)?for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\
\\(\\([^\n,;%(]+\\|([^\n%)]+)\\)+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face append)
     (3 font-lock-reference-face append))
   ;; Items after a switch statements are cool
   '("\\<\\(case\\|switch\\)\\s-+\\({[^}\n]+}\\|[^,%\n]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-reference-face))
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
   (list (concat "^\\s-*\\(classdef\\)"
		 matlab-class-attributes-list-re
		 "\\s-+\\(?3:\\sw+\\)")
	 '(1 font-lock-keyword-face append)
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
   ;; Properties can have a type syntax after them
   '("^\\s-*\\w+\\s-*\\(([:0-9,]+)\\s-*[^{=\n]+\\)"
     (1 font-lock-type-face nil nil))
   ;; Properties blocks are full of variables
   '("^\\s-*\\(properties\\|events\\|arguments\\)\\s-*[(,;%\n]"
     ("^\\s-*\\(\\sw+\\)\\>" ;; This part matches the variable
      ;; extend region to match in
      (save-excursion (matlab-forward-sexp nil t) (beginning-of-line) (point))
      nil
      (1 font-lock-variable-name-face t))
     )
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
  `matlab-cont-level'		Level to indent continuation lines.
  `matlab-cont-requires-ellipsis' Does your MATLAB support implied elipsis.
  `matlab-case-level'		Level to unindent case statements.
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
  `matlab-highlight-block-match-flag'
                                Enable matching block begin/end keywords.
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

  ;; Parens mode support
  (if (and (featurep 'paren) (symbolp 'show-paren-data-function) (symbolp show-paren-data-function))
      (progn
	;; show-paren-mode is nicer than our old thing.
	(make-local-variable 'show-paren-data-function)
	(setq show-paren-data-function #'matlab-show-paren-or-block)
	)
    ;; Enable our own block highlighting if paren mode not around.
    (matlab-enable-block-highlighting 1))

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

  (if matlab-vers-on-startup (matlab-show-version)))

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
      (matlab-beginning-of-command)
      ans)))

(defun matlab-find-prev-line (&optional ignorecomments)
  "Recurse backwards until a code line is found."
  (if (= -1 (forward-line -1)) nil
    (if (or (matlab-ltype-empty)
	    (matlab-ltype-comm-ignore)
	    (and ignorecomments (matlab-ltype-comm)))
	(matlab-find-prev-line ignorecomments) t)))

(defun matlab-prev-line ()
  "Go to the previous line of code or comment.  Return nil if not found."
  (interactive)
  (let ((old-point (point)))
    (if (matlab-find-prev-line) t (goto-char old-point) nil)))

(defun matlab-find-code-line ()
  "Walk forwards until we are on a line of code return t on success.
If the currnet line is code, return immediately.
Ignore comments and whitespace."
  (if (or (matlab-ltype-empty)
	  (matlab-ltype-comm))
      (if (= 1 (forward-line 1))
	  nil ;; end of buffer.
	(matlab-find-code-line)) ;; try again.
    t))


(defvar matlab-in-command-restriction nil
  "Non-nil if currently in a `matlab-with-current-command' form.")

(defmacro matlab-with-current-command (&rest forms)
  "Restrict region to the current command and run FORMS.
Restore restriction after FORMS run.
This command will not add a restriction if we are already
restricted."
  (declare (indent 0) (debug t))
  `(save-restriction
     (when (not matlab-in-command-restriction)
       (narrow-to-region (save-excursion
			   (matlab-beginning-of-command)
			   (beginning-of-line)
			   (point))
			 (1+ (matlab-point-at-eol))))
     (let ((matlab-in-command-restriction t))
       ,@forms
       )))

(defun matlab-valid-end-construct-p ()
  "Return non-nil if the end after point terminates a block.
Return nil if it is being used to dereference an array."
  (let ((p (point))
	(err1 t))
    (if (eq (preceding-char) ?.)
	;; This is a struct field, not valid.
	nil
      (condition-case nil
	  (save-match-data
	    (matlab-with-current-command
	      ;; This used to add some sort of protection, but I don't know what
	      ;; the condition was, or why the simple case doesn't handle it.
	      ;;
	      ;; The above replacement fixes a case where a continuation in an array
	      ;; befuddles the identifier.
	      ;;		      (progn ;;(matlab-end-of-command (point))
	      ;;			(end-of-line)
	      ;;			(if (> p (point))
	      ;;			    (progn
	      ;;			      (setq err1 nil)
	      ;;			      (error)))
	      ;;    		(point))))
	      (save-excursion
		;; beginning of param list
		(matlab-up-list -1)
		;; backup over the parens.  If that fails
		(condition-case nil
		    (progn
		      (forward-sexp 1)
		      ;; If we get here, the END is inside parens, which is not a
		      ;; valid location for the END keyword.  As such it is being
		      ;; used to dereference array parameters
		      nil)
		  ;; This error means that we have an unterminated paren
		  ;; block, so this end is currently invalid.
		  (error nil)))))
	;; an error means the list navigation failed, which also means we are
	;; at the top-level
	(error err1)))))

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

(defcustom matlab-block-indent-tic-toc-flag nil
  "*Non-nil means that tic,toc should indent like a if,end block.
This variable should be set before loading matlab.el"
  :group 'matlab
  :type 'boolean)

(defconst matlab-block-syntax-re
  (concat "\\(function" matlab-mcos-regexp "\\)\\>")
  "Keywords that represent blocks that have custom internal syntax.
Used by `matlab-cursor-on-valid-block-start'.") 

(defconst matlab-innerblock-syntax-re
  (concat "\\(" matlab-mcos-innerblock-regexp "\\)\\>")
  "Keywords that represent blocks that have custom internal syntax.
Used by `matlab-cursor-on-valid-block-start'.") 

(defconst matlab-block-beg-pre-if
  (if matlab-block-indent-tic-toc-flag
      (concat "function\\|parfor\\|spmd\\|for\\|while\\|if\\|switch\\|try\\|tic"
	      matlab-mcos-regexp)
    (concat "function\\|parfor\\|spmd\\|for\\|while\\|if\\|switch\\|try"
	    matlab-mcos-regexp))
  "Keywords which mark the beginning of an indented block.
Includes function.")

(defconst matlab-block-beg-pre-no-if
  (if matlab-block-indent-tic-toc-flag
      (concat "parfor\\|for\\|spmd\\|while\\|if\\|switch\\|try\\|tic"
	      matlab-mcos-regexp)
    (concat "parfor\\|for\\|spmd\\|while\\|if\\|switch\\|try"
	    matlab-mcos-regexp))
  "Keywords which mark the beginning of an indented block.
Excludes function.")

(defun matlab-block-beg-pre ()
  "Partial regular expression to recognize MATLAB block-begin keywords."
  (if matlab-functions-have-end
      matlab-block-beg-pre-if
    matlab-block-beg-pre-no-if))

(defconst matlab-block-mid-pre
  "elseif\\|else\\|catch"
  "Partial regular expression to recognize MATLAB mid-block keywords.")

(defconst matlab-block-end-pre-if
  (if matlab-block-indent-tic-toc-flag
      "end\\|function\\|\\(\\sw+\\s-*\\((.*)\\)?\\s-*=\\s-*\\)?toc"
    "end\\|function")
  "Partial regular expression to recognize MATLAB block-end keywords.")

(defconst matlab-block-end-pre-no-if
  (if matlab-block-indent-tic-toc-flag
      "end\\|\\(\\sw+\\s-*\\((.*)\\)?\\s-*=\\s-*\\)?toc"
    "end")
  "Partial regular expression to recognize MATLAB block-end keywords.")

(defun matlab-block-end-pre ()
  "Partial regular expression to recognize MATLAB block-end keywords."
  (if matlab-functions-have-end
      matlab-block-end-pre-no-if
    matlab-block-end-pre-if))

(defconst matlab-endless-blocks
  "case\\|otherwise"
  "Keywords which initialize new blocks, but don't have explicit ends.
Thus, they are endless.  A new case or otherwise will end a previous
endless block, and end will end this block, plus any outside normal
blocks.")

(defun matlab-block-re ()
  "Regular expression for keywords which begin MATLAB blocks."
  (concat "\\(^\\|[;,]\\)\\s-*\\("
 	  (matlab-block-beg-pre) "\\|"
  	  matlab-block-mid-pre "\\|"
 	  (matlab-block-end-pre) "\\|"
 	  matlab-endless-blocks "\\)\\b"))

(defun matlab-block-start-scan-re ()
  "Expression used to scan over matching pairs of begin/ends.
Assume cursor is on the beginning of the upcoming word."
  (concat "\\("
 	  (matlab-block-beg-pre) "\\|"
 	  matlab-block-mid-pre "\\)\\b"))

(defun matlab-block-scan-re ()
  "Expression used to scan over matching pairs of begin/ends."
  (concat "\\(^\\|[;,]\\)\\s-*\\("
 	  (matlab-block-beg-pre) "\\|"
 	  (matlab-block-end-pre) "\\)\\b"))

(defun matlab-block-beg-re ()
  "Expression used to find the beginning of a block."
  (concat "\\(" (matlab-block-beg-pre) "\\)"))

(defun matlab-block-mid-re ()
  "Expression used to find block center parts (like else)."
  (concat "\\(" matlab-block-mid-pre "\\)"))

(defun matlab-block-end-re ()
  "Expression used to end a block.  Usually just `end'."
  (concat "\\(" (matlab-block-end-pre) "\\)"))

(defun matlab-block-end-no-function-re ()
  "Expression representing and end if functions are excluded."
  (concat "\\<\\(" matlab-block-end-pre-no-if "\\)\\>"))

(defun matlab-endless-blocks-re ()
  "Expression of block starters that do not have associated ends."
  (concat "\\(" matlab-endless-blocks "\\)"))

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

(defconst matlab-cline-start-skip "[ \t]*%[ \t]*"
  "*The regular expression for skipping comment start.")


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
  (matlab-navigation-syntax
    (skip-chars-backward " \t\n")
    (cond
     ;; Not auto-end, and on the end of a block comment
     ((and (not autoend)
	   (matlab-cursor-in-comment)
	   (let ((bcend (save-excursion
			  (beginning-of-line)
			  (re-search-forward "%" (point-at-eol))
			  (goto-char (match-beginning 0))
			  (when (looking-at "%}")
			    (point)))))
	     (if bcend (goto-char bcend))))

      (let ((bc (matlab-block-comment-bounds)))
	(goto-char (car bc)))
      )
     ;; Not auto-end, and not looking @ and end type keyword
     ((and (not autoend)
	   (save-excursion (backward-word 1)
			   (or (not
				(and (looking-at
				      (matlab-block-end-no-function-re))
				     (matlab-valid-end-construct-p)))
			       (matlab-cursor-in-string-or-comment))))
      ;; Go backwards one simple expression
      (matlab-move-simple-sexp-internal -1))

     ;; otherwise go backwards recursively across balanced expressions
     ;; backup over our end
     (t
      (if (not autoend) (forward-word -1))
      (let ((done nil) (start (point)) (returnme t) (bound nil))
        (when (search-backward "\nfunction" nil t)
          (if (progn (forward-char 9) (looking-at "\\b"))
              (setq bound (- (point) 8)))
          (goto-char start))
	(while (and (not done)
		    (or (not matlab-scan-on-screen-only)
			(pos-visible-in-window-p)))
	  (if (re-search-backward (matlab-block-scan-re) bound t)
	      (progn
		(goto-char (match-beginning 2))
		(if (looking-at (matlab-block-end-no-function-re))
		    (if (or (matlab-cursor-in-string-or-comment)
			    (not (matlab-valid-end-construct-p)))
			nil
		      ;; we must skip the expression and keep searching
		      (forward-word 1)
		      (unless (matlab-backward-sexp nil noerror)
			(setq done t
			      returnme nil)))
		  ;; Make sure we are at a valid block construct and not
		  ;; comments or other weird spot.
		  (if (and (not (matlab-cursor-in-string-or-comment))
			   (matlab-cursor-on-valid-block-start))
		      (setq done t))))
	    (goto-char start)
	    (if noerror
		(setq done t
		      returnme nil)
	      (error "Unstarted END construct"))))
	returnme)))))

(defun matlab-forward-sexp (&optional includeelse autostart parentblock)
  "Go forward one balanced set of MATLAB expressions.
Optional argument INCLUDEELSE will stop on ELSE if it matches the starting IF.
If AUTOSTART is non-nil, assume we are already inside a block, and navigate
forward until we exit that block.
PARENTBLOCK is used when recursing to validate block starts as being in
a valid context."
  (interactive "P")
  (let (p) ;; go to here if no error.
    (save-excursion ;; don't go anywhere if there is an error
      (matlab-navigation-syntax
        ;; skip over preceding whitespace
        (skip-chars-forward " \t\n;")
        (cond
	 ;; no autostart, and looking at a block comment.
	 ((and (not autostart)
	       (matlab-ltype-block-comment-start))
	  (goto-char (match-end 0))
	  (let ((bc (matlab-block-comment-bounds)))
	    (when bc (goto-char (cdr bc))))
	  )
	 ;; No autostart, and looking at a block keyword.
	 ((and (not autostart)
	       (or (not (looking-at (matlab-block-start-scan-re)))
		   ;;    ^^ (concat "\\(" (matlab-block-beg-pre) "\\|" (matlab-block-mid-re) "\\)\\>")
		   (matlab-cursor-in-string-or-comment)))
          ;; Go forwards one simple expression
	  (matlab-move-simple-sexp-internal 1))

	 ;; Yes autostart, but already looking @ the END!
	 ((and autostart (looking-at (matlab-block-end-re)))
	  (goto-char (match-end 0)))

	 ;; Default behavior.
	 (t
          ;; Not autostart, skip next word, and also track what it is
	  ;; for nesting purposes.
	  (unless autostart
	    (setq parentblock (buffer-substring-no-properties
			       (point) (progn (forward-word 1) (point)))))
          (let ((done nil) (s nil)
                (expr-scan (if includeelse
                               (matlab-block-re)
                             (matlab-block-scan-re)))
                (expr-look (matlab-block-beg-pre)))
            (while (and (not done)
                        (setq s (re-search-forward expr-scan nil t))
                        (or (not matlab-scan-on-screen-only)
                            (pos-visible-in-window-p)))
              (goto-char (match-beginning 2))
              (if (looking-at expr-look)
                  (if (or (matlab-cursor-in-string-or-comment)
			  ;; We'd like to do this:
			  ;;(not (matlab-cursor-on-valid-block-start))
			  ;; but it travels backwards.  Instead, we need to track the
			  ;; last block we are diving down from and just use that
			  ;; instead of looking it up along the way.
			  (not (matlab-cursor-on-valid-block-start parentblock))
			  )
                      (forward-word 1)
                    ;; we must skip the expression and keep searching
                    ;; NEVER EVER call with value of INCLUDEELSE
                    (matlab-forward-sexp nil nil (match-string-no-properties 1)))
                (forward-word 1)
                (if (and (not (matlab-cursor-in-string-or-comment))
                         (matlab-valid-end-construct-p))
                    (setq done t))))
            (if (not s)
                (error "Unterminated block")))))
        (setq p (point)))) ;; really go here
    (goto-char p)))

(defvar matlab-valid-block-start-slow-and-careful t
  "Be very careful with determining of a block is valid when t.
Set to nil if fast-and-loose is ok.")

(defun matlab-cursor-on-valid-block-start (&optional known-parent-block)
  "Return t if cursor is on a valid block start.
Valid block starts are those that represent a syntax context, like function,
classdef, properties, etc.
KNOWN-PARENT-BLOCK is a string that represents the context cursor is in.
Use this if you know what context you're in."
  (save-match-data
    (save-restriction
      (widen)
      (cond
       ((not (looking-at (matlab-block-beg-re)))
	;; Not looking at a valid block
	nil)
       ((eq (preceding-char) ?.)
	;; Any block preceeded by a '.' is a field in a struct, and not valid.
	nil)
       ;; Else, are we on a block that has special syntax?
       ((not (looking-at matlab-innerblock-syntax-re))
	;; Not an innerblock syntax that only work withing special blocks
	;; thus automatically true.
	t)
   
       ;; Else, a special block keyword.  We need to check the context of this
       ;; block to know if this innerblock is valid.

       ;; Cheap check - if functions don't have end, then always invalid
       ;; since these context blocks can't exist.
       ((not matlab-functions-have-end)
	nil)

       ;; Cheap check - is this block keyword not the first word for this command?
       ((save-excursion (skip-syntax-backward " ") (not (bolp)))
	;; Not first on line, not valid block.
	;; Technically it COULD be valid, but we need some cheap ways
	;; to skip over some types of syntaxes that look dumb.
	nil)

       ;; Cheap check - is this at the beginning of a command line (ignore ... )
       ((not (eq (point) (save-excursion (matlab-beginning-of-command) (point))))
	;; If this statement is not also the first word on this command
	;; then it can't be one of these features.
	nil)
	
       ;; Expensive check - is this block in the right context?
       ((or matlab-valid-block-start-slow-and-careful known-parent-block)
	
	(let ((foundblock (match-string-no-properties 1)))
	  (cond
	   ((string= foundblock "arguments")
	    ;; Argument is only valid if it is the FIRST thing in a funtion.
	    (save-excursion
	      (if (and (matlab-find-prev-code-line) (looking-at "\\s-*function\\>"))
		  ;; If the previous code line (ignoring whitespace and comments)
		  ;; is 'arguments', then that is a valid block.
		  t
		;; Otherewise, all other argument cases are bad.
		nil)))

	   ;; Other special blocks are in a class.  If not in a class file, fail.
	   ((not (eq matlab-functions-have-end 'class))
	    nil)

	   ;; If a known parent block was passed in, then this will be a very
	   ;; fast check, so do that next.  If it is the right thing, then
	   ;; true!
	   (known-parent-block
	    (if (string= known-parent-block "classdef")
		t
	      ;; otherwise not correct.
	      nil))

	   ;; This test goes back one command.  If it is an end, and it is indented just
	   ;; one level, then we are good b/c we already check we're in a classdef file.
	   ((save-excursion
	      (if (not (matlab-find-prev-code-line))
		  nil ;; no code, so not in a class.
		(back-to-indentation)
		(or
		 ;; Looking at correctly indented end
		 (and (looking-at "\\<end\\>")
		      (eq (current-indentation) matlab-indent-level))
		 ;; Looking at the classdef itself
		 (looking-at "\\<classdef\\>") )))
	    t)
	   
	   ;; This is a medium speed test for classdef stuff.  It only navigates backward
	   ;; one step.  If the previous thing also belongs to a class, then we must
	   ;; be in a class.
	   ;;((matlab-previous-line-belongs-to-classdef-p) t)

	   ;; This is a slow operation - navigating backward to find the current syntactic
	   ;; block is pretty expensive, but it also always gets it right.
	   ;;
	   ;; Since We are in a class, identify if this is in a class context.
	   ;; ((let ((myblock (if known-parent-block
	   ;; 		       (cons known-parent-block nil) ;; shortcut if known
	   ;; 		     (matlab-current-syntactic-block))))
	   ;;    (string= (car myblock) "classdef"))
	   ;;    ;; We found correct usage of methods, events, etc.
	   ;;  t)

	   ;; All else fails - so not valid.
	   (t nil)
	   
	   ))) ;; End slow-and-careful

       ;; A cheap version of the expensive check
       ((and (not matlab-valid-block-start-slow-and-careful)
	     (looking-at matlab-innerblock-syntax-re))
	;; If we are not slow and careful, we just need to return t if we see
	;; one of these keywords since other cases where these weren't 1st on the line
	;; or not in a classdef file are already filtered out.
	t)

       ;; If none of the valid cases, must be invalid
       (t nil)
       ))))

(defun matlab-previous-line-belongs-to-classdef-p ()
  "Return the nature of the line of code before this one.
Ignores comments, etc.
Returns non-nil if that previous thing is unique to a classdef."
  (save-excursion
    (save-match-data
      ;; Move to the syntax in question.
      (if (not (matlab-find-prev-code-line))
	  nil ;; No code
	;; Lets see what it is.
	(back-to-indentation)
	;; Is it an end?  Nav backward
	(if (looking-at "\\<end\\>")
	    (if (not (matlab-backward-sexp t t))
		nil ;; failed to nav - just skip it.
	      ;; If no errors so far, compute the kind of block.
	      (if (looking-at matlab-innerblock-syntax-re)
		  t ;; found something that belongs to a classdef.
		nil))
	  ;; Not on an end.  Is that because we are now on a classdef?
	  ;; If so, then this is our context, so that's ok.
	  (if (looking-at "\\<classdef\\>")
	      t ;; Yep, that's a class
	    nil))))))

(defun matlab-current-syntactic-block ()
  "Return information about the current syntactic block the cursor is in.
Value returned is of the form ( BLOCK-TYPE . PT ) where BLOCK-TYPE is a
string, such as 'function' or 'properties', and PT is the location that
the block starts at.

This function skips over blocks such as 'switch' and 'if', and only returns
blocks that change the syntax of their contents, such as:
  function, classdef, properties, events, methods, arguments
"
  (let ((block-type nil)
	(block-beg nil))

    (save-excursion
      ;; By specifying NOERROR, returns nil if we can't move
      ;; backward, which means we should stop.

      ;; backward sexp also knows to skip invlaid block starts, so we'll
      ;; only land on safe blocks.
      (when (and (matlab-backward-sexp t t)
		 (looking-at matlab-block-syntax-re))
	(setq block-type (match-string-no-properties 1)
	      block-beg (point)))
      )

    (cons block-type block-beg)))

(defun matlab-indent-sexp ()
  "Indent the syntactic block starting at point."
  (interactive)
  (indent-region (point) (save-excursion (matlab-forward-sexp) (point)) nil))

(defun matlab-beginning-of-enclosing-defun ()
  "Move cursor to beginning of enclosing function.
If `matlab-functions-have-end', skip over functions with end."
  (catch 'done
    (let ((start (point))
          (beg nil))
      (while (re-search-backward matlab-defun-regex nil t)
        (setq beg (point))
        (condition-case nil
            (progn
              (matlab-forward-sexp)
              (if (> (point) start) (throw 'done beg)))
          (error (throw 'done beg)))
        (goto-char beg)))
    nil))

(defun matlab-beginning-of-defun ()
  "Go to the beginning of the current function."
  (interactive)
  (if matlab-functions-have-end
      (goto-char (or (matlab-beginning-of-enclosing-defun) (point-min)))
    (or (re-search-backward matlab-defun-regex nil t)
        (goto-char (point-min)))))

(defun matlab-end-of-defun ()
  "Go to the end of the current function."
  (interactive)
  (or (progn
	(if (looking-at matlab-defun-regex) (goto-char (match-end 0)))
	(if (re-search-forward matlab-defun-regex nil t)
	    (progn (forward-line -1)
		   t)))
      (goto-char (point-max))))

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
  (interactive)
  (beginning-of-line)
  (save-match-data
    (let ((p nil)
	  (bc (matlab-block-comment-bounds)))
      ;; block comment - just go to the beginning.
      (if bc
	  (goto-char (car bc))

	;; ELSE : Scan across lines that are related.
	;; Step one, skip all comments indented as continutions of a previous.
	;; Using forward-comment is very fast, and just skipps all comments until
	;; we hit a line of code.
	;; NOTE: This may fail with poorly indented code.
	(when (matlab-ltype-continued-comm)
	  (forward-comment -100000))

	;; Now walk backward across continued code lines.
	(while (and (or (setq p (matlab-lattr-array-cont)) ;; do this first b/c fast
			(matlab-prev-line-cont)
			;; We used to do this, now handled w/ forward-comment above.
			;;(matlab-ltype-continued-comm)
			)
		    (save-excursion (beginning-of-line) (not (bobp))))
	  (if p (goto-char p) (matlab-prev-line))
	  (setq p nil)))
      (back-to-indentation))))

(defun matlab-end-of-command ()
  "Go to the end of an M command.
Travells a cross continuations"
  (interactive)
  (while (and (or (matlab-lattr-cont)
		  (save-excursion
		    (forward-line 1)
                    (or (matlab-lattr-array-cont)
                        (matlab-ltype-continued-comm))))
	      ;; This hack is a short circuit.  If a user did not
	      ;; correctly end a matrix, this will short-circuit
	      ;; as soon as something that would never appear in a matrix
	      ;; becomes visible.
	      (not (save-excursion
		     (beginning-of-line)
		     (and (looking-at (matlab-block-scan-re))
			  (not (looking-at (matlab-match-function-re))))))
              ;; If we hit the end of the buffer unexpectedly, this test
              ;; will fail and we'll avoid looping forever.  (E.g., this
              ;; is triggered if a continuation line is the last one in
              ;; the buffer, and the line lacks the final newline.)
              (zerop (forward-line 1))))
  (end-of-line))


;;; Line types, attributes, and string/comment context =================================================

(defun matlab-ltype-empty ()		; blank line
  "Return t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun matlab-ltype-comm-noblock ()
  "Return t if the current line is a MATLAB single-line comment.
Returns nil for Cell start %% and block comments %{, %}.
Used in `matlab-ltype-comm', but specialized for not cell start."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*%\\([^%{}]\\|$\\)")))

(defun matlab-ltype-comm ()		; comment line
  "Return t if current line is a MATLAB comment line.
Return the symbol 'cellstart if it is a double %%.
Return the symbol 'blockcomm if we are in a block comment."
  (save-excursion
    (beginning-of-line)
    (cond
     ((or (matlab-ltype-block-comment-start)
	  (matlab-block-comment-bounds))
      'blockcomm)
     ((matlab-ltype-comm-noblock)
      t)
     ((looking-at "[ \t]*%%")
      'cellstart)
     (t nil))))

(defun matlab-ltype-comm-ignore ()	; comment out a region line
  "Return t if current line is a MATLAB comment region line."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "[ \t]*" matlab-comment-region-s))))

(defun matlab-ltype-help-comm ()
  "Return position of function decl if the point in a MATLAB help comment."
  (save-excursion
    (if (not (matlab-ltype-comm-noblock))
	nil
      (save-restriction
	(widen)
	(while (and (matlab-ltype-comm-noblock) (not (bobp))
		    (matlab-prev-line))
	  (beginning-of-line))
	(when (matlab-ltype-function-definition)
	  (point))))))

;; (global-set-key [f6] 'matlab-debug-block-comm)
(defun matlab-debug-block-comm ()
  "Test block comment detector since font-lock won't let us debug."
  (interactive)
  (let ((pos (matlab-block-comment-bounds t)))
    (if pos
	(pulse-momentary-highlight-region (car pos) (cdr pos))
      (message "No block comment."))))


(defun matlab-ltype-continued-comm ()
  "Return column of previous line's comment start, or nil."
  (save-excursion
    (beginning-of-line)
    (let ((commtype (matlab-ltype-comm-noblock)))
      (if (or (null commtype)
	      (bobp))
	  nil
	;; We use forward-line and not matlab-prev-line because
	;; we want blank lines to terminate this indentation method.
	(forward-line -1)
	(let ((col  (matlab-lattr-comm)))
	  (if col
	      (progn
		(goto-char col)
		(current-column))
	    nil))))))

(defun matlab-ltype-function-definition ()
  "Return t if the current line is a function definition."
  (save-excursion
    (beginning-of-line)
    (looking-at matlab-defun-regex)))

(defun matlab-ltype-code ()		; line of code
  "Return t if current line is a MATLAB code line."
  (and (not (matlab-ltype-empty)) (not (matlab-ltype-comm))))

(defun matlab-lattr-comm ()		; line has comment
  "Return t if current line contain a comment."
  (save-excursion (matlab-comment-on-line)))

(defun matlab-lattr-implied-continuation ()
  "Return non-nil if this line has implied continuation on the next.
This is only useful for new versions of MATLAB where ... is optional."
  (when (not (matlab-lattr-comm))
    (let ((imp nil))
      (save-excursion
	(end-of-line)
	(skip-chars-backward " \t")
	;; Test for operator incompleteness.
	(setq imp
	      (/= (point)
		  ;; Careful, - means range in this expression.
		  (progn (skip-chars-backward "-+=/*.^&~<>")
			 (point))))
	(if (not imp)
	    ;; Test for argument list incompleteness
	    (condition-case nil
		(progn
		  (end-of-line)
		  (matlab-up-list -1)
		  (setq imp (looking-at "(")))
	      (error nil)))
	)
      imp)))

(defun matlab-lattr-cont ()		; line has continuation
  "Return non-nil if current line ends in ... and optional comment.
If `matlab-cont-requires-ellipsis' is nil, then we need to apply
a heuristic to determine if this line would use continuation
based on what it ends with."
  (let* ((pps (syntax-ppss (point-at-eol)))
	 (csc (nth 8 pps)))
    (or
     ;; When the line ends with a comment, it might be an ellipsis.
     ;; Can only be tail comment w/ no % start if it is an ellipsis.
     (and csc (/= (char-after csc) ?\%))
    
     ;; If the line doesn't end in ..., but we have optional ..., then
     ;; use this annoying heuristic.
     (and (null matlab-cont-requires-ellipsis)
	  (matlab-lattr-implied-continuation))
     )))

(defun matlab-prev-line-cont ()
  "Return t if the previous line is a continuation line."
  (save-excursion (and (if (= -1 (forward-line -1)) nil t)
		       (matlab-lattr-cont))))

(defun matlab-lattr-array-cont ()
  "Return non-nil if current line is in an array.
If the entirety of the array is on this line, return nil."
  (condition-case nil
      (save-excursion
	(beginning-of-line)
	(matlab-up-list -1)
	(and (looking-at "[[{]") (point)))
    (error nil)))

(defun matlab-lattr-array-end ()
  "Return non-nil if the current line closes an array.
by close, the first character is the end of an array."
  (save-excursion
    (back-to-indentation)
    (and (looking-at "[]}]") (matlab-lattr-array-cont))))

(defun matlab-lattr-block-cont (&optional eol)
  "Return a number representing the number of unterminated block constructs.
This is any block, such as if or for, that doesn't have an END on this line.
Optional EOL indicates a virtual end of line."
  (let ((v 0))
    (save-excursion
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) (or eol (matlab-point-at-eol)))
	(matlab-navigation-syntax
	  (while (re-search-forward (concat "\\<" (matlab-block-beg-re) "\\>")
				    nil t)
	    (if (or (matlab-cursor-in-string-or-comment)
		    (not (save-excursion (forward-word -1)
					 (matlab-cursor-on-valid-block-start))))
		;; Do nothing if in comment, or if the thing we skipped over was
		;; an invalid block construct (based on local context)
		nil
	      ;; Increment counter, move to end.
	      (setq v (1+ v))
	      (let ((p (point)))
		(forward-word -1)
		(condition-case nil
		    (progn
		      (matlab-forward-sexp)
		      (setq v (1- v)))
		  (error (goto-char p))))))
	  v)))))

(defun matlab-lattr-middle-block-cont ()
  "Return the number of middle block continuations.
This should be 1 or nil, and only true if the line starts with one of these
special items."
  (save-excursion
    (back-to-indentation)
    (if (looking-at (concat (matlab-block-mid-re) "\\>"))
	(if (and (re-search-forward (matlab-block-end-pre)
				    (matlab-point-at-eol)
				    t)
		 (matlab-valid-end-construct-p))
	    ;; If there is an END, we still need to return non-nil,
	    ;; but the number value is a net of 0.
	    0
	  1)
      nil)))

(defun matlab-lattr-endless-block-cont ()
  "Return the number of middle block continuations.
This should be 1 or nil, and only true if the line starts with one of these
special items."
  (save-excursion
    (back-to-indentation)
    (if (looking-at (concat (matlab-endless-blocks-re) "\\>"))
	1
      nil)))

(defun matlab-lattr-block-close (&optional start)
  "Return the number of closing block constructs.
Argument START is where to start searching from."
  (let ((v 0))
    (save-excursion
      (when start (goto-char start))
      (matlab-with-current-command
	(goto-char (point-at-eol))

	;; If in a comment, move out of it first.
	(when (matlab-beginning-of-string-or-comment)
	  ;; in case of no space between comment and end, need to move back
	  ;; over the comment chart for next search to work.
	  ;;(forward-char 1)
	  )

	;; Count every END in the line, skipping over active blocks
	(while (re-search-backward (concat "\\<" (matlab-block-end-re) "\\>")
				   nil t)
	  (let ((startmove (match-end 0))
		(nomove (point)))
	    (cond
	     ((matlab-beginning-of-string-or-comment)
	      ;; Above returns non-nil if it was in a string or comment.
	      ;; In that case, we need to keep going.
	      nil)
	     ((not (matlab-valid-end-construct-p))
	      ;; Not a valid end, just move past it.
	      (goto-char nomove))
	     (t
	      ;; Lets count these end constructs.
	      (setq v (1+ v))
	      (if (matlab-backward-sexp t t)
		  (setq v (1- v))
		(goto-char nomove)))
	     )))
	;; If we can't scoot back, do a cheat-test to see if there
	;; is a matching else or elseif.
	(goto-char (point-min))
	(back-to-indentation)
	(if (looking-at (matlab-block-mid-re))
	    (setq v (1- v)))
	;; Return nil, or a number
	(if (<= v 0) nil v)))))

(defun matlab-lattr-local-end ()
  "Return t if this line begins with an end construct."
  (save-excursion
    (back-to-indentation)
    (and (looking-at (concat "\\<" (matlab-block-end-re) "\\>"))
         (matlab-valid-end-construct-p))))

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
  (let ((eol (matlab-point-at-eol))
	(p (point))
	(signal-error-on-buffer-boundary nil))
    (beginning-of-line)
    (while (and (re-search-forward "%" eol t)
		(save-excursion (forward-char -1) (matlab-cursor-in-string t))))
    (if (not (bolp)) (forward-char -1))
    (if (and (looking-at "%") (not (matlab-cursor-in-string t)))
	(point)
      (goto-char p)
      nil)))

;;; Indent functions ==========================================================

(defun matlab-indent-line ()
  "Indent a line in `matlab-mode'."
  (interactive)
  (let ((i (matlab-calc-indent))
	(ci (current-indentation))
	(cc (current-column)))
    (save-excursion
      (back-to-indentation)
      (if (= i (current-column))
	  nil
	(beginning-of-line)
	(delete-horizontal-space)
	(indent-to i)))
    (if (<= cc ci) (move-to-column (max 0 i)))
    ))

(defun matlab-calc-indent ()
  "Return the appropriate indentation for this line as an integer."
  (interactive)
  ;; The first step is to find the current indentation.
  ;; This is defined to be zero if all previous lines are empty.
  (let* ((ci (if (matlab-lattr-array-cont)
		 ;; If we are inside an array continuation, then we shouldn't
		 ;; need to do anything complicated here b/c we'll just ignore
		 ;; the returned value in the next step.  Return current indentation.
		 (save-excursion
		   (matlab-prev-line)
		   (current-indentation))
	       ;; Else, the next line might recommend an indentation based
	       ;; on it's own context.
	       (save-excursion (if (not (matlab-prev-line))
                                 0
                               (matlab-next-line-indentation)))))
         (sem (matlab-calculate-indentation ci)))
    ;; simplistic
    (nth 1 sem)))

(defconst matlab-functions-have-end-should-be-true
  "This end closes a function definition.\nDo you want functions to have ends? "
  "Prompt the user about whether to change `matlab-functions-have-end'.")

(defun matlab-calculate-indentation (current-indentation)
  "Calculate out the indentation of the current line.
Return a list of descriptions for this line.  Return format is:
 '(TYPE DEPTHNUMBER)
where TYPE is one of (comment, code, function, blockstart, blockmid,
blockendless, blockend) DEPTHNUMBER is how many characters to indent
this line.
  Argument CURRENT-INDENTATION is what the previous line thinks
this line's indentation should be.  See `matlab-next-line-indentation'."
  (matlab-navigation-syntax
    (matlab-calculate-indentation-1 current-indentation)))

(defun matlab--maybe-yes-or-no-p (prompt noninteractive-default)
  "When in non-interactive mode run (yes-or-no-p prompt),
otherwise return NONINTERACTIVE-DEFAULT"
  (if noninteractive
      noninteractive-default
    (yes-or-no-p prompt)))


(defun matlab-calculate-indentation-1 (current-indentation)
  "Do the indentation work of `matlab-calculate-indentation'.
Argument CURRENT-INDENTATION is what the previous line recommends for indentation."
  (let ((ci current-indentation)
	(lvl1 (matlab-compute-line-context 1))
	(blockcomm nil)
	(tmp nil))
    (cond
     ;; COMMENTS
     ((matlab-line-comment-p lvl1)
      (let ((comment-style (matlab-line-comment-style lvl1)))
	(cond
	 ;; BLOCK START is like regular comment
	 ((eq comment-style 'block-start)
	  ;; indent like code, but some users like anti-indent
	  (list 'comment (+ ci matlab-comment-anti-indent))
	  )
	 ;; BLOCK END undoes body indent
	 ((or (eq comment-style 'block-end)
	      (eq comment-style 'block-body-prefix)) ; body prefix has same lineup rule
	  (list 'comment (matlab-line-end-comment-column lvl1)))
	 ;; BLOCK BODY is indented slightly from the start.
	 ((eq comment-style 'block-body)
	  (list 'comment (+ 2 (matlab-line-end-comment-column lvl1))))
	 ;; HELP COMMENT and COMMENT REGION
	 ((setq tmp (matlab-line-comment-help-p lvl1))
	  (list 'comment-help tmp))
	 ;; COMMENT REGION comments
	 ((matlab-line-comment-ignore-p lvl1)
	  (list 'comment-ignore 0))
	 ;; COMMENT Continued From Previous Line
	 ((setq tmp (matlab-ltype-continued-comm)) ;;; TODO : REPLACE
	  (list 'comment tmp))
	 (t
	  (list 'comment (+ ci matlab-comment-anti-indent))))))
     ;; FUNCTION DEFINITION
     ((matlab-line-declaration-p lvl1)
      (if matlab-functions-have-end
          ;; A function line has intrinsic indentation iff function bodies are
          ;; not indented and the function line is nested within another function.
          (if (and (not (matlab-indent-function-body-p))
                   (save-excursion
                     (beginning-of-line)
                     (matlab-beginning-of-enclosing-defun)))
              (setq ci (+ ci matlab-indent-level))
            ;; If no intrinsic indentation, do not change from ci.
            )
        ;; If functions are not nested, functions go to left margin.
        (setq ci 0))
      (list 'function ci))
     ;; END keyword
     ((matlab-line-end-p lvl1)
      ;;(matlab-lattr-local-end)
      (let ((end-of-function
             (let ((matlab-functions-have-end t))
               (save-excursion
                 (beginning-of-line)
                 (matlab-backward-sexp t t)
                 (matlab-ltype-function-definition)))))
        (if end-of-function
            (if (or matlab-functions-have-end
                    (if (matlab--maybe-yes-or-no-p matlab-functions-have-end-should-be-true t)
                        (matlab-functions-have-end-minor-mode 1)
                      (error "Unmatched end")))
                (if (matlab-indent-function-body-p)
                    (setq ci (- ci matlab-indent-level))))
          ;; Next, see if this line starts with an end, and whether the
          ;; end is matched, and whether the line is blank up to the match.
          ;; If so, return the indentation of the match.
          (catch 'indent
            (save-excursion
              (when (progn (beginning-of-line)
                           (and (looking-at "[ \t]*end\\b")
                                (matlab-backward-sexp t t)))
                (let ((match (point)))
                  (beginning-of-line)
                  (looking-at "[ \t]*")
                  (when (= match (match-end 0))
                    (let ((match-col-end
                           (save-excursion
                             (goto-char match)
                             (current-column)))
                          (match-col-beginning
                           (save-excursion
                             (goto-char (match-beginning 0))
                             (current-column)))
                          )
                      (setq ci (- match-col-end match-col-beginning)))
                    (throw 'indent nil)))))
            ;; End of special case for end and match after "^[ \t]*".
            (setq ci (+ ci
                        (* (1- (matlab-lattr-block-cont (point)))
                           matlab-indent-level))))))
      (list 'blockend ci))
     ;; ELSE/CATCH keywords
     ((matlab-line-block-middle-p lvl1)
      ;(matlab-lattr-middle-block-cont)
      (let ((m (match-string 1)))
	(list 'blockmid
	      (condition-case nil
		  (save-excursion
		    (beginning-of-line)
		    (matlab-backward-sexp t)
		    (if (matlab-ltype-function-definition) (error ""))
		    (current-column))
		(error (error "Unmatched %s" m))))))
     ;; CASE/OTHERWISE keywords
     ((matlab-line-block-case-p lvl1)
      ;;(matlab-lattr-endless-block-cont)
      (list 'blockendless
	    (condition-case nil
		(save-excursion
		  (beginning-of-line)
		  (matlab-backward-sexp t)
		  (if (not (looking-at "switch\\>")) (error ""))
		  (+ (current-column)
		     (if (listp matlab-case-level)
			 (car matlab-case-level)
		       matlab-case-level)))
	      (error (error "Unmatched case/otherwise part")))))
     ;; End of a MATRIX
     ((matlab-line-close-paren-p lvl1)
      ;;(matlab-lattr-array-end)
      (list 'array-end (let* ((fc (matlab-line-close-paren-char lvl1)) ;;following-char))
			      (pc (matlab-line-close-paren-col lvl1))
			      (mi (assoc fc matlab-maximum-indents))
			      (max (if mi (if (listp (cdr mi))
					      (car (cdr mi)) (cdr mi))
				     nil))
			      (ind (if mi (if (listp (cdr mi))
					      (cdr (cdr mi)) (cdr mi))
				     nil)))
			 ;; apply the maximum limits.
			 (if (and ind (> (- pc ci) max))
			     (1- ind)	; decor
			   pc))))
     ;; Code lines
     ((and (not (matlab-lattr-array-cont))
	   (not (matlab-prev-line-cont)))

      ;; Old check for base code line was using
      ;; 'matlab-beginning-of-command' which does a lot of work, like
      ;; checking for block comments and such.  We already stripped
      ;; out most options in that cmd and know we are on a code line,
      ;; so only check if the previous line is a continuation line.
      ;; Old code:
      ;;(save-excursion
      ;;  (beginning-of-line)
      ;;  (back-to-indentation)
      ;;  (= (point) (progn (matlab-beginning-of-command) (point))))

      ;; This means we are at the beginning of a command structure.
      ;; Always match up against the previous line.
      (list 'code ci))
     ;; Lines continued from previous statements.
     (t
      (list (if (matlab-ltype-empty) 'empty
	      (if (matlab-lattr-array-cont) 'array-cont 'code))
	    ;; Record beginning of the command
	    (let ((boc (save-excursion
			 (matlab-beginning-of-command)
			 (point))))
	      (condition-case nil
		  (save-excursion
		    (beginning-of-line)
		    (matlab-up-list -1)
		    (if (> boc (point)) (error nil))
		    ;; Ok, it MIGHT be that we are in a program
		    ;; statement, and this particular command is an HG
		    ;; statement that would look better if the
		    ;; following lines lined up AFTER the first
		    ;; argument.  Lets look.
		    (let ((parendepth (current-column)))
		      (cond ((and (= (following-char) ?\( )
				  (save-excursion
				    (matlab-navigation-syntax
				      (forward-word -1)
				      (looking-at
				       matlab-indent-past-arg1-functions)))
				  (let ((start-paren (point)))
				    (while
					(and
					 (re-search-forward
					  "," (matlab-point-at-eol) t)
					 (save-excursion
					   (matlab-up-list -1)
					   (> (point) start-paren))))
				    (if (and
					 (= (preceding-char) ?,)
					 ;; Don't bother if we hit the EOL.
					 (not (looking-at

					       "\\s-*\\(\\.\\.\\.\\|$\\|)\\)")))
					t
				      (move-to-column parendepth)
				      nil)))
			     (skip-chars-forward " \t")
			     (if (> (- (current-column) parendepth)
				    matlab-arg1-max-indent-length)
				 (+ parendepth matlab-arg1-max-indent-length)
			       (current-column)))
			    (t
			     (let* ((fc (following-char))
				    (mi (assoc fc matlab-maximum-indents))
				    (max (if mi
					     (if (listp (cdr mi))
						 (car (cdr mi)) (cdr mi))
					   nil))
				    (ind (if mi
					     (if (listp (cdr mi))
						 (cdr (cdr mi)) (cdr mi))
					   nil)))
			       (forward-char 1)
			       (skip-chars-forward " \t")
			       ;; If we are at the end of a line and
			       ;; this open paren is there, then we
			       ;; DON'T want to indent to it.  Use the
			       ;; standard indent.
			       (if (or (not matlab-align-to-paren)
			               (looking-at "\\.\\.\\.\\|$"))
				   ;; This could happen in another set
				   ;; of matrices.  Find a current
				   ;; indentation based on the
				   ;; previous line.
				   (let ((cci (current-indentation)))
				     (+ cci matlab-cont-level))
				 ;; apply the maximum limits.
				 (if (and ind (> (- (current-column) ci) max))
				     (+ ci ind)
				   (current-column))))))))
		(error
		 ;; Line up to an equals sign.
		 (save-excursion
		   (goto-char boc)
		   (while (and (re-search-forward "=" (matlab-point-at-eol) t)
			       (matlab-cursor-in-string-or-comment)))
		   (if (/= (preceding-char) ?=)
		       (+ ci matlab-cont-level)
		     (skip-chars-forward " \t")
		     (let ((cc (current-column))
			   (mi (assoc ?= matlab-maximum-indents)))
		       (if (looking-at "\\.\\.\\.\\|$")
			   ;; In this case, the user obviously wants the
			   ;; indentation to be somewhere else.
			   (+ ci (cdr (cdr mi)))
			 ;; If the indent delta is greater than the max,
			 ;; use the max + current
			 (if (and mi (> (- cc ci) (if (listp (cdr mi))
						      (car (cdr mi))
						    (cdr mi))))
			     (setq cc (+ ci (if (listp (cdr mi))
						(cdr (cdr mi))
					      (cdr mi)))))
			 cc)))))))))
     )))

(defun matlab-next-line-indentation ()
  "Calculate the indentation for lines following this command line.
Assume that the following line does not contribute its own indentation
\(as it does in the case of nested functions in the following situations):
  o function---positive indentation when not indenting function bodies.
  o end---negative indentation except when the 'end' matches a function and
    not indenting function bodies.
See `matlab-calculate-indentation'."
  (matlab-navigation-syntax
    (let ((startpnt (point-at-eol)))
      (save-excursion
	(matlab-with-current-command
	  ;;(matlab-beginning-of-command)
	  (goto-char (point-min))
	  (back-to-indentation)
	  (let ((cc (or (matlab-lattr-block-close startpnt) 0))
		(end (matlab-lattr-local-end))
		(bc (matlab-lattr-block-cont startpnt))
		(mc (matlab-lattr-middle-block-cont))
		(ec (matlab-lattr-endless-block-cont))
		(hc (and (matlab-indent-function-body-p) (matlab-ltype-help-comm)))
		(rc (and (/= 0 matlab-comment-anti-indent)
			 (matlab-ltype-comm-noblock)
			 (not (matlab-ltype-help-comm))
			 (not (matlab-ltype-continued-comm))))
		(ci (current-indentation)))
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
		     (or (matlab-ltype-function-definition)
			 (and (matlab-lattr-local-end)
			      (save-excursion
				(matlab-backward-sexp t)
				(looking-at "function\\b")))))
		    (if (> bc 0)
			(setq bc (1- bc))
		      (if (>= ci matlab-indent-level)
			  (setq bc -1))))
	      ;; Else, funtions don't have ends in this file.
	      (if (and (matlab-indent-function-body-p) (matlab-ltype-function-definition))
		  (setq bc (1+ bc))))
	    ;; Remove 1 from the close count if there is an END on the beginning
	    ;; of this line, since in that case, the unindent has already happened.
	    (when end (setq cc (1- cc)))
	    ;; Calculate the suggested indentation.
	    (+ ci
	       (* matlab-indent-level bc)
	       (* matlab-indent-level (or mc 0))
	       (* matlab-indent-level (- cc))
	       (* (if (listp matlab-case-level)
		      (cdr matlab-case-level) matlab-case-level)
		  (or ec 0))
	       (if hc matlab-indent-level 0)
	       (if rc (- 0 matlab-comment-anti-indent) 0)
	       )))))))

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
  (if (save-excursion
	(beginning-of-line)
	(looking-at (concat "^\\s-*\\(" (matlab-block-end-re)
			    "\\|" (matlab-block-mid-re)
			    "\\|" (matlab-endless-blocks-re)
			    "\\|function\\)")))
      (matlab-indent-line))
  (newline)
  (matlab-indent-line))

(defvar matlab-quiesce-nosemi-regexp) ;; quiet compiler warning (var is defined below)
(defun matlab-semicolon-on-return ()
  "If needed, add a semicolon at point automatically."
  (if matlab-return-add-semicolon
      (if (and (not (matlab-ltype-empty))
	       (not (save-excursion
		      (skip-chars-backward " \t;" (matlab-point-at-bol))
		      (looking-at "\\s-*;")))
	       (save-excursion
		 (let ((p (point)))
		   (matlab-end-of-command)
		   (eq p (point))))
	       (save-excursion
		 (matlab-beginning-of-command)
		 ;; Note: Compile warning below, but defined later.
		 (not (looking-at matlab-quiesce-nosemi-regexp))))
	  (insert ";"))
    ))

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
  (cond
   ((matlab-ltype-comm)
    (matlab-set-comm-fill-prefix) (newline) (insert fill-prefix)
    (matlab-reset-fill-prefix) (matlab-indent-line))
   ((matlab-lattr-comm)
    (newline) (indent-to comment-column)
    (insert matlab-comment-on-line-s))
   (t
    (newline) (matlab-comment) (matlab-indent-line))))

(defun matlab-comm-from-prev ()
  "If the previous line is a `comment-line' then set up a comment on this line."
  (save-excursion
    ;; If the previous line is a comment-line then set the fill prefix from
    ;; the previous line and fill this line.
    (if (and (= 0 (forward-line -1)) (matlab-ltype-comm))
	(progn
	  (matlab-set-comm-fill-prefix)
	  (forward-line 1) (beginning-of-line)
	  (delete-horizontal-space)
	  (if (looking-at "%") (delete-char 1))
	  (delete-horizontal-space)
	  (insert fill-prefix)
	  (matlab-reset-fill-prefix)))))

(defun matlab-electric-comment (arg)
  "Indent line and insert comment character.
Argument ARG specifies how many %s to insert."
  (interactive "P")
  (self-insert-command (or arg 1))
  (when (matlab-ltype-comm)
    (matlab-indent-line)
    ;; The above seems to put the cursor on the %, not after it.
    (skip-chars-forward "%")))

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
  (cond ((region-active-p)
	 (call-interactively #'comment-or-uncomment-region))
	((matlab-ltype-empty)		; empty line
	 (matlab-comm-from-prev)
	 (if (matlab-lattr-comm)
	     (skip-chars-forward " \t%")
	   (insert matlab-comment-line-s)
	   (matlab-indent-line)))
	((matlab-ltype-comm)		; comment line
	 (matlab-comm-from-prev)
	 (skip-chars-forward " \t%"))
	((matlab-lattr-comm)		; code line w/ comment
	 (beginning-of-line)
	 (re-search-forward "[^%]\\(%\\)[ \t]")
	 (goto-char (match-beginning 1))
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
	 (insert matlab-comment-on-line-s))))

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
  (matlab-calc-indent))

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
  (if (matlab-lattr-comm)
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
  (if (matlab-lattr-cont)
      nil
    (save-restriction
      (narrow-to-region (matlab-point-at-bol) (matlab-point-at-eol))
      ;; get ourselves onto the fill-column.
      (move-to-column fill-column)
      (let ((pos nil)
	    (orig (point)))
	(or
	 ;; Next, if we have a trailing comment, use that.
	 (progn (setq pos (or (matlab-lattr-comm) (matlab-point-at-bol)))
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
		nil))))))

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
			      (if (matlab-cursor-in-string 'incomplete)
				  4 3))
			  0))))
    (if (> (current-column) fill-column)
	(cond
	 ((matlab-ltype-comm-ignore)
	  nil)
	 ((or (matlab-ltype-comm)
	      (and (save-excursion (move-to-column fill-column)
				   (matlab-cursor-in-comment))
		   (matlab-lattr-comm)))
	  ;; If the whole line is a comment, do this.
	  (matlab-set-comm-fill-prefix) (do-auto-fill)
	  (matlab-reset-fill-prefix))
	 ((and (matlab-ltype-code)
	       (not (matlab-lattr-cont))
	       matlab-fill-code)
	  ;; If we are on a code line, we ellipsify before we fill.
	  (let ((m (make-marker)))
	    (move-marker m (point))
	    (set-marker-insertion-type m t)
	    (if (not (matlab-find-convenient-line-break))
		nil
	      (if (not (save-excursion
			 (forward-char -1)
			 (matlab-cursor-in-string 'incomplete)))
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
  (cond ((or (matlab-ltype-comm)
	     (and (matlab-cursor-in-comment)
		  (not (matlab-lattr-cont))))
	 ;; We are in a comment, lets fill the paragraph with some
	 ;; nice regular expressions.
	 ;; Cell start/end markers of %% also separate paragraphs
	 (let ((paragraph-separate "%%\\|%[a-zA-Z]\\|%[ \t]*$\\|[ \t]*$")
	       (paragraph-start "%[a-zA-Z]\\|%[ \t]*$\\|[ \t]*$\\|%\\s-*\\*")
	       (paragraph-ignore-fill-prefix nil)
	       (start (save-excursion (matlab-beginning-of-command)
				      (if (looking-at "%%")
					  (progn (end-of-line)
						 (forward-char 1)))
				      (beginning-of-line)
				      (point)))
	       (end (save-excursion (matlab-end-of-command)
				    (point)))
	       (fill-prefix nil))
	   (matlab-set-comm-fill-prefix)
	   (save-restriction
	     ;; Ben North fixed to handle comment at the end of
	     ;; a buffer.
	     (narrow-to-region start (min (point-max) (+ end 1)))
	     (fill-paragraph arg))))
	((matlab-ltype-code)
	 ;; Ok, lets get the outer bounds of this command, then
	 ;; completely refill it using the smart line breaking code.
	 (save-restriction
	   (narrow-to-region (save-excursion
			       (matlab-beginning-of-command)
			       (beginning-of-line)
			       (point))
			     (save-excursion
			       (matlab-end-of-command)
			       (point)))
	   ;; Remove all line breaks
	   (goto-char (point-min))
	   (while (and (re-search-forward "$" nil t)
		       (not (eobp)))
	     (delete-horizontal-space)
	     ;; Blow away continuation marks
	     (if (matlab-lattr-cont)
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


;;; Show Paren Mode support ==================================================

(defun matlab-show-paren-or-block ()
  "Function to assign to `show-paren-data-function'.
Highlights parens and if/end type blocks.
Returns a list: \(HERE-BEG HERE-END THERE-BEG THERE-END MISMATCH)"
  (unless (or (matlab-cursor-in-string-or-comment) ; Only do this if not in a string.
	      (matlab-block-comment-bounds))
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
		 (when (and (not (eobp)) (not (bobp)) (= (syntax-class here-prev-syntax) 2))
		   (forward-symbol -1))

		 (matlab-navigation-syntax

		   (condition-case err
		       (cond
			((looking-at "function\\>")
			 ;; We are looking at a 'function' start.  Since functions may not have an end, we need
			 ;; to handle this case special.
			 (setq here-beg (match-beginning 0)
			       here-end (match-end 0))
			 (matlab-forward-sexp)
			 (backward-word 1)
			 (looking-at (concat (matlab-block-end-pre) "\\>"))
			 (setq there-beg (match-beginning 0)
			       there-end (match-end 0)
			       mismatch nil)
			 )
			((matlab-cursor-on-valid-block-start)
			 ;; Above is similar to vvv but with special checks.
			 ;; Still need to do vvv b/c we need the match-data.
			 (looking-at (concat (matlab-block-beg-re) "\\>"))
			 
			 ;; We are at the beginning of a block.  Navigate forward to the end
			 ;; statement.
			 (setq here-beg (match-beginning 0)
			       here-end (match-end 0))
			 (matlab-forward-sexp)
			 (backward-word 1)
			 (looking-at (concat (matlab-block-end-pre) "\\>"))
			 (setq there-beg (match-beginning 0)
			       there-end (match-end 0)
			       mismatch nil)
			 )
			((and (looking-at (concat "\\(" (matlab-block-end-pre) "\\)\\>"))
			      (matlab-valid-end-construct-p))
			 ;; We are at the end of a block.  Navigate to the beginning
			 (setq here-beg (match-beginning 0)
			       here-end (match-end 0))
			 (when (matlab-backward-sexp t t)
			   (looking-at (concat (matlab-block-beg-re) "\\>"))
			   (setq there-beg (match-beginning 0)
				 there-end (match-end 0)
				 mismatch nil)
			   ))
			((looking-at (concat (matlab-block-mid-re) "\\>"))
			 ;; We are at a middle-block expression, like "else" or "catch'
			 ;; Ideally we'd show the beginning and the end, but lets just show
			 ;; the beginning.
			 (setq here-beg (match-beginning 0)
			       here-end (match-end 0))
			 (matlab-backward-sexp t)
			 (looking-at (concat (matlab-block-beg-re) "\\>"))
			 (setq there-beg (match-beginning 0)
			       there-end (match-end 0)
			       mismatch nil)
			 )

			((looking-at (concat (matlab-endless-blocks-re) "\\>"))
			 ;; We are at a middle-sub-block expression, like "case"
			 ;; Ideally we'd show the beginning and the end, but lets just show
			 ;; the beginning.
			 (setq here-beg (match-beginning 0)
			       here-end (match-end 0))
			 (matlab-backward-sexp t)
			 (looking-at (concat (matlab-block-beg-re) "\\>"))
			 (setq there-beg (match-beginning 0)
			       there-end (match-end 0)
			       mismatch nil)
			 )


			;; No block matches, just return nothing.
			(t (setq noreturn t))
			)
		     ;; An error occurred.  Assume 'here-*' is set, and setup mismatch.
		     (error (setq mismatch t)))


		   )))

	  (if noreturn
	      nil
	    (list here-beg here-end there-beg there-end mismatch) ))))))


;;; Block highlighting ========================================================

(defvar matlab-block-highlighter-timer nil
  "The timer representing the block highlighter.")

(defun matlab-enable-block-highlighting (&optional arg)
  "Start or stop the block highlighter.
Optional ARG is 1 to force enable, and -1 to disable.
If ARG is nil, then highlighting is toggled."
  (interactive "P")
  (if (not (fboundp 'matlab-run-with-idle-timer))
      (setq matlab-highlight-block-match-flag nil))
  ;; Only do it if it's enabled.
  (if (not matlab-highlight-block-match-flag)
      nil
    ;; Use post command idle hook as a local hook to dissuade too much
    ;; cpu time while doing other things.
    ;;(make-local-hook 'post-command-hook)
    (if (not arg)
	(setq arg
	      (if (member 'matlab-start-block-highlight-timer
			  post-command-hook)
		  -1 1)))
    (if (> arg 0)
	(add-hook 'post-command-hook 'matlab-start-block-highlight-timer nil :local)
      (remove-hook 'post-command-hook 'matlab-start-block-highlight-timer :local))))

(defvar matlab-block-highlight-overlay nil
  "The last highlighted overlay.")
(make-variable-buffer-local 'matlab-block-highlight-overlay)

(defvar matlab-block-highlight-timer nil
  "Last started timer.")
(make-variable-buffer-local 'matlab-block-highlight-timer)

(defun matlab-start-block-highlight-timer ()
  "Set up a one-shot timer if we are in MATLAB mode."
  (if (eq major-mode 'matlab-mode)
      (progn
	(if matlab-block-highlight-overlay
	    (unwind-protect
		(matlab-delete-overlay matlab-block-highlight-overlay)
	      (setq matlab-block-highlight-overlay nil)))
	(if matlab-block-highlight-timer
	    (unwind-protect
		(matlab-cancel-timer matlab-block-highlight-timer)
	      (setq matlab-block-highlight-timer nil)))
	(setq matlab-block-highlight-timer
	      (matlab-run-with-idle-timer
	       1 nil 'matlab-highlight-block-match
	       (current-buffer))))))

(defun matlab-highlight-block-match (&optional buff-when-launched)
  "Highlight a matching block if available.
BUFF-WHEN-LAUNCHED is the buffer that was active when the timer was set."
  (setq matlab-block-highlight-timer nil)
  (if (null buff-when-launched)
      ;; We were passed a null.  This indicates an old version of XEmacs
      ;; so just turn the feature off
      (setq matlab-highlight-block-match-flag nil)
  ;; Only do neat stuff in the same buffer as the one we were
  ;; initialized from.
  (when (and buff-when-launched
	     (eq buff-when-launched (current-buffer)))
    (let ((inhibit-quit nil)		;turn on G-g
	  (matlab-scan-on-screen-only t))
      (if matlab-show-periodic-code-details-flag
	  (matlab-show-line-info))
      (if (not (matlab-cursor-in-string-or-comment))
	  (save-excursion
	    (if (or (bolp)
		    (looking-at "\\s-")
		    (save-excursion (forward-char -1) (looking-at "\\s-")))
		nil
	      (forward-word -1))
	    (if (and (looking-at (concat (matlab-block-beg-re) "\\>"))
		     (not (looking-at "function")))
		(progn
		  ;; We scan forward...
		  (matlab-forward-sexp)
		  (backward-word 1)
		  (if (not (looking-at matlab-block-end-pre-if))
		      nil ;(message "Unterminated block, or end off screen.")
		    (setq matlab-block-highlight-overlay
			  (matlab-make-overlay (point)
					       (progn (forward-word 1)
						      (point))
					       (current-buffer)))
		    (matlab-overlay-put matlab-block-highlight-overlay
					'face 'matlab-region-face)))
	      (if (and (looking-at (concat (matlab-block-end-pre) "\\>"))
		       (not (looking-at "function"))
		       (matlab-valid-end-construct-p))
		  (progn
		    ;; We scan backward
		    (forward-word 1)
		    (condition-case nil
			(progn
			  (matlab-backward-sexp)
			  (if (not (looking-at (matlab-block-beg-re)))
			      nil ;(message "Unstarted block at cursor.")
			    (setq matlab-block-highlight-overlay
				  (matlab-make-overlay (point)
						       (progn (forward-word 1)
							      (point))
						       (current-buffer)))
			    (matlab-overlay-put matlab-block-highlight-overlay
						'face 'matlab-region-face)))
		      (error (message "Unstarted block at cursor."))))
		;; do nothing
		))))))))


;;; M Block Folding with hideshow =============================================

(defun matlab-hideshow-forward-sexp-func (arg)
  "Move forward one sexp for hideshow.
Argument ARG specifies the number of blocks to move forward."
  (beginning-of-line)
  (matlab-forward-sexp arg)
  )

(defun matlab-hideshow-adjust-beg-func (arg)
  "Adjust the beginning of a hideshow block.
Argument ARG to make it happy."
  (end-of-line)
  (point)
  )

;; Use this to enable hideshow in MATLAB.
;; It has not been tested by me enough.

;; REMOVE PUSHNEW FROM THIS LINE
;;(pushnew (list 'matlab-mode
;;	       (matlab-block-beg-pre)
;;	       (matlab-block-end-pre)
;;	       "%"
;;	       'matlab-hideshow-forward-sexp-func
;;	       'matlab-hideshow-adjust-beg-func
;;	       )
;;	 hs-special-modes-alist :test 'equal)


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
    (while (and (or (matlab-ltype-empty) (matlab-ltype-comm))
		(/= (matlab-point-at-eol) (point-max)))
      (forward-line 1))
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
    ;; Skip over whitespace.
    (while (and (or (matlab-ltype-empty) (matlab-ltype-comm))
		(/= (matlab-point-at-eol) (point-max)))
      (forward-line 1))
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
		       (or matlab-functions-have-end (not (looking-at "function"))))
		  (progn
		    (matlab-forward-sexp)
		    (forward-word -1)
		    (if (not (looking-at
			      (concat matlab-block-end-pre-no-if "\\>")))
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
  (let ((go t) (expr (concat "\\<\\(" (matlab-block-end-no-function-re)
			     "\\)\\>")))
    (matlab-navigation-syntax
      (while (and (not fast) go (re-search-backward expr nil t))
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
	(matlab-end-of-command)
	(if (matlab-cursor-in-comment)
	    (progn
	      (matlab-comment-on-line)
	      (skip-chars-backward " \t")))
	(if (and (not (= (preceding-char) ?\;))
		 (not (matlab-cursor-in-string t))
		 (not (save-excursion
			(beginning-of-line)
			(looking-at matlab-quiesce-nosemi-regexp))))
	    (let ((p (point)))
	      (skip-chars-backward " \t")
	      (if (/= p (point))
		  (progn
		    (delete-region p (point))
		    (forward-line -1))
		(if (matlab-mode-highlight-ask (point) (+ 1 (point))
					       "Add Semi colon here? ")
		    (insert ";")))))
	(forward-line 1))))
  (message "Scanning .... done"))



;;; matlab-mode debugging =====================================================

(defun matlab-show-line-info ()
  "Display type and attributes of current line.  Used in debugging."
  (interactive)
  (let ((msg "line-info:")
	(indent (matlab-calculate-indentation (current-indentation)))
	(nexti (matlab-next-line-indentation)))
    (setq msg (concat msg
		      " Line type: " (symbol-name (car indent))
		      " This Line: " (int-to-string (nth 1 indent))
		      " Next Line: " (int-to-string nexti)))
    (if (matlab-lattr-cont)
	(setq msg (concat msg " w/cont")))
    (if (matlab-lattr-comm)
	(setq msg (concat msg " w/comm")))
    (message msg)))


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
;; LocalWords:  Unstarted includeelse autostart lattr zerop cellstart blockcomm
;; LocalWords:  linebounds bol commtype startmove nomove charvector sregex
;; LocalWords:  insregex laststart bolp calc ci sem DEPTHNUMBER blockstart
;; LocalWords:  blockmid blockendless blockend unstarted listp boc parendepth
;; LocalWords:  cci startpnt hc rc nosemi emacsen afterd befored okpos startlst
;; LocalWords:  endlst ellipsify noreturn hs tc matchers hideshow func PUSHNEW
;; LocalWords:  pushnew bn nondirectory un msgpos nexti
