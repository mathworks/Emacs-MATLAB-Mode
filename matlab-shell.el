;;; matlab-shell.el --- Run MATLAB in an inferior process
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@ballista>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; This library supports a MATLAB shell buffer, which runs MATLAB in
;; an inferior shell.  Supports working with the MATLAB command line,
;; and the MATLAB debugger.
;;

;;; Code:
(require 'matlab)
(require 'matlab-compat)
(require 'comint)
(require 'server)

(eval-and-compile
  (require 'gud)
  (require 'shell)
  )

;; Slience warnings from company.el
(declare-function company-mode "company")
(defvar company-idle-delay)
(defvar company-mode)


;;; Customizations
;;
;; Options to configure using matlab-shell
(defgroup matlab-shell nil
  "MATLAB shell mode."
  :prefix "matlab-shell-"
  :group 'matlab)

;;
;; Shell Startup
(defcustom matlab-shell-mode-hook nil
  "*List of functions to call on entry to MATLAB shell mode."
  :group 'matlab-shell
  :type 'hook)

(defcustom matlab-shell-command "matlab"
  "*The name of the command to be run which will start the MATLAB process."
  :group 'matlab-shell
  :type 'string)

(defcustom matlab-shell-command-switches '("-nodesktop")
  "*Command line parameters run with `matlab-shell-command'.
Command switches are a list of strings.  Each entry is one switch."
  :group 'matlab-shell
  :type '(list :tag "Switch: "))

(defvar matlab-custom-startup-command nil
  "Custom matlab command to be run at startup")

(defcustom matlab-shell-echoes t
  "*If `matlab-shell-command' echoes input."
  :group 'matlab-shell
  :type 'boolean)

(defcustom matlab-shell-history-file "~/.matlab/%s/history.m"
  "*Location of the history file.
A %s is replaced with the MATLAB version release number, such as R12.
This file is read to initialize the comint input ring."
  :group 'matlab
  :type 'filename)

;;
;; Edit from MATLAB
(defcustom matlab-shell-emacsclient-command "emacsclient -n"
  "*The command to use as an external editor for MATLAB.
Using emacsclient allows the currently running Emacs to also be the
external editor for MATLAB. Setting this to the empty string
will disable use emacsclient as the external editor."
  :group 'matlab-shell
  :type 'integer)

;;
;; Features in an active shell
(defcustom matlab-shell-input-ring-size 32
  "*Number of history elements to keep."
  :group 'matlab-shell
  :type 'integer)

(defcustom matlab-shell-enable-gud-flag t
  "*Non-nil means to use GUD mode when running the MATLAB shell."
  :group 'matlab-shell
  :type 'boolean)

;;
;; Completion handling
(defcustom matlab-shell-ask-MATLAB-for-completions t
  "When Non-nil, ask MATLAB for a completion list.
When nil, complete against file names."
  :group 'matlab-shell
  :type 'boolean)

(defcustom matlab-shell-tab-use-company t
  "*Use `company' (complete anything) for TAB completions in
MATLAB shell when `company' is installed. Note, when you type to
narrow completions, you may find the responses slow and if so,
you can try turning this off."
  :group 'matlab-shell
  :type 'boolean)

(defvar matlab-shell-tab-company-available (if (locate-library "company") t nil)
  "If we have `company' (completion anything) use it to show
`matlab-shell' completions when `matlab-shell-tab-use-company' is t.")

;;; Font Lock
;;
;; Extra font lock keywords for the MATLAB shell.
(defvar matlab-shell-font-lock-keywords
  (list
   ;; Startup notices
   ;; Various notices
   '(" M A T L A B " 0 'underline)
   '("All Rights Reserved" 0 'italic)
   '("\\(\\(?:(c)\\)?\\s-+Copyright[^\n]+\\)" 1 font-lock-comment-face)
   '("\\(Version\\)\\s-+\\([^\n]+\\)"
     (1 font-lock-function-name-face) (2 font-lock-variable-name-face))
   '("\\(R[0-9]+[ab]\\(?: Update [0-9]+\\)\\) \\([^\n]+\\)"
     (1 font-lock-function-name-face) (2 font-lock-variable-name-face))
   '("^To get started, type doc.$" 0 font-lock-comment-face prepend)
   '("For product information, [^\n]+" 0 font-lock-comment-face)

   ;; How about Errors?
   '("^\\(Error in\\|Syntax error in\\)\\s-+==>\\s-+\\(.+\\)$"
     (1 font-lock-comment-face) (2 font-lock-string-face))
   ;; and line numbers
   '("^\\(\\(On \\)?line [0-9]+\\)" 1 font-lock-comment-face)
   ;; User beep things
   '("\\(\\?\\?\\?[^\n]+\\)" 1 font-lock-comment-face)
   ;; Useful user commands, but not useful programming constructs
   '("\\<\\(demo\\|whatsnew\\|info\\|subscribe\\|help\\|doc\\|lookfor\\|what\
\\|whos?\\|cd\\|clear\\|load\\|save\\|helpdesk\\|helpwin\\)\\>"
     1 font-lock-keyword-face)
   )
  "Additional keywords used by MATLAB when reporting errors in interactive\
mode.")

(defvar matlab-shell-font-lock-keywords-1
  (append matlab-font-lock-keywords matlab-shell-font-lock-keywords)
  "Keyword symbol used for font-lock mode.")

(defvar matlab-shell-font-lock-keywords-2
  (append matlab-shell-font-lock-keywords-1 matlab-gaudy-font-lock-keywords)
  "Keyword symbol used for gaudy font-lock symbols.")

(defvar matlab-shell-font-lock-keywords-3
  (append matlab-shell-font-lock-keywords-2
	  matlab-really-gaudy-font-lock-keywords)
  "Keyword symbol used for really gaudy font-lock symbols.")

;;; ROOT
;;
;;;###autoload
(defun matlab-mode-determine-matlabroot ()
  "Return the MATLABROOT for the 'matlab-shell-command'."
  (let ((path (file-name-directory matlab-shell-command)))
    ;; if we don't have a path, find the MATLAB executable on our path.
    (when (not path)
      (setq path  (matlab-find-executible-directory matlab-shell-command)))
    (when path
      ;; When we find the path, we need to massage it to identify where
      ;; the M files are that we need for our completion lists.
      (if (string-match "/bin/?$" path)
	  (setq path (substring path 0 (match-beginning 0)))))
    path))


;;; Keymaps & Menus
;;
(defvar matlab-shell-mode-map
  (let ((km (make-sparse-keymap 'matlab-shell-mode-map)))
    ;; Mostly use comint mode's map.
    (matlab-set-keymap-parent km comint-mode-map)

    ;; We can jump to errors, so take over this keybinding.
    (substitute-key-definition 'next-error 'matlab-shell-last-error
			       km global-map)

    ;; Help system
    (define-key km [(control h) (control m)] matlab-help-map)

    ;; Completion
    (define-key km (kbd "TAB") 'matlab-shell-tab)
    (define-key km "\C-i" 'matlab-shell-tab)
    (define-key km (kbd "<C-tab>") 'matlab-shell-c-tab)

    ;; Command history
    (define-key km [(control up)] 'comint-previous-matching-input-from-input)
    (define-key km [(control down)] 'comint-next-matching-input-from-input)
    (define-key km [up] 'matlab-shell-previous-matching-input-from-input)
    (define-key km [down] 'matlab-shell-next-matching-input-from-input)

    ;; Editing
    (define-key km [(control return)] 'comint-kill-input)
    (define-key km [(backspace)] 'matlab-shell-delete-backwards-no-prompt)

    ;; Files
    (define-key km "\C-c." 'matlab-find-file-on-path)

    km)

  "Keymap used in `matlab-shell-mode'.")

(easy-menu-define matlab-shell-menu
  matlab-shell-mode-map
  "MATLAB shell menu"
  '("MATLAB"
    ["Goto last error" matlab-shell-last-error t]
    "----"
    ["Stop On Errors" matlab-shell-dbstop-error t]
    ["Don't Stop On Errors" matlab-shell-dbclear-error t]
    "----"
    ["Run Command" matlab-shell-run-command t]
    ["Describe Variable" matlab-shell-describe-variable t]
    ["Describe Command" matlab-shell-describe-command t]
    ["Lookfor Command" matlab-shell-apropos t]
    ["Topic Browser" matlab-shell-topic-browser t]
    "----"
    ["Complete command" matlab-shell-tab t]
    "----"
    ["Demos" matlab-shell-demos t]
    ["Close Current Figure" matlab-shell-close-current-figure t]
    ["Close Figures" matlab-shell-close-figures t]
    "----"
    ["Customize" (customize-group 'matlab-shell)
     (and (featurep 'custom) (fboundp 'custom-declare-variable))
     ]
    ["Exit" matlab-shell-exit t]))
(easy-menu-add matlab-shell-menu matlab-shell-mode-map)


;;; MODE
;;
;; The Emacs major mode for interacting with the matlab shell process.

(defvar matlab-shell-last-error-anchor) ;; Quiet compiler warning

(defun matlab-shell-mode ()
  "Run MATLAB as a subprocess in an Emacs buffer.

This mode will allow standard Emacs shell commands/completion to occur
with MATLAB running as an inferior process.  Additionally, this shell
mode is integrated with `matlab-mode', a major mode for editing M
code.

> From an M file buffer:
\\<matlab-mode-map>
\\[matlab-shell-save-and-go] - Save the current M file, and run it in a \
MATLAB shell.

> From Shell mode:
\\<matlab-shell-mode-map>
\\[matlab-shell-last-error] - find location of last MATLAB runtime error \
in the offending M file.

> From an M file, or from Shell mode:
\\<matlab-mode-map>
\\[matlab-shell-run-command] - Run COMMAND and show result in a popup buffer.
\\[matlab-shell-describe-variable] - Show variable contents in a popup buffer.
\\[matlab-shell-describe-command] - Show online documentation for a command \
in a popup buffer.
\\[matlab-shell-apropos] - Show output from LOOKFOR command in a popup buffer.
\\[matlab-shell-topic-browser] - Topic browser using HELP.

> Keymap:
\\{matlab-mode-map}"
  (setq major-mode 'matlab-shell-mode
	mode-name "M-Shell"
	comint-prompt-regexp "^\\(K\\|EDU\\)?>> *"
	comint-delimiter-argument-list (list [ 59 ]) ; semi colon
	comint-dynamic-complete-functions '(comint-replace-by-expanded-history)
	comint-process-echoes matlab-shell-echoes
	)
  ;; matlab-shell variable setup
  (make-local-variable 'matlab-shell-last-error-anchor)
  (setq matlab-shell-last-error-anchor nil)

  ;; Shell Setup
  (require 'shell)
  (if (fboundp 'shell-directory-tracker)
      (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t)) ;; patch Eli Merriam
  ;; Add a version scraping logo identification filter.
  (add-hook 'comint-output-filter-functions 'matlab-shell-version-scrape)
  ;; Add pseudo html-renderer
  (add-hook 'comint-output-filter-functions 'matlab-shell-render-html-anchor nil t)
  (add-hook 'comint-output-filter-functions 'matlab-shell-render-errors-as-anchor nil t)
  ;; Scroll to bottom after running cell/region
  (add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)

  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (use-local-map matlab-shell-mode-map)
  (set-syntax-table matlab-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-shell-font-lock-keywords-1
			      matlab-shell-font-lock-keywords-2
			      matlab-shell-font-lock-keywords-3)
			     t nil ((?_ . "w"))))
  (set (make-local-variable 'comint-input-ring-size)
       matlab-shell-input-ring-size)
  (set (make-local-variable 'comint-input-ring-file-name)
       (format matlab-shell-history-file "R12"))
  (if (fboundp 'comint-read-input-ring)
      (comint-read-input-ring t))
  (make-local-variable 'gud-marker-acc)

  (if matlab-shell-enable-gud-flag
      (progn
	(gud-def gud-break  "dbstop in %d/%f at %l"  "\C-b" "Set breakpoint at current line.")
	(gud-def gud-remove "dbclear in %d/%f at %l" "\C-d" "Remove breakpoint at current line.")
	(gud-def gud-step   "dbstep in"           "\C-s" "Step one source line, possibly into a function.")
	(gud-def gud-next   "dbstep %p"           "\C-n" "Step over one source line.")
	(gud-def gud-cont   "dbcont"              "\C-r" "Continue with display.")
        (gud-def gud-stop-subjob "dbquit"         nil    "Quit debugging.") ;; gud toolbar stop
	(gud-def gud-finish "dbquit"              "\C-f" "Finish executing current function.")
	(gud-def gud-up     "dbup"                "<"    "Up N stack frames (numeric arg).")
	(gud-def gud-down   "dbdown"              ">"    "Down N stack frames (numeric arg).")
        ;; using (gud-def gud-print  "%e" "\C-p" "Eval expression at point") fails
	(gud-def gud-print  "% gud-print not available" "\C-p" "gud-print not available.")
	(if (fboundp 'gud-make-debug-menu)
	    (gud-make-debug-menu))
	(if (fboundp 'gud-overload-functions)
	    (gud-overload-functions
	     '((gud-massage-args . gud-matlab-massage-args)
	       (gud-marker-filter . gud-matlab-marker-filter)
	       (gud-find-file . gud-matlab-find-file))))
	;; XEmacs doesn't seem to have this concept already.  Oh well.
	(setq gud-marker-acc nil)
	;; XEmacs has problems w/ this variable.  Set it here.
	(set-marker comint-last-output-start (point-max))
	))
  (run-hooks 'matlab-shell-mode-hook)
  (matlab-show-version)
  )


;;; MATLAB SHELL
;;
;; Core shell state handling & startup function.

(defvar matlab-shell-running-matlab-version nil
  "The version of MATLAB running in the current `matlab-shell' buffer.")
(defvar matlab-shell-running-matlab-release nil
  "The release of MATLAB running in the current `matlab-shell' buffer.")
(defvar matlab-shell-use-emacs-toolbox
  ;; matlab may not be on path.  (Name change, explicit load, etc)
  (let* ((mlfile (locate-library "matlab"))
	 (dir (expand-file-name "toolbox/emacsinit.m"
				(file-name-directory (or mlfile "")))))
    (and mlfile (file-exists-p dir)))
  "Add the `matlab-shell' MATLAB toolbox to the MATLAB path on startup.")

(defvar matlab-shell-buffer-name "MATLAB"
  "Name used to create `matlab-shell' mode buffers.
This name will have *'s surrounding it.")

(defvar matlab-prompt-seen nil
  "Track visibility of MATLAB prompt in MATLAB Shell.")

;;
;; Predicate for use by matlab buffers
(declare-function comint-check-proc "comint.el" (buffer))
(defun matlab-shell-active-p ()
  "Return t if the MATLAB shell is active."
  (let ((msbn (get-buffer (concat "*" matlab-shell-buffer-name "*"))))
    (if msbn
        (with-current-buffer msbn
          (if (comint-check-proc (current-buffer))
              (current-buffer))))))
;;;###autoload
(defun matlab-shell ()
  "Create a buffer with MATLAB running as a subprocess.

MATLAB shell cannot work on the MS Windows platform because MATLAB is not
a console application."
  (interactive)
  ;; MATLAB shell does not work by default on the Windows platform.  Only
  ;; permit it's operation when the shell command string is different from
  ;; the default value.  (True when the engine program is running.)
  (if (and (or (eq window-system 'pc) (eq window-system 'w32))
	   (string= matlab-shell-command "matlab"))
      (error "MATLAB cannot be run as a inferior process.  \
Try C-h f matlab-shell RET"))

  (require 'shell)
  (require 'gud)

  ;; Make sure this is safe to use gud to debug MATLAB
  (when (and matlab-shell-enable-gud-flag (not (fboundp 'gud-def)))
    (message "Your emacs is missing `gud-def' which means matlab-shell won't work correctly.")
    (setq matlab-shell-enable-gud-flag nil))

  (switch-to-buffer (concat "*" matlab-shell-buffer-name "*"))
  (if (matlab-shell-active-p)
      nil
    ;; Clean up crufty state
    (kill-all-local-variables)
    (switch-to-buffer
     ;; Thx David Chappaz for reminding me about this patch.
     (let* ((windowid (frame-parameter (selected-frame) 'outer-window-id))
            (newvar (concat "WINDOWID=" windowid))
            (process-environment (cons newvar process-environment)))
       (apply 'make-comint matlab-shell-buffer-name matlab-shell-command
              nil matlab-shell-command-switches)))

    (setq shell-dirtrackp t)
    (comint-mode)

    (when matlab-shell-enable-gud-flag
      (gud-mode)
      (make-local-variable 'matlab-prompt-seen)
      (setq matlab-prompt-seen nil)
      (make-local-variable 'gud-marker-filter)
      (setq gud-marker-filter 'gud-matlab-marker-filter)
      (make-local-variable 'gud-find-file)
      (setq gud-find-file 'gud-matlab-find-file)

      (set-process-filter (get-buffer-process (current-buffer))
                          'gud-filter)
      (set-process-sentinel (get-buffer-process (current-buffer))
                            'gud-sentinel)
      (gud-set-buffer))


    ;; Comint and GUD both try to set the mode.  Now reset it to
    ;; matlab mode.
    (matlab-shell-mode)

    ;; Company mode can be used to display completions for MATLAB in matlab-shell.
    ;; This block enables company mode for this shell, and turns off the idle timer
    ;; so users must press TAB to get the menu.
    (when (and matlab-shell-tab-use-company
	       matlab-shell-tab-company-available)
      
      ;; Only do popup when users presses TAB
      (set (make-local-variable 'company-idle-delay) nil)
      (company-mode))
    ))

;;; STARTUP / VERSION 
;;
;; Handlers for startup output / version scraping

(defun matlab-shell-version-scrape (str)
  "Scrape the MATLAB Version from the MATLAB startup text.
Argument STR is the string to examine for version information."
  (when (string-match "\\(Version\\)\\s-+\\([.0-9]+\\)\\s-+(\\(R[.0-9]+[ab]?\\))" str)
    ;; Extract the release number
    (setq matlab-shell-running-matlab-version
	  (match-string 2 str)
	  matlab-shell-running-matlab-release
	  (match-string 3 str))
    ;; Now get our history loaded
    (setq comint-input-ring-file-name
	  (format matlab-shell-history-file matlab-shell-running-matlab-release))
    (if (fboundp 'comint-read-input-ring)
	(comint-read-input-ring t))
    ;; Remove the scrape from our list of things to do.
    (remove-hook 'comint-output-filter-functions
		 'matlab-shell-version-scrape)))

;;; ANCHORS
;;
;; Scan output for text, and turn into navigable links.

(defvar gud-matlab-marker-regexp-prefix "error:\\|opentoline\\|dbhot"
  "A prefix to scan for to know if output might be scarfed later.")

(defvar matlab-shell-html-map
  (let ((km (make-sparse-keymap)))
    (if (string-match "XEmacs" emacs-version)
	(define-key km [button2] 'matlab-shell-html-click)
      (define-key km [mouse-2] 'matlab-shell-html-click))
    (define-key km [return] 'matlab-shell-html-go)
    km)
  "Keymap used on overlays that represent errors.")

;; Anchor expressions. 
(defvar matlab-anchor-beg "<a href=\"\\(\\(?:matlab:\\)?[^\"]+\\)\">"
  "Beginning of html anchor.")

(defvar matlab-anchor-end "</a>"
  "End of html anchor.")

(defun matlab-shell-render-html-anchor (str)
  "Render html anchors inserted into the MATLAB shell buffer.
Argument STR is the text for the anchor."
  (if (string-match matlab-anchor-end str)
      (save-excursion
        (while (re-search-backward matlab-anchor-beg
				   ;; Arbitrary back-buffer.  We don't
				   ;; usually get text in such huge chunks
				   (max (point-min) (- (point-max) 8192))
				   t)
          (let* ((anchor-beg-start (match-beginning 0))
                 (anchor-beg-finish (match-end 0))
                 (anchor-text (match-string 1))
                 (anchor-end-finish (search-forward matlab-anchor-end))
                 (anchor-end-start (match-beginning 0))
                 (o (matlab-make-overlay anchor-beg-finish anchor-end-start)))
            (matlab-overlay-put o 'mouse-face 'highlight)
            (matlab-overlay-put o 'face 'underline)
            (matlab-overlay-put o 'matlab-url anchor-text)
            (matlab-overlay-put o 'keymap matlab-shell-html-map)
	    (matlab-overlay-put o 'help-echo anchor-text)
            (delete-region anchor-end-start anchor-end-finish)
            (delete-region anchor-beg-start anchor-beg-finish)
            ))))
  )
;;; ERROR HANDLING
;;

;; The regular expression covers to forms in tests/erroexamples.shell.m
;;
(defvar matlab-shell-error-anchor-expression
  (concat "^\\(\\(Error \\(in\\|using\\) \\|Syntax error in \\)\\(?:==> \\)?\\|"
	  "In \\|Error: File: \\|Warning: [^\n]+\n\\)")
  
  "Expressions used to find errors in MATLAB process output.
This variable contains the anchor, or starting text before
a typical error.  See `matlab-shell-error-location-expression' for
a list of expressions for identifying where the error is
after this anchor.")

(defvar matlab-shell-error-location-expression
  (list
   ;; Pulled from R2019b
   "\\(?:^> In \\)?\\([-@.a-zA-Z_0-9/ \\\\:]+\\) (line \\([0-9]+\\))"

   "\\([-@.a-zA-Z_0-9/ \\\\:]+\\) Line: \\([0-9]+\\) Column: \\([0-9]+\\)"
   
   ;; Oldest I have examples for:
   (concat "\\([-@.a-zA-Z_0-9/ \\\\:]+\\)\\(?:>[^ ]+\\)?.*[\n ]\\(?:On\\|at\\)\\(?: line\\)? "
	   "\\([0-9]+\\) ?")
   )
  "List of Expressions to search for after an error anchor is found.
These expressions are listed as matching from newer MATLAB versions
to older MATLABs.
Each expression should have the following match strings:
  1 - The matlab function
  2 - The line number
  3 - The column number (if available)")

;; (global-set-key [f8] 'matlab-shell-scan-for-error)

(defun matlab-shell-scan-for-error (limit)
  "Scan backward for a MATLAB error in the current buffer.
Uses `matlab-shell-error-anchor-expression' to find the error.
Uses `matlab-shell-error-location-expression' to find where the error is.
Returns a list of the form:
  ( STARTPT ENDPT FILE LINE COLUMN )"
  (interactive)
  (let ((ans nil)
	(beginning nil))
    (when (re-search-backward matlab-shell-error-anchor-expression 
			      limit
			      t)
      (save-excursion
	(setq beginning (match-beginning 0))
	(goto-char (match-end 0))
	(dolist (EXP matlab-shell-error-location-expression)
	  (when (looking-at EXP)
	    (setq ans (list beginning
			    (match-end 0)
			    (match-string-no-properties 1)
			    (match-string-no-properties 2)
			    (match-string-no-properties 3)
			    )))))
      )
    ;; This is for interactiv debugging.
    (when (called-interactively-p 'any)
      (when ans
	(pulse-momentary-highlight-region (car ans) (car (cdr ans))))
      (message "Found: %S" ans))
    ans))

(defvar matlab-shell-last-error-anchor nil
  "Last point where an error anchor was set.")
(defvar matlab-shell-last-anchor-as-frame nil
  ;; NOTE: this isn't being used yet.
  "The last error anchor saved, represented as a debugger frame.")

(defun matlab-shell-render-errors-as-anchor (&optional str)
  ;; TODO: Not sure why this takes STR.  Probably a comint thing?
  ;;    delete this todo.
  "Detect non-url errors, and treat them as if they were url anchors.
Argument STR is the text that might have errors in it."
  (save-excursion
    ;; We have found an error stack to investigate.
    (let ((first nil)
	  (ans nil)
	  (overlaystack nil))
      (while (setq ans (matlab-shell-scan-for-error (if matlab-shell-last-error-anchor
							(min matlab-shell-last-error-anchor (point))
						      (point))))
	(let* ((err-start (nth 0 ans))
	       (err-end (nth 1 ans))
	       (err-file (nth 2 ans))
	       (err-line (nth 3 ans))
	       (err-col (nth 4 ans))
	       (o (matlab-make-overlay err-start err-end))
	       (url (concat "opentoline('" err-file "'," err-line ",0)"))
	       )
	  (matlab-overlay-put o 'mouse-face 'highlight)
	  (matlab-overlay-put o 'face 'underline)
	  ;; The url will recycle opentoline code.
	  (matlab-overlay-put o 'matlab-url url)
	  (matlab-overlay-put o 'keymap matlab-shell-html-map)
	  (matlab-overlay-put o 'help-echo (concat "Jump to error at " err-file "."))
	  (setq first url)
	  (push o overlaystack)
	  ;; Save as a frame
	  (setq matlab-shell-last-anchor-as-frame
		(cons err-file err-line))
	  ))
      ;; Keep track of the very first error in this error stack.
      ;; It will represent the "place to go" for "go-to-last-error".
      (dolist (O overlaystack)
	(matlab-overlay-put O 'first-in-error-stack first))
      ;; Once we've found something, don't scan it again.
      (setq matlab-shell-last-error-anchor (point-marker)))))

;;; FILTER
;;
;; MATLAB's process filter handles output from the MATLAB process and
;; inteprets it for formatting text, and for running the debugger.

(defvar gud-matlab-marker-regexp-plain-prompt "^K?>>"
  "Regular expression for finding a prompt.")

(defvar gud-matlab-marker-regexp-1 "^K>>"
  "Regular expression for finding a file line-number.")

(defvar gud-matlab-marker-regexp-2
  (concat "^> In \\(" matlab-anchor-beg
          "\\|\\)\\([-.a-zA-Z0-9_>/@]+\\) \\((\\w+) \\|\\)at line \\([0-9]+\\)[ \n]+")
  "Regular expression for finding a file line-number.
Please note: The leading > character represents the current stack frame, so if
there are several frames, this makes sure we pick the right one to popup.")

(defvar gud-matlab-dbhotlink nil
  "Track if we've sent a dbhotlink request.")
(make-variable-buffer-local 'gud-matlab-dbhotlink)

(defun gud-matlab-massage-args (file args)
  "Argument message for starting matlab file.
I don't think I have to do anything, but I'm not sure.
FILE is ignored, and ARGS is returned."
  args)

(defun gud-matlab-find-file (f)
  "Find file F when debugging frames in MATLAB."
  (save-excursion
    (let* ((realfname (if (string-match "\\.\\(p\\)$" f)
			  (progn
			    (aset f (match-beginning 1) ?m)
			    f)
			f))
	   (buf (find-file-noselect realfname)))
      (set-buffer buf)
      (if (fboundp 'gud-make-debug-menu)
	  (gud-make-debug-menu))
      buf)))

(defvar matlab-shell-prompt-appears-hook nil
  "Hooks run each time a prompt is seen and sent to display.
If multiple prompts are seen together, only call this once.")

(defun matlab-shell--get-emacsclient-command ()
  (if (server-running-p)
      (concat matlab-shell-emacsclient-command
              " -s " (expand-file-name server-name server-socket-dir))
    (message "Emacs server is not running. This means
  >> edit file.m
will not open in the current Emacs session.
You can use `M-x server-start' to start the Emacs server prior to running matlab-shell.
To have it automatically started, add to your ~/.emacs: (server-start)")
    (sit-for 5)
    matlab-shell-emacsclient-command))

(defun gud-matlab-marker-filter (string)
  "Filters STRING for the Unified Debugger based on MATLAB output."
  (if matlab-prompt-seen
      nil
    (when (string-match ">> " string)
      (if matlab-shell-use-emacs-toolbox
	  ;; Use our local toolbox directory.
	  (process-send-string
	   (get-buffer-process gud-comint-buffer)
	   (format "addpath('%s','-begin'); rehash; emacsinit('%s');\n"
		   (expand-file-name "toolbox"
				     (file-name-directory
				      (locate-library "matlab")))
		   (matlab-shell--get-emacsclient-command)))
        ;; Setup is misconfigured - we need emacsinit because it tells us how to debug
        (error "unable to initialize matlab, emacsinit.m and other files missing"))
      (if matlab-custom-startup-command
          (process-send-string
	   (get-buffer-process gud-comint-buffer)
           (concat matlab-custom-startup-command "\n")))
      ;; Mark that we've seen at least one prompt.
      (setq matlab-prompt-seen t)
      ))
  (let ((garbage (concat "\\(" (regexp-quote "\C-g") "\\|"
 			 (regexp-quote "\033[H0") "\\|"
 			 (regexp-quote "\033[H\033[2J") "\\|"
 			 (regexp-quote "\033H\033[2J") "\\)")))
    (while (string-match garbage string)
      (if (= (aref string (match-beginning 0)) ?\C-g)
	  (beep t))
      (setq string (replace-match "" t t string))))

  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output "") (frame nil))

    (when (string-match gud-matlab-marker-regexp-1 gud-marker-acc)
      ;; Look for any frames for case of a debug prompt.
      (let ((url gud-marker-acc)
	    ef el)
	(cond
	 ((string-match "^error:\\(.*\\),\\([0-9]+\\),\\([0-9]+\\)$" url)
	  (setq ef (substring url (match-beginning 1) (match-end 1))
		el (substring url (match-beginning 2) (match-end 2)))
	  )
	 ((string-match "opentoline('\\([^']+\\)',\\([0-9]+\\),\\([0-9]+\\))" url)
	  (setq ef (substring url (match-beginning 1) (match-end 1))
		el (substring url (match-beginning 2) (match-end 2)))
	  )
	 ;; If we have the prompt, but no match (as above),
	 ;; perhaps it is already dumped out into the buffer.  In
	 ;; that case, look back through the buffer.

	 )
	(when ef
	  (setq frame (cons ef (string-to-number el)))))

      ;; Newer MATLABs don't print useful info.  We'll have to
      ;; search backward for the previous line to see if a frame was
      ;; displayed.
      (when (and (not frame) (not gud-matlab-dbhotlink))
	(process-send-string
	 (get-buffer-process gud-comint-buffer)
	 "dbhotlink()%%%\n")
	(setq gud-matlab-dbhotlink t)
	)
      )

    ;; If we're forced to ask for a stack hotlink, we will see it come in via the
    ;; process output.  Don't output anything until a K prompt is seen after the display
    ;; of the dbhotlink command.
    (when gud-matlab-dbhotlink
      (let ((start (string-match "dbhotlink()%%%" gud-marker-acc))
	    (endprompt nil))
	(if start
	    (progn
	      (setq output (substring gud-marker-acc 0 start)
		    gud-marker-acc (substring gud-marker-acc start))

	      ;; The hotlink text will persist until we see the K prompt.
	      (when (string-match "^K?>> " gud-marker-acc)
		(setq endprompt (match-end 0))
		;; We're done with the text!  Remove it from the accumulator.
		(setq gud-marker-acc (substring gud-marker-acc endprompt))
		;; If we got all this at the same time, push output back onto the accumulator for
		;; the next code bit to push it out.
		(setq gud-marker-acc (concat output gud-marker-acc)
		      output ""
		      gud-matlab-dbhotlink nil)
		))
	  ;; Else, waiting for a link, but hasn't shown up yet.
	  ;; TODO - what can I do here to fix var setting if it gets
	  ;; locked?
	  (when (string-match "^>> " gud-marker-acc)
	    ;; A non-k prompt showed up.  We're not going to get out request.
	    (setq gud-matlab-dbhotlink nil))
	  )))
    
    ;; This if makes sure that the entirety of an error output is brought in
    ;; so that matlab-shell-mode doesn't try to display a file that only partially
    ;; exists in the buffer.  Thus, if MATLAB output:
    ;;  error: /home/me/my/mo/mello.m,10,12
    ;; All of that is in the buffer, and it goes to mello.m, not just
    ;; the first half of that file name.
    ;; The below used to match against the prompt, not \n, but then text that
    ;; had error: in it for some other reason wouldn't display at all.
    (if (and matlab-prompt-seen ;; don't pause output if prompt not seen
	     gud-matlab-dbhotlink ;; pause output if waiting on debugger
	     )
	;; We could be collecting debug info.  Wait before output.
	nil
      ;; Finish off this part of the output.  None of our special stuff
      ;; ends with a \n, so display those as they show up...
      (while (string-match "^[^\n]*\n" gud-marker-acc)
	(setq output (concat output (substring gud-marker-acc 0 (match-end 0)))
	      gud-marker-acc (substring gud-marker-acc (match-end 0))))

      (if (string-match "^K?>> $" gud-marker-acc)
	  (setq output (concat output gud-marker-acc)
		gud-marker-acc ""))
      
      ;; Check our output for a prompt, and existence of a frame.
      ;; If this is true, throw out the debug arrow stuff.
      (if (and (string-match "^>> $" output)
	       gud-last-last-frame)
	  (progn
	    (setq overlay-arrow-position nil
		  gud-last-last-frame nil
		  gud-overlay-arrow-position nil)
	    (sit-for 0)
	    )))

    (if frame (setq gud-last-frame frame))

    ;;(message "[%s] [%s]" output gud-marker-acc)

    ;; (string-match gud-matlab-marker-regexp-plain-prompt ">>")
    
    (when (string-match gud-matlab-marker-regexp-plain-prompt output)
      ;; Now that we are about to dump this, run our prompt hook.
      ;;(message "PROMPT!")
      (run-hooks 'matlab-shell-prompt-appears-hook)
      )

    
    output))


;;; COMMANDS
;;
;; Commands for interacting with the MATLAB shell buffer

(defun matlab-shell-next-matching-input-from-input (n)
  "Get the Nth next matching input from for the command line."
  (interactive "p")
  (matlab-shell-previous-matching-input-from-input (- n)))

(defun matlab-shell-previous-matching-input-from-input (n)
  "Get the Nth previous matching input from for the command line."
  (interactive "p")
  (end-of-line) ;; patch: Mark Histed
  (if (comint-after-pmark-p)
      (if (memq last-command '(matlab-shell-previous-matching-input-from-input
			       matlab-shell-next-matching-input-from-input))
	  ;; This hack keeps the cycling working well.
	  (let ((last-command 'comint-previous-matching-input-from-input))
	    (comint-next-matching-input-from-input (- n)))
	;; first time.
	(comint-next-matching-input-from-input (- n)))

    ;; If somewhere else, just move around.
    (forward-line (- n))))

(defun matlab-shell-delete-backwards-no-prompt (&optional arg)
  "Delete one char backwards without destroying the matlab prompt.
Optional argument ARG describes the number of chars to delete."
  (interactive "P")
  (let ((promptend (save-excursion
		     (beginning-of-line)
		     (if (looking-at "K?>> ")
			 (match-end 0)
		       (point))))
	(numchars (if (integerp arg) (- arg) -1)))
    (if (<= promptend (+ (point) numchars))
	(delete-char numchars)
      (error "Beginning of line"))))


;;; COMPLETION
;;
;; Requet list of completions from MATLAB.
;; Support classic emacs in-place completion, or company mode if available.

(defun matlab-shell-completion-list (str)
  "Get a list of completions from MATLAB.
STR is a command substring to complete."
  (let* ((msbn (matlab-shell-buffer-barf-not-running))
         (cmd (concat "emacsdocomplete('" str "')"))
         (comint-scroll-show-maximum-output nil)
         output
         (replacement-text "")
         (cmd-text-to-replace "")
         (completions nil))
    (with-current-buffer msbn
      (if (not (matlab-on-prompt-p))
          (error "MATLAB shell must be non-busy to do that"))
      (setq output (matlab-shell-collect-command-output cmd))
      (if (not (string-match "emacs_completions_output =" output))
          (error "Internal error, '%s' returned unexpected output, %s" cmd output))
      (setq output (substring output (match-end 0)))
      (when (string-match "^'\\([^']+\\)' --> '\\([^']*\\)'" output)
        ;; 'CMD_TEXT_TO_REPLACE' --> 'REPLACEMENT_TEXT'
        ;;     'OPTION1'
        ;;     'OPTION2'
        ;;     ...
        ;; Note, the CMD_TEXT_TO_REPLACE line is only present when there needs
        ;; to be replacement, e.g. imagine a command that takes glob patterns
        ;;   >> mycmd foo*ba<TAB>
        ;;   'foo*-bar' --> 'foo-and-bar'
        ;;     '-or-goo'
        ;;     '-or-too'
        ;; which completes to either 'foo-and-bar-or-goo' OR 'foo-and-bar-or-too'.
        ;; If there is only one completion that needs replacement, don't have options:
        ;;   >> mycmd foo*ba*-too<TAB>
        ;;   'foo*ba*-too' --> 'foo-and-bar-or-too'
        ;; The replacement line is not present when the completion just appends to the
        ;; command str, e.g.
        ;;   >> mycmd foo-and-bar<TAB>
        ;;       '-or-goo'
        ;;       '-or-too'
        (setq cmd-text-to-replace (match-string 1 output))
        (setq replacement-text (match-string 2 output))
        (setq output (substring output (match-end 0))))
      ;; Parse the output string.
      (while (string-match "'" output)
        ;; Hack off the preceding quote
        (setq output (substring output (match-end 0)))
        (string-match "'" output)
        ;; we are making a completion list, so that is a list of lists.
        (setq completions (cons (list (substring output 0 (match-beginning 0)))
                                completions)
              output (substring output (match-end 0))))
      ;; Return them
      (list (cons 'cmd-text-to-replace cmd-text-to-replace)
            (cons 'replacement-text replacement-text)
            (cons 'completions (nreverse completions)))
      )))

(defun matlab-shell-get-completion-limit-pos (last-cmd completions)
  "Used by `matlab-shell-tab' to get the starting location within last-cmd
of the common substring used in matching the completions, i.e.
  (substring last-cmd limit-pos (length last-cmd))
is the common starting substring of each completion in completions."
  (let ((limit-pos (length last-cmd)))
    (when completions
      (let* ((completion (car (car completions)))
             (i (length completion))
             (chomp-num-chars nil))
        (while (> i 0)
          (let ((part (substring completion 0 i)))
            (if (string-suffix-p part last-cmd)
                (progn
                  (setq chomp-num-chars i)
                  (setq i 0))
              (setq i (- i 1)))))
        (if chomp-num-chars
            (setq limit-pos (- (length last-cmd) chomp-num-chars)))))
    limit-pos))

(defun matlab-shell-get-completion-info ()
  "Compute completions needed for `matlab-shell-tab' and `company-matlab-shell'.
Completions are computed based on the prefix on the last command prompt.
No completions are provided anywhere else in the buffer."
  (if (or (not (= (point) (point-max)))
          (not (matlab-on-prompt-p)))
      nil ;; no completions. We can only complete when typing a command.
    (let ((inhibit-field-text-motion t)
          (last-cmd nil)
          (last-cmd-start-point nil)
          (common-substr nil)
          (limit-pos nil)
          (completions nil)
          (common-substr-start-pt nil)
          (common-substr-end-pt nil)
          (did-completion nil))
      ;; Load last-cmd which is the command we are completing on.
      ;; We need to avoid altering point because when we ask for completions, we send
      ;; emacsdocomplete(last-cmd-quoted) to the MATLAB command window and then grab the results
      ;; and erase them.
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        (re-search-forward comint-prompt-regexp)
        (setq last-cmd-start-point (point))
        ;; save the old (last) command
        (setq last-cmd (buffer-substring (point) (matlab-point-at-eol))))

      ;; Get the list of completions.
      ;; When obtaining completions, we can't use save-excursion because we are
      ;; manipulating the text in the *MATLAB* window at the point and this
      ;; move point-marker which causes save-excursion to move to the wrong
      ;; location.  We need to do this before we manipulate the text in the
      ;; *MATLAB* buffer because `matlab-shell-completion-list' sends
      ;; emacsdocompletion('statement') to matlab and matlab produces output in
      ;; the *MATLAB* buffer, then `matlab-shell-completion-list' removes the
      ;; output from the *MATLAB* buffer.
      (let ((last-cmd-quoted last-cmd))
        ;; Load last-cmd-quoted which the expression typed (e.g. "!mv file.").
        ;; Note, this has single quotes doubled up so we can ask
        ;; MATLAB for completions on last-cmd-quoted.
        (while (string-match "[^']\\('\\)\\($\\|[^']\\)" last-cmd-quoted)
          (setq last-cmd-quoted (replace-match "''" t t last-cmd-quoted 1)))

        (let* ((completion-list (matlab-shell-completion-list last-cmd-quoted))
               (cmd-text-to-replace (cdr (assoc 'cmd-text-to-replace completion-list))))
          (setq completions (cdr (assoc 'completions completion-list)))
          (when cmd-text-to-replace
            ;; need to alter the command to replace replacement-text with the common substring
            (let ((replacement-text (cdr (assoc 'replacement-text completion-list)))
                  (last-cmd-start-len (- (length last-cmd) (length cmd-text-to-replace))))
              ;; Replace the text typed in the *MATLAB* and update last-cmd
              (goto-char (+ last-cmd-start-point last-cmd-start-len))
              (delete-region (point) (matlab-point-at-eol))
              (insert replacement-text)
              (setq last-cmd (concat (substring last-cmd 0 last-cmd-start-len) replacement-text))
              (if (not completions)
                  (setq did-completion t))
              ))))

      ;; Consider
      ;;   >> ! touch foo.ext1 foo.ext2
      ;;   >> ! mv foo.<TAB>
      ;; 'completions' will contain (("foo.ext1") ("foo.ext2")) and
      ;; common-substr will be "foo." which is used in displaying the
      ;; completions. The limit-pos in this case will be 5 and
      ;; last-cmd "! mv foo."
      (setq limit-pos (matlab-shell-get-completion-limit-pos last-cmd completions))
      (setq common-substr (substring last-cmd limit-pos))
      
      ;; Mark the subfield of the completion result so we can say no completions
      ;; if there aren't any otherwise we need to remove it.
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        (re-search-forward comint-prompt-regexp)
        (setq common-substr-start-pt (+ (point) limit-pos))
        (setq common-substr-end-pt (matlab-point-at-eol))
        (if (and (eq (length completions) 1)
                 (string-equal (buffer-substring-no-properties
                                common-substr-start-pt common-substr-end-pt)
                               (car (car completions))))
            (setq completions nil))) ;; force display of "No completions"
      ;; Result
      (list (cons 'last-cmd               last-cmd)
            (cons 'common-substr          common-substr)
            (cons 'limit-pos              limit-pos)
            (cons 'completions            completions)
            (cons 'common-substr-start-pt common-substr-start-pt)
            (cons 'common-substr-end-pt   common-substr-end-pt)
            (cons 'did-completion         did-completion)
      ))))


(defun matlab-shell-c-tab ()
  "Send [TAB] to the currently running matlab process and retrieve completion
to show using classing emacs tab completion."
  (interactive)
  (let ((matlab-shell-tab-company-available nil))
    (matlab-shell-tab)))

;; matlab-shell-tab,
;;   This sends the command text at the prompt to emacsdocomplete.m which returns a list
;;   of possible completions. To do this we use comint to 'type' emacsdocomplete(command)
;;   in the *MATLAB* buffer and then we extract the result. The emacsdocomplete returns
;;   a list of possible completions of the command where each completion starts with
;;   zero or more characters of the command "suffix". For example, give a directory
;;   containing files, foo.ext1 and foo.ext2,
;;      >> ! mv foo.<TAB>
;;   will call emacsdocomplete('! mv foo.') which returns (("foo.ext1") ("foo.ext2"))
;;   where we have four characters "foo." matching the suffix of command. There can
;;   be zero suffix match as in
;;      >> !ls /usr/
;;   which on Linux returns a long list of items e.g. (("bin", "include", ...))
;;
;;
;;   Test cases (using R2016b):
;;
;;   >> h=figure;
;;   >> h.Num<TAB>                   Should show Number
;;   >> h.Number<TAB>                Should show Number and NumberTitle
;;   >>          h.Num<TAB>          Should do same. The extra white space shouldn't mess things up.
;;
;;   >> h.NumberTitle<TAB>           Should display message "No completions"
;;   >> set(h,'<TAB>                 Should display a long list
;;      type P<TAB>                  Should narrow to Parent, Position, ...
;;
;;   >> !touch file.ext              Assuming no other fil* names in current directory.
;;   >> !mv file.<TAB>               Should complete to file.ext
;;   >> !mv file.ext<TAB>            Should do nothing
;;
;;   >> !/usr/b<TAB>                 Should complete to /usr/bin
;;   >> !/usr/bin/cpp-4.<TAB>        Should complete to something like /usr/bin/cpp-4.9
;;
;;   >> ls /usr/include/byteswap.<TAB>  Some file with a '.' should complete correctly
;;
;;   >> !touch foo.ext1 foo.ext2
;;   >> ! mv foo.<TAB>               Should give options foo.ext1 foo.ext2
;;   >> ! mv foo.ext1<TAB>           Should say no completions
;;   >> ! mv foo.ext1 <TAB>          Should say no completions (note the space before the TAB)
;;
;;   >> vdp
;;   >> get_param('vdp','Pos<TAB>    Should show several completions
(defun matlab-shell-tab ()
  "Perform completions at the `matlab-shell' command prompt.
By default, uses matlab-shell toolbox command emacsdocomplete.m to get
completions.

If `matlab-shell-ask-MATLAB-for-completions' is nil, then use
`comint-dynamic-complete-filename' instead.

If `matlab-shell-tab-use-company' is non-nil, and if `company-mode' is
installed, then use company to display completions in a popup window.
"
  (interactive)
  (cond
   ;; If we aren't supposed to ask MATLAB for completions, then use
   ;; comint basics.
   ((not matlab-shell-ask-MATLAB-for-completions)
    (call-interactively 'comint-dynamic-complete-filename))

   ;; If company mode is available and we ask for it, use that.
   ((and matlab-shell-tab-company-available matlab-shell-tab-use-company company-mode)
    ;; We don't add to company-backends because we bind TAB to matlab-shell-tab
    ;; which means completions must be explicitly requested. The default
    ;; company-complete tries to complete as you type which doesn't work
    ;; so well because it can take MATLAB a bit to compute completions.
    (call-interactively 'company-matlab-shell))

   ;; Starting in Emacs 23, completion-in-region has everything we need for basic
   ;; in-buffer completion
   ((fboundp 'completion-in-region)
    (matlab-shell-do-completion-light))

   ;; Old school completion
   (t
    (matlab-shell-do-completion))
   ))

(defun matlab-shell-do-completion-light ()
  "Perform completion using `completion-in-region'."
  (let* ((inhibit-field-text-motion t)
         (completion-info        (matlab-shell-get-completion-info))
         (completions            (cdr (assoc 'completions completion-info)))
         (common-substr-start-pt (cdr (assoc 'common-substr-start-pt completion-info)))
         (common-substr-end-pt   (cdr (assoc 'common-substr-end-pt completion-info)))
	 )
    (completion-in-region common-substr-start-pt common-substr-end-pt completions)))

(defun matlab-shell-do-completion ()
  "Perform completion using Emacs buffers.
This should work in version before `completion-in-region' was available."
  (let* ((inhibit-field-text-motion t)
         (completion-info        (matlab-shell-get-completion-info))
         ;;(last-cmd               (cdr (assoc 'last-cmd completion-info)))
         (common-substr          (cdr (assoc 'common-substr completion-info)))
         (limit-pos              (cdr (assoc 'limit-pos completion-info)))
         (completions            (cdr (assoc 'completions completion-info)))
         (common-substr-start-pt (cdr (assoc 'common-substr-start-pt completion-info)))
         (common-substr-end-pt   (cdr (assoc 'common-substr-end-pt completion-info)))
         (did-completion         (cdr (assoc 'did-completion completion-info))))

    (when (not did-completion)
      ;; Whack the old command 'substring' that is starting part of the
      ;; completions so we can insert it back later
      (delete-region common-substr-start-pt common-substr-end-pt)
      (goto-char (point-max))
      ;; Process the completions
      (if (eq (length completions) 1)
	  ;; If there is only one, then there is an obvious thing to do.
          (progn
            (insert (car (car completions)))
	    ;; kill completions buffer if still visible
            (matlab-shell-tab-hide-completions))
	;; else handle multiple completions
        (let ((try nil))
          (setq try (try-completion common-substr completions))
	  ;; Insert in a good completion.
          (cond ((or (eq try nil) (eq try t)
                     (and (stringp try)
                          (string= try common-substr)))
                 (insert common-substr)
		 (let ((cbuff (get-buffer-create "*Completions*")))
		   (with-output-to-temp-buffer cbuff
		     (matlab-display-completion-list (mapcar 'car completions)
						     common-substr))
		   (display-buffer
		    cbuff
		    '((display-buffer-below-selected display-buffer-at-bottom)
		      (inhibit-same-window . t)
		      (window-height . fit-window-to-buffer))
		    )
		   ))
                ((stringp try)
                 (insert try)
                 (matlab-shell-tab-hide-completions))
                (t
                 (insert common-substr))))
        ))))

(defun matlab-shell-tab-hide-completions ()
  "Hide any completion windows for `matlab-shell-tab'."
  (let ((bw (get-buffer-window "*Completions*")))
    (when bw
      (quit-window nil (get-buffer-window "*Completions*")))))

;;; Find Files
;;
;; Finding Files with MATLAB shell.
;; Originally for use with semantic-matlab, but now used in more places.

(defun matlab-shell-which-fcn (fcn)
  "Get the location of FCN's M file.
Returns an alist: ( LOCATION . BUILTINFLAG )
LOCATION is a string indicating where it is, and BUILTINFLAG is
non-nil if FCN is a builtin."
  (save-excursion
    (let* ((msbn (matlab-shell-buffer-barf-not-running))
	   (cmd (concat "which " fcn))
	   (comint-scroll-show-maximum-output nil)
	   output
	   builtin
	   )
      (set-buffer msbn)
      (if (not (matlab-on-prompt-p))
	  (error "MATLAB shell must be non-busy to do that"))
      (setq output (matlab-shell-collect-command-output cmd))
      ;; BUILT-IN
      (cond
       ((string-match "built-in (\\([^)]+\\))" output)
	(cons (concat (substring output (match-beginning 1) (match-end 1))
		      ".m")
	      t))
       ;; Error
       ((string-match "not found" output)
	nil)
       ;; JUST AN M FILE
       (t
	(string-match "$" output)
	(cons (substring output 0 (match-beginning 0)) nil))))))

;;; TODO - only used in semantic-matlab
(defun matlab-shell-matlabroot ()
  "Get the location of this shell's root.
Returns a string path to the root of the executing MATLAB."
  (save-excursion
    (let* ((msbn (matlab-shell-buffer-barf-not-running))
	   (cmd "disp(matlabroot)")
	   (comint-scroll-show-maximum-output nil)
	   output
	   builtin
	   )
      (set-buffer msbn)

      (if (and (boundp 'matlab-shell-matlabroot-run)
	       matlab-shell-matlabroot-run)
	  matlab-shell-matlabroot-run
	;; If we haven't cached it, calculate it now.

	(if (not (matlab-on-prompt-p))
	    (error "MATLAB shell must be non-busy to do that"))
	(setq output (matlab-shell-collect-command-output cmd))

	(string-match "$" output)
	(substring output 0 (match-beginning 0))))))



;;; MATLAB Shell Commands =====================================================
;;
;; These command are in the Menu for the matlab-shell buffer.

(defun matlab-read-word-at-point ()
  "Get the word closest to point, but do not change position.
Has a preference for looking backward when not directly on a symbol.
Snatched and hacked from dired-x.el"
  (let ((word-chars "a-zA-Z0-9_")
	(bol (matlab-point-at-bol))
	(eol (matlab-point-at-eol))
        start)
    (save-excursion
      ;; First see if just past a word.
      (if (looking-at (concat "[" word-chars "]"))
	  nil
	(skip-chars-backward (concat "^" word-chars "{}()\[\]") bol)
	(if (not (bobp)) (backward-char 1)))
      (if (numberp (string-match (concat "[" word-chars "]")
				 (char-to-string (following-char))))
          (progn
            (skip-chars-backward word-chars bol)
            (setq start (point))
            (skip-chars-forward word-chars eol))
        (setq start (point)))		; If no found, return empty string
      (buffer-substring start (point)))))

(defun matlab-read-line-at-point ()
  "Get the line under point, if command line."
  (if (eq major-mode 'matlab-shell-mode)
      (save-excursion
	(let ((inhibit-field-text-motion t))
	  (beginning-of-line)
	  (if (not (looking-at (concat comint-prompt-regexp)))
	      ""
	    (search-forward-regexp comint-prompt-regexp)
	    (buffer-substring (point) (matlab-point-at-eol)))))
    (save-excursion
      ;; In matlab buffer, find all the text for a command.
      ;; so back over until there is no more continuation.
      (while (save-excursion (forward-line -1) (matlab-lattr-cont))
	(forward-line -1))
      ;; Go forward till there is no continuation
      (beginning-of-line)
      (let ((start (point)))
	(while (matlab-lattr-cont) (forward-line 1))
	(end-of-line)
	(buffer-substring start (point))))))

(defun matlab-non-empty-lines-in-string (str)
  "Return number of non-empty lines in STR."
  (let ((count 0)
	(start 0))
    (while (string-match "^.+$" str start)
      (setq count (1+ count)
	    start (match-end 0)))
    count))

(declare-function matlab-shell-help-mode "matlab-topic")
(defun matlab-output-to-temp-buffer (buffer output)
  "Print output to temp buffer, or a message if empty string.
BUFFER is the buffer to output to, and OUTPUT is the text to insert."
  (let ((lines-found (matlab-non-empty-lines-in-string output)))
    (cond ((= lines-found 0)
	   (message "(MATLAB command completed with no output)"))
	  ((= lines-found 1)
	   (string-match "^.+$" output)
	   (message (substring output (match-beginning 0)(match-end 0))))
	  (t (with-output-to-temp-buffer buffer (princ output))
             (with-current-buffer buffer
	       (matlab-shell-help-mode))))))

(defun matlab-shell-run-command (command)
  "Run COMMAND and display result in a buffer.
This command requires an active MATLAB shell."
  (interactive (list (read-from-minibuffer
 		      "MATLAB command line: "
 		      (cons (matlab-read-line-at-point) 0))))
  (let ((doc (matlab-shell-collect-command-output command)))
    (matlab-output-to-temp-buffer "*MATLAB Help*" doc)))

(defun matlab-shell-describe-variable (variable)
  "Get the contents of VARIABLE and display them in a buffer.
This uses the WHOS (MATLAB 5) command to find viable commands.
This command requires an active MATLAB shell."
  (interactive (list (read-from-minibuffer
 		      "MATLAB variable: "
 		      (cons (matlab-read-word-at-point) 0))))
  (let ((doc (matlab-shell-collect-command-output (concat "whos " variable))))
    (matlab-output-to-temp-buffer "*MATLAB Help*" doc)))

(defun matlab-shell-describe-command (command)
  "Describe COMMAND textually by fetching it's doc from the MATLAB shell.
This uses the lookfor command to find viable commands.
This command requires an active MATLAB shell."
  (interactive
   (let ((fn (matlab-function-called-at-point))
	 val)
     (setq val (read-string (if fn
				(format "Describe function (default %s): " fn)
			      "Describe function: ")))
     (if (string= val "") (list fn) (list val))))
  (let ((doc (matlab-shell-collect-command-output (concat "help " command))))
    (matlab-output-to-temp-buffer "*MATLAB Help*" doc)))

(defun matlab-shell-apropos (matlabregex)
  "Look for any active commands in MATLAB matching MATLABREGEX.
This uses the lookfor command to find viable commands."
  (interactive (list (read-from-minibuffer
 		      "MATLAB command subexpression: "
 		      (cons (matlab-read-word-at-point) 0))))
  (let ((ap (matlab-shell-collect-command-output
	     (concat "lookfor " matlabregex))))
    (matlab-output-to-temp-buffer "*MATLAB Apropos*" ap)))

(defun matlab-on-prompt-p ()
  "Return t if we MATLAB can accept input."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (goto-char (point-max))
      (beginning-of-line)
      (looking-at comint-prompt-regexp))))

(defun matlab-on-empty-prompt-p ()
  "Return t if we MATLAB is on an empty prompt."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (goto-char (point-max))
      (beginning-of-line)
      (looking-at (concat comint-prompt-regexp "\\s-*$")))))

(defun matlab-shell-buffer-barf-not-running ()
  "Return a running MATLAB buffer iff it is currently active."
  (or (matlab-shell-active-p)
      (error "You need to run the command `matlab-shell' to do that!")))

(defun matlab-shell-collect-command-output (command)
  "If there is a MATLAB shell, run the MATLAB COMMAND and return it's output.
It's output is returned as a string with no face properties.  The text output
of the command is removed from the MATLAB buffer so there will be no
indication that it ran."
  (let ((msbn (matlab-shell-buffer-barf-not-running)))
    ;; We are unable to use save-excursion to save point position because we are
    ;; manipulating the *MATLAB* buffer by erasing the current text typed at the
    ;; MATLAB prompt (where point is) and then we send command to MATLAB and
    ;; grab the result. After this we erase the output from command and then
    ;; restore the current text at the MATLAB prompt and move to start-point.
    ;; Note, save-excursion works by tracking `point-marker' and when you manipulate
    ;; the text at point, `point-marker' moves causing save-excursion to move
    ;; the point in to a location we don't want. See:
    ;; http://emacs.stackexchange.com/questions/7574/why-save-excursion-doesnt-save-point-position
    ;; Ideally there would be some way to prevent the *MATLAB* buffer from refreshing
    ;; as we are interacting with it, but I couldn't figure out a way to do that.
    (with-current-buffer msbn
      (save-window-excursion
        (let ((pos nil)
              (str nil)
              (lastcmd)
              (inhibit-field-text-motion t)
              (start-point (point)))
          (if (not (matlab-on-prompt-p))
              (error "MATLAB shell must be non-busy to do that"))

          ;; Save the old command
          (goto-char (point-max))
          (beginning-of-line)
          (re-search-forward comint-prompt-regexp)
          ;; Backup if there are extra spaces. To see why, try tab completion on command with
          ;; leading spaces, e.g.
          ;; >> h=figure;
          ;; >>                    h.Num<TAB>
          (re-search-backward ">")
          (forward-char 2)
          (setq lastcmd (buffer-substring (point) (matlab-point-at-eol)))
          (delete-region (point) (matlab-point-at-eol))
          ;; We are done error checking, run the command.
          (setq pos (point))
          ;; Note, comint-simple-send in emacs 24.4 appends a newline and code below assumes
          ;; one prompt indicates command completed, so don't append a newline.
          (comint-simple-send (get-buffer-process (current-buffer)) command)
          ;; Wait for the command to finish, by looking for new prompt.
          (goto-char (point-max))
          ;; Turn on C-g by using wiht-local-quit. This is needed to prevent message:
          ;;  "Blocking call to accept-process-output with quit inhibited!! [115 times]"
          ;; when using `company-matlab-shell' for TAB completions.
          (with-local-quit
	    (while (or (>= (+ pos (string-width command)) (point)) (not (matlab-on-empty-prompt-p)))
	      (accept-process-output (get-buffer-process (current-buffer)))
	      (goto-char (point-max))))

          ;; Get result of command into str
          (goto-char pos)
          (setq str (buffer-substring-no-properties (save-excursion
                                                      (goto-char pos)
                                                      (beginning-of-line)
                                                      (forward-line 1)
                                                      (point))
                                                    (save-excursion
                                                      (goto-char (point-max))
                                                      (beginning-of-line)
                                                      (point))))
          ;; delete the result of command
          (delete-region pos (point-max))
          ;; restore contents of buffer so it looks like nothing happened.
          (insert lastcmd)
          (goto-char start-point)
          ;; return result 'string' from executing MATLAB command
          str)))))

(defun matlab-shell-send-string (string)
  "Send STRING to the currently running matlab process."
  (if (not matlab-shell-echoes)
      (let ((proc (get-buffer-process (current-buffer))))
	(goto-char (point-max))
	(insert string)
	(set-marker (process-mark proc) (point))))
  (comint-send-string (get-buffer-process (current-buffer)) string))

(defun matlab-url-at (p)
  "Return the matlab-url overlay at P, or nil."
  (let ((url nil) (o (matlab-overlays-at p)))
    (while (and o (not url))
      (setq url (matlab-overlay-get (car o) 'matlab-url)
            o (cdr o)))
    url))

(defun matlab-url-stack-top-at (p)
  "Return the matlab-url overlay at P, or nil."
  (let ((url nil) (o (matlab-overlays-at p)))
    (while (and o (not url))
      (setq url (or (matlab-overlay-get (car o) 'first-in-error-stack)
		    (matlab-overlay-get (car o) 'matlab-url))
            o (cdr o)))
    url))

(defun matlab-shell-previous-matlab-url (&optional stacktop)
  "Find a previous occurrence of an overlay with a MATLAB URL.
If STACKTOP is non-nil, then also get the top of some stack, which didn't
show up in reverse order."
  (save-excursion
    (let ((url nil) (o nil) (p (point)))
      (while (and (not url)
                  (setq p (matlab-previous-overlay-change p))
                  (not (eq p (point-min))))
        (setq url
	      (if stacktop
		  (matlab-url-stack-top-at p)
		(matlab-url-at p))))
      url)))

(defvar matlab-shell-mref-converters
  '(
    ;; Does it work as is?
    (lambda (mref) mref)
    ;; p files
    (lambda (mref) (when (string-match "\\.\\(p\\)$" mref)
		     (replace-match "m" nil t mref 1)))
    ;; Function name, no extension.
    (lambda (mref) (when (not (string-match "\\.m$" mref)) (concat mref ".m")))
    ;; Copied from old code, not sure what it matches.
    (lambda (mref) (when (string-match ">" mref)
		     (concat (substring fileref 0 (match-beginning 0)) ".m")))
    ;; Ask matlab where it came from.  Keep last b/c expensive, or won't
    ;; work if ML is busy.
    (lambda (mref) (car (matlab-shell-which-fcn mref)))
    )
  "List of converters to convert MATLAB file references into a filename.
Each element is a function that accepts a file ref, and returns
a file name, or nil if no conversion done.")

(defun matlab-shell-mref-to-filename (fileref)
  "Convert the MATLAB file reference FILEREF into an actual file name.
MATLAB can refer to functions on the path by a short name, or by a .p 
extension, and a host of different ways.  Convert this reference into
something Emacs can load."
  (interactive "sFileref: ")
  (let ((C matlab-shell-mref-converters)
	(ans nil))
    (while (and C (not ans))
      (let ((tmp (funcall (car C) fileref)))
	(when (and tmp (file-exists-p tmp))
	  (setq ans tmp))
	)
      (setq C (cdr C)))
    (when (called-interactively-p 'any) (message "Found: %S" ans))
    ans))

(defun matlab-find-other-window-file-line-column (ef el ec &optional debug)
  "Find file EF in other window and to go line EL and 1-basec column EC.
If DEBUG is non-nil, then setup GUD debugging features."
  (setq ef (matlab-shell-mref-to-filename ef))
  (find-file-other-window ef)
  (with-no-warnings
    (goto-line (string-to-number el)))
  (when debug
    (setq gud-last-frame (cons (buffer-file-name) (string-to-number el)))
    (gud-display-frame))
  (setq ec (string-to-number ec))
  (if (> ec 0) (forward-char (1- ec))))

(defun matlab-find-other-window-via-url (url &optional debug)
  "Find other window using matlab URL and optionally set DEBUG cursor."
  (cond ((string-match "^error:\\(.*\\),\\([0-9]+\\),\\([0-9]+\\)$" url)
         (let ((ef (substring url (match-beginning 1) (match-end 1)))
               (el (substring url (match-beginning 2) (match-end 2)))
               (ec (substring url (match-beginning 3) (match-end 3))))
           (matlab-find-other-window-file-line-column ef el ec debug)))
	((string-match "opentoline('\\([^']+\\)',\\([0-9]+\\),\\([0-9]+\\))" url)
         (let ((ef (substring url (match-beginning 1) (match-end 1)))
               (el (substring url (match-beginning 2) (match-end 2)))
               (ec (substring url (match-beginning 3) (match-end 3))))
           (matlab-find-other-window-file-line-column ef el ec debug)))
        ((string-match "^matlab:*\\(.*\\)$" url)
         (process-send-string
          (get-buffer-process gud-comint-buffer)
          (concat (substring url (match-beginning 1) (match-end 1)) "\n")))))

(defun matlab-shell-last-error ()
  "In the MATLAB interactive buffer, find the last MATLAB error, and go there.
To reference old errors, put the cursor just after the error text."
  (interactive)
  (catch 'done
    (let ((url (matlab-shell-previous-matlab-url t)))
      (if url
          (progn (matlab-find-other-window-via-url url) (throw 'done nil))
        (save-excursion
          (end-of-line) ;; In case we are before the line number 1998/06/05 16:54sk
	  (let ((err (matlab-shell-scan-for-error (point-min))))
	    (when (not err) (error "No errors found!"))
	    (let ((ef (nth 2 err))
		  (el (nth 3 err))
		  (ec (or (nth 4 err) "0")))
	      (matlab-find-other-window-file-line-column ef el ec))))))))

(defun matlab-shell-html-click (e)
  "Go to the error at the location of event E."
  (interactive "e")
  (mouse-set-point e)
  (matlab-shell-html-go))

(defun matlab-shell-html-go ()
  "Go to the error at the location `point'."
  (interactive)
  (let ((url (matlab-url-at (point))))
    (if url (matlab-find-other-window-via-url url))))

(defun matlab-shell-dbstop-error ()
  "Stop on errors."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer))
		      "dbstop if error\n"))

(defun matlab-shell-dbclear-error ()
  "Don't stop on errors."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer))
		      "dbclear if error\n"))

(defun matlab-shell-demos ()
  "MATLAB demos."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "demo\n"))

(defun matlab-shell-close-figures ()
  "Close any open figures."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "close all\n"))

(defun matlab-shell-close-current-figure ()
  "Close current figure."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "delete(gcf)\n"))

(defun matlab-shell-exit ()
  "Exit MATLAB shell."
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "exit\n")
  (kill-buffer nil))


;;; MATLAB mode Shell commands ================================================
;;
;; These commands are provided in MATLAB code buffers to interact with
;; the shell.

(defun matlab-show-matlab-shell-buffer ()
  "Switch to the buffer containing the matlab process."
  (interactive)
  (let ((msbn (concat "*" matlab-shell-buffer-name "*")))
    (if (get-buffer msbn)
	(switch-to-buffer-other-window msbn)
      (message "There is not an active MATLAB process."))))

(defvar matlab-shell-save-and-go-history '("()")
  "Keep track of parameters passed to the MATLAB shell.")

(defun matlab-shell-add-to-input-history (string)
  "Add STRING to the input-ring and run `comint-input-filter-functions' on it.
Similar to  `comint-send-input'."
  (if (and (funcall comint-input-filter string)
	   (or (null comint-input-ignoredups)
	       (not (ring-p comint-input-ring))
	       (ring-empty-p comint-input-ring)
	       (not (string-equal (ring-ref comint-input-ring 0) string))))
      (ring-insert comint-input-ring string))
  (run-hook-with-args 'comint-input-filter-functions
		      (concat string "\n"))
  (if (boundp 'comint-save-input-ring-index);only bound in GNU emacs
      (setq comint-save-input-ring-index comint-input-ring-index))
  (setq comint-input-ring-index nil))

(defun matlab-shell-save-and-go ()
  "Save this M file, and evaluate it in a MATLAB shell."
  (interactive)
  (if (not (eq major-mode 'matlab-mode))
      (error "Save and go is only useful in a MATLAB buffer!"))
  (if (not (buffer-file-name (current-buffer)))
      (call-interactively 'write-file))
  (let ((fn-name (file-name-sans-extension
		  (file-name-nondirectory (buffer-file-name))))
	(msbn (concat "*" matlab-shell-buffer-name "*"))
        (dir (file-name-directory buffer-file-name))
        (change-cd matlab-change-current-directory)
	(param ""))
    (save-buffer)
    ;; Do we need parameters?
    (if (save-excursion
	  (goto-char (point-min))
	  (end-of-line)
	  (forward-sexp -1)
	  (looking-at "([a-zA-Z]"))
	(setq param (read-string "Parameters: "
				 (car matlab-shell-save-and-go-history)
				 'matlab-shell-save-and-go-history)))

    ;; No buffer?  Make it!
    (if (not (get-buffer msbn)) (matlab-shell))
    ;; Ok, now fun the function in the matlab shell
    (if (get-buffer-window msbn t)
	(select-window (get-buffer-window msbn t))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*")))

    ;; change current directory?
    (if change-cd
        (let ((cmd (progn
                     (mapc
                      (lambda (e)
                        (while (string-match (car e) dir)
                          (setq dir (replace-match
                                     (format "', char(%s), '" (cdr e)) t t dir))))
                      '(("ô" . "244")
                        ("é" . "233")
                        ("è" . "232")
                        ("à" . "224")))
                     dir)))
          (matlab-shell-send-string (concat "cd(['" cmd "'])\n"))))

    (let ((cmd (concat fn-name " " param)))
      (matlab-shell-add-to-input-history cmd)
      (matlab-shell-send-string (concat cmd "\n")))))

;;; Running buffer subset
;;
;; Run some subset of the buffer in matlab-shell.

(defun matlab-shell-run-cell ()
  "Run the cell the cursor is in."
  (interactive)
  (let ((start (save-excursion (forward-page -1)
			       (if (looking-at "function")
				   (error "You are not in a cell.  Try `matlab-shell-save-and-go' instead"))
			       (when (matlab-ltype-comm)
				 ;; Skip over starting comment from the current cell.
				 (matlab-end-of-command 1)
				 (end-of-line)
				 (forward-char 1))
			       (point)))
	(end (save-excursion (forward-page 1)
			     (when (matlab-ltype-comm)
			       (beginning-of-line)
			       (forward-char -1))
			     (point))))
    (matlab-shell-run-region start end t)))

(defun matlab-shell-run-region-or-line ()
  "Run region from BEG to END and display result in MATLAB shell.
pIf region is not active run the current line.
This command requires an active MATLAB shell."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (matlab-shell-run-region (mark) (point))
    (matlab-shell-run-region (matlab-point-at-bol) (matlab-point-at-eol))))


(defun matlab-shell-run-region (beg end &optional noshow)
  "Run region from BEG to END and display result in MATLAB shell.
If NOSHOW is non-nil, replace newlines with commas to suppress output.
This command requires an active MATLAB shell."
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg  beg end  end mid)))

  (let ((command (matlab-shell-region-command beg end noshow))
 	(msbn nil)
 	(lastcmd)
	(inhibit-field-text-motion t))

    (save-excursion
      (setq msbn (matlab-shell-buffer-barf-not-running))
      (set-buffer msbn)
      (if (not (matlab-on-prompt-p))
	  (error "MATLAB shell must be non-busy to do that"))
      ;; Save the old command
      (beginning-of-line)
      (re-search-forward comint-prompt-regexp)
      (setq lastcmd (buffer-substring (point) (matlab-point-at-eol)))
      (delete-region (point) (matlab-point-at-eol))
      ;; We are done error checking, run the command.
      (matlab-shell-send-string command)
      (insert lastcmd))
      
    (set-buffer msbn)
    (goto-char (point-max))
    (display-buffer msbn nil "visible") ))

;;; Convert regions to runnable text
;;
;; There are two techniques.
;; Option 1: Convert the region into a single command line, suppress output, and eval.
;; Option 2: Copy into a script, and run the script.
(defun matlab-shell-region-command (beg end &optional noshow)
  "Convert the region between BEG and END into a MATLAB command.
Picks between different options for running the commands."
  (let ((cnt (count-lines beg end)))

    (if (< cnt 2)
	;; OLD WAY
	(matlab-shell-region->commandline beg end noshow)

      ;; else
      ;; NEW WAY
      (matlab-shell-extract-region-to-tmp-file beg end noshow))
    ))

(defun matlab-shell-region->commandline (beg end &optional noshow)
  "Convert the region between BEG and END into a MATLAB command.
Squeeze out newlines.
When NOSHOW is non-nil, supress output by adding ; to commands."
  ;; Assume beg & end are in the right order.
  (let ((str (concat (buffer-substring beg end) "\n")))
    ;; Remove comments
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      ;; Delete all the comments
      (while (search-forward "%" nil t)
	(when (not (matlab-cursor-in-string))
	  (delete-region (1- (point)) (matlab-point-at-eol))))
      (setq str (buffer-substring-no-properties (point-min) (point-max))))

    ;; Strip out blank lines
    (while (string-match "^\\s-*\n" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			(substring str (match-end 0)))))
    ;; Strip out large chunks of whitespace
    (while (string-match "\\s-\\s-+" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			(substring str (match-end 0)))))
    (when noshow
      ;; Remove continuations
      (while (string-match
	      (concat "\\s-*"
		      (regexp-quote matlab-elipsis-string)
		      "\\s-*\n")
	      str)
	(setq str (replace-match " " t t str)))
      (while (string-match "\n" str)
	(setq str (replace-match ", " t t str)))
      (setq str (concat str "\n")))
    str))

(declare-function matlab-semantic-get-local-functions-for-script "semantic-matlab")
(declare-function matlab-semantic-tag-text "semantic-matlab")
(declare-function semantic-tag-name "semantic/tag")

(defun matlab-shell-extract-region-to-tmp-file (beg end &optional noshow)
  "Extract region between BEG & END into a temporary M file.
The tmp file name is based on the name of the current buffer.
The extracted region is unmodifed from src buffer unless NOSHOW is non-nil,
in which case ; are added to quiece the buffer.
Scan the extracted region for any functions that are in the original
buffer,and include them.
Return the name of the temporary file."
  (interactive "r")
  (require 'semantic-matlab)
  (let* ((start (count-lines (point-min) beg))
	 (len (count-lines beg end))
	 (stem (file-name-sans-extension (file-name-nondirectory
					  (buffer-file-name))))
	 (orig (current-buffer))
	 (newf (concat stem "_" (number-to-string start) "_"
		       (number-to-string len)))
	 (bss (buffer-substring-no-properties beg end))
	 (buff (find-file-noselect (concat newf ".m")))
	 (intro "%% Automatically craeted temporary file created to run-region")
	 ;; These variables are for script / fcn tracking
	 (functions (matlab-semantic-get-local-functions-for-script (current-buffer)))
	 )

    ;; TODO : if the directory in which the current buffer is in is READ ONLY
    ;; we should write our tmp buffer to /tmp instead.
    
    (with-current-buffer buff

      (goto-char (point-min))
      
      ;; Clean up old extracted regions.
      (when (looking-at intro) (delete-region (point-min) (point-max)))
      ;; Don't stomp on old code.
      (when (not (= (point-min) (point-max)))
	(error "Region extract to tmp file: Temp file not empty!"))

      (insert intro "\n\n" bss "\n%%\n")

      ;; Some scripts call local functions from the script.  Find them
      ;; and copy those local scripts over.
      (goto-char (point-min))
      (dolist (F functions)
	(save-excursion
	  (when (re-search-forward (semantic-tag-name F) nil t)
	    ;; Found, copy it in.
	    (let ((ft (matlab-semantic-tag-text F orig)))
	      (goto-char (point-max))
	      (insert "% Copy of " (semantic-tag-name F) "\n\n")
	      (insert ft)
	      (insert "\n%%\n"))))
	)

      ;; Save buffer, and setup ability to run this new script.
      (save-buffer)

      ;; Flush any pending MATLAB stuff.
      (accept-process-output)
      
      ;; This sets us up to cleanup our file after it's done running.
      (add-hook 'matlab-shell-prompt-appears-hook `(lambda () (matlab-shell-cleanup-extracted-region ,(buffer-file-name buff))))

      (kill-buffer)
      )

    ;; Return the command.
    (concat "run('" (expand-file-name newf) "')\n")))

(defun matlab-shell-cleanup-extracted-region (fname)
  "Cleanup the file created when we previously extracted a region."
  (condition-case nil
      (delete-file fname)
    (error nil))

  (remove-hook 'matlab-shell-prompt-appears-hook
	       ;; The below needs to be a perfect match to the setter.
	       `(lambda () (matlab-shell-cleanup-extracted-region ,fname)))
  )

;;; M File path stuff =========================================================

(defcustom matlab-mode-install-path
  (list
   (expand-file-name "toolbox" (matlab-mode-determine-matlabroot)))
  "Base path pointing to the locations of all the m files used by matlab.
All directories under each element of `matlab-mode-install-path' are
checked, so only top level toolbox directories need be added.
Paths should be added in the order in which they should be searched."
  :group 'matlab-shell
  :type '(repeat (string :tag "Path: ")))

(defun matlab-find-file-under-path (path filename)
  "Return the pathname or nil of PATH under FILENAME."
  (if (file-exists-p (concat path filename))
      (concat path filename)
    (let ((dirs (if (file-directory-p path)
		    ;; Not checking as a directory first fails on XEmacs
		    ;; Stelios Kyriacou <kyriacou@cbmv.jhu.edu>
		    (directory-files path t nil t)))
	  (found nil))
      (while (and dirs (not found))
	(if (and (car (file-attributes (car dirs)))
 		 ;; require directory readable
 		 (file-readable-p (car dirs))
		 ;; don't redo our path names
		 (not (string-match "/\\.\\.?$" (car dirs)))
		 ;; don't find files in object directories.
		 (not (string-match "@" (car dirs))))
	    (setq found
		  (matlab-find-file-under-path (concat (car dirs) "/")
					       filename)))
	(setq dirs (cdr dirs)))
      found)))

(defun matlab-find-file-on-path (filename)
  "Find FILENAME on the current MATLAB path.
The MATLAB path is determined by `matlab-mode-install-path' and the
current directory.  You must add user-installed paths into
`matlab-mode-install-path' if you would like to have them included."
  (interactive
   (list
    (let ((default (matlab-read-word-at-point)))
      (if default
	  (let ((s (read-string (concat "File (default " default "): "))))
	    (if (string= s "") default s))
	(read-string "File: ")))))
  (if (string= filename "")
      (error "You must specify an M file"))
  (if (not (string-match "\\.m$" filename))
      (setq filename (concat filename ".m")))
  (let ((fname nil)
	(dirs matlab-mode-install-path))
    (if (file-exists-p (concat default-directory filename))
	(setq fname (concat default-directory filename)))
    (while (and (not fname) dirs)
      (if (stringp (car dirs))
	  (progn
	    (message "Searching for %s in %s" filename (car dirs))
	    (setq fname (matlab-find-file-under-path (car dirs) filename))))
      (setq dirs (cdr dirs)))
    (if fname (find-file fname)
      (error "File %s not found on any known paths.  \
Check `matlab-mode-install-path'" filename))))

(defun matlab-find-file-click (e)
  "Find the file clicked on with event E on the current path."
  (interactive "e")
  (mouse-set-point e)
  (let ((f (matlab-read-word-at-point)))
    (if (not f) (error "To find an M file, click on a word"))
    (matlab-find-file-on-path f)))

(provide 'matlab-shell)

;;; matlab-shell.el ends here
