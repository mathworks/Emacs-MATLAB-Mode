;;; matlab-topic.el --- Topic browswer that runs from MATLAB shell.
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
;; Browsing topics via the MATLAB shell in an Emacs buffer is a nice
;; feature.  Moving as optional as this is rarely used.

;;; Code:
(require 'matlab)
(require 'matlab-shell)

(defcustom matlab-shell-topic-mode-hook nil
  "*MATLAB shell topic hook."
  :group 'matlab-shell
  :type 'hook)

(defvar matlab-shell-topic-current-topic nil
  "The currently viewed topic in a MATLAB shell topic buffer.")

;;;###autoload
(defun matlab-shell-topic-browser ()
  "Create a topic browser by querying an active MATLAB shell using HELP.
Maintain state in our topic browser buffer."
  (interactive)
  ;; Reset topic browser if it doesn't exist.
  (if (not (get-buffer "*MATLAB Topic*"))
      (setq matlab-shell-topic-current-topic nil))
  (let ((b (get-buffer-create "*MATLAB Topic*")))
    (switch-to-buffer b)
    (if (string= matlab-shell-topic-current-topic "")
	nil
      (matlab-shell-topic-mode)
      (matlab-shell-topic-browser-create-contents ""))))

(defvar matlab-shell-topic-mouse-face-keywords
  '(;; These are subtopic fields...
    ("^\\(\\w+/\\w+\\)[ \t]+-" 1 font-lock-reference-face)
    ;; These are functions...
    ("^[ \t]+\\(\\w+\\)[ \t]+-" 1 font-lock-function-name-face)
    ;; Here is a See Also line...
    ("[ \t]+See also "
     ("\\(\\w+\\)\\([,.]\\| and\\|$\\) *" nil  nil (1 font-lock-reference-face))))
  "These are keywords we also want to put mouse-faces on.")

(defvar matlab-shell-topic-font-lock-keywords
  (append matlab-shell-topic-mouse-face-keywords
	  '(("^[^:\n]+:$" 0 font-lock-keyword-face)
	    ;; These are subheadings...
	    ("^[ \t]+\\([^.\n]+[a-zA-Z.]\\)$" 1 'underline)
	    ))
  "Keywords useful for highlighting a MATLAB TOPIC buffer.")

(defvar matlab-shell-help-font-lock-keywords
  (append matlab-shell-topic-mouse-face-keywords
	  '(;; Function call examples
	    ("[ \t]\\([A-Z]+\\)\\s-*=\\s-*\\([A-Z]+[0-9]*\\)("
	     (1 font-lock-variable-name-face)
	     (2 font-lock-function-name-face))
	    ("[ \t]\\([A-Z]+[0-9]*\\)("
	     (1 font-lock-function-name-face))
	    ;; Parameters: Not very accurate, unfortunately.
	    ("[ \t]\\([A-Z]+[0-9]*\\)("
	     ("'?\\(\\w+\\)'?\\([,)]\\) *" nil  nil
	      (1 font-lock-variable-name-face))
	     )
	    ;; Reference uppercase words
	    ("\\<\\([A-Z]+[0-9]*\\)\\>" 1 font-lock-reference-face)))
  "Keywords for regular help buffers.")

;; View-major-mode is an emacs20 thing.  This gives us a small compatibility
;; layer.
(eval-and-compile
  (if (not (fboundp 'view-major-mode)) (defalias 'view-major-mode 'view-mode)))

(defvar matlab-shell-help-mode-menu) ;; Quiet compiler warning (var is defined below)

;;;###autoload
(define-derived-mode matlab-shell-help-mode
  view-major-mode "M-Help"
  "Major mode for viewing MATLAB help text.
Entry to this mode runs the normal hook `matlab-shell-help-mode-hook'.

Commands:
\\{matlab-shell-help-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-shell-help-font-lock-keywords)
			     t nil ((?_ . "w"))))
  ;; This makes sure that we really enter font lock since
  ;; kill-all-local-variables is not used by old view-mode.
  (and (boundp 'global-font-lock-mode) global-font-lock-mode
       (not font-lock-mode) (font-lock-mode 1))
  (easy-menu-add matlab-shell-help-mode-menu matlab-shell-help-mode-map)
  (matlab-shell-topic-mouse-highlight-subtopics)
  )

(define-key matlab-shell-help-mode-map [return] 'matlab-shell-topic-choose)
(define-key matlab-shell-help-mode-map "t" 'matlab-shell-topic-browser)
(define-key matlab-shell-help-mode-map "q" 'bury-buffer)
(define-key matlab-shell-help-mode-map
  [(control h) (control m)] matlab-help-map)
(if (string-match "XEmacs" emacs-version)
    (define-key matlab-shell-help-mode-map [button2] 'matlab-shell-topic-click)
  (define-key matlab-shell-help-mode-map [mouse-2] 'matlab-shell-topic-click))

(defvar mode-motion-hook) ;; quiet compiler warning (used in XEmacs)
(easy-menu-define
 matlab-shell-help-mode-menu matlab-shell-help-mode-map
 "MATLAB shell topic menu"
 '("MATLAB Help"
   ["Describe This Command" matlab-shell-topic-choose t]
   "----"
   ["Describe Command" matlab-shell-describe-command t]
   ["Describe Variable" matlab-shell-describe-variable t]
   ["Command Apropos" matlab-shell-apropos t]
   ["Topic Browser" matlab-shell-topic-browser t]
   "----"
   ["Exit" bury-buffer t]))

(defvar matlab-shell-topic-mode-menu) ;; forward declaration to quite compiler warning

(define-derived-mode matlab-shell-topic-mode
  matlab-shell-help-mode "M-Topic"
  "Major mode for browsing MATLAB HELP topics.
The output of the MATLAB command HELP with no parameters creates a listing
of known help topics at a given installation.  This mode parses that listing
and allows selecting a topic and getting more help for it.
Entry to this mode runs the normal hook `matlab-shell-topic-mode-hook'.

Commands:
\\{matlab-shell-topic-mode-map}"
  (setq font-lock-defaults '((matlab-shell-topic-font-lock-keywords)
			     t t ((?_ . "w"))))
  (if (string-match "XEmacs" emacs-version)
      (setq mode-motion-hook 'matlab-shell-topic-highlight-line))
  (easy-menu-add matlab-shell-topic-mode-menu matlab-shell-topic-mode-map)
  )

(easy-menu-define
 matlab-shell-topic-mode-menu matlab-shell-topic-mode-map
 "MATLAB shell topic menu"
 '("MATLAB Topic"
   ["Select This Topic" matlab-shell-topic-choose t]
   ["Top Level Topics" matlab-shell-topic-browser t]
   "----"
   ["Exit" bury-buffer t]))

(defun matlab-shell-topic-browser-create-contents (subtopic)
  "Fill in a topic browser with the output from SUBTOPIC."
  (matlab-read-only-mode -1)
  (erase-buffer)
  (insert (matlab-shell-collect-command-output (concat "help " subtopic)))
  (goto-char (point-min))
  (forward-line 1)
  (delete-region (point-min) (point))
  (setq matlab-shell-topic-current-topic subtopic)
  (if (not (string-match "XEmacs" emacs-version))
      (matlab-shell-topic-mouse-highlight-subtopics))
  (matlab-read-only-mode 1)
  )

(defun matlab-shell-topic-click (e)
  "Click on an item in a MATLAB topic buffer we want more information on.
Must be bound to event E."
  (interactive "e")
  (mouse-set-point e)
  (matlab-shell-topic-choose))

(defun matlab-shell-topic-choose ()
  "Choose the topic to expand on that is under the cursor.
This can fill the topic buffer with new information.  If the topic is a
command, use `matlab-shell-describe-command' instead of changing the topic
buffer."
  (interactive)
  (let ((topic nil) (fun nil) (p (point)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\w+/\\(\\w+\\)[ \t]+-")
	  (setq topic (match-string 1))
	(if (looking-at "^[ \t]+\\(\\(\\w\\|_\\)+\\)[ \t]+-")
	    (setq fun (match-string 1))
	  (if (and (not (looking-at "^[ \t]+See also"))
		   (not (save-excursion (forward-char -2)
					(looking-at ",$"))))
	      (error "You did not click on a subtopic, function or reference")
	    (goto-char p)
	    (forward-word -1)
	    (if (not (looking-at "\\(\\(\\w\\|_\\)+\\)\\([.,]\\| and\\|\n\\)"))
		(error "You must click on a reference")
	      (setq topic (match-string 1)))))))
    (message "Opening item %s..." (or topic fun))
    (if topic
	(matlab-shell-topic-browser-create-contents (downcase topic))
      (matlab-shell-describe-command fun))
    ))

(defun matlab-shell-topic-mouse-highlight-subtopics ()
  "Put a `mouse-face' on all clickable targets in this buffer."
  (save-excursion
    (let ((el matlab-shell-topic-mouse-face-keywords))
      (while el
	(goto-char (point-min))
	(while (re-search-forward (car (car el)) nil t)
	  (let ((cd (car (cdr (car el)))))
	    (if (numberp cd)
		(put-text-property (match-beginning cd) (match-end cd)
				   'mouse-face 'highlight)
	      (while (re-search-forward (car cd) nil t)
		(put-text-property (match-beginning (car (nth 3 cd)))
				   (match-end (car (nth 3 cd)))
				   'mouse-face 'highlight)))))
	(setq el (cdr el))))))

(defvar mouse-grabbed-buffer) ;; Suppress compilation warning in Emacs (an XEmacs only variable)
(defvar mode-motion-extent)   ;; Suppress compilation warning in Emacs (an XEmacs only variable)

(defun matlab-shell-topic-highlight-line (event)
  "A value of `mode-motion-hook' which will highlight topics under the mouse.
EVENT is the user mouse event."
  ;; XEMACS only function
  (let* ((buffer (when (fboundp 'event-buffer) (event-buffer event)))
	 (point (and buffer (when (fboundp 'event-point) (event-point event)))))
    (if (and buffer (not (eq buffer mouse-grabbed-buffer)))
	(save-excursion
	  (save-window-excursion
	    (set-buffer buffer)
	    (when (fboundp 'mode-motion-ensure-extent-ok) (mode-motion-ensure-extent-ok event))
	    (if (not point)
		(when (fboundp 'detach-extent) (detach-extent mode-motion-extent))
	      (goto-char point)
	      (end-of-line)
	      (setq point (point))
	      (beginning-of-line)
	      (if (or (looking-at "^\\w+/\\(\\w+\\)[ \t]+-")
		      (looking-at "^[ \t]+\\(\\(\\w\\|_\\)+\\)[ \t]+-"))
		  (when (fboundp 'set-extent-endpoints)
                    (set-extent-endpoints mode-motion-extent (point) point))
		(when (fboundp 'detach-extent) (detach-extent mode-motion-extent)))))))))

(provide 'matlab-topic)

;;; matlab-topic.el ends here
