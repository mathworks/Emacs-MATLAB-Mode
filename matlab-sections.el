;;; matlab-sections.el --- Support for code sections in matlab mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Nidish Narayanaa Balaji.
;; Author: Nidish Narayanaa Balaji <nidbid@gmail.com>
;; Created: 2024-05-14
;; Renamed: 2024-10-22
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; NOTE: What is referred to as "sections" herein were previously
;; referred to as code cells.
;;
;; This creates a minor mode called `matlab-sections-mode' that adds
;; utilities for working with code sections in matlab code.  The basic mechanic
;; is to redefine the page-delimiter (locally) to any line that starts
;; with "%%" as the first non-empty characters followed by some
;; comment strings.
;; Consequently, the line that is detected in the above manner is
;; highlighted by the face `matlab-sections-cellbreak-face'.  By defailt,
;; this is bold-faced and has an overline above it.
;;
;; The section that point is on is highlighted by the face
;; `matlab-sections-highlight-face'.  By default this is set to
;; "extra-bold".  The section-highlight can be toggled using
;; `matlab-sections-highlight-section' (defaults to "t").
;;
;; Another variable, `matlab-sections-sticky-flag' is defined, that
;; defines whether the current section is highlighted even when point
;; moves to another window (defaults to "t").
;;
;; Finally, the minor-mode provides the following interactive
;; navigation functions (default keybindings provided within []): 
;; 1. `matlab-sections-forward-section' : Move point to the beginning of the
;;    section right below. [C-s-<down>]
;; 2. `matlab-sections-backward-section' : Move point to the end of the section
;;    right above. [C-s-<up>]
;; 3. `matlab-sections-beginning-of-section' : Move point to beginning of
;;    current section.  Return (point). [C-s-<left>]
;; 4. `matlab-sections-end-of-section' : Move point to end of current section.
;;    Return (point). [C-s-<right>]
;; 5. `matlab-sections-move-section-up' : Move the contents of the current section
;;    \"up\", so that it occurs before the previous. [s-<up>]
;; 6. `matlab-sections-move-section-down' : Move the contents of the current
;;    section \"down\", so that it occurs after the next. [s-<down>]
;; 7. `matlab-sections-run-till-point' : Run all the sections from beginning
;;    till previous section. [s-<return>]
;; 8. `matlab-sections-mark-section' : Mark the current section. [s-c]
;; (Note that some default keybindings may clash with existing
;; keyindings in the desktop environment)
;;
;; Other than this, there are some utility functions to help
;; development.
;;
;; Major parts of the code are modified from python-cell.el by Thomas
;; Hisch (currently at: https://github.com/twmr/python-cell.el).
;;
;;; Code:
;;
;; Customizable Variables and Faces
(defgroup matlab-sections nil
  "MATLAB-GUI-like sections in matlab-mode."
  :group 'matlab)

(defface matlab-sections-highlight-face
  '((t :weight extra-bold))
  "Default face for highlighting the current section in matlab-sections minor mode."
  :group 'matlab-sections)

(defface matlab-sections-cellbreak-face
  '((t :weight bold :overline t))
  "Default face for the section separation line in matlab-sections minor mode."
  :group 'matlab-sections)

(defcustom matlab-sections-highlight-section t
  "Non-nil tells matlab-sections mode to highlight the current section."
  :type 'boolean
  :group 'matlab-sections
  :safe 'booleanp)

(defcustom matlab-sections-cellbreak-regexp
  (rx line-start (* space)
      (group "%%" (* (not (any "\n"))) line-end))
  "Regexp used for detecting the section boundaries of code sectopms."
  :type 'string
  :group 'matlab-sections
  :safe 'stringp)

(defvar matlab-sections-mode)

(defvar matlab-sections-overlay nil
  "Overlay used by matlab-sections mode to highlight the current section.")
(make-variable-buffer-local 'matlab-sections-overlay)

(defcustom matlab-sections-highlight-face 'matlab-sections-highlight-face
  "Face with which to highlight the current section in matlab-sections mode."
  :type 'face
  :group 'matlab-sections
  :set (lambda (symbol value)
	 (set symbol value)
	 (dolist (buffer (buffer-list))
	   (with-current-buffer buffer
	     (when matlab-sections-overlay
	       (overlay-put matlab-sections-overlay 'face matlab-sections-highlight-face))))))

(defcustom matlab-sections-sticky-flag t
  "Non-nil means the matlab-sections mode highlight appears in all windows.
Otherwise matlab-sections mode will highlight only in the selected
window.  Setting this variable takes effect the next time you use
the command `matlab-sections-mode' to turn matlab-sections mode on."
  :type 'boolean
  :group 'matlab-sections)

;; Function to obtain range of current section

(defun matlab-sections-range-function ()
  "Function to call to return highlight range.
The function of no args should return a cons section; its car value
is the beginning position of highlight and its cdr value is the
end position of highlight in the buffer.
It should return nil if there's no region to be highlighted."
  (save-match-data
    (let ((r-start (save-excursion
		     (progn (end-of-line)
			    (if (re-search-backward matlab-sections-cellbreak-regexp nil t)
				(progn (goto-char (match-beginning 0))
				       (point))
			      (point-min)))))
	  (r-end (save-excursion
		   (progn (end-of-line)
			  (if (re-search-forward matlab-sections-cellbreak-regexp nil t)
			      (progn (goto-char (match-beginning 0))
				     (point))
			    (point-max))))))
      (progn
	;; (message "cp is %s start is %s; end is %s" (point) r-start r-end)
	(if (and (eq r-start (point-min)) (eq r-end (point-max)))
	    nil
	  `(,r-start . ,r-end))))))

;; Navigation

(defun matlab-sections-move-section-up (&optional arg)
  "Move the contents of the current section up.
Optionally a prefix argument ARG can be provided for repeating it a
bunch of times."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((rngc (matlab-sections-range-function))
	  (rngp (save-excursion (matlab-sections-backward-section)
				(matlab-sections-range-function))))
      (goto-char (car rngp))
      (kill-region (car rngc) (cdr rngc))
      (yank)
      (matlab-sections-backward-section)))
  )

(defun matlab-sections-move-section-down (&optional arg)
  "Move the contents of the current section down.
Optionally a prefix argument ARG can be provided for repeating it a
  bunch of times."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((rngc (matlab-sections-range-function))
	  (rngn (save-excursion (matlab-sections-forward-section)
				(matlab-sections-range-function))))
      (goto-char (cdr rngn))
      (kill-region (car rngc) (cdr rngc))
      (yank)
      (forward-char -1)
      (matlab-sections-beginning-of-section)))
  )

(defun matlab-sections-forward-section  (&optional arg aggressive)
  "Move point forward by a section.
Optionally provide prefix argument ARG to move by that many sections.
Optionally provide argument AGGRESSIVE to specify whether to move
  aggressively to next section or just move to end of current section if
  next section is not visible."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((endp (save-excursion (matlab-sections-end-of-section))))
      (if (and (not (eq (point) endp))
	       (not (pos-visible-in-window-p endp))
	       (not aggressive))
	  (goto-char endp)
	(goto-char endp)
    (if (re-search-forward matlab-sections-cellbreak-regexp nil t)
	(progn (end-of-line)
	       (forward-char 1))
      (goto-char (point-max)))
    )))
  )

(defun matlab-sections-backward-section  (&optional arg aggressive)
  "Move point backwards by a section.
Optionally provide prefix argument ARG to move by that many sections.
Optionally provide argument AGGRESSIVE to specify whether to move
  aggressively to previous section or just move to beginning of current
  section if previous section is not visible."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((begp (save-excursion (matlab-sections-beginning-of-section))))
      (if (and (not (eq (point) begp))
	       (not (pos-visible-in-window-p begp))
	       (not aggressive))
	  (goto-char begp)
	(goto-char begp)
	(forward-char -1)
	(beginning-of-line)
	(and (save-excursion (re-search-backward matlab-sections-cellbreak-regexp
						 nil t))
	     (= (match-beginning 0) (save-excursion
				      (forward-char -1) (beginning-of-line) (point)))
	     (goto-char (match-beginning 0)))

	(if (> (point) (point-min))
	    (forward-char -1))
	(if (re-search-backward matlab-sections-cellbreak-regexp nil t)
	    (progn (goto-char (match-end 0))
		   (end-of-line)
		   (forward-char 1))
	  (goto-char (point-min)))
	)))
  )

(defun matlab-sections-beginning-of-section ()
  "Move point to beginning of section."
  (interactive)

  (end-of-line)
  (if (re-search-backward matlab-sections-cellbreak-regexp nil t)
      (progn (goto-char (match-end 0))
	     (end-of-line)
	     (forward-char 1))
    (goto-char (point-min)))
  (point)
  )

(defun matlab-sections-end-of-section ()
  "Move point to end of section."
  (interactive)

  (end-of-line)
  (if (re-search-forward matlab-sections-cellbreak-regexp nil t)
      (progn (goto-char (match-beginning 0))
	     (forward-char -1))
    (goto-char (point-max)))
  (point)
  )

(defun matlab-sections-mark-section ()
  "Mark the contents of the current section. Replaces `mark-page'."
  (interactive)
  (let ((rng (matlab-sections-range-function)))
    (set-mark (car rng))
    (goto-char (cdr rng))
    )
  )

;; Execution

(defun matlab-sections-shell-run-section ()
  "Run the section point is in, in matlab-shell."
  (interactive)
  (let ((rng (matlab-sections-range-function)))
    (matlab-shell-run-region (car rng) (cdr rng)))
  )

(defun matlab-sections-run-till-point ()
  "Run all sections until point, not including the section point is in."
  (interactive)
  (let ((pt (point)))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (>= pt (point))
	  (save-window-excursion (matlab-sections-shell-run-section))
	  (matlab-sections-forward-section)
	  (matlab-sections-end-of-section))))))

;;; Section Highlighting

(defun matlab-sections-highlight ()
  "Activate the matlab-sections overlay on the current line."
  (if matlab-sections-mode  ; Might be changed outside the mode function.
      (progn
	(unless matlab-sections-overlay
	  (setq matlab-sections-overlay (make-overlay 1 1)) ; to be moved
	  (overlay-put matlab-sections-overlay 'face matlab-sections-highlight-face))
	(overlay-put matlab-sections-overlay
		     'window (unless matlab-sections-sticky-flag (selected-window)))
	(matlab-sections-move-overlay matlab-sections-overlay))
    (matlab-sections-unhighlight)))

(defun matlab-sections-unhighlight ()
  "Deactivate the matlab-sections overlay on the current line."
  (when matlab-sections-overlay
    (delete-overlay matlab-sections-overlay)))

(defun matlab-sections-move-overlay (overlay)
  "Move the matlab-sections overlay given as OVERLAY."
  (if-let ((start-end (matlab-sections-range-function)))
      (move-overlay overlay (car start-end) (cdr start-end))
    (move-overlay overlay 1 1)))

(defun matlab-sections-setup-sectionhighlight ()
  "Setup section highlighting."
  ;; In case `kill-all-local-variables' is called.
  (add-hook 'change-major-mode-hook #'matlab-sections-unhighlight nil t)
  (if matlab-sections-sticky-flag
      (remove-hook 'pre-command-hook #'matlab-sections-unhighlight t)
    (add-hook 'pre-command-hook #'matlab-sections-unhighlight nil t))
  (matlab-sections-highlight)
  (add-hook 'post-command-hook #'matlab-sections-highlight nil t))

;;; Keymap
(defvar matlab-sections-mode-map  
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s-<down>") #'matlab-sections-forward-section)
    (define-key map (kbd "C-s-<up>") #'matlab-sections-backward-section)
    (define-key map (kbd "C-s-<left>") #'matlab-sections-beginning-of-section)
    (define-key map (kbd "C-s-<right>") #'matlab-sections-end-of-section)
    (define-key map (kbd "s-<up>") #'matlab-sections-move-section-up)
    (define-key map (kbd "s-<down>") #'matlab-sections-move-section-down)
    (define-key map (kbd "s-<return>") #'matlab-sections-run-till-point)
    (define-key map (kbd "s-c") #'matlab-sections-mark-section)
    map)
  "Key map for matlab-sections minor mode. ")

(defalias 'matlab-sections-what-section #'what-page)
(defalias 'matlab-sections-narrow-to-section #'narrow-to-page)

;;; Minor mode:

;;;###autoload
(define-minor-mode matlab-sections-mode
  "Highlight MATLAB-like sections and navigate between them.
The minor-mode provides the following interactive navigation
functions. The default keybindings are provided in square brackets for 
each: 
1. `matlab-sections-forward-section' : Move point to the beginning of the
   section right below. [C-s-<down>]
2. `matlab-sections-backward-section' : Move point to the end of the section
   right above. [C-s-<up>]
3. `matlab-sections-beginning-of-section' : Move point to beginning of
   current section.  Return (point). [C-s-<left>]
4. `matlab-sections-end-of-section' : Move point to end of current section.
   Return (point). [C-s-<right>]
5. `matlab-sections-move-section-up' : Move the contents of the current section
   \"up\", so that it occurs before the previous. [s-<up>]
6. `matlab-sections-move-section-down' : Move the contents of the current
   section \"down\", so that it occurs after the next. [s-<down>]
7. `matlab-sections-run-till-point' : Run all the sections from beginning
   till previous section. [s-<return>]
8. `matlab-sections-mark-section' : Mark the current section. [s-c]"
  :init-value nil
  :keymap matlab-sections-mode-map
  
  ;; (let ((arg `((,matlab-sections-cellbreak-regexp 1 'matlab-sections-cellbreak-face prepend))))
  (make-local-variable 'page-delimiter)
  (setq page-delimiter matlab-sections-cellbreak-regexp)
  ;; (font-lock-add-keywords nil arg)
  (when matlab-sections-highlight-section
    (matlab-sections-setup-sectionhighlight))
  ;; (font-lock-flush))
  )

;;;###autoload
(defun matlab-sections-mode-enable ()
  "Enable matlab-sections-mode."
  (matlab-sections-mode 1))

;;;###autoload
(defun matlab-sections-mode-disable ()
  "Disable matlab-sections-mode."
  (matlab-sections-mode 0))

(provide 'matlab-sections)
;;; matlab-sections.el ends here
