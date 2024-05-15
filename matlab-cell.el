;;; matlab-cell.el --- Support for cells in matlab mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Nidish Narayanaa Balaji.
;; Author: Nidish Narayanaa Balaji <nidbid@gmail.com>
;; Created: 2024-05-14

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This creates a minor mode called 'matlab-cell-mode' that adds
;; utilities for working with cells in matlab code. The basic mechanic
;; is to redefine the page-delimiter (locally) to any line that starts
;; with "%%" as the first non-empty characters followed by some
;; comment strings.
;; Consequently, the line that is detected in the above manner is
;; highlighted by the face 'matlab-cell-cellbreak-face'. By defailt,
;; this is bold-faced and has an overline above it.
;;
;; The cell point is on is highlighted by the face
;; 'matlab-cell-highlight-face'. By default this is set to
;; "extra-bold". The cell-highlight can be toggled using
;; 'matlab-cell-highlight-cell' (defaults to "t").
;;
;; Another variable, 'matlab-cell-sticky-flag' is defined, that
;; defines whether the current cell is highlighted even when point
;; moves to another window (defaults to "t").
;;
;; Finally, the minor-mode provides the following interactive navigation functions:
;; 1. 'matlab-cell-forward-cell' :| Move point to the beginning of the cell right below.
;; 2. 'matlab-cell-backward-cell' : Move point to the end of the cell right above.
;; 3. 'matlab-cell-beginning-of-cell' : Move point to beginning of current cell. Return (point).
;; 4. 'matlab-cell-end-of-cell' : Move point to end of current cell. Return (point).
;; 5. 'matlab-move-cell-up' : Move the contents of the current cell "up", so that it occurs before the previous.
;; 6. 'matlab-move-cell-down' : Move the contents of the current cell "down", so that it occurs after the next.
;; 7. 'matlab-cell-run-till-point' : Run all the cells from beginning till previous cell.
;;
;; Other than this, there are some utility functions to help
;; development.
;;
;; Major parts of the code are modified from python-cell.el by Thomas
;; Hisch (currently at: https://github.com/twmr/python-cell.el). 

;;; Code:
;; (require 'matlab-shell)

;; Customizable Variables and Faces
(defgroup matlab-cell nil
  "MATLAB-GUI-like cells in matlab-mode."
  :group 'matlab)

(defface matlab-cell-highlight-face
  '((t :weight extra-bold))
  "Default face for highlighting the current cell in Matlab-Cell mode."
  :group 'matlab-cell)

(defface matlab-cell-cellbreak-face
  '((t :weight bold :overline t))
  "Default face for the cell separation line in Matlab-Cell mode."
  :group 'matlab-cell)

(defcustom matlab-cell-highlight-cell t
  "Non-nil tells Matlab-Cell mode to highlight the current cell."
  :type 'boolean
  :group 'matlab-cell
  :safe 'booleanp)

(defcustom matlab-cell-cellbreak-regexp
  (rx line-start (* space)
      (group "%%" (* (not (any "\n"))) line-end))
  "Regexp used for detecting the cell boundaries of code cells/blocks."
  :type 'string
  :group 'matlab-cell
  :safe 'stringp)

(defvar matlab-cell-mode)

(defvar matlab-cell-overlay nil
  "Overlay used by Matlab-Cell mode to highlight the current cell.")
(make-variable-buffer-local 'matlab-cell-overlay)

(defcustom matlab-cell-highlight-face 'matlab-cell-highlight-face
  "Face with which to highlight the current cell in Matlab-Cell mode."
  :type 'face
  :group 'matlab-cell
  :set (lambda (symbol value)
         (set symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when matlab-cell-overlay
               (overlay-put matlab-cell-overlay 'face matlab-cell-highlight-face))))))

(defcustom matlab-cell-sticky-flag t
  "Non-nil means the Matlab-Cell mode highlight appears in all windows.
Otherwise Matlab-Cell mode will highlight only in the selected
window.  Setting this variable takes effect the next time you use
the command `matlab-cell-mode' to turn Matlab-Cell mode on."
  :type 'boolean
  :group 'matlab-cell)

;; Navigation
(defun matlab-cell-move-cell-up (&optional arg)
  "Move the contents of the current cell up.
Optionally a prefix argument ARG can be provided for repeating it a
bunch of times."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((rngc (matlab-cell-range-function))
	  (rngp (save-excursion (matlab-cell-backward-cell)
				(matlab-cell-range-function))))    
      (goto-char (car rngp))
      (kill-region (car rngc) (cdr rngc))
      (yank)
      (matlab-cell-backward-cell)))
  )

(defun matlab-cell-move-cell-down (&optional arg)
  "Move the contents of the current cell down.
Optionally a prefix argument ARG can be provided for repeating it a
  bunch of times." 
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((rngc (matlab-cell-range-function))
	  (rngn (save-excursion (matlab-cell-forward-cell)
				(matlab-cell-range-function))))    
      (goto-char (cdr rngn))
      (kill-region (car rngc) (cdr rngc))
      (yank)
      (forward-char -1)
      (matlab-cell-beginning-of-cell)))
  )

(defun matlab-cell-forward-cell  (&optional arg)
  "Move point forward by a cell.
Optionally provide prefix argument ARG to move by that many cells."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (matlab-cell-end-of-cell)
    (if (re-search-forward matlab-cell-cellbreak-regexp nil t)
	(progn (end-of-line)
               (forward-char 1))
      (goto-char (point-max)))
    )
  )

(defun matlab-cell-backward-cell  (&optional arg)
  "Move point backwards by a cell.
Optionally provide prefix argument ARG to move by that many cells."  
  (interactive "p")
  
  (dotimes (_ (or arg 1))
    (matlab-cell-beginning-of-cell)
    (forward-char -1)
    (beginning-of-line)
    (and (save-excursion (re-search-backward matlab-cell-cellbreak-regexp
                                             nil t))
	 (= (match-beginning 0) (save-excursion
                                  (forward-char -1) (beginning-of-line) (point)))
	 (goto-char (match-beginning 0)))

    (if (> (point) (point-min))
	(forward-char -1))
    (if (re-search-backward matlab-cell-cellbreak-regexp nil t)
	(progn (goto-char (match-end 0))
               (end-of-line)
               (forward-char 1))
      (goto-char (point-min)))
    )
  )

(defun matlab-cell-beginning-of-cell ()
  "Move point to beginning of cell."
  (interactive)

  (end-of-line)
  (if (re-search-backward matlab-cell-cellbreak-regexp nil t)
      (progn (goto-char (match-end 0))
             (end-of-line)
             (forward-char 1))
    (goto-char (point-min)))
  (point)
  )

(defun matlab-cell-end-of-cell ()
  "Move point to end of cell."
  (interactive)

  (end-of-line)
  (if (re-search-forward matlab-cell-cellbreak-regexp nil t)
      (progn (goto-char (match-beginning 0))
             (forward-char -1))
    (goto-char (point-max)))
  (point)
  )

;; Execution

(defun matlab-cell-shell-run-cell ()
  "Run the cell point is in, in matlab-shell."
  (interactive)
  (let ((rng (matlab-cell-range-function)))
    (matlab-shell-run-region (car rng) (cdr rng)))
  )

(defun matlab-cell-run-till-point ()
  "Run all cells until point, not including the cell point is in."
  (interactive)
  (let ((pt (point)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (>= pt (point))
	  (save-window-excursion (matlab-cell-shell-run-cell))
          (matlab-cell-forward-cell)
          (matlab-cell-end-of-cell))))))

;;; Cell Highlighting

(defun matlab-cell-range-function ()
  "Function to call to return highlight range.
The function of no args should return a cons cell; its car value
is the beginning position of highlight and its cdr value is the
end position of highlight in the buffer.
It should return nil if there's no region to be highlighted."
  (save-match-data
    (let ((r-start (save-excursion
                     (progn (end-of-line)
                            (if (re-search-backward matlab-cell-cellbreak-regexp nil t)
                                (progn (goto-char (match-beginning 0))
                                       (point))
                              (point-min)))))
          (r-end (save-excursion
                   (progn (end-of-line)
                          (if (re-search-forward matlab-cell-cellbreak-regexp nil t)
                              (progn (goto-char (match-beginning 0))
                                     (point))
                            (point-max))))))
      (progn
        ;; (message "cp is %s start is %s; end is %s" (point) r-start r-end)
        (if (and (eq r-start (point-min)) (eq r-end (point-max)))
            nil
          `(,r-start . ,r-end))))))

(defun matlab-cell-highlight ()
  "Activate the Matlab-Cell overlay on the current line."
  (if matlab-cell-mode  ; Might be changed outside the mode function.
      (progn
        (unless matlab-cell-overlay
          (setq matlab-cell-overlay (make-overlay 1 1)) ; to be moved
          (overlay-put matlab-cell-overlay 'face matlab-cell-highlight-face))
        (overlay-put matlab-cell-overlay
                     'window (unless matlab-cell-sticky-flag (selected-window)))
        (matlab-cell-move-overlay matlab-cell-overlay))
    (matlab-cell-unhighlight)))

(defun matlab-cell-unhighlight ()
  "Deactivate the Matlab-Cell overlay on the current line."
  (when matlab-cell-overlay
    (delete-overlay matlab-cell-overlay)))

(defun matlab-cell-move-overlay (overlay)
  "Move the Matlab-Cell overlay."
  (if-let ((start-end (matlab-cell-range-function)))
      (move-overlay overlay (car start-end) (cdr start-end))
    (move-overlay overlay 1 1)))

(defun matlab-cell-setup-cellhighlight ()
  "Setup cell highlighting."
  ;; In case `kill-all-local-variables' is called.
  (add-hook 'change-major-mode-hook #'matlab-cell-unhighlight nil t)
  (if matlab-cell-sticky-flag
      (remove-hook 'pre-command-hook #'matlab-cell-unhighlight t)
    (add-hook 'pre-command-hook #'matlab-cell-unhighlight nil t))
  (matlab-cell-highlight)
  (add-hook 'post-command-hook #'matlab-cell-highlight nil t))

;;; Keymap
(defvar matlab-cell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [(control return)] 'matlab-shell-run-cell)
    ;; (define-key map [(control down)] 'matlab-cell-forward-cell)
    ;; (define-key map [(control up)] 'matlab-cell-backward-cell)
    map)
  "Key map for Matlab-Cell minor mode.")

(defalias 'matlab-cell-what-cell #'what-page)
(defalias 'matlab-cell-narrow-to-cell #'narrow-to-page)

;;; Minor mode:

;;;###autoload
(define-minor-mode matlab-cell-mode
  "Highlight MATLAB-like cells and navigate between them.
The minor-mode provides the following interactive navigation
functions: 
1. 'matlab-cell-forward-cell' : Move point to the beginning of the cell right below. 
2. 'matlab-cell-backward-cell' : Move point to the end of the cell right above.
3. 'matlab-cell-beginning-of-cell' : Move point to beginning of current cell. Return (point).
4. 'matlab-cell-end-of-cell' : Move point to end of current cell. Return (point).
5. 'matlab-move-cell-up' : Move the contents of the current cell \"up\", so that it occurs before the previous.
6. 'matlab-move-cell-down' : Move the contents of the current cell \"down\", so that it occurs after the next.
7. 'matlab-cell-run-till-point' : Run all the cells from beginning till previous cell.
"
  :keymap matlab-cell-mode-map
  (let ((arg `((,matlab-cell-cellbreak-regexp 1 'matlab-cell-cellbreak-face prepend))))
    (if (not matlab-cell-mode) ;; OFF
        (font-lock-remove-keywords nil arg)
      (make-local-variable 'page-delimiter)
      (setq page-delimiter matlab-cell-cellbreak-regexp)
      (font-lock-add-keywords nil arg)
      (when matlab-cell-highlight-cell
        (matlab-cell-setup-cellhighlight))))
  (font-lock-flush))

;;;###autoload
(defun matlab-cell-mode-enable ()
  (matlab-cell-mode 1))

;;;###autoload
(defun matlab-cell-mode-disable ()
  (matlab-cell-mode 0))

(provide 'matlab-cell)
;;; matlab-cell.el ends here
