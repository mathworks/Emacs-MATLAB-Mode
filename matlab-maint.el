;;; matlab-maint.el --- matlab mode maintainer utilities
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <eludlam@emacsvm>
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
;; Interface for maintainers of matlab mode.

(require 'matlab)
(require 'matlab-shell)

;;; Code:

;;; Minor Mode Definition
;;
(defvar matlab-maint-mode-map 
  (let ((km (make-sparse-keymap)))
    (define-key km [f8] 'matlab-maint-run-tests)
    (define-key km [f9] 'matlab-maint-compile-matlab-emacs)
    km)
  "Keymap used by matlab mode maintainers.")

(easy-menu-define
  matlab-maint-menu matlab-maint-mode-map "MATLAB Maintainer's Minor Mode"
  '("MMaint"
    ["Run Tests" matlab-maint-run-tests t]
    ))

;;;###autoload
(define-minor-mode matlab-maint-minor-mode
  "Minor mode for matlab-mode maintainrs."
  nil " MMaint" matlab-maint-mode-map
  )

;;;###autoload
(define-global-minor-mode global-matlab-maint-minor-mode
  matlab-maint-minor-mode
  (lambda ()
    "Should we turn on in this buffer? Only if in the project."
    (let ((dir (expand-file-name default-directory))
	  (ml (file-name-directory (expand-file-name (locate-library "matlab")))))
      (when (string= ml (substring dir 0 (min (length dir) (length ml))))
	(matlab-maint-minor-mode 1))))
  )


;;; Commands
;;
;; Helpful commands for maintainers.

(defun matlab-maint-compile-matlab-emacs ()
  "Run make for the matlab-emacs project."
  (interactive)
  (save-excursion
    (matlab-maint-set-buffer-to "matlab.el")
    (compile "make")))

(defun matlab-maint-run-tests (arg)
  "Run the tests for matlab mode.
With universal arg, ask for the code to be run with output tracking turned on."
  (interactive "P")
  (save-excursion
    (matlab-maint-set-buffer-to "tests/Makefile")
    (if arg
	;; Ask for dbug
	(compile "make TESTDEBUG=1")
      ;; No debugging
      (compile "make")))
  (switch-to-buffer "*compilation*")
  (delete-other-windows)
  (goto-char (point-max)))


(defun matlab-maint-set-buffer-to (file)
  "Set the current buffer to FILE found in matlab-mode's source.
Return the buffer."
  (let* ((ml (file-name-directory (locate-library "matlab")))
	 (newf (expand-file-name file ml)))
    (set-buffer (find-file-noselect newf))))

(provide 'matlab-maint)

;;; matlab-maint.el ends here
