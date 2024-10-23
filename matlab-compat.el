;;; matlab-compat.el --- Compatibility Code
;;
;; Copyright (C) 2024 Eric Ludlam
;;
;; Author: Eric Ludlam <eludlam@osboxes>
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
;; To support a wide range of different Emacs versions, these compat
;; functions will hide away the shims needed to work cross platform.

;;; Code:

;; Keymaps
(if (fboundp 'set-keymap-parent)
    (defalias 'matlab-set-keymap-parent 'set-keymap-parent)
      ;; 19.31 doesn't have set-keymap-parent

  (eval-when-compile
    (require 'comint))
  
  (defun matlab-set-keymap-parent (keymap parent)
    "Set KEYMAP's parent to be PARENT."
    (nconc keymap comint-mode-map)))

;; String trim
(if (locate-library "subr-x")
    (progn
      (require 'subr-x)
      (defalias 'matlab-string-trim 'string-trim)
      )
  
  (defsubst matlab-string-trim (string &optional regexp)
    "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
    (let ((out string)
	  (regexp_ (or regexp "[ \t\n\r]+")))
      
      (when (string-match (concat "\\`\\(?:" regexp_ "\\)") out)
	(setq out (substring out (match-end 0))))
      
      (when (string-match (concat "\\(?:" regexp_ "\\)\\'") out)
	(setq out (substring out 0 (match-beginning 0))))
      
      out))
  )

;; OBARRAYS
(if (fboundp 'obarray-make)
    (defalias 'matlab-obarray-make 'obarray-make)
  (defun matlab-obarray-make (sz) (make-vector sz 0)))

;; Finding executables
(defun matlab-find-executable-directory (program)
  "Find the executable PROGRAM on the exec path, following any links.
Return the base directory it is in."
  (let ((dir nil))
    
    (dolist (P exec-path)
      (let ((nm (expand-file-name program P)))
        (when (and (file-exists-p nm) (file-executable-p nm))
          (let* ((fa (file-attributes nm))
                 (lnk (car fa)))
            ;; The car is t for a directory, a string for a link, nil otherwise
            (if (stringp lnk)
                ;; We have a link - use that as our directory.
                (setq dir (file-name-directory lnk))
              ;; No link - just use this path.
              (setq dir P)))
          )))
    dir))

;; Completion Tools
(defun matlab-display-completion-list (completions common-substring)
  "Method for displaying COMPLETIONS with a COMMON-SUBSTRING."
  ;; In emacs 24.4 the common-substring is no longer needed.
  (let ((args (if (or (< emacs-major-version 24)
                      (and (= emacs-major-version 24) (< emacs-minor-version 4)))
                  (list completions common-substring)
                (list completions))))
    (apply 'display-completion-list args)))

;; Font lock
(require 'font-lock)
(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-fontify-buffer))

;; CEDET compat if CEDET isn't around
(condition-case nil
    (progn
      (require 'pulse)
      )
  (error
   (defun pulse-momentary-highlight-region (start end &optional face)
     "Compat impl of pulse command." nil)))

;; EIEIO compatibility
(condition-case nil
    (progn
      (require 'eieio)
      (unless (fboundp 'cl-defgeneric)
	;; We are in an antique Emacs that uses the old eieio.
	(defalias 'cl-defmethod 'defmethod)
	)
      (unless (fboundp 'cl-call-next-method)
	;; We are in an antique Emacs that uses the old eieio.
	(defalias 'cl-call-next-method 'call-next-method)
	)
      )
  (error (message "EIEIO not available.  Only MATLAB editing enabled.")))

;;; Finding EmacsClient
(defun matlab-find-emacsclient ()
  "Locate the emacsclient correspoinding to the current emacs
binary defined by `invocation-name' in `invocation-directory'"
  (let ((ec "emacsclient"))
    (cond
     ;; Mac
     ((equal system-type 'darwin)
      (if (file-exists-p (concat invocation-directory "emacsclient")) ;; running the default emacs?
          (setq ec (concat invocation-directory "emacsclient"))
        ;; On Mac, one can install into
        ;;    /Applications/Emacs.app/Contents/MacOS/Emacs
        ;;    /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
        (if (file-exists-p (concat invocation-directory "bin/emacsclient"))
            (setq ec (concat invocation-directory "bin/emacsclient")))))
     ;; Windows
     ((equal system-type 'windows-nt)
      (if (file-exists-p (concat invocation-directory "emacsclientw.exe"))
          (setq ec (concat invocation-directory "emacsclientw.exe"))
        (error "unable to locate emacsclientw.exe. It should be in %s" invocation-directory)))
     ;; Linux or other UNIX system
     (t
      ;; Debian 9 can be setup to have:
      ;;   /usr/bin/emacs
      ;;   /usr/bin/emacsclient
      ;;   /usr/bin/emacs24
      ;;   /usr/bin/emacsclient.emacs24
      ;;   /usr/bin/emacs25
      ;;   /usr/bin/emacsclient.emacs25
      (if (and (equal invocation-name "emacs")
                 (file-exists-p (concat invocation-directory "emacsclient")))
          (setq ec (concat invocation-directory "emacsclient"))
        (if (file-exists-p (concat invocation-directory "emacsclient." invocation-name))
            (setq ec (concat invocation-directory "emacsclient." invocation-name))))))
    ;; Return, ec, the emacsclient to use
    ec
    ))

(when (not (fboundp 'string-replace)) ;; string-replace appeared in Emacs 28
  (defun string-replace (fromstring tostring instring)
    (let ((case-fold-search nil))
      (replace-regexp-in-string (regexp-quote fromstring) tostring instring t t))))

(provide 'matlab-compat)

;;; matlab-compat.el ends here

;; LocalWords:  el Ludlam eludlam osboxes Ee progn defalias fboundp itimer
;; LocalWords:  defun boundp defvaralias bol eol defmacro Keymaps keymap comint
;; LocalWords:  KEYMAP's nconc dolist nm lnk setq
