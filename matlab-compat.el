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
  (let ((args (list completions)))
    (apply 'display-completion-list args)))

;;; Finding EmacsClient
(defun matlab-find-emacsclient ()
  "Locate the emacsclient corresponding for current Emacs.
Emacs binary is defined by variable `invocation-name' in variable
`invocation-directory'"
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
        (error "Unable to locate emacsclientw.exe.  It should be in %s" invocation-directory)))
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
  (defun string-replace (from-string to-string in-string)
    (let ((case-fold-search nil))
      (replace-regexp-in-string (regexp-quote from-string) to-string in-string t t))))

(provide 'matlab-compat)
;;; matlab-compat.el ends here

;; LocalWords:  Ludlam eludlam osboxes defun dolist lnk stringp setq emacsclient ec darwin nt
;; LocalWords:  emacsclientw usr fboundp
