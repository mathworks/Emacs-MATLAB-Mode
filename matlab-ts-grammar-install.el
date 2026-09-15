;;; matlab-ts-grammar-install.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses>.

;;; Commentary:
;;
;; Download
;;   ~/.emacs.d/tree-sitter/libtree-sitter-matlab.so (or .dll or .dylib)
;; from
;;   https://github.com/mathworks/Emacs-MATLAB-Mode/matlab-ts-bin/

;;; Code:

(require 'files)
(require 'org-src)
(require 'url)

(defvar matlab--ts-grammar-release "matlab-ts-abi14-20260826-f03d034") ;; v1.3.1

(defun matlab--ts-grammar-check-version ()
  "Validate Emacs and grammar version are okay, error if not."
  (when (< emacs-major-version 30)
    (user-error "Unsupported Emacs version, %d" emacs-major-version))

  (let ((ts-abi-min-compatible-ver (treesit-library-abi-version t))
        (release-abi-ver (if (string-match "\\`matlab-ts-abi\\([0-9]+\\)-"
                                           matlab--ts-grammar-release)
                             (string-to-number (match-string 1 matlab--ts-grammar-release))
                           (error "Unexpected matlab--ts-grammar-release %s"
                                  matlab--ts-grammar-release))))
    (when (> ts-abi-min-compatible-ver release-abi-ver)
      (user-error
       "Emacs oldest compatible treesit ABI version is %d and the matlab-ts grammar version is %d,\
 which indicates a new build is required for matlab-ts" ts-abi-min-compatible-ver release-abi-ver))))

(defun matlab--ts-get-grammar-branch-and-install-dir (prompt)
  "Get branch and tree-sitter install directory.
When PROMPT, prompt for these."

  (let* ((branch-default "default")
         (branch (if prompt
                     (let ((ans ""))
                       (while (string= ans "")
                         (setq ans (string-trim (read-string "Branch: " branch-default))))
                       ans)
                   branch-default))
         (dir-default (concat user-emacs-directory "tree-sitter/"))
         (dir (if prompt
                  (let ((ans ""))
                    (while (string= ans "")
                      (setq ans (read-directory-name "Download directory: " dir-default)))
                    ans)
                dir-default)))

    (if (string= dir dir-default)
        (when (not (file-directory-p dir))
          (make-directory dir t))
      (when (not (file-directory-p dir))
        (user-error "%s is not an existing directory" dir)))

    (setq dir (file-name-as-directory (file-truename dir)))

    (cons branch dir)))

(defun matlab--ts-grammar-arch-and-shared-lib ()
  "Return ARCH/libtree-sitter-matlab.SLIB_EXT.
ARCH is the same as the MATLAB computer('arch') command result."
  (let ((result (pcase system-type
                  ('darwin (cond
                            ((string-prefix-p "aarch64" system-configuration)
                             "maca64/libtree-sitter-matlab.dylib")
                            ((string-prefix-p "x86_64" system-configuration)
                             "maca64/libtree-sitter-matlab.dylib")))
                  ('gnu/linux (cond
                               ((string-prefix-p "x86_64" system-configuration)
                                "glnxa64/libtree-sitter-matlab.so")))
                  ('windows-nt "win64/libtree-sitter-matlab.dll"))))
    (when (not result)
      (user-error "Unsupported system-type %s, system-configuration %s" system-type
                  system-configuration))
    result))

(defun matlab--files-equal-p (file1 file2)
  "Return t if the contents of FILE1 and FILE2 are identical, nil otherwise."
  (let ((content1 (with-temp-buffer
                    (insert-file-contents-literally file1)
                    (buffer-string)))
        (content2 (with-temp-buffer
                    (insert-file-contents-literally file2)
                    (buffer-string))))
    (equal content1 content2)))

(defun matlab--ts-grammar-setup ()
  "Tell Emacs to use the matlab tree-sitter grammar shared library."

  ;; 1. Add '(matlab-mode . matlab-ts-mode) to `major-mode-remap-alist' if needed.
  (let* ((remap-alist major-mode-remap-alist)
         (value (alist-get 'matlab-mode remap-alist)))

    (when (and value
               (not (eq value 'matlab-ts-mode))) ;; bad value?
      (setq remap-alist (delq (assoc 'matlab-mode remap-alist) remap-alist))
      (setq value nil))

    (when (not value)
      (push '(matlab-mode . matlab-ts-mode) remap-alist)
      (message "Updating 'major-mode-remap-alist to contain '(matlab-mode . matlab-ts-mode)")
      (customize-save-variable 'major-mode-remap-alist remap-alist)))

  ;; 2. Add ("matlab" . "matlab-ts") to org-src-lang-modes
  (let* ((lang-modes org-src-lang-modes)
         (value (alist-get "matlab" lang-modes nil nil 'equal)))

    (when (and value
               (not (string= value "matlab-ts-mode"))) ;; bad value?
      (setq lang-modes (delq (assoc "matlab" lang-modes) lang-modes))
      (setq value nil))

    (when (not value)
      (push '("matlab" . "matlab-ts") lang-modes)
      (message "Updating 'major-mode-remap-alist to contain '(\"matlab\" . \"matlab-ts\")")
      (customize-save-variable 'org-src-lang-modes lang-modes))))

;;;###autoload
(defun matlab-ts-grammar-install (arg)
  "Download the matlab tree-sitter grammar shared library.

Download libtree-sitter-matlab.SLIB-EXT (SLIB-EXT = so on Linux, dll on
Windows, or dylib on Mac) from the matlab-ts-bin directory in
https://github.com/mathworks/Emacs-MATLAB-Mode

The matlab tree-sitter grammar shared library is required for
`matlab-ts-mode'.

With prefix ARG, prompt for

 - DIR to place libtree-sitter-matlab.SLIB.EXT.  This defaults to the
   tree-sitter subdirectory of `user-emacs-directory' which is typically
   ~/.emacs.d/tree-sitter/.

 - BRANCH in https://github.com/mathworks/Emacs-MATLAB-Mode.  This defaults
   to the \"default\" branch.

When libtree-sitter-matlab.SLIB-EXT already exists on your system,

 - If it is up-to-date, it will not be touched and a message is displayed
   that it is up-to-date.

 - If it is out-of-date, it will be updated and you will be prompted
   to restart Emacs so the new library can be used."

  (interactive "P")

  (matlab--ts-grammar-check-version)

  (let* ((branch-and-dir (matlab--ts-get-grammar-branch-and-install-dir arg))
         (branch (car branch-and-dir))
         (dir (cdr branch-and-dir)))

    (let* ((download-url (concat "https://raw.githubusercontent.com/mathworks/Emacs-MATLAB-Mode/"
                                 branch "/matlab-ts-bin/" matlab--ts-grammar-release "/"
                                 (matlab--ts-grammar-arch-and-shared-lib)))
           (grammar-slib (concat dir (file-name-nondirectory download-url)))
           (grammar-slib-tmp (concat grammar-slib ".tmp")))

      (when (y-or-n-p (format "Download %s\nto %s? " download-url grammar-slib))

        (url-copy-file download-url grammar-slib-tmp t)

        (let (update-type)
          (if (not (file-exists-p grammar-slib))
              (setq update-type 'downloaded)
            (if (matlab--files-equal-p grammar-slib grammar-slib-tmp)
                (message "%s is already up-to-date and thus was not updated" grammar-slib)
              (setq update-type 'downloaded-and-updated)))

          (if (not update-type)
              (delete-file grammar-slib-tmp)
            ;; Update grammar-slib
            (condition-case err
                (rename-file grammar-slib-tmp grammar-slib t)
              (error
               (error "\
Failed to update %s
  %s
This can be do the grammar library being used.
Try exiting Emacs and re-running the install before loading any *.m files"
                      grammar-slib (error-message-string err))))

            ;; Tell Emacs to use matlab-ts-mode for *.m files and matlab org-mode source blocks
            (matlab--ts-grammar-setup)

            (pcase update-type
              ('downloaded
               (message "Downloaded %s" grammar-slib))
              ('downloaded-and-updated
               (let ((prompt (concat "Downloaded and updated " grammar-slib "\n"
                                     "If an older grammar library was in use, "
                                     "Emacs must be restarted.\n"
                                     "Exit Emacs? ")))
                 (when (y-or-n-p prompt)
                   (save-buffers-kill-terminal))))
              (_
               (error "Assert - invalid update-type %S" update-type)))))))))

(provide 'matlab-ts-grammar-install)
;;; matlab-ts-grammar-install.el ends here

;; LocalWords:  libtree dylib defun SLIB pcase darwin aarch maca linux nt buf cadr alist YYYYMMDD
;; LocalWords:  SHA setq slib truename nondirectory tmp delq lang repeat:nil abi ce da treesit cdr
