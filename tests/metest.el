;;; metest.el --- Testing suite for MATLaB Emacs
;;
;; Copyright (C) 2019 Eric Ludlam
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
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Suite of unit tests that load in demo .m files and verify operation.

;;; Code:

(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (d (file-name-directory (directory-file-name d1)))
       )
  (defvar met-testfile-path d1
    "Location of test MATLAB code.")
  
  (add-to-list 'load-path (expand-file-name d) t))

(require 'matlab)

(defvar met-stringtest-files '("strings.m")
  "List of files for running string tests on.")

(defun metest-comment-string-syntax-test ()
  "Run a test to make sure string nd comment highlighting work."
  (dolist (F met-stringtest-files)
    (let ((buf (find-file-noselect (expand-file-name F met-testfile-path)))
	  (cnt 0))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(while (re-search-forward "#\\([csve]\\)#" nil t)
	  (goto-char (match-end 1))
	  (let ((md (match-data))
		(mc (match-string 1))
		(qd (matlab-cursor-comment-string-context)))
	    ;; Test 1 - what are we?
	    (unless (or (and (string= "v" mc) (eq 'charvector qd))
			(and (string= "s" mc) (eq 'string qd))
			(and (string= "c" mc) (eq 'comment qd))
			(and (string= "e" mc) (eq 'elipsis qd))
			)
	      (error "Syntax Test Failure @ line %d: Expected %s but found %S"
		     (line-number-at-pos)
		     (cond ((string= mc "v") "charvector")
			   ((string= mc "s") "string")
			   ((string= mc "c") "comment")
			   ((string= mc "e") "elipsis")
			   (t "unknown test token"))
		     qd))
	    ;; Test 2 - is match-data unchanged?
	    (unless (equal md (match-data))
	      (error "Syntax checking transmuted the match data"))
	    ;; Track
	    (setq cnt (1+ cnt))
	    ))
	(kill-buffer buf))
      (message "Comment and string syntax test: %d points passed" cnt)
      )))
  
       

(provide 'metest)

;;; metest.el ends here
