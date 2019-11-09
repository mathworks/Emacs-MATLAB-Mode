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
(require 'cedet-matlab)
(require 'semantic-matlab)

;; Enable semantic
(semantic-mode 1)
(matlab-cedet-setup)

(defun metest-all-syntax-tests ()
  "Run all the syntax tests in this file."
  (metest-comment-string-syntax-test)
  (metest-sexp-counting-test)
  (metest-sexp-traversal-test)
  (metest-indents-test)
  (metest-parse-test)
  )

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
	(message ">> Starting search loop in %S" (current-buffer))
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
      (message "<< Comment and string syntax test: %d points passed" cnt)
      ))
  (message ""))
  
(defvar met-sexptest-files '("expressions.m" "mclass.m" "indents.m")
  "List of files for running syntactic expression tests.")

(defun metest-sexp-counting-test ()
  "Run a test to make sure string nd comment highlighting work."
  (dolist (F met-sexptest-files)
    (let ((buf (find-file-noselect (expand-file-name F met-testfile-path)))
	  (cnt 0))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(message ">> Starting sexp counting loop in %S" (current-buffer))
	(while (re-search-forward "#\\([0-9]\\)#" nil t)
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (skip-chars-backward " %")	; skip comment part
	    (let* ((num (string-to-number (match-string 1))))
	      (save-restriction
		(narrow-to-region (point-at-bol) (point))
		(matlab-move-simple-sexp-internal (- num))
		(skip-chars-backward " \t;.=%")
		(if (not (eq (point) (point-min)))
		    (save-restriction
		      (widen)
		      (error "error at %d: Backward Sexp miscount tried %d, point %d, min %d"
			     (line-number-at-pos)
			     num (point) (point-at-bol))))
		(skip-chars-forward " \t;.=%")
		(matlab-move-simple-sexp-internal num)
		(skip-chars-forward " \t\n;.=%")
		(if (not (eq (point) (point-max)))
		    (save-restriction
		      (widen)
		      (error "Error at %d: Forward Sexp miscount tried %d, point %d, dest %d"
			     (line-number-at-pos)
			     num (point) (point-at-eol)))))
	      ))
	  (end-of-line)
	  (setq cnt (1+ cnt))))
      (kill-buffer buf)
      (message "<< Sexp counting syntax test: %d points passed" cnt)
      ))
  (message ""))

(defun metest-sexp-traversal-test ()
  "Run a test to make sure high level block navigation works."
  (dolist (F met-sexptest-files)
    (let ((buf (find-file-noselect (expand-file-name F met-testfile-path)))
	  (cnt 0))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(message ">> Starting sexp traversal loop in %S" (current-buffer))
	(while (re-search-forward ">>\\([0-9]+\\)" nil t)
	  (let* ((num (string-to-number (match-string 1)))
		 (num2 0)
		 (begin nil))
	    (skip-chars-forward " \n\t;%")
	    (setq begin (point))
	    (matlab-forward-sexp)
	    (skip-chars-forward " \n\t;%")
	    (if (not (looking-at "<<\\([0-9]+\\)"))
		(error "Error at %d: Failed to find matching test end token for %d"
		       (line-number-at-pos) num)
	      (setq num2 (string-to-number (match-string 1)))
	      (when (/= num num2)
		(error "Error at %d: Failed to match correct test token. Start is %d, end is %d"
		       (line-number-at-pos) num num2)))
	    (matlab-backward-sexp)
	    (when (/= (point) begin)
	      (error "Error at %d: Failed to reverse navigate sexp for %d"
		     (line-number-at-pos) num))
	    )
	  (end-of-line)
	  (setq cnt (1+ cnt))))
      (kill-buffer buf)
      (message "<< Sexp counting syntax test: %d points passed" cnt)
      ))
  (message ""))


(defvar met-indents-files '("indents.m" "mclass.m")
  "List of files for running syntactic indentation tests.")

(defun metest-indents-test ()
  "Run a test to make sure high level block navigation works."
  (dolist (F met-indents-files)
    (let ((buf (find-file-noselect (expand-file-name F met-testfile-path)))
	  (cnt 0))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	;; (indent-region (point-min) (point-max))
	(message ">> Starting indents loop in %S" (current-buffer))
	(while (re-search-forward "!!\\([0-9]+\\)" nil t)
	  (let* ((num (string-to-number (match-string 1)))
		 (calc (matlab-calc-indent))
		 (begin nil))
	    (when (not (= num calc))
	      (error "Error at %d: Indentation found is %d, expected %d"
		     (line-number-at-pos) calc num))
	    )
	  (end-of-line)
	  (setq cnt (1+ cnt))))
      (kill-buffer buf)
      (message "<< Indentation syntax test: %d points passed" cnt)
      ))
  (message ""))

(defvar met-parser-files '("mpclass.m")
  "List of files for running semantic parsing tests.")

(defun metest-parse-test ()
  "Run the semantic parsing test to make sure the parse works."
  
  (dolist (F met-parser-files)
    (let ((buf (find-file-noselect (expand-file-name F met-testfile-path)))
	  exp act
	  (cnt 0))
      (save-excursion
	(set-buffer buf)

	;; Prep buffer for test
	(semantic-idle-scheduler-mode -1)
	(semantic-clear-toplevel-cache)

	;; Do the test
	(goto-char (point-min))
	(message ">> Starting semantic parser test in %S" (current-buffer))

	(unless (re-search-forward "^%%\\s-*>>\\s-+SEMANTIC TEST" nil t)
	  (error "Semantic parser test: Failed to find test cookie."))
	(unless (re-search-forward "^%{[ \t\n]+\\(((\\)" nil t)
	  (error "Semantic parser test: Failed to find expected values."))
	(goto-char (match-beginning 1))
	(setq exp (read (buffer-substring (point)
					  (save-excursion (re-search-forward "%}" nil t)
							  (match-beginning 0)))))
	(setq act (semantic-fetch-tags))
	
	;; Compare the two lists ... simply.
	(while (and exp act)
	  (unless (metest-compare-tags (car exp) (car act))
	    (error "Expected tag %s, found %s" (semantic-format-tag-prototype (car exp))
		   (semantic-format-tag-prototype (car act))))
	  (setq exp (cdr exp) act (cdr act) cnt (1+ cnt))
	  )
	(when (or exp act)
	  (error "Found tags and expected tag lists differnet lengths.\nExpected Remains: %S\nActual Remains: %S"
		 exp act))
	
	)
      
      (message ">> Semantic parser test: %d tags matched" cnt))))


(defun metest-compare-tags (EXP ACT)
  "Return non-nil if EXP tag is similiar to ACT"
  (semantic-tag-similar-p EXP ACT :documentation)

  )

(provide 'metest)

;;; metest.el ends here
