;;; metest.el --- Testing suite for MATLAB Emacs
;;
;; Copyright (C) 2019-2023 Eric Ludlam
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

(defvar met-testfile-path) ; quiet compiler

(require 'matlab-load)
(require 'matlab)
(require 'matlab-complete)

(defun metest-all-syntax-tests ()
  "Run all the syntax tests in this file."
  (setq debug-on-error t)
  (matlab-scan-stat-reset) ;; Enable scanner statistics logging.
  
  (metest-log-init)

  (setq-default matlab-indent-function-body 'guess) ;; Force the guess system to be exercised.
  (metest-run 'metest-end-detect-test)
  (setq-default matlab-indent-function-body 'MathWorks-Standard) ;; put it back

  (metest-run 'metest-comment-string-syntax-test)
  (metest-run 'metest-fontlock-test)
  (metest-run 'metest-sexp-counting-test)
  (metest-run 'metest-sexp-traversal-test)

  ;; Randomize indentation first before indenting
  ;; to force the indenter to make changes and give
  ;; the cahce and performance a harder problem.
  (metest-indents-randomize-files)
  (metest-run 'metest-indents-test)

  ;; Parsing and completion are high level tools
  (metest-run 'metest-complete-test)

  (metest-log-report (metest-log-write))

  (matlab-scan-stats-print)
  )

(defun metest-run (test)
  "Run and time TEST."
  (let* ((config (symbol-value test))
	 (name (if (stringp config) config (car config)))
	 (files (or (cdr-safe config) '("")))
	 (strlen (apply 'max (mapcar 'length files))))
    (message ">> Starting %s loop on %S" name files)
    (dolist (F files)
      (princ (format (concat "<< %s %-" (number-to-string strlen) "s ") name F) 'external-debugging-output)
      (let ((old debug-on-error)
	    (out (progn (setq debug-on-error nil)
			(metest-timeit test F))))
	(setq debug-on-error old)
	(when (listp out)
	  (princ (format "passed: %s  %.2f s\n" (cdr out) (car out)) 'external-debugging-output)
	  )
	))
    (message "")))

(defvar metest-test-error nil)
(defmacro metest-condition-case-error-msg (&rest forms)
  "Run FORMS, capturing any errors and associating with (point)."
  (declare (indent 0) (debug t))
  `(condition-case err
       ,@forms
     (error (cond (metest-test-error (error (car (cdr err))))
		  (t (metest-error "Lisp: %s" (error-message-string err))))
	    0)
     ))

(defvar met-end-detect-files '("empty.m" "stringtest.m" "mfuncnoend.m" "mfuncnoendblock.m" "mfuncends.m" "mclass.m" "mfuncspacey.m" "mfuncnoendindent.m" "mfuncnofuncindent.m")
  "List of files for running end detection tests on.")

(defvar metest-end-detect-test (cons "END detection" met-end-detect-files))
(defun metest-end-detect-test (F)
  "Run a test to make sure we correctly detect the state of managing 'end'."
  (let ((buf (metest-find-file F))
	(ret nil)
	(cnt 0))
    (with-current-buffer buf
      (goto-char (point-min))
      ;;(message ">> Checking END detection in %S" (current-buffer))
      (if (re-search-forward "%%%\\s-*\\(\\w+\\)\\s-+\\(\\w+\\)\\s-+\\(\\w+\\)$" nil t)
	  (let ((st-expect (intern (match-string-no-properties 1)))
		(end-expect (intern (match-string-no-properties 2)))
		(indent-expect (intern (match-string-no-properties 3)))
		(st-actual (matlab-guess-script-type))
		(end-actual (matlab-do-functions-have-end-p))
		(indent-actual (matlab-indent-function-body-p))
		)
	    (unless (eq st-actual st-expect)
	      (metest-error "Script type detection failure: Expected %s but found %s"
			    st-expect st-actual))
	    (unless (eq end-actual end-expect)
	      (metest-error "Script end detection failure: Expected %s but found %s"
			    end-expect end-actual))
	    (unless (eq indent-actual indent-expect)
	      (metest-error "Script indent detection failure: Expected %s but found %s"
			    indent-expect indent-actual))
	      
	    (setq ret (list "script[" st-actual "]  end[" end-actual "]  indent-p[" indent-actual "]"))
	    ;;(message "<< Script type and end detection passed: %s, %s" st-actual end-actual)
	    )
	;; No expected values found in the file.
	(metest-error "Test file did not include expected script-type cookie")
	))
    ret))

(defvar met-stringtest-files '("stringtest.m")
  "List of files for running string tests on.")

(defvar metest-comment-string-syntax-test (cons "string/comment detection" met-stringtest-files))
(defun metest-comment-string-syntax-test (F)
  "Run a test to make sure string nd comment highlighting work."
    (let ((buf (metest-find-file F))
	  (cnt 0)
	  (noninteractive nil) ;; fake out font lock
	  )
      (with-current-buffer buf
	(goto-char (point-min))

	(let ((md (match-data)))
	  ;; Force font lock to throw catchable errors.
	  (font-lock-mode 1)
	  (font-lock-flush (point-min) (point-max))
	  (font-lock-ensure (point-min) (point-max))
	  (font-lock-fontify-region (point-min) (point-max))

	  ;; FL test 1: make sure font lock is on and match data didn't change.
	  (unless font-lock-mode
	    (metest-error "Font Lock failed to turn on."))
	  ;;(unless (equal md (match-data))
	  ;;  (metest-error "Font Locking transmuted the match data"))
	  (when (not (get-text-property 2 'fontified))
	    (metest-error "Font Lock Failure: can't run test because font lock failed to fontify region."))
	  )
	    
	
	;;(message ">> Starting string/comment detect loop in %S" (current-buffer))
	(while (re-search-forward "#\\([cCisSvVebdr]\\)#" nil t)
	  (let* ((md  (match-data))
		 (pt  (match-end 1))
		 (mc  (match-string-no-properties 1))
		 (fnt (get-text-property pt 'face))
		 (lv1 (matlab-compute-line-context 1))
		 (bc  (metest-condition-case-error-msg (matlab-line-block-comment-start lv1)))
		 (qd  (metest-condition-case-error-msg (matlab-cursor-comment-string-context)))
		 )
	    (goto-char pt)
	    
	    ;; Test 1 - what are we?
	    (unless (or (and (string= "b" mc) (and bc (eq 'comment qd)))
			(and (string= "v" mc) (eq 'charvector qd))
			(and (string= "V" mc) (eq 'charvector qd))
			(and (string= "s" mc) (eq 'string qd))
			(and (string= "S" mc) (eq 'string qd))
			(and (string= "c" mc) (eq 'comment qd))
			(and (string= "C" mc) (eq 'comment qd))
			(and (string= "i" mc) (eq 'comment qd))
			(and (string= "e" mc) (eq 'ellipsis qd))
			(and (string= "d" mc) (eq 'commanddual qd))
			(and (string= "r" mc) (eq nil qd))
			)
	      (metest-error "Syntax Test Failure @ char %d: Expected %s but found %S"
			    pt
			    (cond ((string= mc "b") "block comment")
				  ((string= mc "v") "charvector")
				  ((string= mc "V") "charvector")
				  ((string= mc "s") "string")
				  ((string= mc "S") "string")
				  ((string= mc "c") "comment")
				  ((string= mc "C") "comment")
				  ((string= mc "i") "comment")
				  ((string= mc "e") "ellipsis")
				  ((string= mc "d") "commanddual")
				  ((string= mc "r") "normal code")
				  (t "unknown test token"))
			    qd))
	    ;; Test 2 - is match-data unchanged?
	    (unless (equal md (match-data))
	      (metest-error "Syntax checking transmuted the match data"))

	    ;; FL test 2 - Is the matched location fontified correctly?
	    (when (consp fnt) (setq fnt (car fnt)))
	    (unless (or (and (string= "b" mc) (eq fnt 'font-lock-comment-face))
			(and (string= "v" mc) (eq fnt 'font-lock-string-face))
			(and (string= "V" mc) (eq fnt 'matlab-unterminated-string-face))
			(and (string= "s" mc) (eq fnt 'font-lock-string-face))
			(and (string= "S" mc) (eq fnt 'matlab-unterminated-string-face))
			(and (string= "c" mc) (eq fnt 'font-lock-comment-face))
			(and (string= "C" mc) (eq fnt 'matlab-cellbreak-face))
			(and (string= "i" mc) (eq fnt 'matlab-ignored-comment-face))
			(and (string= "e" mc) (eq fnt 'font-lock-comment-face))
			(and (string= "d" mc) (eq fnt 'matlab-commanddual-string-face))
			(and (string= "r" mc) (eq fnt nil))
			)
	      (metest-error "Font Lock Failure @ char %d: Expected %s but found %S"
			    pt
			    (cond ((string= mc "b") "comment face")
				  ((string= mc "v") "string face")
				  ((string= mc "V") "unterminated string face")
				  ((string= mc "s") "string face")
				  ((string= mc "S") "unterminated string face")
				  ((string= mc "c") "comment face")
				  ((string= mc "C") "cellbreak face")
				  ((string= mc "i") "ignored comment face")
				  ((string= mc "e") "comment face")
				  ((string= mc "d") "commanddual string face")
				  ((string= mc "r") "regular code / no face")
				  (t "unknown test token"))
			    (get-text-property pt 'face)))
	    ;; Track
	    (setq cnt (1+ cnt))
	    ))
	(kill-buffer buf))
      
      (list cnt "tests")))
  
(defvar met-sexptest-files '("expressions.m" "mclass.m" "blocks.m")
  "List of files for running syntactic expression tests.")

(defvar metest-sexp-counting-test (cons "sexp counting" met-sexptest-files))
(defun metest-sexp-counting-test (F)
  "Run a test to make sure string and comment highlighting work."
    (let ((buf (metest-find-file F))
	  (cnt 0))
      (with-current-buffer buf
	(goto-char (point-min))
	;;(message ">> Starting sexp counting loop in %S" (current-buffer))
	(while (re-search-forward "#\\([0-9]\\)#" nil t)
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (skip-chars-backward " %")	; skip comment part
	    (let* ((num (string-to-number (match-string 1))))
	      (save-restriction
		(narrow-to-region (point-at-bol) (point))
		(metest-condition-case-error-msg
		 (matlab-move-simple-sexp-internal (- num)))
		(skip-chars-backward " \t;.=%")
		(if (not (eq (point) (point-min)))
		    (save-restriction
		      (widen)
		      (metest-error "Backward Sexp miscount tried %d, point %d, min %d"
			num (point) (point-at-bol))))
		(skip-chars-forward " \t;.=%")
		(matlab-move-simple-sexp-internal num)
		(skip-chars-forward " \t\n;.=%")
		(if (not (eq (point) (point-max)))
		    (save-restriction
		      (widen)
		      (metest-error "Forward Sexp miscount tried %d, point %d, dest %d"
			num (point) (point-at-eol)))))
	      ))
	  (end-of-line)
	  (setq cnt (1+ cnt))))
      (kill-buffer buf)
      (list cnt "tests")))

(defvar metest-sexp-traversal-test (cons "sexp block traversal" met-sexptest-files))
(defun metest-sexp-traversal-test (F)
  "Run a test to make sure high level block navigation works."
    (let ((buf (metest-find-file F))
	  (cnt 0))
      (with-current-buffer buf
	(goto-char (point-min))
	;;(message ">> Starting sexp traversal loop in %S" (current-buffer))
	(while (re-search-forward ">>\\([0-9]+\\)" nil t)
	  (let* ((num (string-to-number (match-string 1)))
		 (num2 0)
		 (begin nil))
	    (skip-chars-forward " \n\t;%")
	    (setq begin (point))
	    (metest-condition-case-error-msg (matlab--scan-block-forward))
	    (save-excursion
	      (skip-chars-forward " \n\t;%")
	      (if (not (looking-at "<<\\([0-9]+\\)"))
		  (metest-error "Failed to find matching test end token for %d"
				num)
		(setq num2 (string-to-number (match-string 1)))
		(when (/= num num2)
		  (metest-error "Failed to match correct test token. Start is %d, end is %d"
				num num2))))
	    (metest-condition-case-error-msg (matlab--scan-block-backward))
	    (when (/= (point) begin)
	      (metest-error "Failed to reverse navigate sexp for %d"
			    num))
	    )
	  (end-of-line)
	  (setq cnt (1+ cnt))))
      (kill-buffer buf)
      (list cnt "test")))


(defvar met-indents-files '("indents.m" "continuations.m" "mclass.m" "blocks.m" "mfuncends.m" "mfuncnoendblock.m" "mclass_cont.m" "mfuncnofuncindent.m")
  "List of files for running syntactic indentation tests.")

(defun metest-indents-randomize-files ()
  "Randomize the indentation in the inents test files."
  (interactive)
  (message "<< Flattening indentation ...")
  (let ((matlab-scan-temporal-cache nil)) ;; disable cache for file load
    (dolist (F met-indents-files)
      (with-current-buffer (metest-find-file F)
	(goto-char (point-min))
	(while (not (eobp))
	  (beginning-of-line)
	  (if (looking-at "^\\s-*$")
	      (matlab--change-indentation 0)
	    (matlab--change-indentation 3)) ;;(random 13)?
	  (forward-line 1)
	  )
	;; And don't delete - leave it to find for the next test.
	;; but we do want to restart the mode and force a re-guess of the file type.
	(matlab-mode)
	))))

(defvar metest-indents-test (cons "indenting" met-indents-files))
(defvar metest-indent-counts 0)
(defun metest-indents-test (F)
  "Run a test to make sure high level block navigation works."
  (with-current-buffer (metest-find-file F)
    (goto-char (point-min))
    (let ((metest-indent-counts 0)
	  (matlab--change-indentation-override #'metest-indents-test-hook-fcn))
      (metest-condition-case-error-msg
       (matlab-indent-region (point-min) (point-max) nil t))
      (kill-buffer (current-buffer))
      (list metest-indent-counts "tests"))))

(defun metest-indents-test-hook-fcn (indent)
  "Hook fcn used to capture indents from `indent-region'."
  (save-excursion
    (beginning-of-line)

    (when (re-search-forward "!!\\([0-9]+\\)" (point-at-eol) t)
      (let ((num (string-to-number (match-string 1))))
	(setq metest-indent-counts (1+ metest-indent-counts))
	(when (not (eq num indent))
	  (metest-error "Indentation computed is %s, expected %s"
			indent num))))

    ;; Now do the indent in case a bad indent will trigger a bug later.
    (matlab--change-indentation indent)
    ))

(defun metest-compare-tags (EXP ACT)
  "Return non-nil if EXP tag is similiar to ACT"
  (semantic-tag-similar-p EXP ACT :documentation)
  )

(defconst met-kw-font-alist '(( "kw" . font-lock-keyword-face )
			      ( "ty" . font-lock-type-face )
			      ( "fn" . font-lock-function-name-face )
			      ( "vn" . font-lock-variable-name-face )
			      ( "vc" . (font-lock-variable-name-face
					matlab-cross-function-variable-face) )
			      ( "cn" . font-lock-constant-face )
			      ( "co" . font-lock-comment-face )
			      ( "st" . font-lock-string-face )
			      ( "bi" . font-lock-builtin-face )

			      ( "cb" . matlab-cellbreak-face )
			      ( "ig" . matlab-ignored-comment-face )
			      ( "pr" . matlab-pragma-face )
			      ( "cd" . matlab-commanddual-string-face )
			      ( "us" . matlab-unterminated-string-face )
			      ( "ma" . matlab-math-face )
			      ( "si" . matlab-simulink-keyword-face )

			      ( "bo" . bold )
			      ( "df" . nil )
			      )
  "List of testing keywords and associated faces.")


(defvar met-complete-files '("complete.m")
  "List of files for running font completion tests.")

(defvar met-complete-tools '((var . matlab-find-recent-variable)
			     (fcn . matlab-find-user-functions)
			     )
  "List of tools that generate completions.")

(defvar metest-complete-test (cons "completion" met-complete-files))
(defun metest-complete-test (F)
  "Test the completion tools in matlab-complete.el"
    (let ((buf (metest-find-file F))
	  exp act
	  (cnt 0))
      (with-current-buffer buf
	(goto-char (point-min))

	(while (re-search-forward "%\\s-*@@" nil t)
	  (setq exp (read (buffer-substring-no-properties (point) (scan-sexps (point) 1))))
	  ;; Move to end of previous line, and try to do a complete
	  (matlab-with-context-line (matlab-previous-code-line (matlab-compute-line-context 2))
	    (end-of-line)
	    (let* ((prefix (buffer-substring-no-properties
			    (save-excursion (forward-word -1) (point))
			    (point)))
		   (sem (matlab-lattr-semantics prefix))
		   )
	      ;; Did we get the expected semantics of this location?
	      (when (not (eq sem (car exp)))
		(metest-error "Completion Semantic Missmatch: Expected %s but found %s" (car exp) sem))

	      (let* ((expR (nthcdr 2 exp))
		     (fcn (assoc (nth 1 exp) met-complete-tools))
		     (act (funcall (cdr fcn) prefix)))
		(when (not (equal act expR))
		  (metest-error "Completion Missmatch: Expected %S but found %S using function %S"
				expR act fcn))
		)
	      ))
	  (setq cnt (1+ cnt))
	  ;; Skip this match, find the next.
	  (end-of-line)))
      (list cnt "tests")))


(defvar met-fontlock-files '("fontlock.m" "mclass.m" "blocks.m")
  "List of files for running font lock tests.")

(defvar metest-fontlock-test (cons "font lock" met-fontlock-files))
(defun metest-fontlock-test (F)
  "Run the semantic parsing test to make sure the parse works."
    (let ((buf (metest-find-file F))
	  (noninteractive nil) ;; fake out font lock
	  (cnt 0) (fntcnt 0))
      (with-current-buffer buf

	(goto-char (point-min))

	(let ((md (match-data)))
	  ;; Force font lock to throw catchable errors.
	  (font-lock-mode 1)
	  (font-lock-flush (point-min) (point-max))
	  (font-lock-ensure (point-min) (point-max))
	  (font-lock-fontify-region (point-min) (point-max))

	  ;; FL test 1: make sure font lock is on and match data didn't change.
	  (unless font-lock-mode
	    (metest-error "Font Lock failed to turn on."))
	  ;;(unless (equal md (match-data))
	  ;;  (metest-error "Font Locking transmuted the match data"))
	  (when (not (get-text-property 2 'fontified))
	    (metest-error "Font Lock Failure: can't run test because font lock failed to fontify region."))
	  )

	;; Lines that start with %^ comments are FL keyword test features.
	;; Find the line, then look for every ^ and find it's column and match
	;; to previous line's column.
	(while (re-search-forward "^\\s-*%\\(?: \\$\\$\\$\\)?\\^" nil t)
	  (let ((next (point-at-eol))
		(prevstart (save-excursion (forward-line -1) (point-at-bol)))
		)
	    (while (re-search-forward "\\^\\(\\w\\w\\)\\>" (point-at-eol) t)
	      (let* ((col (- (match-beginning 0) (point-at-bol)))
		     (fk  (match-string-no-properties 1))
		     (pt (+ prevstart col))
		     (fnt (get-text-property pt 'face))
		     (fnt1 (if (consp fnt) (car fnt) fnt))
		     (fnt2 (if (consp fnt) (nth 1 fnt) nil))
		     (exp (cdr (assoc fk met-kw-font-alist))))

		(cond
		 ((consp exp)
		  (when (not (eq (car exp) fnt1))
		    (metest-error "Bad font layer 1 found @ col %d: Expected %S but found %S"
				  col (car exp) fnt1))
		  (when (not (eq (nth 1 exp) fnt2))
		    (metest-error "Bad font layer 2 found @ col %d: Expected %S but found %S"
				  col (nth 1 exp) fnt2)))
		 (t
		  (when (not (eq exp fnt1))
		    (metest-error "Bad font found @ col %d: Expected %S but found %S"
				  col exp fnt))))

		(setq fntcnt (1+ fntcnt))
		))
	    (goto-char next)
	    (setq cnt (1+ cnt))))

	(list cnt "lines with " fntcnt "fonts tested"))))

;;; UTILS
;;

(defun metest-find-file (file)
  "Read FILE into a buffer and return it.
Do error checking to provide easier debugging."
  (let ((F (expand-file-name file met-testfile-path)))
    (unless (file-exists-p F)
      (error "Test file %s does not exist in %s" file met-testfile-path))
    (find-file-noselect F)))

(defvar metest-error-context-lines 4)
(defun metest-error (&rest args)
  "Produce an err with standardized file/line prefix."
  (declare (indent 1))
  (let* ((lineno (line-number-at-pos))
	 (fname (file-name-nondirectory (buffer-file-name)))
	 (pre (format "\n%s:%d: Error: " fname lineno))
	 (post (apply 'format args))
	 (prelines (min lineno metest-error-context-lines)))
    (message "\n--vv buffer snip: %s vv--" fname)
    (save-excursion
      (forward-line (- prelines))
      (while (> prelines 0)
	(message "|%s" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	(forward-line 1)
	(setq prelines (1- prelines)))
      (message ">%s" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
      (forward-line 1)
      (while (and (> metest-error-context-lines prelines) (not (eobp)))
	(message "|%s" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	(forward-line 1)
	(setq prelines (1+ prelines))))
    (message "---^^ buffer snip ^^---")
    (setq metest-test-error t)
    (error (concat pre post))))

;;; Logging prormance data for the tests
;;
(defvar metest-log-file "metest_timing_log.dat"
  "File to store timing data to.")

(defvar metest-time-log nil
  "Data stored for each run.")

(defun metest-log-init ()
  "Init the log file and data variable."
  (setq metest-time-log nil)
  )

(defun metest-shorten (sym)
  "Convert SYM into a column header."
  (let ((str (symbol-name sym)))
    (substring str 7 -5)))

(defun metest-log-write ()
  "Write dta into our log file."
  (save-current-buffer
    (set-buffer (find-file-noselect metest-log-file))
    (let ((LOG (reverse metest-time-log)))
      (when (= (point-min) (point-max))
	;; Initialize the new buffer
	(insert "Time\t")
	(insert (mapconcat (lambda (log) (metest-shorten (car log))) LOG "\t")))
      ;; Insert our measurements
      (goto-char (point-max))
      (newline)
      (insert (format-time-string "\"%Y/%m/%d %H:%M\"\t" (current-time)))
      (insert (mapconcat (lambda (log2) (format "%f" (cdr log2))) LOG "\t"))
      (save-buffer)
      ;; Go back and find our baseline and return it.
      (goto-char (point-min))
      (forward-line 1)
      (read (concat "(" (buffer-substring-no-properties (point-at-bol) (point-at-eol)) ")"))
      )))

(defun metest-log-report (baseline)
  "Report via message what happened during the test suite."
  (let ((log (reverse metest-time-log))
	(base (cdr baseline)))
    (princ "Baseln\tRun\tImprovement\tTest\n")
    (while (and log base)
      (princ (format "%.4f\t" (car base)))
      (princ (format "%.4f\t" (cdr (car log))))
      (princ (format "%.4f\t\t" (- (car base) (cdr (car log)))))
      (princ (metest-shorten (car (car log))))
      (princ "\n")
      (setq log (cdr log)
	    base (cdr base)))
    ))

(defun metest-timeit (fcn &optional file)
  "Time running FCN and save result in LOGFILE.
Use this to track perforamnce improvements during development automatically."
  (let* ((start (current-time))
	 (out (funcall fcn file))
	 (end (current-time))
	 (diff (float-time (time-subtract end start))))
    (if (eq fcn (car-safe (car-safe metest-time-log)))
	;; Same fcn, append our number
	(setcdr (car metest-time-log) (+ diff (cdr (car metest-time-log))))
      (push (cons fcn diff) metest-time-log))
    (cons diff out)))

(provide 'metest)

;;; metest.el ends here
