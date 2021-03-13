;;; matlab-scan.el --- Tools for contextually scanning a MATLAB buffer
;;
;; Copyright (C) 2021 Eric Ludlam
;;
;; Author:  <eludlam@mathworks.com>
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
;; Handle all the scanning and computing for MATLAB files.
;;
;;  * Regular expressions for finding different kinds of syntax
;;  * Systems for detecting what is on a line
;;  * Systems for computing indentation

(require 'matlab-syntax)

;;; Code:

;;; Keyword and REGEX constants
;;
(defconst matlab-block-keyword-list '(("end" . end) 
				      ("function" . decl)
				      ("classdef" . decl)
				      ("arguments" . args)
				      ("properties" . mcos)
				      ("methods" . mcos)
				      ("events" . mcos)
				      ("enumeration" . mcos)
				      ("if" . ctrl)
				      ("elseif" . mid)
				      ("else" . mid)
				      ("ifelse" . mid)
				      ("for" . ctrl)
				      ("parfor" . ctrl)
				      ("while" . ctrl)
				      ("switch" . ctrl)
				      ("case" . case)
				      ("otherwise" . case)
				      ("try" . ctrl)
				      ("catch" . mid)
				      )
  "List of keywords that are part of code blocks.")

(defconst matlab-keyword-table
  (let ((ans (obarray-make 23)))
    (mapc (lambda (elt) (set (intern (car elt) ans) (cdr elt)))
	  matlab-block-keyword-list)
    ans)
  "Keyword table for fast lookups of different keywords and their purpose.")

;;;  Context Parsing
;;
;; Find some fast ways to identify the context for a given line.
;; Use tricks to derive multiple pieces of information and do lookups
;; as quickly as possible.


(defun matlab-compute-line-context (level &rest context)
  "Compute and return the line context for the current line of MATLAB code.
LEVEL indicates how much information to return.
LEVEL of 1 is the most primitive / simplest data.
LEVEL of 2 is stuff that is derived from previous lines of code.
This function caches computed context onto the line it is for, and will
return the cache if it finds it."
  (cond ((= level 1)
	 (let ((ctxt (save-excursion
		       (back-to-indentation)
		       (matlab-scan-cache-get))))
	   (unless ctxt
	     (setq ctxt (matlab-compute-line-context-lvl-1))
	     (matlab-scan-cache-put ctxt))
	   ctxt))
	((= level 2)
	 (apply 'matlab-compute-line-context-lvl-2 context))
	(t
	 nil))
  )

;;; LEVEL 1 SCANNER
;;
;; This scanner pulls out context available on the current line.
;; This inludes the nature of the line, and anything sytax-ppss gives is.
(defconst mlf-ltype 0)
(defconst mlf-stype 1)
(defconst mlf-point 2)
(defconst mlf-indent 3)
(defconst mlf-entity-start 4)
(defconst mlf-paren-depth 5)
(defconst mlf-paren-inner-char 6)
(defconst mlf-paren-inner-col 7)
(defconst mlf-paren-outer-char 8)
(defconst mlf-paren-outer-point 9)
(defconst mlf-paren-delta 10)
(defconst mlf-end-comment-type 11)
(defconst mlf-end-comment-pt 12)

(defun matlab-compute-line-context-lvl-1 ()
  "Compute and return the level1 context for the current line of MATLAB code.
Level 1 contexts are things quickly derived from `syntax-ppss'
and other simple states.
Computes multiple styles of line by checking for multiple types of context
in a single call using fastest methods."
  (save-excursion
    (back-to-indentation)
    (let* ((ppsend (save-excursion (syntax-ppss (point-at-eol))))
	   (pps (syntax-ppss (point)))
	   (ltype 'empty)
	   (stype nil)
	   (pt (point))
	   (indent (current-indentation))
	   (start (point))
	   (paren-depth (nth 0 pps))
	   (paren-inner-char nil)
	   (paren-inner-col nil)
	   (paren-outer-char nil)
	   (paren-outer-point nil)
	   (paren-delta (- (car pps) (car ppsend)))
	   (ec-type nil)
	   (ec-col nil)
	   (cont-from-prev nil)
	  )

      ;; This means we are somewhere inside a cell, array, or arg list.
      ;; Find out the kind of list we are in.
      ;; Being in a multi-line list is valid for all other states like
      ;; empty lines, and block comments
      (when (> (nth 0 pps) 0)
	(save-excursion
	  (goto-char (car (last (nth 9 pps))))
	  (setq paren-inner-char (char-after (point))
		paren-inner-col  (current-column)
		paren-outer-point (car (nth 9 pps))
		paren-outer-char (char-after paren-outer-point) )))

      (cond
       ;; For comments - We can only ever be inside a block comment, so
       ;; check for that.
       ;; 4 is comment flag.  7 is '2' if block comment
       ((and (nth 4 pps) (eq (nth 7 pps) 2))
	(setq ltype 'comment
	      stype (cond ((looking-at "%}\\s-*$")
			   'block-end)
			  ((looking-at "%")
			   'block-body-prefix)
			  (t 'block-body))
	      start (nth 8 pps)))

       ;; If indentation lands on end of line, this is an empty line
       ;; so nothing left to do.  Keep after block-comment-body check
       ;; since empty lines in a block comment are valid.
       ((eolp)
	nil)

       ;; Looking at a % means one of the various comment flavors.
       ((eq (char-after (point)) ?\%)
	(setq ltype 'comment
	      stype (cond ((looking-at "%{\\s-*$")
			   'block-start)
			  ((looking-at "%%")
			   'cell-start)
			  ((looking-at "% \\$\\$\\$")
			   'indent-ignore)
			  (t nil))))

       ;; Looking at word constituent.  If so, identify if it is one of our
       ;; special identifiers.
       ((looking-at "\\w+\\>")
	(if (/= paren-depth 0)
	    (setq ltype 'code)
	  
	  ;; If not in parens, this might be a keyword.
	  ;; Look up our various keywords.
	  (let* ((word (match-string-no-properties 0))
		 (sym (intern-soft word matlab-keyword-table))
		 )
	    (if sym
		(if (eq (symbol-value sym) 'end)
		    ;; Special end keyword is in a class all it's own
		    (setq ltype 'end)
		  ;; If we found this in our keyword table, then it is a start
		  ;; of a block with a subtype.
		  (setq ltype 'block-start
			stype (symbol-value sym)))
	      ;; Else - not a sym - just some random code.
	      (setq ltype 'code)
	      ))))

       ;; Looking at a close paren.
       ((and (< 0 paren-depth) (looking-at "\\s)"))
	(setq ltype 'close-paren))
       
       ;; Last stand - drop in 'code' to say - yea, just some code.
       (t (setq ltype 'code))
       
       )

      ;; NEXT - Check context at the end of this line, and determine special
      ;;        stuff about it.

      ;; When the line ends with a comment.
      ;; Also tells us about continuations and comment start for lining up tail comments.
      (let ((csc (nth 8 ppsend)))
	(when (and (not csc) (eq stype 'block-end))
	  ;; block comment end lines must end in a comment, so record
	  ;; the beginning of that block comment instead.
	  (setq csc (nth 8 pps)) )

	;; If we have something, record what it is.
	(when csc
	  (setq ec-col csc
		ec-type (if (= (char-after csc) ?\%) 'comment 'ellipsis))) ;; type
	)

      (list ltype stype pt indent start paren-depth
	    paren-inner-char paren-inner-col paren-outer-char paren-outer-point paren-delta
	    ec-type ec-col
	    ;;cont-from-prev
	    )
      )))

;;; Accessor Utilities for LEVEL 1
;;
;; Use these to query a context for a piece of data
(defmacro matlab-with-context-line (__context &rest forms)
  "Save excursion, and move point to the line specified by CONTEXT.
Takes a lvl1 or lvl2 context.
Returns the value from the last part of forms."
  (declare (indent 1))
  `(save-excursion
     ;; The CAR of a LVL2 is a LVL1.  If __context is LVL1, then
     ;; the car-safe will return nil
     (goto-char (nth mlf-point (or (car-safe (car ,__context)) ,__context)))
     ,@forms))

(defsubst matlab-line-empty-p (lvl1)
  "Return t if the current line is empty based on LVL1 cache."
  (eq (car lvl1) 'empty))

;; Comments
(defsubst matlab-line-comment-p (lvl1)
  "Return t if the current line is a comment based on LVL1 cache."
  (eq (car lvl1) 'comment))

(defsubst matlab-line-regular-comment-p (lvl1)
  "Return t if the current line is a boring style comment based on LVL1 cache."
  (and (eq (car lvl1) 'comment) (eq (nth 1 lvl1) nil)))

(defsubst matlab-line-comment-ignore-p (lvl1)
  "Return t if the current line is an indentation ignored comment."
  (and (matlab-line-comment-p lvl1) (eq (nth mlf-stype lvl1) 'indent-ignore)))

(defsubst matlab-line-comment-style (lvl1)
  "Return type type of comment on this line."
  (and (matlab-line-comment-p lvl1) (nth mlf-stype lvl1)))

(defsubst matlab-line-end-comment-column (lvl1)
  "Return column of comment on line, or nil if no comment.
All lines that start with a comment end with a comment."
  (when (eq (nth mlf-end-comment-type lvl1) 'comment)
    (save-excursion
      (goto-char (nth mlf-end-comment-pt lvl1))
      (current-column))))

(defsubst matlab-line-ellipsis-p (lvl1)
  "Return if this line ends with a comment.
All lines that start with a comment end with a comment."
  (eq (nth mlf-end-comment-type lvl1) 'ellipsis))

(defsubst matlab-line-block-comment-start (lvl1)
  "Return the start of the block comment we are in, or nil."
  (when (and (matlab-line-comment-p lvl1)
	     (memq (nth mlf-stype lvl1)
		   '(block-start block-end block-body block-body-prefix)))
    (nth mlf-entity-start lvl1)))

;; Code and Declarations
(defsubst matlab-line-code-p (lvl1)
  "Return t if the current line is boring old code."
  (eq (car lvl1) 'code))

(defsubst matlab-line-block-start-keyword-p (lvl1)
  "Return t if the current line starts with block keyword."
  (eq (car lvl1) 'block-start))

(defsubst matlab-line-declaration-p (lvl1)
  "If the current line is a declaration, return the column it starts on.
Declarations are things like function or classdef."
  (and (matlab-line-block-start-keyword-p lvl1) (eq (nth mlf-stype lvl1) 'decl)))

(defsubst matlab-line-end-p (lvl1)
  "Non nil If the current line starts with an end."
  (eq (car lvl1) 'end))

(defsubst matlab-line-block-middle-p (lvl1)
  "Non nil If the current line starts with a middle block keyword.
These are keywords like `else' or `catch'."
  (and (eq (car lvl1) 'block-start) (eq (nth 1 lvl1) 'mid)))

(defsubst matlab-line-block-case-p (lvl1)
  "Non nil If the current line starts with a middle block keyword.
These are keywords like `else' or `catch'."
  (and (eq (car lvl1) 'block-start) (eq (nth 1 lvl1) 'case)))

(defsubst matlab-line-end-of-code (&optional lvl1)
  "Go to the end of the code on the current line.
If there is a comment or ellipsis, go to the beginning of that.
If the line starts with a comment return nil, otherwise t."
  (unless lvl1 (setq lvl1 (matlab-compute-line-context 1)))
  (goto-char (nth mlf-point lvl1))
  (if (or (matlab-line-empty-p lvl1) (matlab-line-comment-p lvl1))
      nil
    ;; Otherwise, look for that code.
    (if (eq (nth mlf-end-comment-type lvl1) 'comment)
	(goto-char (nth mlf-end-comment-pt lvl1))
      (goto-char (point-at-eol)))))

;; Parenthetical blocks
(defsubst matlab-line-close-paren-p (lvl1)
  "Non nil If the current line starts with closing paren (any type.)"
  (eq (car lvl1) 'close-paren))

(defsubst matlab-line-paren-depth (lvl1)
  "The current depth of parens at the start of this line"
  (nth mlf-paren-depth lvl1))

(defsubst matlab-line-close-paren-inner-char (lvl1)
  "Return the paren character for the parenthetical expression LVL1 is in."
  (nth mlf-paren-inner-char lvl1))

(defsubst matlab-line-close-paren-inner-col (lvl1)
  "Return the paren column for the prenthetical expression LVL1 is in."
  (nth mlf-paren-inner-col lvl1))

(defsubst matlab-line-close-paren-outer-char (lvl1)
  "The paren character for the outermost prenthetical expression LVL1 is in."
  (nth mlf-paren-outer-char lvl1))

(defsubst matlab-line-close-paren-outer-point (lvl1)
  "The poit the outermost parenthetical expression start is at."
  (nth mlf-paren-outer-point lvl1))

;;; LEVEL 2 SCANNER
;;
;; This scanner extracts information that affects the NEXT line of ML code.
;; This inludes things like ellipsis and keyword chains like if/end blocks.
;;
;; Level 2 scanning information cascades from line-to-line, several fields will
;; be blank unless a previous line is also scanned.
(defconst mlf-level1 0)
(defconst mlf-previous-level1 1)
(defconst mlf-comment 2)
(defconst mlf-comment-col 3)
(defconst mlf-comment-begin 4)

(defun matlab-compute-line-context-lvl-2 (&optional lvl1 previous2 full)
  "Compute the level 2 context for the current line of MATLAB code.
Level 2 context are things that are derived from previous lines of code.

Some of that context is derived from the LVL1 context such as paren depth,
and some scaning previous lines of code.

LVL1 will be computed if not provided.

This function will generate a mostly empty structure, and will
fill in context from the PREVIOUS2 input as needed.  Empty stats
will be computed by accessors on an as needed basis.  If PREVIOUS
2 is not provided, it will go back 1 line, scan it for lvl1 data
and use that.

If the 3rd argument FULL is non-nil, it will take the time to search backward
for all the information it can.

The returned LVL2 structure will fill out to be a chain of all previous
LVL2 outputs up to a context break.  The chains will be summarized in slots
in the returned list for quick access."
  (when (not lvl1) (setq lvl1 (matlab-compute-line-context-lvl-1)))
  ;; matlab-ltype-help-comm
  ;; block start
  ;; block end
  ;; change in # blocks on this line.

  (save-excursion
    (let ((prev-lvl1 (if previous2 (car previous2)
		       ;; Not provided, go back 1 and get lvl1 data.
		       (save-excursion)
		       (beginning-of-line)
		       (when (not (bobp))
			 (forward-char -1)
			 (matlab-compute-line-context 1))))
	  (comment nil)
	  (comment-col 0)
	  (comment-begin nil)
	  (tmp nil)
	  )

      ;; COMMENT DATA
      (cond
       ;; If we are in a block comment, no other needs.
       ((setq tmp (matlab-line-block-comment-start lvl1))
	(setq comment 'comment-block
	      comment-col (save-excursion (goto-char tmp)
					  (current-column))))
       ;; If prev line has a comment, fill in.
       ((setq tmp (matlab-line-end-comment-column prev-lvl1))
	(setq comment 'comment
	      comment-col tmp)
	(when (matlab-line-comment-p prev-lvl1)
	  (if (and previous2 )
	      (setq comment-begin (nth mlf-comment-begin previous2))
	    ;; If not provided, compute.
	    (matlab-with-context-line prev-lvl1
	      (forward-comment -100000)
	      (matlab-compute-line-context 1)))))
       )

    


      (list lvl1 prev-lvl1 comment comment-col comment-begin
	  
	    ))))

;;; Scanning Accessor utilities
;;
;; Some utilities require some level of buffer scanning to get the answer.
;; Keep those separate so they can depend on the earlier decls.
(defun matlab-scan-comment-help-p (lvl1 &optional pt)
  "Return declaration column if the current line is part of a help comment.
Declarations are things like functions and classdefs.
Indentation a help comment depends on the column of the declaration.
Optional PT, if non-nil, means return the point instead of column"
  (and (matlab-line-comment-p lvl1)
       (save-excursion
	(beginning-of-line)
	(forward-comment -100000)
	(let ((c-lvl1 (matlab-compute-line-context 1)))
	  (when (matlab-line-declaration-p c-lvl1)
	    (if pt (point) (current-indentation))))
	)))

(defun matlab-scan-previous-line-ellipsis-p ()
  "Return the column of the previous line's continuation if there is one.
This is true iff the previous line has an ellipsis, but not if this line
is in an array with an implied continuation."
  (save-excursion
    (beginning-of-line)
    (when (not (bobp))
      (forward-char -1)
      (let* ((pps (syntax-ppss (point)))
	     (csc (nth 8 pps)))
	;; If the comment active on eol does NOT start with %, then it must be
	;; and ellipsis.
	(and csc
	     (/= (char-after csc) ?\%)
	     (goto-char csc)
	     (current-column))))))

(defun matlab-scan-beginning-of-command (&optional lvl1)
  "Return point in buffer at the beginning of this command.
This function walks up any enclosing parens, and skips
backward over lines that include ellipsis."
  (unless lvl1 (setq lvl1 (matlab-compute-line-context 1)))
  ;; If we are in a block comment, just jump to the beginning, and
  ;; that's it.
  (let ((bcs (matlab-line-block-comment-start lvl1)))
    (when bcs
      (goto-char bcs)
      (setq lvl1 (matlab-compute-line-context 1)))
      
    ;; If we are in a help comment, jump over that first.
    (setq bcs (matlab-scan-comment-help-p lvl1 'point))
    (when bcs
      (goto-char bcs)
      (setq lvl1 (matlab-compute-line-context 1)))
    
    ;; Now scan backward till we find the beginning.
    (let ((found nil))
      (while (not found)
	;; first - just jump to our outermost point.
	(goto-char (or (matlab-line-close-paren-outer-point lvl1) (point)))
	;; Second - is there an ellipsis on prev line?
	(let ((prev (matlab-scan-previous-line-ellipsis-p)))
	  (if (not prev)
	      (setq found t)
	    ;; Move to prev location if not found.
	    (goto-char prev))))
      (back-to-indentation)
      (point))))

;;; Caching
;;
(defvar matlab-scan-temporal-cache nil
  "Cache of recently computed line contexts.
Used to speed up repeated queries on the same set of lines.")
(make-variable-buffer-local 'matlab-scan-temporal-cache)
(defvar matlab-scan-cache-max 10
  "Largest size of the cache.
Larger means less computation, but more time scanning.
Since the list isn't sorted, not optimizations possible.")

(defun matlab-scan-cache-get ()
  "Get a cached context."
  (let ((pt (point))
	(cache matlab-scan-temporal-cache))
    (while (and cache (/= pt (nth mlf-point (car cache))))
      (setq cache (cdr cache)))
    ;; If we found a match, return it.
    (when (and cache (= pt (nth mlf-point (car cache))))
      (car cache))))

(defun matlab-scan-cache-put (ctxt)
  "Put a context onto the cache.
Make sure the cache doesn't exceed max size."
  (push ctxt matlab-scan-temporal-cache)
  (setcdr (or (nthcdr matlab-scan-cache-max matlab-scan-temporal-cache)
	      (cons nil nil))
	  nil))


(defun matlab-scan-after-change-fcn (start end length)
  "Function run in after change hooks."
  (setq matlab-scan-temporal-cache nil))

(defun matlab-scan-setup ()
  "Setup use of the indent cache for the current buffer."
  (interactive)
  (add-hook 'after-change-functions 'matlab-scan-after-change-fcn t)
  (setq matlab-scan-temporal-cache nil))

(defun matlab-scan-disable ()
  "Setup use of the indent cache for the current buffer."
  (interactive)
  (remove-hook 'after-change-functions 'matlab-scan-after-change-fcn t)
  (setq matlab-scan-temporal-cache nil))


;;; Debugging and Querying
;;
(defun matlab-describe-line-indent-context ()
  "Describe the indentation context for the current line."
  (interactive)
  (back-to-indentation)
  (let* ((MSG1 "")
	 (MSG2 "")
	 (lvl1 (matlab-compute-line-context 1))
	 (lvl2 (matlab-compute-line-context 2)))
    (let* ((paren-inner-char (nth mlf-paren-inner-char lvl1))
	   (open (format "%c" (or paren-inner-char ?\()))
	   (close (format "%c"
			  (cond ((not paren-inner-char) ?\))
				((= paren-inner-char ?\() ?\))
				((= paren-inner-char ?\[) ?\])
				((= paren-inner-char ?\{) ?\})
				(t ??))))
	   (innerparenstr (format "%s%d%s" open (nth mlf-paren-depth lvl1) close))
	   (outerp-char (nth mlf-paren-outer-char lvl1))
	   (outerp-open (if outerp-char (format "%c" outerp-char) ""))
	   (outerp-close (if (not outerp-char) ""
			   (format "%c"
				   (cond ((= outerp-char ?\() ?\))
					 ((= outerp-char ?\[) ?\])
					 ((= outerp-char ?\{) ?\})
					 (t ??)))))
	   (outerparenopen "")
	   (outerparenclose "")
	   (extraopen "")
	   (extraclose "")
	   )
      (cond ((= (nth mlf-paren-depth lvl1) 0)
	     ;; 0 means no parens - so shade out parens to indicate.
	     (setq open (propertize open 'face 'shadow)
		   close (propertize close 'face 'shadow)))
	    ((<= (nth mlf-paren-depth lvl1) 1)
	     ;; If 1 or fewer parens, clear out outer chars
	     (setq outerp-open ""
		   outerp-close ""))
	    ((> (nth mlf-paren-depth lvl1) 2)
	     ;; If more than 2, signal more unknown parens in between
	     (setq outerp-open (concat outerp-open (string (decode-char 'ucs #x2026)))
		   outerp-close (concat (string (decode-char 'ucs #x2026)) outerp-close))))
      (if (< (nth mlf-paren-delta lvl1) 0)
	  (setq extraopen (format "<%d" (abs (nth mlf-paren-delta lvl1))))
	(when (> (nth mlf-paren-delta lvl1) 0)
	  (setq extraclose (format "%d>" (nth mlf-paren-delta lvl1)))))


      (setq MSG1
	    (format "%s%s >>%d %s%s%s%s%s %s %s" (nth mlf-ltype lvl1)
		    (format " %s" (or (nth mlf-stype lvl1) ""))
		    (nth mlf-indent lvl1)
		    ;; paren system
		    extraopen
		    outerp-open
		    innerparenstr
		    outerp-close
		    extraclose
		    (cond ((eq (nth mlf-end-comment-type lvl1) 'comment) "%")
			  ((eq (nth mlf-end-comment-type lvl1) 'ellipsis) "...")
			  (t ""))
		    (if (matlab-line-end-comment-column lvl1)
			(format " %d" (matlab-line-end-comment-column lvl1))
		      "")
		    ))
      )
    (let* ((lvl2 (matlab-compute-line-context-lvl-2 lvl1))
	   ;;(comment
	   )
      
      )

    (message "%s" (concat MSG1 MSG2))
    ))


(provide 'matlab-scan)

;;; matlab-indent.el ends here
