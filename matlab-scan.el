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
;; List of our keywords, and tools to look up keywords and find out
;; what they are.
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
				      ("spmd" . ctrl)
				      ("switch" . ctrl)
				      ("case" . case)
				      ("otherwise" . case)
				      ("try" . ctrl)
				      ("catch" . mid)
				      ("break" . keyword)
				      ("continue" . keyword)
				      ("return" . keyword)
				      ("global" . vardecl)
				      ("persistent" . vardecl)
				      )
  "List of keywords that are part of code blocks.")

(defconst matlab-keyword-table
  (let ((ans (matlab-obarray-make 23)))
    (mapc (lambda (elt) (set (intern (car elt) ans) (cdr elt)))
	  matlab-block-keyword-list)
    ans)
  "Keyword table for fast lookups of different keywords and their purpose.")

(defun matlab-keyword-p (word)
  "Non nil if WORD is a keyword.
If word is a number, it is a match-string index for the current buffer."
   (let* ((local-word (if (numberp word)
			  (match-string-no-properties word)
			word))
	  (sym (intern-soft local-word matlab-keyword-table)))
     (and sym (symbol-value sym))))

(defsubst matlab-on-keyword-p ()
  "Return the type of keyword under point, or nil."
  (when (matlab-valid-keyword-syntax)
    ;; Not in invalid context, look it up.
    (matlab-keyword-p (buffer-substring-no-properties
		       (save-excursion (skip-syntax-backward "w_") (point))
		       (save-excursion (skip-syntax-forward "w_") (point))))))

(defvar matlab-kwt-all nil)
(defvar matlab-kwt-decl nil)
(defvar matlab-kwt-indent nil)
(defvar matlab-kwt-end nil)
(defvar matlab-kwt-blocks nil)

(defun matlab-keyword-regex (types)
  "Find keywords that match TYPES and return optimized regexp.
TYPES can also be a single symbol that represents a common list of
keyword types.  These include:
   all    - any kind of keyword
   decl   - declarations, like class or function
   indent - any keyword that causes an indent
   end    - the end keyword (that causes dedent)
   blocks - indent and end keywords
Caches some found regexp to retrieve them faster."
  (cond
   ((or (eq types nil) (eq types 'all))
    (or matlab-kwt-all (setq matlab-kwt-all (matlab--keyword-regex nil))))
   ((eq types 'decl)
    (or matlab-kwt-decl (setq matlab-kwt-decl (matlab--keyword-regex '(decl)))))
   ((eq types 'indent)
    (or matlab-kwt-indent (setq matlab-kwt-indent (matlab--keyword-regex '(decl ctrl args mcos)))))
   ((eq types 'end)
    (or matlab-kwt-end (setq matlab-kwt-end (matlab--keyword-regex '(end)))))
   ((eq types 'blocks)
    (or matlab-kwt-blocks (setq matlab-kwt-blocks (matlab--keyword-regex '(end decl ctrl args mcos)))))
   (t
    (matlab--keyword-regex types))))

(defun matlab--keyword-regex (types)
  "Find keywords that match TYPES and return an optimized regexp."
  (let ((lst nil))
    (mapc (lambda (C) (when (or (null types) (memq (cdr C) types)) (push (car C) lst)))
	  matlab-block-keyword-list)
    (regexp-opt lst 'symbols)))


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
  (save-match-data
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
	   nil)))
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
	   (symval nil)
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
       ((and (= paren-depth 0)
	     (setq symval (matlab-on-keyword-p)))

	(cond
	 ;; Special end keyword is in a class all it's own
	 ((eq symval 'end)
	  (setq ltype 'end))
	 ;; If we found this in our keyword table, then it is a start
	 ;; of a block with a subtype.
	 ((memq symval '(decl args mcos ctrl mid case))
	  (setq ltype 'block-start
		stype symval))
	 ;; Some keywords aren't related to blocks with indentation
	 ;; controls.  Those are treated as code, with a type.
	 (t
	  (setq ltype 'code
		stype symval))
	 ))
       
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
		ec-type (cond ((= (char-after csc) ?\%) 'comment)
			      ((= (char-after csc) ?\.) 'ellipsis)
			      (t 'commanddual)))
	  ))

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
  (declare (indent 1) (debug (form &rest form)))
  `(save-excursion
     ;; The CAR of a LVL2 is a LVL1.  If __context is LVL1, then
     ;; the car-safe will return nil
     (goto-char (nth mlf-point (if (consp (car ,__context))
				   (car ,__context)
				 ,__context)))
     ,@forms))

(defsubst matlab-line-point (lvl1)
  "Return the point at beginning of indentation for line specified by lvl1."
  (nth mlf-point lvl1))

(defsubst matlab-line-indentation (lvl1)
  "Return the indentation of the line specified by LVL1."
  (nth mlf-indent lvl1))

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
      ;; NOTE: if line is in a block comment, this end pt is
      ;;       really the beginning of the block comment.
      (goto-char (nth mlf-end-comment-pt lvl1))
      (current-column))))

(defsubst matlab-line-ellipsis-p (lvl1)
  "Return if this line ends with a comment."
  (eq (nth mlf-end-comment-type lvl1) 'ellipsis))

(defsubst matlab-line-commanddual-p (lvl1)
  "Return if this line ends with command duality string."
  (eq (nth mlf-end-comment-type lvl1) 'commanddual))

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
  "The point the outermost parenthetical expression start is at."
  (nth mlf-paren-outer-point lvl1))


;;; LEVEL 2 SCANNER
;;
;; This scanner extracts information that affects the NEXT line of ML code.
;; This inludes things like ellipsis and keyword chains like if/end blocks.
;;
;; Level 2 scanning information cascades from line-to-line, several fields will
;; be blank unless a previous line is also scanned.
(defconst mlf-level1 0)
(defconst mlf-previous-line1 1)
(defconst mlf-previous-line2 2)
(defconst mlf-previous-command-beginning 3)
(defconst mlf-previous-nonempty 4)
(defconst mlf-previous-code 5)
(defconst mlf-previous-block 6)
(defconst mlf-previous-fcn 7)

(defun matlab-compute-line-context-lvl-2 (&optional lvl1 previous2)
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

Empty stats are filled in as t.  nil means there is no context, or
a lvl1 stat block for the line with that meaning.

The returned LVL2 structure will fill out to be a chain of all previous
LVL2 outputs up to a context break.  The chains will be summarized in slots
in the returned list for quick access."
  (when (not lvl1) (setq lvl1 (matlab-compute-line-context-lvl-1)))
  ;; matlab-ltype-help-comm
  ;; block start
  ;; block end
  ;; change in # blocks on this line.

  (save-excursion
    (let ((prev-lvl1 t)
	  (prev-lvl2 t)
	  (prev-cmd-begin t)
	  (prev-nonempty t)
	  (prev-code1 t)
	  (prev-block1 t)
	  (prev-fcn1 t)
	  (tmp nil)
	  )

      ;; copy data from previous2.
      (if previous2
	  (progn
	    (setq prev-lvl1 (car previous2)
		  prev-lvl2 previous2
		  prev-cmd-begin (nth mlf-previous-command-beginning previous2)
		  prev-nonempty (nth mlf-previous-nonempty previous2)
		  prev-code1 (nth mlf-previous-code previous2)
		  prev-block1 (nth mlf-previous-block previous2)
		  prev-fcn1 (nth mlf-previous-fcn previous2)
		  )
	    (matlab-scan-stat-inc 'prev2)
	    )
	;; Else - previous LVL1 is the one thing we'll compute since we need to
	;; init our trackers.
	(save-excursion
	  (beginning-of-line)
	  (if (bobp)
	      (setq prev-lvl1 nil)
	    (forward-char -1)
	    (setq prev-lvl1 (matlab-compute-line-context 1))
	    (matlab-scan-stat-inc 'prev2miss)
	  )))

      ;; prev line can be nil if at beginning of buffer.
      (if (not prev-lvl1)
	  (setq prev-lvl2 nil
		prev-cmd-begin nil
		prev-nonempty nil
		prev-code1 nil
		prev-block1 nil
		prev-fcn1 nil
		)
	
	;; If we do have a previous lvl1, then we can compute from it.
	;; Override parts of our data based on prev-lvl2 which might
	;; be one of the things we care about.
	(when (not (matlab-line-empty-p prev-lvl1))
	  ;; Our prev line was non empty, so remember.
	  (setq prev-nonempty prev-lvl1)
	
	  (if (not (memq (car prev-lvl1) '(empty comment)))
	      (progn
		;; Our prev line wasn't a comment or empty, so remember.
		(setq prev-code1 prev-lvl1)

		(when (and (= (matlab-line-paren-depth prev-lvl1) 0)
			   (not (matlab-line-ellipsis-p prev-lvl1)))
		  ;; Our previous line some code, but was not in an
		  ;; array, nor an ellipse.  Thus, reset beginning of cmd
		  ;; to this line we are on.
		  (setq prev-cmd-begin lvl1)
		  )
		
		(when (eq (car prev-lvl1) 'block-start)
		  ;; We have a block start, so remember
		  (setq prev-block1 prev-lvl1)

		  (when (eq (nth mlf-stype prev-lvl1) 'decl)
		    (setq prev-fcn1 prev-lvl1))
		  ))

	    ;; Do something with comment here ??
	    )))

      (list lvl1 prev-lvl1 prev-lvl2 prev-cmd-begin prev-nonempty prev-code1 prev-block1 prev-fcn1
	  
	    ))))

;;; Refresh this lvl2
;;
(defun matlab-refresh-line-context-lvl2 (lvl2 &optional lvl1)
  "Refresh the content of this lvl2 context.
Assume ONLY the line this lvl2 context belongs to has changed
and we don't have any caches in later lines."
  (matlab-with-context-line lvl2
    (when (not lvl1) (setq lvl1 (matlab-compute-line-context 1)))
    ;; cmd begin can be same as self.  Check and replace
    (when (eq (car lvl2) (nth mlf-previous-command-beginning lvl2))
      (setcdr (nthcdr mlf-previous-command-beginning lvl2) lvl1))
    ;; Replace self.
    (setcar lvl2 lvl1)
    ))

;;; Simple Accessors
;;
(defun matlab-get-lvl1-from-lvl2 (lvl2)
  "Return a LVL1 context.
If input LVL2 is a level 2 context, return the lvl1 from it.
If the input is a lvl1, then return that.
If LVL2 is nil, compute it."
  (if lvl2
      (if (consp (car lvl2)) (car lvl2) lvl2)
    (matlab-compute-line-context 1)))

(defun matlab-previous-line (lvl2)
  "Return the previous line from lvl2 context."
  (matlab-scan-stat-inc 'prevline)
  (nth mlf-previous-line1 lvl2))

(defun matlab-previous-line-lvl2 (lvl2)
  "Return the previous line from lvl2 context."
  (let ((prev (nth mlf-previous-line2 lvl2)))
    (if (eq prev t)
	(save-excursion
	  (matlab-scan-stat-inc 'prevline2miss)
	  (matlab-with-context-line (matlab-previous-line lvl2)
	    (setq prev (matlab-compute-line-context 2)))
	  (setcar (nthcdr mlf-previous-line1 lvl2) prev))
      ;; Else, return
      (matlab-scan-stat-inc 'prevline2)
      prev)))

(defun matlab-previous-nonempty-line (lvl2)
  "Return lvl1 ctxt for previous non-empty line."
  (let ((prev (nth mlf-previous-nonempty lvl2))
	)    
    (if (eq prev t)
	;; Compute it and stash it.
	(save-excursion
	  (matlab-scan-stat-inc 'nonemptymiss)
	  (beginning-of-line)
	  (skip-syntax-backward " >") ;; skip spaces, and newlines w/ comment end on it.
	  ;; TODO - this stops on ignore comments.
	  (setq prev (matlab-compute-line-context 1))
	  (setcar (nthcdr mlf-previous-nonempty lvl2) prev))
      ;; else record a cache hit
      (matlab-scan-stat-inc 'nonempty)
      )
    prev))

(defun matlab-previous-code-line (lvl2)
  "Return lvl1 ctxt for previous non-empty line."
  (let ((prev (nth mlf-previous-code lvl2))
	)    
    (if (eq prev t)
	;; Compute it and stash it.
	(save-excursion
	  (matlab-scan-stat-inc 'codemiss)
	  (beginning-of-line)
	  (when (not (bobp))
	    (forward-comment -100000) ;; Skip over all whitespace and comments.
	    (setq prev (matlab-compute-line-context 1))
	    (setcar (nthcdr mlf-previous-code lvl2) prev)))
      ;; else record a cache hit
      (matlab-scan-stat-inc 'code)
      )
    prev))

(defun matlab-previous-command-begin (lvl2)
  "Return lvl1 ctxt for previous non-empty line."
  (let ((prev (nth mlf-previous-command-beginning lvl2))
	)    
    (if (eq prev t)
	;; Compute it and stash it.
	(save-excursion
	  (matlab-scan-stat-inc 'cmdbeginmiss)
	  (beginning-of-line)
	  (when (not (bobp))
	    (forward-comment -100000) ;; Skip over all whitespace and comments.
	    (matlab-scan-beginning-of-command)
	     ;; TODO!! 
	    (setq prev (matlab-compute-line-context 1))
	    (setcar (nthcdr mlf-previous-command-beginning lvl2) prev)))
      ;; else record a cache hit
      (matlab-scan-stat-inc 'cmdbegin)
      )
    prev))

;;(defun matlab-previous-fcn-line (lvl2)
;;  "Return lvl1 ctxt for previous non-empty line."
;;  (let ((prev (nth mlf-previous-fcn lvl2))
;;	)    
;;    (if (eq prev t)
;;	;; Compute it and stash it.
;;	(save-excursion
;;	  (matlab-scan-stat-inc 'fcnmiss)
;;	  (beginning-of-line)
;;	  ;; TODO
;;	  (setq prev (matlab-compute-line-context 1))
;;	  (setcar (nthcdr mlf-previous-fcn lvl2) prev))
;;      ;; else record a cache hit
;;      (matlab-scan-stat-inc 'fcn)
;;      )
;;    prev))



;;; MATLAB focused queries (more specific names than for first set)
;;
(defun matlab-line-in-array (lvl2)
  "Return the location of an opening paren if in array parens."
  (matlab-line-close-paren-outer-point (matlab-get-lvl1-from-lvl2 lvl2)))


;;; Scanning Accessor utilities
;;
;; Some utilities require some level of buffer scanning to get the answer.
;; Keep those separate so they can depend on the earlier decls.
(defun matlab-scan-comment-help-p (ctxt &optional pt)
  "Return declaration column if the current line is part of a help comment.
Use the context CTXT as a lvl1 or lvl2 context to compute.
Declarations are things like functions and classdefs.
Indentation a help comment depends on the column of the declaration.
Optional PT, if non-nil, means return the point instead of column"
  (let ((lvl2 nil) (lvl1 nil))
    (if (symbolp (car ctxt))
	(setq lvl1 ctxt)
      (setq lvl1 (matlab-get-lvl1-from-lvl2 ctxt)
	    lvl2 ctxt))
    
    (when (matlab-line-comment-p lvl1)
      ;; Try to get from lvl2 context
      (let ((c-lvl1 (when lvl2 (matlab-previous-code-line lvl2))))
	(unless c-lvl1
	  ;; If not, compute it ourselves.
	  (save-excursion
	    (beginning-of-line)
	    (forward-comment -100000)
	    (setq c-lvl1 (matlab-compute-line-context 1))))
	;; On previous code line - was it a declaration?
	(when (matlab-line-declaration-p c-lvl1)
	  (matlab-with-context-line c-lvl1
	    (if pt (point) (current-indentation))))))))

(defun matlab-scan-previous-line-ellipsis-p ()
  "Return the position of the previous line's continuation if there is one.
This is true iff the previous line has an ellipsis."
  (save-excursion
    (beginning-of-line)
    (when (not (bobp))
      (forward-char -1)
      (let* ((pps (syntax-ppss (point)))
	     (csc (nth 8 pps)))
	;; Ellipsis start has a syntax of 11 (comment-start).
	;; Other comments have high-bit flags, so don't == 11.
	(when (and csc (= (car (syntax-after csc)) 11))
	  csc)))))

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
	    (goto-char prev)
	    (setq lvl1 (matlab-compute-line-context 1))
	    )))
      (back-to-indentation)
      (point))))

(defun matlab-scan-end-of-command (&optional lvl1)
  "Return point in buffer at the end of this command.
This function walks down past continuations and open arrays."
  (unless lvl1 (setq lvl1 (matlab-compute-line-context 1)))
  ;; If we are in a block comment, just jump to the end, and
  ;; that's it.
  (let ((bcs (matlab-line-block-comment-start lvl1)))
    (if bcs
	(progn
	  (goto-char bcs)
	  (forward-comment 1)
	  (setq lvl1 (matlab-compute-line-context 1)))

      (let ((done nil)
	    (lvlwalk lvl1))
	(while (not done)
	  (end-of-line)
	  (cond ((matlab-end-of-outer-list)
		 ;; If we are in a list, this moves to the end and
		 ;; returns non-nil.  We are now where we want to be
		 ;; so nothing to do.
		 nil)

		 ((matlab-line-ellipsis-p lvlwalk)
		  ;; This is a continuation, keep going.
		  (forward-line 1))
		 
		(t
		 ;; None of these conditions were true, so
		 ;; we must be done!
		 (setq done t))
		)

	  ;; Protect against travelling too far.
	  (when (eobp) (setq done t))
	  ;; Scan the next line.
	  (when (not done) (setq lvlwalk (matlab-compute-line-context 1)))
	  ))

      ;; Return where we ended up
      (end-of-line)
      (point-at-eol))))


;;; BLOCK SCANNING and SEARCHING
;;
;; Focused scanning across block structures, like if / else / end.

(defvar matlab-functions-have-end)
(defvar matlab-indent-level)

(defsubst matlab--mk-keyword-node ()
  "Like `matlab-on-keyword-p', but returns a node for block scanning.
The elements of the return node are:
  0 - type of keyword, like ctrl or decl
  1 - text of the keyword
  2 - buffer pos for start of keyword"
  ;; Don't check the context - assume our callers have vetted this
  ;; point.  This is b/c the search fcns already skip comments and
  ;; strings for efficiency.
  (let* ((start (save-excursion (skip-syntax-backward "w_") (point))) 
	 (txt (buffer-substring-no-properties
	       start
	       (save-excursion (skip-syntax-forward "w_") (point))))
	 (type (matlab-keyword-p txt)))
    (when type (list type txt start) )))

;;; Valid keyword locations
;;
(defsubst matlab--valid-keyword-point ()
  "Return non-nil if point is valid for keyword.
Returns nil for failing `matlab-valid-keyword-syntax'.
Returns nil if preceeding non-whitespace char is `.'"
  (and (matlab-valid-keyword-syntax)
       (not (matlab-syntax-keyword-as-variable-p))))

(defsubst matlab--known-parent-block(parent)
  "Return PARENT if it is a known parent block."
  (if (or (not parent) (eq (car parent) 'unknown))
      nil
    parent))

(defun matlab--valid-mcos-keyword-point (&optional parentblock)
  "Return non-nil if at a location that is valid for MCOS keywords.
This means that the parent block is a classdef.
Optional input PARENTBLOCK is a precomputed keyword node
representing the current block context point is in.
Assume basic keyword checks have already been done."
  (and
   ;; Must be first thing on line - before checking other stuff.
   (save-excursion (skip-syntax-backward "w") (skip-syntax-backward " ") (bolp))
  
   ;; If a parent was provided, use that.
   (if (matlab--known-parent-block parentblock)
       (string= (nth 1 parentblock) "classdef")

     ;; else more expensive check
     (and (eq matlab-functions-have-end 'class) ;; not a class, no mcos allowed.
	  ;; Otherwise, roll back a single command.  in MUST be
	  ;; an END indent 4 or CLASSDEF
	  (save-excursion
	    (skip-syntax-backward "w")
	    (forward-comment -100000)
	    (back-to-indentation) ;; TODO -> this should be beginning-of-command
	    (let ((prev (matlab--mk-keyword-node)))
	      (or (string= (nth 1 prev) "classdef")
		  (and (string= (nth 1 prev) "end")
		       (= (current-indentation) matlab-indent-level)))))))))

(defun matlab--valid-arguments-keyword-point (&optional parentblock)
  "Return non-nil if at a location that is valid for ARGUMENTS keyword.
This means that the parent block is a function, and this is first cmd in
the function.
Optional input PARENTBLOCK is a precomputed keyword node
representing the current block context point is in.
Assume basic keyword checks have already been done."
  (save-excursion
    (skip-syntax-backward "w")
    (and
     ;; Must be first thing on line - before checking other stuff.
     (save-excursion (skip-syntax-backward "w") (skip-syntax-backward " ") (bolp))
     ;; More expensive checks
     (let ((parent
	    (or (matlab--known-parent-block parentblock) ;; technically this can lie, but it's fast.
		(save-excursion (forward-comment -100000)
				(matlab-scan-beginning-of-command)
				(and (matlab--valid-keyword-point)
				     (matlab--mk-keyword-node))))))
       (string= (nth 1 parent) "function")))))

(defun matlab--scan-derive-block-state (providedstate filter)
  "Return a block state for current point.
If PROVIDEDSTATE is non nil, use that.
Return nil if no valid block under pt."
  (or providedstate
      (let ((thiskeyword (matlab--mk-keyword-node)))
	(if (or (not thiskeyword)
		(not (matlab--valid-keyword-point))
		(not (memq (car thiskeyword) filter))
		)
	    nil
	  (push thiskeyword providedstate)))))

;;; Block Scanning
;;
(defun matlab--scan-block-forward (&optional bounds state)
  "Scan forward over 1 MATLAB block construct.
Return current state on exit.
  nil     - success
  non-nil - indicates incomplete scanning
Also skips over all nexted block constructs along the way.
Assumes cursor is in a valid starting state, otherwise ERROR.
If cursor is on a middle-block construct like else, case, ERROR.

Optional BOUNDS is a point in the buffer past which we won't scan. 
Optional STATE is the current parsing state to start from.
Use STATE to stop/start block scanning partway through."
  (let ((blockstate (matlab--scan-derive-block-state state '(decl args mcos ctrl)))
	(thiskeyword nil)
	(stop nil)
	(regex (matlab-keyword-regex 'blocks))
	)

    (when (not blockstate) (error "Not on valid block start."))

    (when (not state) (skip-syntax-forward "w")) ;; skip keyword

    (while (and blockstate (not stop))
      (if (not (setq thiskeyword (matlab-re-search-keyword-forward regex bounds t)))
	  (progn
	    (setq stop t)
	    (when (and (not matlab-functions-have-end)
		       (eq (car (car blockstate)) 'decl))
	      (goto-char (point-max))
	      (pop blockstate)))
	(cond ((eq (car thiskeyword) 'end)
	       ;; On end, pop last start we pushed
	       (pop blockstate))
	      ((eq (car thiskeyword) 'mcos)
	       (if (matlab--valid-mcos-keyword-point (car blockstate))
		   (push thiskeyword blockstate)
		 ;; else, just skip it
		 ))
	      ((eq (car thiskeyword) 'args)
	       (if (matlab--valid-arguments-keyword-point (car blockstate))
		   (push thiskeyword blockstate)
		 ;; else, just skip it, not a keyword
		 ))
	      ((and (not matlab-functions-have-end)
		    (eq (car thiskeyword) 'decl)
		    (eq (car (car blockstate)) 'decl)
		    )
	       ;; No ends on functions - in this case we need treat a function as an end.
	       ;; b/c you can't have nested functions, but only if the thing we try to match
	       ;; it to is another fcn.
	       ;; This POP should result in empty state.
	       (pop blockstate)
	       (goto-char (match-beginning 1)))
	      (t
	       (push thiskeyword blockstate)))
	))
    blockstate))

(defun matlab--scan-block-forward-up (&optional bounds)
  "Like `matlab--scan-block-forward', but cursor is not on a keyword.
Instead, travel to end as if on keyword."
  (let ((currentstate '((unknown "" 0))))
    (matlab--scan-block-forward bounds currentstate)))


(defun matlab--scan-block-backward (&optional bounds state)
  "Scan forward over 1 MATLAB block construct.
Return current state on exit.
  nil     - success
  non-nil - indicates incomplete scanning
Also skips over all nexted block constructs along the way.
Assumes cursor is in a valid starting state, otherwise ERROR.
If cursor is on a middle-block construct like else, case, ERROR.

Optional BOUNDS is a point in the buffer past which we won't scan. 
Optional STATE is the current parsing state to start from.
Use STATE to stop/start block scanning partway through."
  (let ((blockstate (matlab--scan-derive-block-state state '(end)))
	(thiskeyword nil)
	(stop nil)
	(regex (matlab-keyword-regex 'blocks))
	)

    (when (not blockstate) (error "Not on valid block end."))

    (when (not state) (skip-syntax-backward "w")) ;; skip keyword

    (while (and blockstate (not stop))
      (if (not (setq thiskeyword (matlab-re-search-keyword-backward regex bounds t)))
	  (setq stop t)
	(cond ((eq (car thiskeyword) 'end)
	       ;; On end, push this keyword
	       (push thiskeyword blockstate))
	      ((eq (car thiskeyword) 'mcos)
	       (when (matlab--valid-mcos-keyword-point nil)
		 (pop blockstate)
		 ))
	      ((eq (car thiskeyword) 'args)
	       (when (matlab--valid-arguments-keyword-point nil)
		 (pop blockstate)
		 ))
	      (t
	       (pop blockstate)))
	))
    blockstate))

(defun matlab--scan-block-backward-up (&optional bounds)
  "Like `matlab--scan-block-forward', but cursor is not on a keyword.
Instead, travel to end as if on keyword."
  (let ((currentstate '((end "end" 0))))
    (matlab--scan-block-backward bounds currentstate)))

;;; Searching for keywords
;;
;; These utilities will simplify searching for code bits by skipping
;; anything in a comment or string.
(defun matlab-re-search-keyword-forward (regexp &optional bound noerror)
  "Like `re-search-forward' but will not match content in strings or comments."
  (let ((ans nil) (case-fold-search nil))
    (save-excursion
      (while (and (not ans)
		  (or (not bound) (< (point) bound))
		  (setq ans (re-search-forward regexp bound noerror)))
	;; Check for simple cases that are invalid for keywords
	;; for strings, comments, and lists, skip to the end of them
	;; to not waste time searching for keywords inside.
	(cond ((matlab-end-of-string-or-comment t)
	       (setq ans nil))
	      ((matlab-end-of-outer-list)
	       (setq ans nil))
	      ((matlab-syntax-keyword-as-variable-p)
	       (setq ans nil))
	      )))
    (when ans (goto-char ans) (matlab--mk-keyword-node))))

(defun matlab-re-search-keyword-backward (regexp &optional bound noerror)
  "Like `re-search-backward' but will not match content in strings or comments."
  (let ((ans nil) (case-fold-search nil))
    (save-excursion
      (while (and (not ans)
		  (or (not bound) (> (point) bound))
		  (setq ans (re-search-backward regexp bound noerror)))
	;; Check for simple cases that are invalid for keywords
	;; for strings, comments, and lists, skip to the end of them
	;; to not waste time searching for keywords inside.
	(cond ((matlab-beginning-of-string-or-comment t)
	       (setq ans nil))
	      ((matlab-beginning-of-outer-list)
	       (setq ans nil))
	      ((matlab-syntax-keyword-as-variable-p)
	       (setq ans nil))
	      )))
    (when ans (goto-char ans) (matlab--mk-keyword-node))))

;;; Quick Queries
;;
(defun matlab-scan-block-start-context ()
  "Return a context for the block start matching block point is in.
assumes pt is NOT on an end.  List contains:
  0 - type of keyword the end matched.
  1 - column the keyword is on.
  2 - lvl1 context of line the keyword is on
  3 - lvl1 context of line at beginning of cmnd found keyword is in.

Items 2 and 3 are likely the same but could be different."
  (save-excursion
    (matlab--scan-block-backward-up)
    (let* ((keyword (matlab-on-keyword-p))
	   (column (current-column))
	   (lvl1-match (matlab-compute-line-context 1))
	   (lvl1-bgn (save-excursion (matlab-scan-beginning-of-command lvl1-match)
				     (matlab-compute-line-context 1))))
      (list keyword column lvl1-match lvl1-bgn))))
    


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
    (if (and cache (= pt (nth mlf-point (car cache))))
	(progn
	  (matlab-scan-stat-inc 'lvl1)
	  (car cache))
      (matlab-scan-stat-inc 'lvl1-miss)
      nil)))

(defun matlab-scan-cache-put (ctxt)
  "Put a context onto the cache.
Make sure the cache doesn't exceed max size."
  (push ctxt matlab-scan-temporal-cache)
  (setcdr (or (nthcdr matlab-scan-cache-max matlab-scan-temporal-cache)
	      (cons nil nil))
	  nil))


(defun matlab-scan-before-change-fcn (start end &optional length)
  "Function run in after change hooks."
  ;;(setq matlab-scan-temporal-cache nil))
  (let ((pt (point))
	(cache matlab-scan-temporal-cache)
	(newcache nil))
    ;; Flush whole lines.
    (save-excursion
      (goto-char start)
      (setq start (point-at-bol)))
    ;; Only drop items AFTER the start of our region.
    (while cache
      (if (<= start (matlab-line-point (car cache)))
	  (matlab-scan-stat-inc 'flushskip)
	(push (car cache) newcache)
	(matlab-scan-stat-inc 'flush))
      (setq cache (cdr cache)))
    ;;(setq newcache nil)
    ;;(when (and newcache (symbolp (car newcache))) (debug))
    (setq matlab-scan-temporal-cache newcache)
    ))

(defun matlab-scan-setup ()
  "Setup use of the indent cache for the current buffer."
  (interactive)
  (add-hook 'before-change-functions 'matlab-scan-before-change-fcn t)
  (setq matlab-scan-temporal-cache nil))

(defun matlab-scan-disable ()
  "Setup use of the indent cache for the current buffer."
  (interactive)
  (remove-hook 'before-change-functions 'matlab-scan-before-change-fcn t)
  (setq matlab-scan-temporal-cache nil))


;;; Debugging and Querying
;;
(defvar matlab-scan-cache-stats nil
  "Cache stats for tracking effectiveness of the cache.")
(defun matlab-scan-stat-reset (&optional arg)
  "Reset the stats cache.
With no arg, disable gathering stats.
With arg, enable gathering stats, and flush old stats."
  (interactive "P")
  (if arg
      (progn (setq matlab-scan-cache-stats nil)
	     (message "Disable matlab scanner stats gathering."))
    (message "Emable matlab scanner stats gathering.")
    (setq matlab-scan-cache-stats (matlab-obarray-make 13))))

(defun matlab-scan-stat-inc (thing)
  "Increment the stat associated with thing."
  (when matlab-scan-cache-stats
    (let ((sym (intern-soft (symbol-name thing) matlab-scan-cache-stats)))
      (when (not sym)
	(set (setq sym (intern (symbol-name thing) matlab-scan-cache-stats)) 0))
      (set sym (1+ (symbol-value sym))))
    (matlab-scan-stats-print 'summary)))

(defun matlab-scan-stats-print (&optional summary)
  "Display stats for scanner hits."
  (interactive "P")
  (let ((res nil))
    (mapatoms (lambda (sym)
		   (push (cons (symbol-name sym) (symbol-value sym)) res))
		 matlab-scan-cache-stats)
    (setq res (sort res (lambda (a b) (string< (car a) (car b)))))
    (if summary
	(when (not noninteractive)
	  ;; show a short form.
	  (message (mapconcat (lambda (pair)
				(concat (car pair) ":" (format "%d" (cdr pair))))
			      res " ")))
      ;; Else, show a long form.
      (let ((printfcn (lambda (pair)
			(princ (concat (format "%-8s" (concat (car pair) ":")) "\t"
				       (format "%d" (cdr pair)) "\n")))))
	(if noninteractive
	    (progn
	      ;; Print to stdout when in batch mode.
	      (princ "\nCache Key\tHits\n")
	      (mapc printfcn res)
	      (princ "---\n"))
	  ;; Display in a buffer
	  (with-output-to-temp-buffer "*MATLAB SCANNER STATS*"
	    (princ "Cache Key\tHits\n----------\t------\n")
	    (mapc printfcn res)))
	))))
  
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
			  ((eq (nth mlf-end-comment-type lvl1) 'commanddual) "-command dual")
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
