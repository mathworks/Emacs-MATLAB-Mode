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


(defun matlab-compute-line-context (level)
  "Compute and return the line context for the current line of MATLAB code.
LEVEL indicates how much information to return.
LEVEL of 1 is the most primitive / simplest data.
This function caches compuated context onto the line it is for, and will
return the cache if it finds it."
  (cond ((= level 1)
	 (let ((ctxt (matlab-scan-cache-get 1)))
	   (unless ctxt
	     (setq ctxt (matlab-compute-line-context-lvl-1))
	     (matlab-scan-cache-put ctxt 1))
	   ctxt))
	(t
	 nil))
  )

(defconst mlf-ltype 0)
(defconst mlf-stype 1)
(defconst mlf-indent 2)
(defconst mlf-paren-depth 3)
(defconst mlf-paren-char 4)
(defconst mlf-paren-col 5)
(defconst mlf-paren-delta 6)
(defconst mlf-end-comment-type 7)
(defconst mlf-end-comment-col 8)

(defun matlab-compute-line-context-lvl-1 ()
  "Compute and return the level1 context for the current line of MATLAB code.
Level 1 contexts are things quickly derived from `syntax-ppss'
and other simple states.
Computes multiple styles of line by checking for multiple types of context
in a single call using fastest methods.
Return list has these fields:
  0 - Primary line type
  1 - Secondary line type
  2 - Indentation
  3 - Parenthisis depth
  4 - Char for innermost beginning paren
  5 - Column of innermost beginning paren
  6 - Parenthisis depth change on this line
  7 - End Comment type (ellipsis, comment, or nil)
  8 - End Comment start column
"
  (save-excursion
    (back-to-indentation)
    (let* ((ppsend (save-excursion (syntax-ppss (point-at-eol))))
	   (pps (syntax-ppss (point)))
	   (ltype 'empty)
	   (stype nil)
	   (cc 0)
	   (paren-depth (nth 0 pps))
	   (paren-char nil)
	   (paren-col nil)
	   (paren-delta (- (car pps) (car ppsend)))
	   (ec-type nil)
	   (ec-col nil)
	   
	  )

      ;; This means we are somewhere inside a cell, array, or arg list.
      ;; Find out the kind of list we are in.
      ;; Being in a multi-line list is valid for all other states like
      ;; empty lines, and block comments
      (when (> (nth 0 pps) 0)
	(save-excursion
	  (goto-char (car (last (nth 9 pps))))
	  (setq paren-char (char-after (point))
		paren-col  (current-column))))

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
			  (t 'block-body))))

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

      (list ltype stype cc paren-depth paren-char paren-col paren-delta ec-type ec-col)
      )))


(defun matlab-compute-line-context-lvl-2 (lvl1)
  "Compute level 2 line contexts for indentation.
These are more expensive checks queued off of a lvl1 context."

  ;; matlab-ltype-help-comm

  ;; block start
  ;; block end
  ;; change in # blocks on this line.

  )

;;; Accessor Utilities
;;
;; Use these to query a context for a piece of data
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
  "Return t if the current line is a comment based on LVL1 cache."
  (and (matlab-line-comment-p lvl1) (eq (nth mlf-stype lvl1) 'indent-ignore)))

(defsubst matlab-line-comment-style (lvl1)
  "Return type type of comment on this line."
  (and (matlab-line-comment-p lvl1) (nth mlf-stype lvl1)))

(defsubst matlab-line-end-comment-column (lvl1)
  "Return column of comment on line, or nil if no comment.
All lines that start with a comment end with a comment."
  (when (eq (nth mlf-end-comment-type lvl1) 'comment)
    (save-excursion
      (goto-char (nth mlf-end-comment-col lvl1))
      (current-column))))

(defsubst matlab-line-ellipsis-p (lvl1)
  "Return if this line ends with a comment.
All lines that start with a comment end with a comment."
  (eq (nth mlf-end-comment-type lvl1) 'ellipsis))

;; Code and Declarations
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

;; Parenthetical blocks
(defsubst matlab-line-close-paren-p (lvl1)
  "Non nil If the current line starts with closing paren (any type.)"
  (eq (car lvl1) 'close-paren))

(defsubst matlab-line-close-paren-char (lvl1)
  "Non nil If the current line starts with closing paren (any type.)"
  (nth mlf-paren-char lvl1))

(defsubst matlab-line-close-paren-col (lvl1)
  "Non nil If the current line starts with closing paren (any type.)"
  (nth mlf-paren-col lvl1))


;;; Scanning Accessor utilities
;;
;; some utilities require some level of buffer scanning to get the answer.
;; Keep those separate so they can depend on the earlier decls.
(defun matlab-line-comment-help-p (lvl1)
  "Return declaration column if the current line is part of a help comment.
Declarations are things like functions and classdefs.
Indentation a help comment depends on the column of the declaration."
  (and (matlab-line-comment-p lvl1)
       (save-excursion
	(beginning-of-line)
	(forward-comment -100000)
	(let ((c-lvl1 (matlab-compute-line-context 1)))
	  (when (matlab-line-declaration-p c-lvl1)
	    (current-indentation)))
	)))


;;; Caching
;;
(defvar matlab-scan-temporal-cache nil
  "Cache of recently computed line contexts.
Used to speed up repeated queries on the same set of lines.")
(make-variable-buffer-local 'matlab-scan-temporal-cache)

(defun matlab-scan-cache-get (level)
  "Get a cached context at level."
  ;; TODO
  nil)

(defun matlab-scan-cache-put (ctxt level)
  "Get a cached context at level."
  ;; TODO
  nil)


(defun matlab-scan-after-change-fcn (start end length)
  "Function run in after change hooks."
  (setq matlab-scan-temporal-cache nil))

(defun matlab-scan-setup ()
  "Setup use of the indent cache for the current buffer."
  (interactive)
  (add-hook 'after-change-functions 'matlab-scan-after-change-fcn t)
  )

(defun matlab-scan-disable ()
  "Setup use of the indent cache for the current buffer."
  (interactive)
  (remove-hook 'after-change-functions 'matlab-scan-after-change-fcn t)
  )

;;; Debugging and Querying
;;
(defun matlab-describe-line-indent-context ()
  "Describe the indentation context for the current line."
  (interactive)
  (let* ((lvl1 (matlab-compute-line-context 1))
	 (paren-char (nth mlf-paren-char lvl1))
	 (open (format "%c" (or paren-char ?\()))
	 (close (format "%c"
			(cond ((not paren-char) ?\))
			      ((= paren-char ?\() ?\))
			      ((= paren-char ?\[) ?\])
			      ((= paren-char ?\{) ?\})
			      (t ??))))
	 (extraopen "")
	 (extraclose "")
	 )
    (when (= (nth mlf-paren-depth lvl1) 0)
      (setq open (propertize open 'face 'shadow)
	    close (propertize close 'face 'shadow)))
    (if (< (nth mlf-paren-delta lvl1) 0)
	(setq extraopen (format "<%d" (abs (nth mlf-paren-delta lvl1))))
      (when (> (nth mlf-paren-delta lvl1) 0)
	(setq extraclose (format "%d>" (nth mlf-paren-delta lvl1)))))

    
    (message "%s%s %s%s%d%s%s %s %s" (nth mlf-ltype lvl1)
	     (format " %s" (or (nth mlf-stype lvl1) ""))
	     ;; paren system
	     extraopen
	     open
	     (nth mlf-paren-depth lvl1)
	     close
	     extraclose
	     (cond ((eq (nth mlf-end-comment-type lvl1) 'comment) "%")
		   ((eq (nth mlf-end-comment-type lvl1) 'ellipsis) "...")
		   (t ""))
	     (if (matlab-line-end-comment-column lvl1)
		 (format " %d" (matlab-line-end-comment-column lvl1))
	       "")
	     )

    )
  )


(provide 'matlab-scan)

;;; matlab-indent.el ends here
