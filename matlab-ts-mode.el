;;; matlab-ts-mode.el --- MATLAB(R) Tree-Sitter Mode -*- lexical-binding: t -*-

;; Version: 8.2.1
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Jul-7-2025
;; Keywords: MATLAB

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tree-sitter, https://tree-sitter.github.io/tree-sitter
;; based matlab mode: `matlab-ts-mode'
;; using https://github.com/acristoffers/tree-sitter-matlab
;;
;; Install tree-sitter-matlab by taking the matlab.EXT (EXT= .dll, .so, .dylib)
;; from the latest release of https://github.com/emacs-tree-sitter/tree-sitter-langs
;; and rename it to ~/.emacs.d/tree-sitter/libtree-sitter-matlab.EXT
;;

;;; Code:

(require 'compile)
(require 'treesit)

(require 'matlab--access)
(require 'matlab--shared)
(require 'matlab-is-matlab-file)
(require 'matlab-sections)
(require 'matlab-ts-mode--builtins)
(require 'matlab-ts-mode--ei)

;;; Customizations

(defgroup matlab-ts nil
  "MATLAB(R) tree-sitter mode."
  :prefix "matlab-ts-mode-"
  :group 'languages)

(defcustom matlab-ts-mode-font-lock-level 3
  "*Level of font lock for MATLAB code.
The \"Standard\" level plus either MLint flycheck or the MATLAB Language
Server gives all syntactic faces along with error indicators.

The \"Standard plus parse errors\" can result in too much use of the
`font-lock-warning-face' when there are syntax errors and is thus
not recommended."
  :type '(choice (const :tag "Minimal" 1)
                 (const :tag "Low" 2)
                 (const :tag "Standard" 3)
                 (const :tag "Standard plus parse errors" 4)))

(defcustom matlab-ts-mode-on-save-fixes
  '(matlab-ts-mode-on-save-fix-name)
  "*List of function symbols which offer to fix *.m files on save.
During save these functions are called and will prompt to fix issues in
*.m files.  Each function gets no arguments, and returns nothing.  They
can move point, but it will be restored for them."
  :type '(repeat (choice :tag "Function: "
                         (matlab-ts-mode-on-save-fix-name))))

(defcustom matlab-ts-mode-highlight-comment-markers t
  "*Highlight triple-x, to-do, and fix-me comment markers?
See \\[matlab-ts-mode-comment-marker-help]."
  :type 'boolean)

(defcustom matlab-ts-mode-enable-mlint-flycheck t
  "*Enable MLint code analyzer via flycheck.
This requires that you install the flycheck package
https://www.flycheck.org can be installed by adding
to your ~/.emacs
  (require \\='package)
  (add-to-list \\='package-archives
               \\='(\"MELPA Stable\" . \"https://stable.melpa.org/packages/\") t)
Then restart Emacs and run
  \\[package-install] RET flycheck RET
You can also install via use-package or other methods."
  :type 'boolean)

(defcustom matlab-ts-mode-electric-ends t
  "*If t, insert end keywords to complete statements and insert % for doc comments.
For example, if you type
   classdef foo<RET>
an end statement will be inserted resulting in:
   classdef foo<RET>
       ^                  <== point here
   end
Insertion of end keywords works well when the code is
indented.  If you are editing code that is not indented,
you may wish to turn this off.

This will also add \"% \" for documentation comments.  For example,
   function foo
   % help for foo<RET>
   %
     ^                  <== \"% \" is inserted and point is here
   end"
  :type 'boolean)

(defgroup matlab-ts-faces nil
  "Faces used by `matlab-ts-mode'."
  :group 'matlab-ts)

(defface matlab-ts-mode-pragma-face
  '((t :inherit font-lock-comment-face
       :bold t))
  "*Face to use for comment lines which start with %#.
These include lines like
  %#ok          // pragmas used by the MATLAB Code Analyzer, mlint
  %#function    // pragmas used by the MATLAB Compiler")

(defface matlab-ts-mode-string-delimiter-face
  '((t :inherit font-lock-string-face
       :bold t))
  "*Face to use for \\='single quote\\=' and \"double quote\" string delimiters.")

(defface matlab-ts-mode-comment-heading-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "*Face for \"%% code section\" headings.")

(defface matlab-ts-mode-comment-to-do-marker-face
  '((((class color) (background light))
     :inherit font-lock-comment-face
     :background "yellow"
     :weight bold)
    (((class color) (background dark))
     :inherit font-lock-comment-face
     :background "yellow4"
     :weight bold))
  ;; Note we split up the markers with spaces below so C-s when editing this file
  ;; doesn't find them.
  (concat "*Face use to highlight "
          "TO" "DO," " FIX" "ME," " and XX" "X markers ignoring case in comments.
Guidelines:
 - FIX" "ME" " and XX" "X markers should be fixed prior to committing
   code to a source repository.
 - TO" "DO markers can remain in code and be committed with the code to a
   source repository.  TO" "DO markers should reflect improvements are
   not problems with the existing code."))

(defface matlab-ts-mode-operator-face
  '((((class color) (background light)) :foreground "navy")
    (((class color) (background dark))  :foreground "LightBlue")
    (t :inverse-video t
       :weight bold))
  "*Face for operators: *, /, +, -, etc.")

(defface matlab-ts-mode-command-arg-face
  '((t :inherit font-lock-string-face
       :slant italic))
  "*Face used to highlight command dual arguments.")

(defface matlab-ts-mode-system-command-face
  '((t :inherit font-lock-builtin-face
       :slant italic))
  "*Face used to highlight \"!\" system commands.")

(defface matlab-ts-mode-property-face
  '((t :inherit font-lock-property-name-face
       :slant italic))
  "*Face used on properties, enumerations, and events.")

(defface matlab-ts-mode-number-face
  '((t :inherit font-lock-constant-face))
  "*Face used for numbers.")

(defface matlab-ts-mode-end-number-face
  '((t :inherit matlab-ts-mode-number-face
       :slant italic))
  "*Face used for \"end\" when used as an array or cell dimension number index.
For example:
  mat1 = [1:10;
          2:2:20];
  mat2 = mat1(2:end, end:end);")

(defface matlab-function-signature-face
  '((t :inherit font-lock-type-face
       :weight bold
       :slant italic))
  "*Face used for classdef abstract method function signature declarations.
In this example, fcn1 will get this face:
  classdef MyClass
      methods(Abstract)
          a = fcn1(obj);
      end
  end")

(defface matlab-ts-mode-variable-override-builtin-face
  '((t :inherit font-lock-variable-name-face
       :underline t))
  "*Face used for variable overriding a builtin.
For example, it is valid to override the disp command:
  disp = 1:10;
and then trying to use disp to display results will not work.")

(defface matlab-ts-region-face
  '((t :inherit region))
  "*Face used to highlight a region of text when prompting for input.
For example, if the function name and *.m file name are different
the function name will be highlighted and you will see prompt
   Function name and file names are different.  Fix?
when saving.")

;;; Global variables used in multiple code ";;; sections"

(defvar matlab-ts-mode--comment-section-heading-re
  "^[ \t]*\\(%%\\(?:[ \t].+\\)?\\)$"
  "Regexp matching \"%% headings\" lines.")

;;; Utilities

(defun matlab-ts-mode--real-node-at-point ()
  "Return \\='(pt . node) where node is not a newline.
The MATLAB tree-sitter grammar includes a newline as a node.
If we are on a newline, this will backup to a real node.
The returned point, pt, reflects that location."
  (let* ((pt (point))
         (node-at-point (treesit-node-at pt))
         ;; Handle case at end of a start node, e.g. point just after properties:
         ;;    properties
         ;;              ^
         ;;        foo;
         ;;    end
         ;; Likewise for comments
         ;;    % comment
         ;;             ^
         (real-node (if (and (equal "\n" (treesit-node-type node-at-point))
                             (> pt 1))
                        (progn
                          (setq pt (1- pt))
                          (treesit-node-at pt))
                      node-at-point)))
    (cons pt real-node)))

(defun matlab-ts-mode--child-last-node (parent-node)
  "Get the logical child last node of PARENT-NODE.
Consider:

  classdef enum1 < uint32
      enumeration
          A (0)
          B  (1)
      end
  end

with parse tree:

  (source_file
   (class_definition classdef name: (identifier)
    (superclasses <
     (property_name (identifier)))
    \\n
    (enumeration enumeration \\n
     (enum (identifier) ( (number) ))
     \\n
     (enum (identifier) ( (number) ))
     \\n end \\n)
    end)
   \\n)

The last node of the \"(enumeration ... \\n)\" is \"\\n\" and the
logical last node is \"end\"."
  (let* ((children (treesit-node-children parent-node))
         (last-idx (1- (length children)))
         (last-node (nth last-idx children)))
    (while (and (>= last-idx 0)
                (string= (treesit-node-type last-node) "\n"))
      (setq last-idx (1- last-idx))
      (setq last-node (nth last-idx children)))
    last-node))

;;; File encoding

(defun matlab-ts-mode--check-file-encoding ()
  "Check/set file encoding.
Error is signaled if contents are corrupt because non-utf8 printable
content can crash Emacs via the matlab tree-sitter parser."

  ;; MCR check. Version info is at start.
  ;; R2025a example: V2MCC8000MEC2000MCR2000<binary-data>
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^V[0-9A-Z]+MCR[0-9]+" t)
      (fundamental-mode)
      (user-error "Not activating matlab-ts-mode because this is MATLAB Compiler Runtime content")))

  ;; We could check for utf-8 and error when non-utf8, but that may cause grief. Suppose
  ;; a malformed character is in a comment. That should be allowed.
  ;;    (when (not (string-match "utf-8" (symbol-name buffer-file-coding-system)))
  ;;       (user-error "Buffer does not have utf-8 encoding"))
  ;; Note we cannot
  ;;       (set-buffer-file-coding-system 'utf-8))
  ;; because this would modify it and modes shouldn't modify the buffer.

  (let ((bad-char-point (save-excursion
                          (goto-char (point-min))
                          ;; Due to the matlab-ts-mode syntax table entry
                          ;;      (modify-syntax-entry ?\n ">"     st)
                          ;; [:space:] doesn't match newlines.
                          (when (re-search-forward "[^[:print:][:space:]\n\r]" nil t)
                            (point)))))
    (when bad-char-point
      (fundamental-mode)
      (goto-char bad-char-point)
      (user-error
       "Not entering matlab-ts-mode due to non-printable utf8 character \"%c\" at point %d"
       (char-before) bad-char-point))))

;;; Syntax table

(defvar matlab-ts-mode--syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; Comment Handling:
    ;; 1. Single line comments: % text (single char start),
    ;;                          note includes "%{ text"
    ;; 2. Multiline comments:   %{
    ;;                            lines
    ;;                          %}
    ;; 3. Ellipsis line continuations comments: "... optional text"
    ;;    are handled in `matlab-ts-mode--syntax-propertize'
    (modify-syntax-entry ?%  "< 13"  st)
    (modify-syntax-entry ?{  "(} 2c" st)
    (modify-syntax-entry ?}  "){ 4c" st)
    ;; \n is a comment ender. Therefore, regex [:space:] won't recognize \n
    (modify-syntax-entry ?\n ">"     st)

    ;; String Handling:
    ;;   Single quoted string (character vector):    'text'
    ;;   Double-quoted string:                       "text"
    ;;   Transpose:           varname'
    ;;   Quoted quotes:       ' don''t '    or " this "" "
    ;;   Unterminated Char V: ' text
    (modify-syntax-entry ?'  "\"" st)
    (modify-syntax-entry ?\" "\"" st)

    ;; Words and Symbols:
    (modify-syntax-entry ?_  "_" st)

    ;; Punctuation:
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?*  "." st)
    (modify-syntax-entry ?/  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)

    ;; Parenthetical blocks:
    ;;   Note: these are in standard syntax table, repeated here for completeness.
    (modify-syntax-entry ?\(  "()" st)
    (modify-syntax-entry ?\)  ")(" st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?{   "(}" st)
    (modify-syntax-entry ?}   "){" st)

    st)
  "The matlab-ts-mode syntax table.")

(defun matlab-ts-mode--put-char-category (pos category)
  "At character POS, put text property CATEGORY."
  (when (not (eobp))
    (put-text-property pos (1+ pos) 'category category)
    (put-text-property pos (1+ pos) 'mcm t)))

(defmacro matlab-ts-mode--syntax-symbol (symbol syntax doc)
  "Create a new SYMBOL with DOC used as a text property category with SYNTAX."
  (declare (indent defvar) (debug (sexp form sexp)) (doc-string 3))
  `(progn (defconst ,symbol ,syntax ,doc)
          (put ',symbol 'syntax-table ,symbol)))

;; In the Syntax Table descriptors, "<" is for comments
(matlab-ts-mode--syntax-symbol matlab-ts-mode--ellipsis-syntax (string-to-syntax "<")
                               "Syntax placed on ellipsis to treat them as comments.")

(defun matlab-ts-mode--syntax-propertize (&optional start end)
  "Scan region between START and END to add properties.
If region is not specified, scan the whole buffer.
This will mark ellipsis line continuation's
  ... optional text
as comments which is how they are treated by MATLAB."
  (save-match-data ;; avoid 'Syntax Checking transmuted the match-data'
    (save-excursion
      ;; Scan region, but always expand to beginning of line
      (goto-char (or start (point-min)))
      (forward-line 0)

      ;; Edits can change what properties characters can have so remove ours and reapply
      (remove-text-properties (point) (save-excursion
                                        (goto-char (or end (point-max)))
                                        (let ((inhibit-field-text-motion t)) (end-of-line))
                                        (point))
                              '(category nil mcm nil))

      ;; Tell Emacs that ellipsis (...) line continuations are comments.
      (while (and (not (>= (point) (or end (point-max)))) (not (eobp)))
        (if (treesit-search-forward-goto (treesit-node-at (point))
                                         (rx bos "line_continuation" eos)
                                         t) ;; goto start of: ... optional text
            (matlab-ts-mode--put-char-category (point) 'matlab-ts-mode--ellipsis-syntax)
          (goto-char (point-max)))))))

;;; font-lock

(defvar matlab-ts-mode--keywords
  ;; Nodes like "if" are captured by their text because they are part of a bigger node that captures
  ;; them as such (and need more than just their text to define the node), but it doesn't make much
  ;; sense to create a node for the text "break", "continue", etc. because that would create two
  ;; nodes for the same purpose, where one is sufficient.  In other words, "break" like nodes are
  ;; captured as named nodes, not as unnamed ones, so you need to use their node names instead of
  ;; the "content".  See https://github.com/acristoffers/tree-sitter-matlab/issues/25
  ;;
  ;; Keywords are documented here https://www.mathworks.com/help/matlab/ref/iskeyword.html
  ;; Note, arguments, methods, properties are semi-keywords in that in the right location
  ;; the are keywords, otherwise in the wrong location they are variables, but tree-sitter
  ;; correctly handles them by letting us look for these as content of the nodes.
  '("arguments"
    (break_statement)
    "case"
    "catch"
    "classdef"
    (continue_statement)
    "else"
    "elseif"
    "end"
    "enumeration"
    "events"
    "for"
    "function"
    "get." ;; when used in a classdef method, e.g. function value = get.propName(obj)
    "global"
    "if"
    "methods"
    "otherwise"
    "parfor"
    "persistent"
    "properties"
    (return_statement)
    "set." ;; when used in a classdef method, e.g. function obj = set.propName(obj, val)
    "spmd"
    "switch"
    "try"
    "while")
  "The matlab-ts-mode font-lock keywords.")

(defvar matlab-ts-mode--operators
  ;; https://www.mathworks.com/help/matlab/matlab_prog/matlab-operators-and-special-characters.html
  '("+"         ;; Addition or Unary plus, +value
    "-"         ;; Subtraction or Unary minus, -value
    ".*"        ;; Element-wise multiplication
    "*"         ;; Matrix multiplication
    "./"        ;; Element-wise right division
    "/"         ;; Matrix right division
    ".\\"       ;; Element-wise left division
    "\\"        ;; Matrix left division (also known as backslash)
    ".^"        ;; Element-wise power
    "^"         ;; Matrix power
    ".'"        ;; Transpose
    "'"         ;; Complex conjugate transpose
    "=="        ;; Equal to
    "~="        ;; Not equal to
    ">"         ;; Greater than
    ">="        ;; Greater than or equal to
    "<"         ;; Less than
    "<="        ;; Less than or equal to
    "&"         ;; Find logical AND
    "|"         ;; Find logical OR
    "&&"        ;; Find logical AND (with short-circuiting)
    "||"        ;; Find logical OR (with short-circuiting)
    "~"         ;; Find logical NOT
    "@"         ;; Create anonymous functions and function handles, call superclass methods
    ;; "!"      ;; "!" is like an operator, but has command-dual like syntax, so handled elsewhere
    "?"         ;; Retrieve metaclass information for class name
    "~"         ;; Represent logical NOT, suppress specific input or output arguments.
    "="         ;; Variable creation and indexing assignment.
    "<" "&"     ;; Specify one or more superclasses in a class definition.
    ".?"        ;; Specify the fields of a name-value structure as the names of all
    ;;           ;   writable properties of the class.
    ;;           ;   However, this isn't parsed correctly by matlab tree-sitter, see
    ;;           ;   https://github.com/acristoffers/tree-sitter-matlab/issues/35
    )
  "The matlab-ts-mode font-lock operators.")

(defvar matlab-ts-mode--type-functions
  '("double"
    "single"
    "int8"
    "int16"
    "int32"
    "int64"
    "uint8"
    "uint16"
    "uint32"
    "uint64")
  "The matlab-ts-mode data type functions.")

(cl-defun matlab-ts-mode--get-doc-comment-candidate (comment-node)
  "Get candidate doc comment node or nil.
Return a comment node based on COMMENT-NODE if it is a candidate for a
help doc comment."

  ;; Backup over a copyright comment line
  (when (string-match-p "\\`[ \t]*%[ \t]*copyright\\b"
                        (treesit-node-text comment-node))
    (setq comment-node (treesit-node-prev-sibling comment-node))
    (when (not (string= "comment" (treesit-node-type comment-node)))
      (cl-return-from matlab-ts-mode--get-doc-comment-candidate)))

  (let ((prev-node (treesit-node-prev-sibling comment-node)))
    (when prev-node

      (while (and prev-node
                  (string-match-p (rx bos (or "line_continuation" "\n") eos)
                                  (treesit-node-type prev-node)))
        (setq prev-node (treesit-node-prev-sibling prev-node)))

      (let ((prev-type (or (treesit-node-type prev-node) "")))
        ;; The true (t) cases. Note line continuation ellipsis are allowed.
        ;;    function foo          function foo(a)
        ;;    % doc comment         % doc comment
        ;;    end                   end
        (when (or (string-match-p (rx bos (or
                                           "function_arguments" ;; function input args?
                                           "superclasses")      ;; subclass
                                      eos)
                                  prev-type)
                  (and (string= prev-type "identifier")           ;; id could be a fcn or class id
                       (let* ((prev-sibling (treesit-node-prev-sibling prev-node))
                              (prev-type (and prev-sibling (treesit-node-type prev-sibling))))
                         (and prev-type
                              (or
                               (string-match-p
                                (rx bos
                                    (or "function"         ;; fcn without in and out args
                                        "function_output"  ;; fcn w/out args and no in args
                                        "classdef")        ;; base class
                                    eos)
                                prev-type)
                               (and (string= prev-type "attributes")
                                    (equal (treesit-node-type (treesit-node-parent prev-sibling))
                                           "class_definition")))))))
          comment-node)))))

(defun matlab-ts-mode--is-doc-comment (comment-node parent)
  "Is the COMMENT-NODE under PARENT a help doc comment.
In MATLAB,

  function out = myFunction
  % The documentation help comment for myFunction immediately follows the
  % function definition.

      % code comments are preceded with a blank line
      out = 1;
  end

  function out = myFunction
  % The documentation help comment for myFunction immediately follows the
  % function definition.

  % copyright at column 0 and preceded by blank lines after the help comment

      % code comments are preceded with a blank line
      out = 1;
  end

  function out = myFunctionWithoutHelp

      % code comments are preceded with a blank line
      out = 1;
  end

Similar behavior for classdef's."

  (setq comment-node (matlab-ts-mode--get-doc-comment-candidate comment-node))
  (when (and comment-node
             (string-match-p (rx bos (or "function_definition" "class_definition") eos)
                             (treesit-node-type parent)))
    (let ((definition-point (treesit-node-start parent)))
      (save-excursion
        (goto-char (treesit-node-start comment-node))
        (forward-line 0)

        ;; result - is doc comment?
        (or (<= (point) definition-point) ;; at definition?
            (and (> (point) definition-point)
                 (not (re-search-backward "^[ \t]*$" definition-point t))))))))

(defun matlab-ts-mode--doc-comment-capture (comment-node override start end &rest _)
  "Fontify function/classdef documentation comments.
COMMENT-NODE is the tree-sitter node from a treesit-font-lock-rules rule
and OVERRIDE is from that rule.  START and END specify the region to be
fontified which could be smaller or larger than the COMMENT-NODE
start-point and end-point."
  (when (matlab-ts-mode--is-doc-comment comment-node (treesit-node-parent comment-node))
    (treesit-fontify-with-override
     (treesit-node-start comment-node) (treesit-node-end comment-node)
     font-lock-doc-face override start end)))

(defun matlab-ts-mode--comment-heading-capture (comment-node override start end &rest _)
  "Fontify the \"%% heading\" start line in COMMENT-NODE.
COMMENT-NODE is the tree-sitter node from a treesit-font-lock-rules rule
and OVERRIDE is from that rule.  START and END specify the region to be
fontified which could be smaller or larger than the COMMENT-NODE
start-point and end-point."
  (save-excursion
    (goto-char (treesit-node-start comment-node))
    (forward-line 0)
    (when (looking-at matlab-ts-mode--comment-section-heading-re)
      (let ((heading-start (match-beginning 1))
            (heading-end (match-end 1)))
        (treesit-fontify-with-override heading-start heading-end
                                       'matlab-ts-mode-comment-heading-face
                                       override start end)))))

(defun matlab-ts-mode--comment-pragma-capture (comment-node override start end &rest _)
  "Fontify %#pragma, %-indent-mode=minimal, %-indent=mode=full.
COMMENT-NODE is the tree-sitter comment node from a
treesit-font-lock-rules rule and OVERRIDE is from that rule.  START and
END specify the region to be fontified which could be smaller or larger
than the COMMENT-NODE start-point and end-point."
  ;; Note comment-node can contain many single-line comments, e.g. this is one comment node:
  ;;   %#ok
  ;;   % some other comment
  (save-excursion
    (goto-char (treesit-node-start comment-node))
    (let ((comment-end (treesit-node-end comment-node)))
      (while (< (point) comment-end)
        (if (re-search-forward
             ;; String below is the efficient form of
             ;;    (rx (group (or (seq "%" "#" (1+ (not space)))
             ;;                   (seq "%-indent-mode=minimal" word-end)
             ;;                   (seq "%-indent-mode=full" word-end)))
             "\\(%\\(?:#[^ \t\n\r]+\\|\\(?:-indent-mode=\\(?:minimal\\|full\\)\\)\\>\\)\\)"
             comment-end t)
            (let ((pragma (match-string 1))
                  (pragma-start (match-beginning 1))
                  (pragma-end (match-end 1)))
              (when (or (string-match-p "^%#" pragma) ;; %#pragma can be on any line
                        (save-excursion            ;; %-indent-mode=* must be on line by itself
                          (goto-char pragma-start)
                          (forward-line 0)
                          (= (1- (re-search-forward "[^ ]")) pragma-start)))
                (treesit-fontify-with-override pragma-start pragma-end
                                               'matlab-ts-mode-pragma-face
                                               override start end)))
          (goto-char comment-end))))))

(defvar matlab-ts-mode--comment-markers
  (list (concat "XX" "X")
        (concat "FIX" "ME")
        (concat "TO" "DO")))

(defvar matlab-ts-mode--comment-markers-re
  (rx-to-string `(seq word-start (group (or ,@matlab-ts-mode--comment-markers) word-end))))

(defun matlab-ts-mode--comment-to-do-capture (comment-node override start end &rest _)
  "Fontify triple-x, fix me, and to do markers in comments.
For guidelines on using these comment markers see:
  \\[matlab-ts-mode-comment-marker-help]
COMMENT-NODE is the tree-sitter comment node from a
treesit-font-lock-rules rule and OVERRIDE is from that rule.  START and
END specify the region to be fontified which could be smaller or larger
than the COMMENT-NODE start-point and end-point."
  (when matlab-ts-mode-highlight-comment-markers
    (save-excursion
      (let ((comment-end (treesit-node-end comment-node)))
        (goto-char (treesit-node-start comment-node))
        (while (< (point) comment-end)
          ;; Note, the markers below have spaces in them so we don't find them when searching "C-s"
          ;; while editing this file.
          (if (re-search-forward matlab-ts-mode--comment-markers-re comment-end t)
              (let ((keyword-start (match-beginning 1))
                    (keyword-end (match-end 1)))
                (treesit-fontify-with-override keyword-start keyword-end
                                               'matlab-ts-mode-comment-to-do-marker-face
                                               override start end))
            (goto-char comment-end)))))))

(defun matlab-ts-mode--namespace-path (namespace-node)
  "Get the a.b.c path of NAMESPACE-NODE."
  (let ((children (treesit-node-children namespace-node))
        path)
    (cl-loop for child in children do
             (let ((child-type (treesit-node-type child)))
               (cond
                ;; Case: (identifier) or "."
                ((or (string= child-type "identifier")
                     (string= child-type "."))
                 (setq path (concat path (treesit-node-text child))))
                ;; Case: (function_call)
                ((string= child-type "function_call")
                 ;; Consider: a.b.c{1}{2} or a.b.c{1}(1)
                 (let ((fcn-name-node (treesit-node-child-by-field-name child "name")))
                   (let ((fcn-name-type (treesit-node-type fcn-name-node)))
                     (while (string= fcn-name-type "function_call")
                       (setq fcn-name-node (treesit-node-child-by-field-name fcn-name-node "name")
                             fcn-name-type (treesit-node-type fcn-name-node)))
                     (if (string= fcn-name-type "field_expression")
                         (setq path (concat path (matlab-ts-mode--namespace-path fcn-name-node)))
                       (setq path (concat path (treesit-node-text fcn-name-node)))))))
                ;; Case: other - shouldn't be able to get here, but be safe.
                (t
                 (cl-return)))))
    path))

(defun matlab-ts-mode--namespace-builtins-capture (namespace-node override start end &rest _)
  "Fontify foo.bar.goo when it is a builtin function.
NAMESPACE-NODE is the tree-sitter field_expression or a superclass
property_name node.  These nodes have children that for a PATH,
e.g. \"foo.bar.goo\".  This is connected to the namespace-builtins
treesit-font-lock-rules rule and OVERRIDE is from that rule.  START and
END specify the region to be fontified which could be smaller or larger
than the FILED-EXPRESSION-NODE start-point and end-point."
  (let ((path (matlab-ts-mode--namespace-path namespace-node))) ;; The "path" e.g. foo.bar.goo
    (let ((builtin-type (gethash path matlab-ts-mode--builtins-ht)))
      (when builtin-type
        (let ((builtin-start (treesit-node-start namespace-node))
              builtin-end
              prop-start
              prop-end)
          (if (eq builtin-type t)
              (setq builtin-end (+ builtin-start (length path)))
            ;; builtin-type is 'property or 'enumeration
            (when (not (string-match "\\`\\(.+\\.\\)\\([^.]+\\)\\'" path))
              (error "Assert: failed to parse path, %s" path))
            (setq builtin-end (+ builtin-start (- (length (match-string 1 path)) 2)))
            ;; We give the "." before the property or enum font-lock-delimiter-face, hence the +2.
            (setq prop-start (+ builtin-end 2))
            (setq prop-end (+ prop-start (length (match-string 2 path)))))

          (treesit-fontify-with-override builtin-start builtin-end 'font-lock-builtin-face
                                         override start end)
          (when prop-start
            (treesit-fontify-with-override prop-start prop-end 'matlab-ts-mode-property-face
                                           override start end)))))))

(defun matlab-ts-mode--is-fcn-name-value (identifier-node)
  "Return t if IDENTIFIER-NODE is a function name-value property."
  (let ((next-sibling (treesit-node-next-sibling identifier-node)))
    (and next-sibling
         (string= (treesit-node-text next-sibling) "="))))

(defun matlab-ts-mode--is-identifier-builtin (identifier-node)
  "Return t if IDENTIFIER-NODE is a function provided with MATLAB."
  (let ((parent (treesit-node-parent identifier-node)))
    ;; Consider
    ;;    foo.bar.disp.goo = 1;  % both foo and disp are builtin functions, but in this case fields
    ;;    foo.bar(1)             % +foo/bar.m - bar is a builtin, but foo.bar(1) is not.
    (when (and (not (equal (treesit-node-type parent) "field_expression"))
               (not (equal (treesit-node-type (treesit-node-parent parent)) "field_expression")))
      (let ((id (treesit-node-text identifier-node)))
        (gethash id matlab-ts-mode--builtins-ht)))))

(defun matlab-ts-mode--is-command-builtin (command-node)
  "Return t if COMMAND-NODE is a function provided with MATLAB."
  (let ((command (treesit-node-text command-node)))
    (gethash command matlab-ts-mode--builtins-ht)))

(defun matlab-ts-mode--is-variable-overriding-builtin (variable-node)
  "Is VARIABLE-NODE overriding a builtin?
Example, disp variable is overriding the disp builtin function:
   disp = 1:10;"
  (let ((variable (treesit-node-text variable-node)))
    (or (gethash variable matlab-ts-mode--builtins-ht)
        ;; Initially arguments, etc. capabilities didn't exist in MATLAB. When they were added,
        ;; compatibility was kept when these were used as variables. So, they are "semi-keywords".
        (string-match-p (rx bos (or "arguments" "enumeration" "events" "methods" "properties") eos)
                        (treesit-node-text variable-node)))))

(defvar matlab-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; F-Rule: Comments and line continuation: ... optional text
   :language 'matlab
   :feature 'comment
   '(
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_types.m
     (comment) @font-lock-comment-face
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation_fcn.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation_fcn_g2.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation_multiArgFcn.m
     (line_continuation) @font-lock-comment-face)

   ;; F-Rule: special comments that override normal comment font
   :language 'matlab
   :feature 'comment-special
   :override t
   '(;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_pragma_in_fcn.m
     ((comment) @matlab-ts-mode--comment-pragma-capture)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_heading.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_sections.m
     ((comment) @matlab-ts-mode--comment-heading-capture) ;; %% comment section heading
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_no_doc_help.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_fcn.m
     (function_definition (comment) @matlab-ts-mode--doc-comment-capture) ;; doc help comments
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_MyClass.m
     (class_definition (comment) @matlab-ts-mode--doc-comment-capture)) ;; doc help comments

   ;; F-Rule: to do, fix me, triple-x marker comment keywords
   ;; See: test-matlab-ts-mode-font-lock-files/font_lock_comment_markers.m
   :language 'matlab
   :feature 'comment-marker
   :override t
   '(((comment) @matlab-ts-mode--comment-to-do-capture)
     ((line_continuation) @matlab-ts-mode--comment-to-do-capture))

   ;; F-Rule: Constant literal numbers, e.g. 1234, 12.34, 10e10
   ;; We could use this for items like true, false, pi, etc. See some of these numbers in:
   ;;   https://www.mathworks.com/content/dam/mathworks/fact-sheet/
   ;;   matlab-basic-functions-reference.pdf
   ;; however, they do show up as builtins, which to me seems more accurate.
   ;; This rule needs to come before the "F-Rule: keywords: if, else, end, etc." because
   ;; we want the end_keyword when used as a number index into a cell/matrix to be a number font.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_numbers.m
   :language 'matlab
   :feature 'number
   '(((number) @matlab-ts-mode-number-face)
     ((end_keyword) @matlab-ts-mode-end-number-face))

   ;; F-Rule: variable
   ;; Could add font-lock-variable-name-face to variable uses.  Consider
   ;;     i1 = [1, 2];
   ;;     i2 = i1(1) + i3 + i4(i3);
   ;; we know i1 and i2 are variables from the (assignment left: (identifier))
   ;; However, we don't know if i3 or i4 are variables or functions because a function
   ;; can be called with no arguments, e.g. to call i3 function use i3 or i3(). i4 could
   ;; be a variable indexed by i1 or a function.
   ;;
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_variable.m
   :language 'matlab
   :feature 'variable
   '(((assignment left: (identifier) @matlab-ts-mode-variable-override-builtin-face
                  (:pred matlab-ts-mode--is-variable-overriding-builtin
                         @matlab-ts-mode-variable-override-builtin-face)))
     (assignment left: (identifier) @font-lock-variable-name-face)
     (multioutput_variable (identifier) @matlab-ts-mode-variable-override-builtin-face
                           (:pred matlab-ts-mode--is-variable-overriding-builtin
                                  @matlab-ts-mode-variable-override-builtin-face))
     (multioutput_variable (identifier) @font-lock-variable-name-face)
     (global_operator (identifier) @matlab-ts-mode-variable-override-builtin-face
                      (:pred matlab-ts-mode--is-variable-overriding-builtin
                             @matlab-ts-mode-variable-override-builtin-face))
     (global_operator (identifier) @font-lock-variable-name-face)
     (persistent_operator (identifier) @matlab-ts-mode-variable-override-builtin-face
                          (:pred matlab-ts-mode--is-variable-overriding-builtin
                                 @matlab-ts-mode-variable-override-builtin-face))
     (persistent_operator (identifier) @font-lock-variable-name-face)
     (for_statement (iterator (identifier) @matlab-ts-mode-variable-override-builtin-face
                              (:pred matlab-ts-mode--is-variable-overriding-builtin
                                     @matlab-ts-mode-variable-override-builtin-face)))
     (for_statement (iterator (identifier) @font-lock-variable-name-face))
     ;; Function inputs: functionName(in1, in2, in3)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_small_in_args.m
     (function_arguments arguments:
                         (identifier)      @font-lock-variable-name-face
                         ("," (identifier) @font-lock-variable-name-face) :*)
     ;; Function single output argument: function out = functionName(in1, in2)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_small_out_args.m
     (function_output (identifier) @font-lock-variable-name-face)
     ;; Function multiple output arguments: function [out1, out2] = functionName(in1, in2)
     (function_output (multioutput_variable (identifier) @font-lock-variable-name-face))
     ;; Enumeration's
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_enum.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_enum_FlowRate.m
     (enum (identifier) @matlab-ts-mode-property-face)
     ;; Events block in classdef
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_events.m
     (events (identifier) @matlab-ts-mode-property-face)
     ;; Namespaces, structs, classdef methods/property access. Note any keyword is allowed,
     ;; e.g. foo.methods.function = 1;
     (field_expression (identifier) @default))

   ;; F-Rule: keywords: if, else, end, etc.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_keywords.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_methods.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_keyword_spmd.m
   :language 'matlab
   :feature 'keyword
   `([,@matlab-ts-mode--keywords] @font-lock-keyword-face)

   ;; F-Rule: Types, e.g. int32()
   :language 'matlab
   :feature 'type
   `((function_call name: (identifier)
                    @font-lock-type-face
                    (:match ,(rx-to-string `(seq bos
                                                 (or ,@matlab-ts-mode--type-functions)
                                                 eos)
                                           t)
                            @font-lock-type-face))
     (property name: (identifier) (identifier) @font-lock-type-face :?)
     (property name: (property_name (identifier)) (identifier) @font-lock-type-face :?))

   ;; F-Rule: namespaces (the +dir's, class methods, etc.)
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_namespaces.m
   :language 'matlab
   :feature 'namespace-builtins
   :override t
   `((superclasses (property_name) @matlab-ts-mode--namespace-builtins-capture)
     (field_expression) @matlab-ts-mode--namespace-builtins-capture)

   ;; F-Rule: factory items that come with MATLAB, Simulink, or add-on products
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_builtins.m
   :language 'matlab
   :feature 'builtins
   `(((identifier) @font-lock-builtin-face
      (:pred matlab-ts-mode--is-identifier-builtin @font-lock-builtin-face))
     ((command_name) @font-lock-builtin-face
      (:pred matlab-ts-mode--is-command-builtin @font-lock-builtin-face)))

   ;; F-Rule: function/classdef and items defining them, e.g. the function arguments
   :language 'matlab
   :feature 'definition
   '(
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_issue55_abstract.m
     (function_signature name: (identifier) @matlab-function-signature-face)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_small_no_args.m
     (function_definition name: (identifier) @font-lock-function-name-face)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_symPosDef.m
     (class_definition name: (identifier) @font-lock-function-name-face)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_MySubClass.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_MySubSubClass.m
     (superclasses (property_name (identifier)) @font-lock-function-name-face)
     ;; Fields of: arguments ... end , properties ... end
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_arguments.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_properties.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_MultiplePropBlocks.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_prop_access.m
     (property (validation_functions (identifier) @font-lock-function-call-face))
     (property name: (identifier) @matlab-ts-mode-property-face)
     (property name: (property_name (identifier) @matlab-ts-mode-property-face))
     ;; Attributes of properties, methods
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_attributes.m
     (attribute (identifier) @font-lock-type-face "=" (identifier) @font-lock-builtin-face)
     (attribute (identifier) @font-lock-type-face))

   ;; F-Rule: validation functions from namespace
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_arguments2_issue57.m
   :language 'matlab
   :feature 'definition
   :override t
   '((property (validation_functions (field_expression (identifier) @font-lock-function-call-face)))
     (property (validation_functions ((field_expression
                                       (function_call name: (identifier)
                                                      @font-lock-function-call-face))))))

   ;; F-Rule: Function Name = Value arguments
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_name_value_properties.m
   :language 'matlab
   :feature 'fcn-name-value
   '(((function_call (arguments (identifier) @matlab-ts-mode-property-face))
      (:pred matlab-ts-mode--is-fcn-name-value @matlab-ts-mode-property-face)))

   ;; F-Rule: command dual arguments
   :language 'matlab
   :feature 'command-arg
   '((command_argument) @matlab-ts-mode-command-arg-face)

   ;; F-Rule: system and command dual commands.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_command.m
   :language 'matlab
   :feature 'command-name
   '(((command_name) @matlab-ts-mode-system-command-face
      ;; System command: ! ls *.m *.txt
      (:match "^!" @matlab-ts-mode-system-command-face))
     ;; Command-dual with at least one arg: myFunction arg
     ;; Commands with no args could be a variable or a function
     ((command (command_name) @font-lock-function-call-face (command_argument))))

   ;; F-Rule: strings "double quote" and 'single quote'
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_strings.m
   :language 'matlab
   :feature 'string
   '((string_content) @font-lock-string-face
     ((string_content) ["\"" "'"]) @matlab-ts-mode-string-delimiter-face
     (string ["\"" "'"] @matlab-ts-mode-string-delimiter-face)
     (escape_sequence) @font-lock-escape-face
     (formatting_sequence) @font-lock-escape-face)

   ;; F-rule: operators: *, /, +, -, etc.
   ;; Note, this rule must come after the string rule because single quote (') can be a transpose or
   ;; string delimiter.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_operators.m
   :language 'matlab
   :feature 'operator
   `([,@matlab-ts-mode--operators] @matlab-ts-mode-operator-face)

   ;; F-Rule: Brackets
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_brackets.m
   :language 'matlab
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   ;; F-Rule: Delimiters, e.g. semicolon
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_delimiters.m
   :language 'matlab
   :feature 'delimiter
   '((["." "," ":" ";"]) @font-lock-delimiter-face)

   ;; F-Rule: Syntax errors
   ;; Some errors span too much of the buffer's text, so one will probably not want this
   ;; enabled. Perhaps, once https://github.com/acristoffers/tree-sitter-matlab/issues/36
   ;; is fixed we can enable this, though a lot more testing is needed.
   ;;
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_error.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_error_small.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_error_missing_continuation.m
   :language 'matlab
   :feature 'syntax-error
   :override t
   '((ERROR) @font-lock-warning-face)
   )
  "The matlab-ts-mode font-lock settings.")

;;; Indent (see doc/matlab_code_indent.org)

(defvar matlab-ts-mode--indent-level 4
  "Indentation level.")
(defvar matlab-ts-mode--switch-indent-level (/ matlab-ts-mode--indent-level 2)
  "Indentation level for switch-case statements.")
(defvar matlab-ts-mode--array-indent-level 2
  "Indentation level for elements in an array.")

(defvar matlab-ts-mode--i-error-row-matcher-pair)

(defun matlab-ts-mode--i-error-row-matcher (_node _parent bol &rest _)
  "Is point within an ERROR node and can we determine indent?
If so, set `matlab-ts-mode--i-error-row-matcher-pair' to the anchor and
offset we should use and return non-nil.  BOL, beginning-of-line point,
is where we start looking for the error node."
  ;; 1) Given
  ;;            mat = [ [1, 2]; [3, 4];
  ;;        TAB>
  ;;    we have parse tree
  ;;        (ERROR (identifier) = [
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;)
  ;;
  ;; 2) Now add ellipsis continuation:
  ;;            mat = [ [1, 2]; [3, 4]; ...
  ;;        TAB>
  ;; we'll have parse tree
  ;;       (source_file
  ;;        (ERROR (identifier) = [
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;)
  ;;        (line_continuation))
  ;; Notice the line_continuation is not under the ERROR node, so we need to find that,
  ;; hence below we navigate over line_continuation's.
  ;;
  ;; See https://github.com/acristoffers/tree-sitter-matlab/issues/46

  (save-excursion
    (goto-char bol)

    (when (string= (treesit-node-type (treesit-node-at (point))) "\n")
      (re-search-backward "[^ \t\n\r]" nil t))

    ;; Move inside the error node if at an error node.
    (if (string= (treesit-node-type (treesit-node-at (point))) "ERROR")
        (re-search-backward "[^ \t\n\r]" nil t)
      ;; If on a "[" or "{" move back (we don't want to shift the current line)
      (when (string-match-p (rx bos (or "[" "{") eos) (treesit-node-type (treesit-node-at (point))))
        (re-search-backward "[^ \t\n\r]" nil t)))

    ;; Walk over line_continuation, ",", ";" and identify the "check node" we should be looking
    ;; at.
    (let ((check-node (treesit-node-at (point))))
      (while (let ((check-type (treesit-node-type check-node)))
               (cond
                ((string-match-p (rx bos (or "line_continuation" "," ";") eos) check-type)
                 (goto-char (treesit-node-start check-node))
                 (if (re-search-backward "[^ \t\n\r]" nil t)
                     t
                   (setq check-node nil)))
                ((and (string-match-p (rx bos "ERROR" eos) check-type)
                      (looking-at ";" t))
                 (goto-char (1- (point)))
                 (when (looking-at "[ \t\n\r]" t)
                   (re-search-backward "[^ \t\n\r]" nil t))
                 t)))
        (if (not (bobp))
            (let* ((pt (point))
                   (node-at-pt (treesit-node-at pt)))
              ;; Be robust to node-at-point range not covering pt
              ;; See: https://github.com/acristoffers/tree-sitter-matlab/issues/116
              ;; TODO - do we need to do this elsewhere?
              (while (and node-at-pt
                          (or (<  pt (treesit-node-start node-at-pt))
                              (>= pt (treesit-node-end   node-at-pt))))
                (setq node-at-pt (treesit-node-parent node-at-pt)))
              (setq check-node node-at-pt))
          ;; at start of buffer
          (setq check-node nil)))

      (when check-node
        ;; Look to see if we are under an ERROR node
        (let ((error-node (treesit-node-parent check-node)))
          (while (and error-node
                      (not (string= (treesit-node-type error-node) "ERROR")))
            (setq error-node (treesit-node-parent error-node)))

          ;; In an error-node, see if this error is due to an incomplete cell or matrix,
          ;; if so return the start point of the row to anchor against.
          (when error-node
            (while (and check-node
                        (not (string-match-p (rx bos (or "row" "[" "{") eos)
                                             (treesit-node-type check-node))))
              (setq check-node (treesit-node-parent check-node)))

            (when check-node
              (cond
               ((string-match-p (rx bos (or "[" "{") eos) (treesit-node-type check-node))
                (setq matlab-ts-mode--i-error-row-matcher-pair
                      (cons (treesit-node-start check-node) 2)))
               ;; Case: looking at row?
               (t
                ;; Find first row to anchor against
                (let ((prev-sibling (treesit-node-prev-sibling check-node)))
                  (while prev-sibling
                    (when (string= (treesit-node-type prev-sibling) "row")
                      (setq check-node prev-sibling))
                    (setq prev-sibling (treesit-node-prev-sibling prev-sibling))))
                ;; In an ERROR node of a matrix or cell, return anchor
                (setq matlab-ts-mode--i-error-row-matcher-pair
                      (cons (treesit-node-start check-node) 0)))))))))))

(defun matlab-ts-mode--i-error-row-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-error-row-matcher'."
  (car matlab-ts-mode--i-error-row-matcher-pair))

(defun matlab-ts-mode--i-error-row-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-error-row-matcher'."
  (cdr matlab-ts-mode--i-error-row-matcher-pair))

(defun matlab-ts-mode--i-doc-block-comment-matcher (node parent bol &rest _)
  "Is NODE, PARENT within a function/classdef doc block comment \"%{ ... %}\"?
BOL, beginning-of-line point, is where we start looking for the error
node."
  (and (not node)
       (string= "comment" (treesit-node-type parent))
       (save-excursion
         (goto-char (treesit-node-start parent))
         (looking-at "%{" t))
       (not (save-excursion (goto-char bol)
                            (looking-at "%" t)))
       (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent))))

(defvar matlab-ts-mode-i-doc-comment-matcher-pair)

(defun matlab-ts-mode--i-doc-comment-matcher (node parent bol &rest _)
  "Is NODE, PARENT, BOL a function/classdef doc comment?"
  (let (doc-comment-node)
    (cond
     ((and (string= "comment" (or (treesit-node-type node) ""))
           (matlab-ts-mode--is-doc-comment node parent))
      (setq doc-comment-node parent))

     ((and (not node)
           (string= "comment" (treesit-node-type parent))
           (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent)))
      ;; Case:        function a = foo(b) %#ok
      ;;        TAB>  % doc comment
      ;; tests/test-matlab-ts-mode-indent-files/indent_mlint_suppression_on_fcn.m
      (setq doc-comment-node (treesit-node-parent parent))))

    (when doc-comment-node
      (setq matlab-ts-mode-i-doc-comment-matcher-pair
            (cons (treesit-node-start doc-comment-node)
                  (if (save-excursion
                        (goto-char bol)
                        (and (not (looking-at "%"))
                             (re-search-backward "^[ \t]*%{[ \t]*$"
                                                 (treesit-node-start (treesit-node-at (point))) t)))
                      2
                    0))))))

(defun matlab-ts-mode--i-doc-comment-anchor (_node _parent _bol &rest _)
  "Return anchor for the function/classdef doc comment based on PARENT."
  (car matlab-ts-mode-i-doc-comment-matcher-pair))

(defun matlab-ts-mode--i-doc-comment-offset (_node _parent _bol &rest _)
  "Return offset for the function/classdef doc comment based on PARENT."
  (cdr matlab-ts-mode-i-doc-comment-matcher-pair))

(defun matlab-ts-mode--i-in-block-comment-matcher (node parent bol &rest _)
  "Is NODE, PARENT within a block \"{% ... %)\" and BOL does not start with \"%\"?"
  (and (not node)
       (string= "comment" (treesit-node-type parent))
       (not (save-excursion (goto-char bol)
                            (looking-at "%" t)))))

(defvar matlab-ts-mode--i-block-comment-end-matcher-anchor-val)

(defun matlab-ts-mode--i-block-comment-end-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-block-comment-end-matcher'."
  matlab-ts-mode--i-block-comment-end-matcher-anchor-val)

(defun matlab-ts-mode--i-block-comment-end-matcher (node parent bol &rest _)
  "Is NODE, PARENT, BOL last line of \"{% ... %)\"?
Also handle single-line comment blocks, e.g.
          a = 1 % comment1 about a
   TAB>         % comment2 about a
but not
        function foo %#ok
   TAB> % doc comment line 1"

  (when (and (not node)
             (string= "comment" (treesit-node-type parent))
             ;; AND comment only line
             (save-excursion
               (goto-char bol)
               (looking-at "%" t))
             ;; AND not a function/classdef doc comment
             (not (matlab-ts-mode--i-doc-comment-matcher node parent bol)))
    (let ((anchor (if (save-excursion
                        (forward-line 0)
                        (backward-char)
                        (forward-line 0) ;; beginning of prior line
                        (looking-at "^[ \t]*end[ \t]*%"))
                      ;; Prior line isn't an end statement, e.g.
                      ;;        if x
                      ;;           y = 1;
                      ;;        end % foo
                      ;;   TAB> % this comment should be at column 1
                      (treesit-node-parent parent)
                    parent)))
      (setq matlab-ts-mode--i-block-comment-end-matcher-anchor-val
            (treesit-node-start anchor)))))

;; `matlab-ts-mode--function-indent-level'
;;
;; It is recommended that all function statements have terminating end statements.  In some cases
;; for compatibility MATLAB doesn't require a function end statement.  When functions do not have an
;; end, we don't indent the body of the function per older MATLAB coding standard.  Example:
;;
;;     function a = fcn1    |    function a = fcn2
;;     a = 1;               |        a = 1;
;;                          |    end
;;
;; If a *.m file contains functions, and one of the functions is terminated with end, then every
;; function in the file must be terminated with end.  If a *.m file contains a function with one or
;; more nested functions, then every function in the file must be terminated with end.  If a script
;; contains one or more local functions, then every function in the file must be terminated with
;; end.
;;
;; When we enter `matlab-ts-mode', we examine the content and set the state of this variable.  If
;; matlab-ts--function-indent-level is nil, while editing, if functions become terminated with
;; ends, we set this to t.  We never go from t to nil because it's easy to get in a temporary nil
;; state during edits and we don't want to cause unexpected indentation behavior.")

(defvar-local matlab-ts-mode--function-indent-level 'unset
  "Function indent level is 0 or `matlab-ts-mode--indent-level'.")

(defun matlab-ts-mode--set-function-indent-level (&optional _node parent _bol &rest _)
  "Setup the function indent level for end vs end-less functions.
For optional _NODE, PARENT, and _BOL see `treesit-simple-indent-rules'."

  ;; - When matlab-ts-mode--function-indent-level is 'unset we set this to
  ;;     0 or matlab-ts-mode--indent-level
  ;;   based on the buffer content.
  ;; - Otherwise, matlab-ts-mode--function-indent-level is 0, we will "upgrade" it to
  ;;   matlab-ts-mode--indent-level if function end's appear.

  (when (eq matlab-ts-mode--function-indent-level 'unset)
    (let ((root (if (and parent
                         (string= (treesit-node-type parent) "function_definition"))
                    parent
                  (treesit-buffer-root-node))))
      (if (treesit-search-subtree (treesit-buffer-root-node) "\\`ERROR\\'")
          ;; If we have syntax errors, assume that functions will have ends when entering
          ;; matlab-ts-mode, otherwise leave matlab-ts--function-indent-level unchanged.
          (when (equal matlab-ts-mode--function-indent-level 'unset)
            (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level))
        (let ((first-fcn (treesit-search-subtree root (rx bos "function_definition" eos))))
          (if (not first-fcn)
              ;; assume that if functions are added they will have ends
              (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level)
            (let ((have-end (string= (treesit-node-type (treesit-node-child first-fcn -1)) "end")))
              (if (equal matlab-ts-mode--function-indent-level 'unset)
                  (setq-local matlab-ts-mode--function-indent-level
                              (if have-end
                                  matlab-ts-mode--indent-level
                                0))
                (when have-end
                  (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level))
                )))))))
  matlab-ts-mode--function-indent-level)

(defun matlab-ts-mode--set-function-indent-level-for-gp (node parent bol &rest _)
  "Set and return offset for parent of PARENT (grand-parent of NODE) at BOL."
  (matlab-ts-mode--set-function-indent-level node (treesit-node-parent parent) bol))

(defvar matlab-ts--indent-debug-rule
  '((lambda (node parent bol)
      (message "-->N:%S P:%S L:%d BOL:%S GP:%S NPS:%S"
               node parent (line-number-at-pos) bol
               (treesit-node-parent parent)
               (treesit-node-prev-sibling node))
      nil)
    nil
    0))

(defvar matlab-ts-mode--indent-assert nil
  "Tests should set this to t to identify when we fail to find an indent rule.")

(defun matlab-ts-mode--indent-assert-no-rule (node parent bol &rest _)
  "Report no indent rule for NODE PARENT BOL."
  (when matlab-ts-mode--indent-assert
    (let ((in-error (or (string= (treesit-node-type (or node parent)) "ERROR")
                        (treesit-parent-until (or node parent) (rx bos "ERROR" eos)))))
      ;; We shouldn't indent if we don't handle the particular error (no matched rule is okay
      (when (not in-error)
        (error "Assert: no indent rule for: N:%S P:%S BOL:%S GP:%S NPS:%S BUF:%S"
               node parent bol
               (treesit-node-parent parent)
               (treesit-node-prev-sibling node)
               (buffer-name))))))

(defvar matlab-ts-mode--i-error-switch-matcher-pair)

(defun matlab-ts-mode--i-error-switch-matcher (node _parent bol &rest _)
  "Tab on an empty line after switch, case, or otherwise?
NODE is nil in this case and BOL, beginning-of-line point, is where we
are indenting.  If so, set
`matlab-ts-mode--i-error-switch-matcher-pair'.

  Example:  switch fcn1(a)
      TAB>    ^

Similar for case and otherwise statements."
  (when (not node)
    (save-excursion
      (goto-char bol)
      (re-search-backward "[^ \t\n\r]" nil t)

      (let* ((check-node (treesit-node-at (point)))
             (error-node (treesit-node-parent check-node)))
        (while (and error-node
                    (not (string= (treesit-node-type error-node) "ERROR")))
          (setq error-node (treesit-node-parent error-node)))

        ;; In an error-node, see if this error is due to an incomplete switch statement.
        (when error-node
          (let ((child-node (treesit-node-child error-node 0)))
            (when (and child-node
                       (string-match-p (rx bos (or "switch" "case" "otherwise") eos)
                                       (treesit-node-type child-node)))
              (setq matlab-ts-mode--i-error-switch-matcher-pair
                    (cons (treesit-node-start child-node)
                          matlab-ts-mode--switch-indent-level)))))))))

(defun matlab-ts-mode--i-error-switch-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-error-switch-matcher'."
  (car matlab-ts-mode--i-error-switch-matcher-pair))

(defun matlab-ts-mode--i-error-switch-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-error-switch-matcher'."
  (cdr matlab-ts-mode--i-error-switch-matcher-pair))

(defun matlab-ts-mode--i-cont-matcher (node _parent _bol &rest _)
  "Is current NODE part of a valid continuation?"
  (let ((prev-sibling (treesit-node-prev-sibling node)))
    (and prev-sibling
         (string= (treesit-node-type prev-sibling) "line_continuation"))))

(defvar matlab-ts-mode--i-prior-line-anchor-point nil)

(defun matlab-ts-mode--i-prior-line-anchor (&rest _)
  "Return anchor point for `matlab-ts-mode--i-prior-line-matcher'."
  matlab-ts-mode--i-prior-line-anchor-point)

(defun matlab-ts-mode--i-prior-line-matcher (node _parent bol &rest _)
  "Keep current indent level when NODE is nil.
This occurs when RET was typed on prior line.
BOL is beginning of line point for NODE.
See: tests/test-matlab-ts-mode-indent-files/indent_line_cont_multiple_times.m"
  (when (not node) ;; RET on prior line?
    (save-excursion
      (goto-char bol)
      (when (>= (forward-line -1) 0)
        (when (looking-at "^[ \t]*\\.\\.\\.")
          (matlab-ts-mode--ei-fast-back-to-indentation)
          (setq matlab-ts-mode--i-prior-line-anchor-point (point))
          t)))))

(defun matlab-ts-mode--i-cont-offset (node parent _bol &rest _)
  "Get the ellipsis continuation offset based on NODE with PARENT.
This is `matlab-ts-mode--indent-level' or 0 when in a cell or matrix
row."
  (let ((row-node (let ((n parent))
                    (while (and n
                                (not (string= (treesit-node-type n) "row")))
                      (setq n (treesit-node-parent n)))
                    n)))
    (cond
     (row-node
      0)
     ((string= (treesit-node-type parent) "superclasses")
      (if (and node
               (string= (treesit-node-type node) "&"))
          ;;     classdef indent_classdef_super_continued3 ...
          ;;         < otherThing ...
          ;; TAB>    & otherThing2 ...
          0
        ;;      classdef indent_classdef_super_continued < otherThing & ...
        ;; TAB>                                            otherThing2 & ...
        ;; See: test-matlab-ts-mode-indent-files/indent_classdef_super_continued.m
        2))
     (t
      matlab-ts-mode--indent-level))))


(defvar matlab-ts-mode--i-cont-incomplete-matcher-pair)

(cl-defun matlab-ts-mode--i-cont-incomplete-matcher (node parent bol &rest _)
  "Is current line part of an ellipsis line continuation?
If so, set `matlab-ts-mode--i-cont-incomplete-matcher-pair'.  This is for
incomplete statements where NODE is nil and PARENT is line_continuation
or when NODE is non-nil and is a line_continuation only line.
NODE is at BOL."

  (when (and (equal (treesit-node-type node) "line_continuation")
             (save-excursion
               (goto-char bol)
               (forward-line 0)
               (looking-at "^[ \t]*\\.\\.\\.")))
    (save-excursion
      (when (>= (forward-line -1) 0)
        (matlab-ts-mode--ei-fast-back-to-indentation)
        (setq matlab-ts-mode--i-cont-incomplete-matcher-pair
              (cons (treesit-node-start (treesit-node-at (point))) 0))
        (cl-return-from matlab-ts-mode--i-cont-incomplete-matcher t))))

  ;; Case when code is incomplete:
  ;;
  ;;        x = 1 + myfcn(a, ...
  ;;   TAB>               ^       NODE is nil and PARENT is line_continuation

  (let ((check-node
         (cond ((and (not node)
                     (string= (treesit-node-type parent) "line_continuation"))
                parent)
               ((and node
                     (let ((prev-sibling (treesit-node-prev-sibling node)))
                       (when (equal (treesit-node-type prev-sibling) "line_continuation")
                         (treesit-node-prev-sibling node))))))))

    (save-excursion
      ;; Walk over line_continuation and identify the "check node" we should be looking at.
      (while (equal "line_continuation" (treesit-node-type check-node))
        (goto-char (treesit-node-start check-node))
        (if (re-search-backward "[^ \t\n\r]" nil t)
            (setq check-node (treesit-node-at (point)))
          ;; at start of buffer
          (setq check-node nil)))

      (when check-node
        (let ((anchor-node check-node)
              saw-close-paren)

          ;; Look for open paren of a function-call, index, or expression grouping
          (while (and anchor-node
                      (let ((anchor-type (treesit-node-type anchor-node)))
                        (pcase anchor-type
                          (")"
                           (setq saw-close-paren t)
                           t)
                          ("("
                           (if saw-close-paren
                               (progn
                                 (setq saw-close-paren nil)
                                 t)
                             nil ;; found an unbalanced open parenthesis
                             ))
                          (_
                           t))))
            (setq anchor-node (treesit-node-prev-sibling anchor-node)))


          ;; If anchor-node is non-nil, we found "(", anchor off it, e.g. "(arg"
          (when (and anchor-node
                     (save-excursion
                       (goto-char (treesit-node-start anchor-node))
                       ;; When not an argument after the "("
                       ;; Examples: "(-1,..." "(+1,...") "(a, ..."  "((1+2),..."  "({1},..."
                       (not (looking-at "([ \t]*[-+a-zA-Z0-9{(]" t))))

            (let ((prev-node (treesit-node-prev-sibling anchor-node))  ;; "fcn(" ?
                  id-start)
              (when (and (equal (treesit-node-type prev-node) "identifier")
                         (save-excursion
                           (setq id-start (treesit-node-start prev-node))
                           (goto-char id-start)
                           (forward-line 0)
                           (matlab-ts-mode--ei-fast-back-to-indentation)
                           (= (point) id-start)))
                ;; Incomplete:      something.foo4 = ...
                ;;                       someFcn2( ...
                ;;             TAB>          1, ...
                (setq matlab-ts-mode--i-cont-incomplete-matcher-pair
                      (cons id-start matlab-ts-mode--indent-level))
                (cl-return-from matlab-ts-mode--i-cont-incomplete-matcher t)))
            (setq anchor-node nil) ;; can't anchor off the "("
            )

          ;; An expression of an if, while, or switch statement? Consider:
          ;;   if a > 1 && ...
          ;;      ^                     <== TAB goes here
          ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_if_cond.m
          (when (not anchor-node)
            (setq anchor-node check-node)
            (while (and anchor-node
                        (not (string-match-p (rx bos (or "if_statement" "while_statement"
                                                         "switch_statement")
                                                 eos)
                                             (treesit-node-type anchor-node))))
              (setq anchor-node (treesit-node-parent anchor-node)))
            (when anchor-node
              (setq anchor-node (treesit-node-child-by-field-name anchor-node "condition"))))

          ;; Look for parent that we can use for indent
          (when (not anchor-node)
            (setq anchor-node check-node)
            (while (and anchor-node
                        (not (string-match-p (rx bos (or "ERROR" "arguments") eos)
                                             (treesit-node-type anchor-node))))
              (setq anchor-node (treesit-node-parent anchor-node))))

          ;; Setup to indent if we found an anchor-node
          (when anchor-node
            (setq matlab-ts-mode--i-cont-incomplete-matcher-pair
                  (cons (treesit-node-start anchor-node)
                        (pcase (treesit-node-type anchor-node)
                          ("(" 1)
                          ("ERROR" matlab-ts-mode--indent-level)
                          (_ 0)))))
          )))))

(defun matlab-ts-mode--i-cont-incomplete-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-cont-incomplete-matcher'."
  (car matlab-ts-mode--i-cont-incomplete-matcher-pair))

(defun matlab-ts-mode--i-cont-incomplete-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-cont-incomplete-matcher'."
  (cdr matlab-ts-mode--i-cont-incomplete-matcher-pair))

(defun matlab-ts-mode--is-next-line-fcn-args-anchor-node (anchor-node)
  "Is ANCHOR-NODE for a  \"fcn(\ ...\", if so return new anchor node."
  (when (string= (treesit-node-type anchor-node) "(")
    (save-excursion
      (goto-char (treesit-node-start anchor-node))
      (and (< (point) (point-max))
           (goto-char (1+ (point)))
           (re-search-forward "[^ \t]" nil t)
           (equal (treesit-node-type (treesit-node-at (point))) "line_continuation")
           (goto-char (treesit-node-start anchor-node))
           (> (point) 1)

           (goto-char (1- (point))) ;; just before the "("
           (progn ;; skip back to first non-whitespace char before the "("
             (when (looking-at "[ \t]")
               (re-search-backward "[^ \t\r\n]" nil t))
             t)
           ;; is "fcn(" or "fcn ("?
           ;;       ^         ^
           (looking-at "[a-z0-9_]" t)

           ;; Found something that looks like a function-call, e.g. "id("
           ;; Return id-node if it's a fcn-call in a fcn-call.
           (or (let ((id-node (treesit-node-at (point))))

                 (goto-char (treesit-node-start id-node))

                 ;; Move over field expression, other1.other2.fcn1(foo.bar.fcn2(
                 (when (> (current-column) 0)
                   (goto-char (1- (point)))
                   (while (and (> (current-column) 0)
                               (looking-at "\\." t))
                     (goto-char (1- (point)))
                     (when (looking-at "[a-z0-9_]" t)
                       (setq id-node (treesit-node-at (point)))
                       (goto-char (treesit-node-start id-node))
                       (when (> (current-column) 0)
                         (goto-char (1- (point)))))))

                 (when (looking-at "[ \t]")
                   (re-search-backward "[^ \t\r\n]" nil t))
                 (when (looking-at "(")
                   id-node))
               (treesit-node-parent anchor-node))
           ))))

(defvar matlab-ts-mode--i-next-line-pair)

(defun matlab-ts-mode--is-prev-sibling-fcn-args (node)
  "Is a prev-sibling of NODE a function_arguments node?"
  (let* ((prev-sibling (treesit-node-prev-sibling node))
         (prev-type (treesit-node-type prev-sibling)))
    (while (and prev-sibling
                (not (string= prev-type "function_arguments"))
                ;; matlab tree-sitter uses a newline after arguments statement
                (not (string= prev-type "\n")))
      (setq prev-sibling (treesit-node-prev-sibling prev-sibling)))
    (when prev-sibling t)))

(defun matlab-ts-mode--i-next-line-indent-level (node bol anchor-node last-child-of-error-node)
  "Get indent level for `matlab-ts-mode--i-cont-incomplete-matcher'.

NODE is the node being indented using ANCHOR-NODE.
LAST-CHILD-OF-ERROR-NODE is nil or the last child of the error node we
are in.

BOL, is the beginning-of-line that is being indented.

Sets `matlab-ts-mode--i-next-line-pair' to (ANCHOR-NODE . OFFSET)"

  (let ((anchor-last-child (treesit-node-child anchor-node -1)))
    (when (and (equal (treesit-node-type anchor-last-child) "end")
               (or (not node)
                   (< (treesit-node-start anchor-last-child) (treesit-node-start node))))
      (setq anchor-node anchor-last-child)))

  (let ((updated-anchor-node (matlab-ts-mode--is-next-line-fcn-args-anchor-node anchor-node)))
    (when updated-anchor-node
      ;; Case: var_b = my_function( ...
      ;;           ^                        <== RET/TAB to here
      ;; See:  tests/test-matlab-ts-mode-indent-xr-files/indent_xr_fun_call1.m
      (setq anchor-node updated-anchor-node
            last-child-of-error-node nil)))

  (let ((indent-level
         (if (and node (string= (treesit-node-type node) "end"))
             (progn
               (when (string= "property" (treesit-node-type anchor-node))
                 (setq anchor-node (treesit-node-parent anchor-node)))
               0)
           (let ((anchor-type (treesit-node-type anchor-node)))
             (pcase anchor-type

               ("property"
                (if (or last-child-of-error-node
                        ;; Continuation:    arguments
                        ;;                     firstArgument ...
                        ;;         TAB>            double
                        (let ((prev-sibling (treesit-node-prev-sibling node)))
                          (and prev-sibling
                               (string= (treesit-node-type prev-sibling) "line_continuation"))))
                    matlab-ts-mode--indent-level
                  0))

               ((rx (seq bos (or "properties" "events" "methods" "arguments") eos))
                (cond
                 ((equal "end" (treesit-node-type (treesit-node-child anchor-node -1)))
                  ;; after properties, etc.
                  0)
                 ((string= anchor-type "arguments")
                  (if (or (not node)
                          (matlab-ts-mode--is-prev-sibling-fcn-args node))
                      (if (let ((prev-sibling (treesit-node-prev-sibling node)))
                            (and prev-sibling
                                 (string= (treesit-node-type prev-sibling) "line_continuation")))
                          ;; Continuation:    arguments
                          ;;                     firstArgument ...
                          ;;         TAB>            ^
                          (* 2 matlab-ts-mode--indent-level)
                        matlab-ts-mode--indent-level)
                    ;; Else function-call arguments
                    ;; See: tests/test-matlab-ts-mode-indent-files/indent_fcn_call_last_paren.m
                    0))
                 (t
                  matlab-ts-mode--indent-level)))

               ("("
                1)

               ("[" ;; either a matrix or function output
                (if (save-excursion
                      (goto-char (treesit-node-start anchor-node))
                      (while (and (re-search-backward "[^ \t\r\n]" nil t)
                                  (string= (treesit-node-type
                                            (treesit-node-at (point)))
                                           "line_continuation")))
                      (string= (treesit-node-type (treesit-node-at (point)))
                               "function"))
                    ;; function output
                    1
                  matlab-ts-mode--array-indent-level))

               ("{"
                (if (save-excursion
                      (goto-char (treesit-node-start anchor-node))
                      (when (> (point) 1)
                        (goto-char (1- (point)))
                        (looking-at "%" t)))
                    ;; Anchored under a block comment: %{
                    ;;                                   ^    <== TAB to here, one more than the "{"
                    1
                  matlab-ts-mode--array-indent-level))

               ((rx (seq bos (or "function" "function_definition") eos))
                (if (equal "end" (treesit-node-type (treesit-node-child anchor-node -1)))
                    ;; after function
                    0
                  ;; else under function
                  matlab-ts-mode--function-indent-level))

               ("row"
                0)

               ("try"
                (if (and node
                         (string-match-p (rx bos "catch_clause" eos)
                                         (treesit-node-type node)))
                    0
                  matlab-ts-mode--indent-level))

               ((rx (seq bos (or "if" "elseif") eos))
                (if (and node
                         ;; else, else_clause, elseif, or elseif_clause
                         (string-match-p (rx bos "else")
                                         (treesit-node-type node)))
                    0
                  (if (save-excursion
                        (goto-char bol)
                        (let ((prev-node (treesit-node-at (point))))
                          (when (looking-at "[^ \t\n\r]")
                            (setq prev-node (treesit-node-prev-sibling prev-node)))
                          (when (equal (treesit-node-type prev-node) "line_continuation")
                            (goto-char (treesit-node-start prev-node))
                            (matlab-ts-mode--ei-fast-back-to-indentation)
                            ;; move to first item after the if or elseif condition
                            (when (looking-at (rx (or "if" "elseif") (or " " "\t")))
                              (re-search-forward "[ \t]" nil t))
                            (let ((condition-node (treesit-node-at (point))))
                              (when (not (string= (treesit-node-type condition-node)
                                                  "line_continuation"))
                                (setq anchor-node condition-node))))))
                      ;; Case:   if condition || ...
                      ;;            ^                       <== TAB to here
                      0
                    ;; Case: if condition
                    ;;           ^                           <== TAB to here
                    matlab-ts-mode--indent-level)))

               ((rx bos (or "case" "otherwise"))
                (if (and node
                         (string-match-p (rx bos (or "case" "otherwise")) (treesit-node-type node)))
                    0
                  matlab-ts-mode--switch-indent-level))

               ("switch"
                matlab-ts-mode--switch-indent-level)

               ("end"
                0)

               ("classdef"
                (if (and (string= (treesit-node-type node) "comment")
                         (save-excursion
                           (goto-char (treesit-node-start node))
                           (forward-line -1)
                           (matlab-ts-mode--ei-fast-back-to-indentation)
                           (equal (treesit-node-at (point)) anchor-node)))
                    ;; function doc comment
                    0
                  matlab-ts-mode--indent-level))

               ("<" ;; superclass
                (if (and node
                         (string= (treesit-node-type node) "&"))
                    ;;     classdef indent_classdef_super_continued3 ...
                    ;;         < otherThing ...
                    ;; TAB>    & otherThing2 ...
                    0
                  ;;      classdef indent_classdef_super_continued < otherThing & ...
                  ;; TAB>                                            otherThing2 & ...
                  ;; See: test-matlab-ts-mode-indent-files/indent_classdef_super_continued.m
                  2))

               (_
                matlab-ts-mode--indent-level)
               )))))
    (setq matlab-ts-mode--i-next-line-pair
          (cons (treesit-node-start anchor-node) indent-level))))

(defvar matlab-ts-mode--i-next-line-anchors-rx (rx (seq bos (or "function_definition"
                                                                "function"
                                                                "arguments"
                                                                "classdef"
                                                                "properties"
                                                                "property"
                                                                "enumeration"
                                                                "events"
                                                                "methods"
                                                                "if"
                                                                "elseif"
                                                                "switch"
                                                                "case"
                                                                "case_clause"
                                                                "otherwise"
                                                                "otherwise_clause"
                                                                "for"
                                                                "parfor"
                                                                "while"
                                                                "try"
                                                                "catch"
                                                                "("
                                                                "["
                                                                "{"
                                                                "row"
                                                                "spmd"
                                                                "<" ;; superclasses
                                                                )
                                                        eos)))

(defun matlab-ts-mode--i-next-line-matcher-comment (node)
  "Is NODE a comment we should indent when in context of an error?"

  (when (and node
             (string-match-p (rx (seq bos "comment" eos)) (treesit-node-type node)))
    (let ((prev-sibling (treesit-node-prev-sibling node)))
      (when (equal (treesit-node-type prev-sibling) "ERROR")
        (let ((last-child (treesit-node-child prev-sibling -1)))

          ;; function foo(a,b,stuff,cmddual1fake,cmddual2fake)
          ;;     if foo %!!4
          ;;         C = "this is the end of the line";
          ;;         % !!8                                      <== TAB to here
          (when (and last-child
                     (string= (treesit-node-type last-child) ";"))
            (setq last-child (treesit-node-prev-sibling last-child)))

          (when last-child
            ;; classdef foo
            ;;     properties
            ;;         foo1;
            ;;     end
            ;;     % comment    <== RET on this node, last-child == "end"
            ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_classdef3.m
            (setq matlab-ts-mode--i-next-line-pair
                  (cons (treesit-node-start last-child) 0))
            t))))))

(defun matlab-ts-mode--not-node-and-blank (node bol)
  "Is NODE nil with BOL on a blank line?"
  ;; Consider the one-liner:
  ;;    classdef indent_xr_classdef1
  ;;        ^                                 TAB should go here
  ;;  on entry node is nil and parent is ERROR, so backup to the newline:
  ;;   (source_file
  ;;    (ERROR classdef (identifier) \n))
  ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_classdef1.m
  (and (not node)
       (save-excursion
         (goto-char bol)
         (forward-line 0)
         (looking-at "^[ \t]*$" t))))

(defun matlab-ts-mode--last-child-of-error (node parent)
  "Get updated parent if ellipsis NODE with PARENT is not part of an error."

  ;; Handle continuation where the node is not part of the error node. Example:
  ;;    (source_file
  ;;     (ERROR classdef (identifier) \n properties \n (identifier) =)
  ;;     (line_continuation))
  ;; For
  ;;    classdef indent_xr_classdef2
  ;;        properties
  ;;            p3continued = ...
  ;;                ^                       << RET on prior line should go here
  ;; In this case, last-child-of-error-node will be the "=" node.
  (when (and (not node)
             (string= (treesit-node-type parent) "line_continuation"))
    (let ((prev-sibling (treesit-node-prev-sibling parent)))
      (when (equal (treesit-node-type prev-sibling) "ERROR")
        (treesit-node-child prev-sibling -1)))))

(defun matlab-ts-mode--i-next-line-prev-sibling-to-check (last-child-of-error-node node-to-check)
  "Locate the prev-sibling-to-check for `matlab-ts-mode--i-next-line-matcher'.
For LAST-CHILD-OF-ERROR-NODE and NODE-TO-CHECK see
`matlab-ts-mode--i-next-line-matcher'"

  (let (prev-sibling-to-check
        prev-sibling-error-node
        (close-paren-count 0)
        (prev-sibling (if last-child-of-error-node
                          last-child-of-error-node ;; is our "prev-sibling"
                        (treesit-node-prev-sibling node-to-check))))
    (while (and prev-sibling
                (not prev-sibling-to-check)
                (not prev-sibling-error-node))
      (let ((prev-sibling-type (treesit-node-type prev-sibling)))

        ;; Backup over "..." continuations because they may not be prev-sibling's.
        ;; Consider:
        ;;    function ...
        ;;        [    ...    <== TAB here
        ;; (source_file (ERROR function) (line_continuation) (ERROR [) (line_continuation))
        (save-excursion
          (while (and prev-sibling
                      (string= prev-sibling-type "line_continuation")
                      (let ((prev-sibling2 (treesit-node-prev-sibling prev-sibling)))
                        (or (not prev-sibling2)
                            (string= (treesit-node-type prev-sibling2) "ERROR"))))
            (goto-char (treesit-node-start prev-sibling))
            (when (re-search-backward "[^ \t\n\r]" nil t)
              (setq prev-sibling (treesit-node-at (point))
                    prev-sibling-type (when prev-sibling
                                        (treesit-node-type prev-sibling))))))

        (when prev-sibling
          (cond
           ((string= prev-sibling-type "ERROR")
            (setq prev-sibling-error-node prev-sibling))

           ((string= prev-sibling-type ")")
            (setq close-paren-count (1+ close-paren-count)))

           ((string= prev-sibling-type "(")
            (if (> close-paren-count 0)
                (setq close-paren-count (1- close-paren-count))
              (setq prev-sibling-to-check prev-sibling)))

           ((and (string-match-p matlab-ts-mode--i-next-line-anchors-rx prev-sibling-type)
                 (or (not (string= prev-sibling-type "("))
                     ;; See: test/test-matlab-ts-mode-indent-xr-files/indent_xr_i_cont_incomplete5.m
                     ;; result = longFunction( ...
                     ;;     ^                           <== RET on prior line or TAB goes here
                     ;;
                     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_fun3.m
                     ;; function out=indent_xr_fun3(in1, ...
                     ;;                             ^   <== RET on prior line or TAB goes here

                     ;; no function or similar item  before paren: e.g. no longFunction(
                     (not (let* ((pp-s (treesit-node-prev-sibling prev-sibling))
                                 (pp-s-type (treesit-node-type pp-s)))
                            (and pp-s-type
                                 (string-match-p (rx bos (or "identifier" "spmd") eos) pp-s-type))))

                     ;; OR we have something after the paren: e.g. (in1, ...
                     (not (equal (treesit-node-type
                                  (treesit-node-next-sibling prev-sibling))
                                 "line_continuation"))
                     ))
            (setq prev-sibling-to-check prev-sibling)))

          (setq prev-sibling (if prev-sibling-error-node
                                 nil
                               (treesit-node-prev-sibling prev-sibling))))))

    (cons prev-sibling-to-check prev-sibling-error-node)))

(cl-defun matlab-ts-mode--i-next-line-matcher (node parent bol &rest _)
  "Matcher for indent on a newline being inserted when in presence of errors.
If so, set `matlab-ts-mode--i-next-line-pair'.

NODE may or may not be nil.  When NODE is nil in this case and BOL,
beginning-of-line point, is where we are indenting.  If NODE is non-nil,
we check if it is an ERROR node.  Another case is when PARENT is be a newline
due to a RET on the prior line.

Example: in this case NODE will be nil and PARENT is a newline.  Example:
   % -*- matlab-ts -*-
   classdef foo
       ^                     <== TAB or RET on prior line goes here.
Hierarchy:
  #<treesit-node source_file in 1-36>
    #<treesit-node ERROR in 21-36>
      #<treesit-node \"
\" in 33-36>
Prev-siblings:
  > #<treesit-node \"classdef\" in 21-29>
    > #<treesit-node identifier in 30-33>
      > #<treesit-node \""

  (when (matlab-ts-mode--i-next-line-matcher-comment node)
    (cl-return-from matlab-ts-mode--i-next-line-matcher t))

  (when (matlab-ts-mode--not-node-and-blank node bol)
    (setq parent (treesit-node-at (point))))

  (let ((last-child-of-error-node (matlab-ts-mode--last-child-of-error node parent)))
    (when last-child-of-error-node
      (setq parent last-child-of-error-node))

    (when (or last-child-of-error-node
              ;; Is node, parent, or grandparent an error? We could look to other ancestors,
              ;; but that would indicate many syntax errors and result in unexpected indent.
              (and node (string= (treesit-node-type node) "ERROR"))
              (string-match-p (rx (seq bos (or "ERROR" "\n") eos)) (treesit-node-type parent))
              (equal (treesit-node-type (treesit-node-parent parent)) "ERROR"))

      (let ((node-to-check (or last-child-of-error-node node parent))
            ancestor-error-node ;; is node, parent, or one of it's ancestors an ERROR?
            prev-sibling-error-node ;; is a previous sibling under an ERROR?
            prev-sibling-to-check
            ancestor-to-check)

        ;; ancestor-to-check
        (let ((ancestor node-to-check))
          (while (and ancestor (not ancestor-error-node))
            (let ((ancestor-type (treesit-node-type ancestor)))
              (cond

               ((string= ancestor-type "ERROR")
                (setq ancestor-error-node ancestor))

               ((and (not ancestor-to-check)
                     (string-match-p matlab-ts-mode--i-next-line-anchors-rx ancestor-type))
                (setq ancestor-to-check ancestor)))

              (setq ancestor (if ancestor-error-node
                                 nil
                               (treesit-node-parent ancestor)))))
          (when (eq ancestor-to-check node-to-check)
            (setq ancestor-to-check nil)))

        ;; prev-sibling-to-check
        (let ((pair (matlab-ts-mode--i-next-line-prev-sibling-to-check
                     last-child-of-error-node
                     node-to-check)))
          (setq prev-sibling-to-check (car pair)
                prev-sibling-error-node (cdr pair)))

        ;; We use regular matching rules when there is NO error.
        (when (and (not ancestor-error-node)
                   (not prev-sibling-error-node))
          (cl-return-from matlab-ts-mode--i-next-line-matcher nil))

        (let ((anchor-node (or ancestor-to-check
                               prev-sibling-to-check)))
          (when anchor-node
            (let* ((a-error-start (when ancestor-error-node
                                    (treesit-node-start ancestor-error-node)))
                   (p-error-start (when prev-sibling-error-node
                                    (treesit-node-start prev-sibling-error-node)))
                   (error-node (if (or (not p-error-start)
                                       (and a-error-start
                                            (> a-error-start p-error-start)))
                                   ancestor-error-node
                                 prev-sibling-error-node)))
              (when (and (or (not node)
                             (not (equal node error-node)))
                         ;; For condition (not (= bol (treesit-node-start error-node))), see
                         ;; tests/test-matlab-ts-mode-indent-files/indent_parse_error_in_class.m
                         (not (= bol (treesit-node-start error-node)))
                         (> (treesit-node-start error-node) (treesit-node-start anchor-node)))
                (setq anchor-node error-node)))

            (matlab-ts-mode--i-next-line-indent-level node bol anchor-node last-child-of-error-node)
            ;; t ==> matched
            t))))))

(defun matlab-ts-mode--i-next-line-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-next-line-matcher'."
  (car matlab-ts-mode--i-next-line-pair))

(defun matlab-ts-mode--i-next-line-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-next-line-matcher'."
  (cdr matlab-ts-mode--i-next-line-pair))

(defvar matlab-ts-mode--i-comment-under-fcn-pair)

(defun matlab-ts-mode--i-comment-under-fcn-matcher (node _parent _bol &rest _)
  "Matcher when NODE is a comment and under a function.
Example:
   function a=foo
       a=1;
       %comment <== TAB goes here."
  (let ((comment-or-ellipsis-re (rx bos (or "comment" "line_continuation"))))

    (when (and node
               (string-match-p comment-or-ellipsis-re (treesit-node-type node)))
      (let ((prev-sibling (treesit-node-prev-sibling node)))
        (while (and prev-sibling
                    (string-match-p comment-or-ellipsis-re (treesit-node-type prev-sibling)))
          (setq prev-sibling (treesit-node-prev-sibling prev-sibling)))

        (when (and prev-sibling
                   (string= (treesit-node-type prev-sibling) "function_definition"))
          (when (not (equal (treesit-node-type (treesit-node-child prev-sibling -1)) "end"))
            (setq matlab-ts-mode--i-comment-under-fcn-pair
                  (cons (treesit-node-start prev-sibling) matlab-ts-mode--indent-level))
            t))))))

(defun matlab-ts-mode--i-comment-under-fcn-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-comment-under-fcn-matcher'."
  (car matlab-ts-mode--i-comment-under-fcn-pair))

(defun matlab-ts-mode--i-comment-under-fcn-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-comment-under-fcn-matcher'."
  (cdr matlab-ts-mode--i-comment-under-fcn-pair))

(defun matlab-ts-mode--i-top-level (node parent _bol &rest _)
  "Is NODE with PARENT a top-level classdef, function, or code?"
  (or (and (not node)
           (or (let ((parent-type (treesit-node-type parent)))
                 (or (string= parent-type "source_file")
                     (and (string= parent-type "\n")
                          (string= (treesit-node-type (treesit-node-parent parent))
                                   "source_file"))))))
      (and node
           (not (string-match-p (rx bos (or "line_continuation" "\n") eos)
                                (treesit-node-type node)))
           (equal (treesit-node-type parent) "source_file"))))

(defun matlab-ts-mode--column-0 (_node _parent bol &rest _)
  "Return column-0 for BOL.
Note treesit column-0 moves point, fixed in Emacs 31."
  (save-excursion
    (goto-char bol)
    (pos-bol)))

(defvar matlab-ts-mode--i-fcn-args-next-line-pair)

(defun matlab-ts-mode--i-fcn-args-next-line-matcher (_node parent _bol &rest _)
  "Is NODE/PARENT for function definition arguments?
Example:
   function someFunction( ...
       a, b)                        <== TAB to here"

  (let ((parent-type (treesit-node-type parent)))

    (cond
     ((and (string= parent-type "function_arguments")
           (or
            ;; function someFunction(...
            ;;    a, b)                     <== TAB to here
            (save-excursion
              (goto-char (1+ (treesit-node-start parent)))
              (and (re-search-forward "[^ \t]" nil t)
                   (equal (treesit-node-type (treesit-node-at (point))) "line_continuation")))
            ;; I-Rule: function args next line
            ;;            function [a, b] = foo( ...
            ;;  TAB>      a, b)                          // should move over by 4
            ;;            end
            (save-excursion
              (goto-char (1+ (treesit-node-start parent)))
              (and (re-search-forward "[^ \t]" nil t)
                   (string= "line_continuation" (treesit-node-type (treesit-node-at (point)))))))
           ;; AND not
           ;;     function ...
           ;;         out ...
           ;;         = ...
           ;;         indent_ellipsis ...
           ;;         ( ...
           ;;          arg ...                 <== TAB here
           (save-excursion
             (goto-char (1- (treesit-node-start parent)))
             (not (looking-at "[ \t]" t))))
      (setq matlab-ts-mode--i-fcn-args-next-line-pair
            (cons (treesit-node-start (treesit-node-parent parent))
                  matlab-ts-mode--indent-level))
      t)

     ;; function someFunction(...
     ;;     in1, ...                  <== RET type "in1, ..." or TAB to here
     ;;     a, b)
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_fcn_args.m
     ((and (string= parent-type "line_continuation")
           (let ((gp (treesit-node-parent parent)))
             (and (equal (treesit-node-type gp) "function_arguments")
                  (save-excursion
                    (goto-char (1+ (treesit-node-start gp)))
                    (and (re-search-forward "[^ \t]" nil t)
                         (string= "line_continuation" (treesit-node-type
                                                       (treesit-node-at (point)))))))))
      (setq matlab-ts-mode--i-fcn-args-next-line-pair
            (cons (treesit-node-start (treesit-node-parent (treesit-node-parent parent)))
                  matlab-ts-mode--indent-level))
      t)

     (t
      ;; no match
      nil))))

(defun matlab-ts-mode--i-fcn-args-next-line-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-fcn-args-next-line-matcher'."
  (car matlab-ts-mode--i-fcn-args-next-line-pair))

(defun matlab-ts-mode--i-fcn-args-next-line-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-fcn-args-next-line-matcher'."
  (cdr matlab-ts-mode--i-fcn-args-next-line-pair))

(defvar matlab-ts-mode--i-assign-cont-pair nil)

(defun matlab-ts-mode--i-assign-cont-anchor (&rest _)
  "Return anchor for `matlab-ts-mode--i-assign-cont-matcher'."
  (car matlab-ts-mode--i-assign-cont-pair))

(defun matlab-ts-mode--i-assign-cont-offset (&rest _)
  "Return anchor for `matlab-ts-mode--i-assign-cont-matcher'."
  (cdr matlab-ts-mode--i-assign-cont-pair))

(defun matlab-ts-mode--i-assign-cont-matcher (node parent _bol &rest _)
  "Is NODE, PARENT part of an assignment line continuation?"
  (and node
       (equal (treesit-node-type (treesit-node-prev-sibling node)) "line_continuation")
       (string= (treesit-node-type parent) "function_call")

       (let ((grand-parent (treesit-node-parent parent))
             full-fcn-node
             assign-node)

         (pcase (treesit-node-type grand-parent)
           ("assignment"
            (setq full-fcn-node parent
                  assign-node grand-parent))
           ("field_expression"
            (let ((great-gp (treesit-node-parent grand-parent)))
              (when (equal (treesit-node-type great-gp) "assignment")
                (setq full-fcn-node grand-parent
                      assign-node great-gp)))))

         (when full-fcn-node
           (save-excursion
             (goto-char (treesit-node-start full-fcn-node))
             (forward-line 0)
             (matlab-ts-mode--ei-fast-back-to-indentation)
             (setq matlab-ts-mode--i-assign-cont-pair ;; Matched one of two cases
                   (cons (if (= (point) (treesit-node-start full-fcn-node))
                             ;;     something.foo4 = ...
                             ;;         someFcn2( ...
                             ;; TAB>        1, ...
                             (point)
                           ;;      something.foo2 = someFcn(...
                           ;;  TAB>    1, ...
                           (treesit-node-start assign-node))
                         matlab-ts-mode--indent-level)))))))

(defvar matlab-ts-mode--i-arg-namespace-fcn-prop-anchor-value nil)

(defun matlab-ts-mode--i-arg-namespace-fcn-prop-matcher (node parent _bol &rest _)
  "Is NODE, PARENT a property default value?
Example:
        properties (Constant)
            property1 = containers.Map(...
    TAB>                    {"
  (and (equal (treesit-node-type node) "arguments")
       (string= (treesit-node-type parent) "function_call")
       (let ((grand-parent (treesit-node-parent parent)))
         (and (string= (treesit-node-type grand-parent) "field_expression")
              (let ((great-grand-parent (treesit-node-parent grand-parent)))
                (and (string= (treesit-node-type great-grand-parent) "default_value")
                     (let ((great-great-grand-parent (treesit-node-parent great-grand-parent)))
                       (and (string= (treesit-node-type great-great-grand-parent) "property")
                            (setq matlab-ts-mode--i-arg-namespace-fcn-prop-anchor-value
                                  (treesit-node-start great-great-grand-parent))))))))))

(defun matlab-ts-mode--i-arg-namespace-fcn-prop-anchor (_node _parent _bol &rest _)
  "Return anchor for `matlab-ts-mode--i-arg-namespace-fcn-prop-matcher'."
  matlab-ts-mode--i-arg-namespace-fcn-prop-anchor-value)

(defvar matlab-ts-mode--i-row-matcher-pair nil)

(cl-defun matlab-ts-mode--i-row-matcher (node parent _bol &rest _)
  "NODE, PARENT matcher for row nodes of cell and matrices.
Example:
   m1 = [100, 2
           3, 4]       <== TAB to here.
   m2 = [
          100, 2
            3, 4       <== TAB to here.
        ]"
  (when (or (not node)
            (not (string= (treesit-node-type node) "row")))
    (cl-return-from matlab-ts-mode--i-row-matcher))

  ;; When electric indent AND parent node is a multi-line matrix (m-matrix)
  ;; Align to first column width
  ;;       m1 = [   23      |    m2 = [45678      |    m3 = [  23
  ;; TAB>        45678]     |             23]     |           333
  ;;                                              |          4444]
  (when (and (string= "matrix" (treesit-node-type parent))
             matlab-ts-mode--electric-indent)
    (if-let* ((first-col-width (matlab-ts-mode--ei-get-mat-first-col-width-for-i-row parent)))
        (let ((n-el-spaces (if (equal (treesit-node-type (treesit-node-child node 0)) "string")
                               0
                             (let* ((el (treesit-node-child node 0)) ;; 1st matrix row element
                                    (el-width (- (treesit-node-end el) (treesit-node-start el))))
                               (- first-col-width el-width)))))
          (setq matlab-ts-mode--i-row-matcher-pair
                (cons (treesit-node-start parent) (1+ n-el-spaces)))
          (cl-return-from matlab-ts-mode--i-row-matcher t))))

  ;; Have row of either a matrix or cell
  (save-excursion
    (goto-char (treesit-node-start parent))
    (forward-char)
    (when (and (looking-at "[ \t]")
               (re-search-forward "[^ \t]" (pos-eol) t))
      (backward-char))
    ;; Have something after the "[", e.g. "[  123" or "[ ..."?
    (let ((node-at-pt (treesit-node-at (point))))
      (when (and (looking-at "[^ \t\r\n]")
                 (not (equal (treesit-node-type node-at-pt) "line_continuation")))
        (setq matlab-ts-mode--i-row-matcher-pair (cons (treesit-node-start node-at-pt) 0))
        t))))

(defun matlab-ts-mode--i-row-matcher-anchor (&rest _)
  "Return anchor for `matlab-ts-mode--i-row-matcher'."
  (car matlab-ts-mode--i-row-matcher-pair))

(defun matlab-ts-mode--i-row-matcher-offset (&rest _)
  "Return offset for `matlab-ts-mode--i-row-matcher'."
  (cdr matlab-ts-mode--i-row-matcher-pair))

(defvar matlab-ts-mode--i-ret-pair)

(defun matlab-ts-mode--i-ret-matcher (node parent bol &rest _)
  "Is NODE, PARENT, and BOL for RET on prior line indent matcher?"
  (when (and (not node)
             (string= "\n" (treesit-node-type parent)))
    (let ((grand-parent (treesit-node-parent parent))
          offset)
      (when (string-match-p (rx bos (or "class_definition"
                                        "properties"
                                        "enumeration"
                                        "methods"
                                        "events"
                                        "function_definition"
                                        "arguments_statement"
                                        "spmd_statement"
                                        "try_statement"
                                        "catch_clause"
                                        "while_statement"
                                        "for_statement"
                                        "if_statement"
                                        "else_clause"
                                        "elseif_clause"))
                            (treesit-node-type grand-parent))
        (save-excursion
          (goto-char bol)
          (if (and (looking-at "^[ \t]*$")
                   (re-search-backward "[^ \t\r\n]" nil t)
                   (string= "end" (treesit-node-type (treesit-node-at (point)))))
              (setq offset 0)
            (setq offset matlab-ts-mode--indent-level)))


        (setq matlab-ts-mode--i-ret-pair (cons (treesit-node-start grand-parent) offset))))))

(defun matlab-ts-mode--i-ret-anchor (&rest _)
  "Return anchor for `matlab-ts-mode--i-ret-matcher'."
  (car matlab-ts-mode--i-ret-pair))

(defun matlab-ts-mode--i-ret-offset (&rest _)
  "Return offset for `matlab-ts-mode--i-ret-matcher'."
  (cdr matlab-ts-mode--i-ret-pair))

(defvar matlab-ts-mode--i-validation-functions-offset-value)

(defun matlab-ts-mode--i-validation-functions-offset (&rest _)
  "Return offset for `matlab-ts-mode--validation-functions-matcher'."
  matlab-ts-mode--i-validation-functions-offset-value)

(defun matlab-ts-mode--i-validation-functions-matcher (_node parent _bol &rest _)
  "Is PARENT a validation function?"
  (when (string= (treesit-node-type parent) "validation_functions")
    (save-excursion
      (goto-char (treesit-node-start parent)) ;; goto opening "{" of the validation functions cell
      (forward-char)
      (if (and (re-search-forward "[^ \t]" (pos-eol) t)  ;; no validation fcn on the "{" line?
               (progn (backward-char)
                      (not (looking-at "\\.\\.\\."))))
          (setq matlab-ts-mode--i-validation-functions-offset-value 1)
        (setq matlab-ts-mode--i-validation-functions-offset-value 2)))
    t))

(defvar matlab-ts-mode--indent-rules
  `((matlab

     ;; I-Rule: last line of code block comment "%{ ... %}"?
     (,#'matlab-ts-mode--i-block-comment-end-matcher
      ,#'matlab-ts-mode--i-block-comment-end-anchor
      0)

     ;; I-Rule: within a code block comment "%{ ... %}"?
     (,#'matlab-ts-mode--i-in-block-comment-matcher parent 2)

     ;; I-Rule: RET on an incomplete statement. Example:
     ;;         classdef foo
     ;;             ^                     <== TAB or RET on prior line goes here.
     (,#'matlab-ts-mode--i-next-line-matcher
      ,#'matlab-ts-mode--i-next-line-anchor
      ,#'matlab-ts-mode--i-next-line-offset)

     ;; I-Rule: comment under function, e.g. typing the following (no end):
     ;;         function a=foo
     ;;             a=1;
     ;;             %comment    <== TAB goes here
     (,#'matlab-ts-mode--i-comment-under-fcn-matcher
      ,#'matlab-ts-mode--i-comment-under-fcn-anchor
      ,#'matlab-ts-mode--i-comment-under-fcn-offset)

     ;; I-Rule: classdef's, function's, or code for a script that is at the top-level
     (,#'matlab-ts-mode--i-top-level
      ,#'matlab-ts-mode--column-0
      0)

     ;; I-Rule: within a function/classdef doc block comment "%{ ... %}"?
     (,#'matlab-ts-mode--i-doc-block-comment-matcher parent 2)

     ;; I-Rule: function/classdef doc comment?
     (,#'matlab-ts-mode--i-doc-comment-matcher
      ,#'matlab-ts-mode--i-doc-comment-anchor
      ,#'matlab-ts-mode--i-doc-comment-offset)

     ;; I-Rule: switch case and otherwise statements
     ((node-is ,(rx bos (or "case_clause" "otherwise_clause") eos))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: nested functions
     ((n-p-gp ,(rx bos "function_definition" eos)
              ,(rx bos "block" eos)
              ,(rx bos "function_definition" eos))
      parent 0)

     ;; I-Rule: elseif, else, catch, end statements go back to parent level
     ((node-is ,(rx bos (or "elseif_clause" "else_clause" "catch_clause" "end") eos)) parent 0)


     ;; I-Rule: first line of code within a switch case or otherwise statement, node is block
     ((parent-is ,(rx bos (or "switch_statement" "case_clause" "otherwise_clause") eos))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: function's
     ((parent-is ,(rx bos "function_definition" eos))
      parent ,#'matlab-ts-mode--set-function-indent-level-for-gp)

     ;; I-Rule: function a = indent_xr_fun()<RET>
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_fun*.m
     ((n-p-gp nil ,(rx bos "\n" eos) ,(rx bos (or "function_definition") eos))
      grand-parent ,#'matlab-ts-mode--set-function-indent-level)

     ;; I-Rule: constructs within classdef or function's.
     ((node-is ,(rx bos (or "arguments_statement" "block" "enumeration" "enum" "methods" "events"
                            "function_definition" "property" "properties")
                    eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: classdef abstract methods
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_classdef_abs_methods.m
     ((n-p-gp nil ,(rx bos "\n" eos) ,(rx bos "function_signature" eos)) grand-parent 0)
     ((n-p-gp ,(rx bos "\n" eos) ,(rx bos "function_signature" eos) nil) parent 0)

     ;; I-Rule: items in blocks
     ((n-p-gp nil ,(rx bos "property" eos) ,(rx bos "properties" eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: property continuation
     ((n-p-gp nil ,(rx bos "default_value" eos) ,(rx bos "property" eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: end argument/property continuation
     ((n-p-gp ,(rx bos "}" eos) ,(rx bos "validation_functions" eos) nil) parent 0)

     ;; I-Rule: argument/property continuation
     ;;         arguments
     ;;             a ...
     ;;                 { ...
     ;;                   mustBeReal ...
     ;;                   ^                     <== TAB/RET to here
     (,#'matlab-ts-mode--i-validation-functions-matcher
      parent
      ,#'matlab-ts-mode--i-validation-functions-offset)

     ;; I-Rule: property after a property
     ;;         properties
     ;;             p1
     ;;             ^              <= RET on prior line goes here
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_classdef2.m
     ((n-p-gp nil nil ,(rx bos "property" eos)) grand-parent 0)

     ;; I-Rule: code in if, for, methods, arguments statements, etc.
     ((parent-is ,(rx bos (or "if_statement" "for_statement" "while_statement"
                              "methods" "events" "enumeration"
                              "function_definition" "arguments_statement"
                              "properties")
                      eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: case 10<RET>
     ((n-p-gp nil ,(rx bos "\n" eos) ,(rx bos (or "switch_statement" "case_clause"
                                                  "otherwise_clause")
                                          eos))
      grand-parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule:  if condition1 || ...     |      if condition1 + condition2 == ...
     ;; <TAB>       condition2 || ...     |         2770000 ...
     ((parent-is ,(rx bos (or "boolean_operator" "comparison_operator") eos)) parent 0)

     ;; I-Rule:  elseif ...
     ;; <TAB>        condition2 || ...
     ((parent-is ,(rx bos (or "else_clause" "elseif_clause") eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: if variable ...
     ;;             ^             <== TAB or RET on prior line
     ;;         end
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_statement_body.m
     (,#'matlab-ts-mode--i-ret-matcher
      ,#'matlab-ts-mode--i-ret-anchor
      ,#'matlab-ts-mode--i-ret-offset)

     ;; I-Rule: grandparent is block
     ;;      for i=1:10
     ;;          disp(i)
     ;;          ^                         <== RET on prior line or tab goes here
     ;;      end
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_grandparent_is_block.m
     ((n-p-gp nil ,(rx bos "\n" eos) ,(rx bos "block" eos)) grand-parent 0)

     ;; I-Rule: disp(myMatrix(1:  ...
     ;; <TAB>                 end));
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_ranges.m
     ((parent-is ,(rx bos "range" eos)) parent 0)

     ;; I-Rule: try<RET>    |   catch<RET>
     ((parent-is ,(rx bos (or "try_statement" "catch_clause") eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  catch err %#ok          (the comment causes a block to appear)
     ;;    TAB>     disp('catch');
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_catch_with_comment.m
     ((n-p-gp nil ,(rx bos "block" eos) ,(rx bos "catch_clause" eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function a
     ;;             x = 1;
     ;; <TAB>       y = 2;
     ((parent-is ,(rx bos "block" eos)) parent 0)

     ;; I-Rule: classdef indent_class_prop_continued
     ;;              properties
     ;;                  ListArrayHeight = struct( ...
     ;;         TAB>         'Short',  {1}, ...
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_class_prop_continued.m
     ((n-p-gp ,(rx bos (or ")" "arguments" "line_continuation") eos)
              ,(rx bos "function_call" eos)
              ,(rx bos "default_value" eos))
      great-grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = ...
     ;; <TAB>        1;
     ((parent-is ,(rx bos "assignment" eos)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = 2 * ...
     ;; <TAB>        1;
     ((parent-is ,(rx bos "binary_operator" eos)) parent 0)

     ;; I-Rule:      something.foo4 = ...
     ;;                  someFcn2( ...
     ;;          TAB>        1, ...
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_fcn_cont.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_fcn_call_last_paren.m
     (,#'matlab-ts-mode--i-assign-cont-matcher
      ,#'matlab-ts-mode--i-assign-cont-anchor
      ,#'matlab-ts-mode--i-assign-cont-offset)

     ;; I-Rule:  a = ( ...       |     a = [  ...     |     a = {  ...
     ;;               1 ...      |          1 ...     |          1 ...
     ;; <TAB>        );          |         ];         |         };
     ((node-is ,(rx bos (or ")" "]" "}") eos)) parent 0)

     ;; I-Rule:  a = ( ...
     ;; <TAB>         1 ...
     ((parent-is ,(rx bos "parenthesis" eos)) parent 1)

     ;; I-Rule:  a = [    2 ...       |   function a ...
     ;; <TAB>             1 ...       |            = fcn
     ((parent-is ,(rx bos (or "row" "function_output") eos)) parent 0)

     ;; I-Rule: a = [1, 2, ...
     ;;              3, 4]
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_matrix.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_cell.m
     (,#'matlab-ts-mode--i-row-matcher
      ,#'matlab-ts-mode--i-row-matcher-anchor
      ,#'matlab-ts-mode--i-row-matcher-offset)

     ;; I-Rule:  a = [   ...    |    a = { ...
     ;; <TAB>          2 ...    |          2 ...
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_matrix.m
     ((parent-is ,(rx bos (or "cell" "matrix") eos)) parent ,matlab-ts-mode--array-indent-level)

     ;; I-Rule:   cell1 = { ...
     ;; See: ./test-matlab-ts-mode-indent-xr-files/indent_xr_cell1.m
     ((n-p-gp nil ,(rx bos "line_continuation" eos) ,(rx bos (or "cell" "matrix") eos))
      grand-parent ,matlab-ts-mode--array-indent-level)

     ;; I-Rule: function args next line
     ;;           function someFunction(...
     ;;               a, b)                          <== TAB/RET to here
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_fcn_args.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_fcn_args_on_next_line.m
     (,#'matlab-ts-mode--i-fcn-args-next-line-matcher
      ,#'matlab-ts-mode--i-fcn-args-next-line-anchor
      ,#'matlab-ts-mode--i-fcn-args-next-line-offset)

     ;;
     ;; I-Rule:  function [   ...              |    function name (a, ... % comment
     ;; <TAB>              a, ... % comment    |                   b, ... % comment
     ((parent-is ,(rx bos (or "multioutput_variable" "function_arguments") eos)) parent 1)

     ;; I-Rule:  a = ...
     ;; <TAB>        1;
     ((n-p-gp nil nil ,(rx bos "assignment" eos)) grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = my_function(1, ...
     ;; <TAB>                    2, ...
     ((parent-is ,(rx bos "arguments" eos)) parent 0)

     ;; I-Rule:      properties (Constant)
     ;;                  property1 = containers.Map(...
     ;;        TAB>          {
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_class_prop_continued2.m
     (,#'matlab-ts-mode--i-arg-namespace-fcn-prop-matcher
      ,#'matlab-ts-mode--i-arg-namespace-fcn-prop-anchor
      ,matlab-ts-mode--indent-level)

     ;; I-Rule:      someNamespace1.subNamespace2.myFunction( ...
     ;;        TAB>      a, ... % comment for param1
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_namespace_fcn_continued.m
     ((n-p-gp ,(rx bos "arguments" eos)
              ,(rx bos "function_call" eos)
              ,(rx bos "field_expression" eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:    mat = [
     ;;                    a.b.c.(d)
     ;;        TAB>        {'other'}
     ;; See: test-matlab-ts-mode-electric-indent-files/electric_indent_mat_with_field_expression.m
     ((n-p-gp ,(rx bos (or "(" "{") eos) nil ,(rx bos "field_expression" eos))
      grand-parent 0)

     ;; I-Rule:  my_function( ...
     ;; <TAB>        1, ...
     ((node-is ,(rx bos "arguments" eos)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function indent_tab_between_fcns   |   function indent_tab_in_fcn
     ;;         end                                |      disp('here')
     ;; <TAB>                                      |
     ;;         function b                         |   end
     ;;         end                                |
     ((lambda (node parent bol)
        (and (not parent)
             (string= (treesit-node-type parent) "\n")))
      grand-parent 0)

     ;; I-Rule: comments in classdef's
     ((parent-is ,(rx bos "class_definition" eos)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: comment within spmd
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_spmd.m
     ((parent-is ,(rx bos "spmd")) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: comment after property:
     ;;         arguments
     ;;             a
     ;;    TAB>     % comment
     ;;         end
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_comment_after_prop.m
     ((node-is "comment") parent 0)

     ;; I-Rule: cell/matrix row matcher when in an error node
     ;;         mat0 = [1, 2
     ;;                 ^            <-- TAB or RET on prior line goes here
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_mat*.m
     (,#'matlab-ts-mode--i-error-row-matcher
      ,#'matlab-ts-mode--i-error-row-anchor
      ,#'matlab-ts-mode--i-error-row-offset)

     ;; I-Rule: error-switch-matcher
     ;;         switch fcn1(a)
     ;;           ^                       <== TAB to here
     (,#'matlab-ts-mode--i-error-switch-matcher
      ,#'matlab-ts-mode--i-error-switch-anchor
      ,#'matlab-ts-mode--i-error-switch-offset)

     ;; I-Rule: line continuations for incomplete continuations
     ;;            myVariable = 1 + myfcn(a, ...
     ;;                                   ^              <== TAB or RET on prior line goes here
     ;;
     ;;            myVariable = 1 + myfcn(a, b) + ...
     ;;                ^                                 <== TAB or RET on prior line goes here
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_i_cont_incomplete*.m
     (,#'matlab-ts-mode--i-cont-incomplete-matcher
      ,#'matlab-ts-mode--i-cont-incomplete-anchor
      ,#'matlab-ts-mode--i-cont-incomplete-offset)

     ;; I-Rule: line continuation for valid statements
     ;;             a1 = { ...
     ;;                    1 ...
     ;;                    + ...
     ;;                    2                    <== TAB or RET on prior line to here
     ;;                  };
     ;; See: test-matlab-ts-mode-indent-files/indent_cell.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_line_continuation.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_line_continuation_row.m
     (,#'matlab-ts-mode--i-cont-matcher parent ,#'matlab-ts-mode--i-cont-offset)

     ;; I-Rule: prior line node
     (,#'matlab-ts-mode--i-prior-line-matcher
      ,#'matlab-ts-mode--i-prior-line-anchor
      0)

     ;; I-Rule: indent blank line when adding args to fcn call
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_blank_line_in_fcn_call.m
     ((and no-node
           (parent-is ,(rx bos "function_call" eos)))
      (lambda (_node parent _bol &rest _)
        (let ((fcn-name (treesit-node-child-by-field-name parent "name")))
          (treesit-node-start (treesit-node-next-sibling fcn-name))))
      1)

     ;; I-Rule: Assert if no rule matched and asserts are enabled.
     (,#'matlab-ts-mode--indent-assert-no-rule 0 0)
     ))
  "Tree-sitter indent rules for `matlab-ts-mode'.")

(defvar matlab-ts-mode--tab-indent-line-limit 50)

(defvar matlab-ts-mode--newline-entered-linenum nil)

(defvar matlab-ts-mode--error-query (when (treesit-available-p)
                                         (treesit-query-compile 'matlab '((ERROR) @e))))
(defun matlab-ts-mode--query-errors ()
  "Query tree-sitter for ERROR's and populate `matlab-ts-mode--ei-errors-map'."
  (let ((curr-err-map matlab-ts-mode--ei-errors-map)
        (error-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                            matlab-ts-mode--error-query nil nil t)))
    (setq matlab-ts-mode--ei-errors-map (make-hash-table))
    (dolist (error-node error-nodes)
      (matlab-ts-mode--ei-mark-error-lines error-node))
    (let ((err-map matlab-ts-mode--ei-errors-map))
      (setq matlab-ts-mode--ei-errors-map curr-err-map)
      ;; result
      err-map)))

(defun matlab-ts-mode--get-region-for-tab-indent ()
  "Return (cons BEG END) to indent.
BEG and END are both start of a line.
This is done such that electric TAB indent via `indent-for-tab-command'.
Example, TAB on any line in:
  foo =1;
  a = 2;
  foobar    =3  ;
results in:
  foo    = 1;
  a      = 2;
  foobar = 3;"
  (let (beg end)
    (save-excursion
      (save-restriction
        (widen)
        (let ((start-bol (pos-bol)))

          (save-excursion
            (setq beg start-bol)
            (cl-loop
             for direction in '(-1 1) do
             (goto-char start-bol)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (when (and (or (not matlab-ts-mode--newline-entered-linenum)
                             (not (= (line-number-at-pos) matlab-ts-mode--newline-entered-linenum)))
                         (looking-at (rx bol (0+ (or " " "\t"))
                                         ;; comment line == no code
                                         (zero-or-one (seq "%" (0+ anychar)))
                                         eol)))
                (cl-return))
              (if (= direction -1)
                  (setq beg (point))
                (setq end (point)))))

            ;; end is at bol of line following the region we want to indent.
            (when end (goto-char end))
            (forward-line)
            (setq end (point)))

          ;; Do not indent too many consecutive lines (large matrices are too slow).
          ;; TODO remove this after speeding up indent.
          (let ((limit matlab-ts-mode--tab-indent-line-limit)
                (beg-linenum (line-number-at-pos beg))
                (end-linenum (line-number-at-pos end)))
            (when (> (1+ (- end-linenum beg-linenum)) limit)
              (let* ((curr-linenum (line-number-at-pos))
                     (n-prior-lines (- curr-linenum beg-linenum)))
                (if (>= n-prior-lines limit)
                    (setq beg (save-excursion (forward-line (1+ (- limit))) (point))
                          end (save-excursion (forward-line) (point)))
                  ;; else keep all prior lines and any following lines up to limit
                  (let ((n-following-lines (- end-linenum curr-linenum)))
                    (setq limit  (- limit n-prior-lines))
                    (when (> n-following-lines limit)
                      (setq end (save-excursion (forward-line limit)
                                                (point)))))))))

          ;; Don't indent error lines (excluding current line)
          ;; TODO - can we avoid the query because we are going to do
          ;;        matlab-ts-mode--ei-line-nodes-in-region and
          ;;        that gets errors which avoids indent?
          (let ((err-map (matlab-ts-mode--query-errors)))
            (cl-loop
             for direction in '(-1 1) do
             (goto-char start-bol)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)

              (if (and (= direction -1) ;; moving backward and at beginning?
                       (< (point) beg))
                  (goto-char (point-min))
                (when (and (= direction 1) ;; moving forward and at end?
                           (> (point) end))
                  (cl-return)))

              ;; On an error line?
              (when (gethash (pos-bol) err-map)
                (if (= direction -1)
                      (progn
                        (forward-line)
                        (setq beg (pos-bol))
                        (goto-char (point-min)))
                  (setq end (pos-bol))
                  (cl-return)))))))))

    ;; region to indent
    (cons beg end)))

(defun matlab-ts-mode--indent-line ()
  "Call `treesit-indent', then do electric indent."
  (treesit-indent) ;; treesit-indent before electric indent to get updated point on the line
  (when matlab-ts-mode--electric-indent
    (save-restriction
      (widen)
      (matlab-ts-mode--ei-workaround-143 (pos-bol) (pos-eol))
      (matlab-ts-mode--ei-indent-elements-in-line))))

(defun matlab-ts-mode-tab-indent (&optional _arg)
  "Indent current and adjacent lines.
Optional prefix _ARG is ignored and provided for compatibility with
`indent-for-tab-command' because we bind TAB to
`matlab-ts-mode-tab-indent'."
  (interactive "P")
  (if (save-excursion ;; at blank line or single-line comment?
        (forward-line 0)
        (looking-at "^[ \t]*\\(?:%.*\\)?$"))
      (matlab-ts-mode--indent-line) ;; line indent only
    ;; Else indent the line plus the non-blank lines before and after the current line
    (let ((beg-end (matlab-ts-mode--get-region-for-tab-indent)))
      (matlab-ts-mode--ei-indent-region (car beg-end) (cdr beg-end))
      (when (save-excursion ;; at start of line?
              (not (re-search-backward "[^ \t]" (pos-bol) t)))
        (back-to-indentation)))))

(defun matlab-ts-mode--delete-whitespace-if-blank-line ()
  "If current line has only whitespace, delete the whitespace."
  (when (and (save-excursion
               (back-to-indentation)
               (not (looking-at "[^ \t\n\r]")))
             (not (= (pos-bol) (pos-eol))))
    (delete-region (pos-bol) (pos-eol))))

(defun matlab-ts-mode-newline (&optional _arg)
  "Wrapper around `newline' to enable electric indent on RET.
_ARG is ignored and for compatibility with `newline'."
  (interactive "*P")
  (if (looking-at "[ \t]*$") ;; At EOL, do "smart" indent
      (let (e-insert-pair)
        (when matlab-ts-mode-electric-ends
          (setq e-insert-pair (matlab-ts-mode--insert-electric-ends)))
        (if e-insert-pair ;; Did insert?
            (progn (matlab-ts-mode--indent-line)
                   (insert "\n")
                   (treesit-indent)
                   (save-excursion
                     (let ((extra-insert (car e-insert-pair)))
                       (when extra-insert
                         (insert extra-insert))))
                   (when (cdr e-insert-pair)
                     (end-of-line)))
          ;; else "TAB" indent after newline
          (if (save-excursion ;; current-line has non-blank content
                (forward-line 0)
                (re-search-forward "[^ \t\n\r]" (pos-eol) t))
              (let (;; Consider typing C-m at end-of "var1 = 10;" line
                    ;;   var1 = 10;
                    ;;   v2   = 20;
                    ;; we get because of the entered linenum setting
                    ;;   var1 = 10;
                    ;;
                    ;;   v2 = 20;
                    (matlab-ts-mode--newline-entered-linenum
                     (when (save-excursion (end-of-line)
                                           (and (not (eobp))
                                                (progn
                                                  (forward-line 1)
                                                  (looking-at "[ \t]*[^ \t\n\r]"))))
                       (1+ (line-number-at-pos)))))

                (insert "\n")
                (let ((curr-linenum (line-number-at-pos)))
                  (forward-line -1)
                  (matlab-ts-mode-tab-indent)
                  (goto-char (point-min))
                  (forward-line (1- curr-linenum)))
                (matlab-ts-mode--indent-line))
            ;; else - just insert a newline because prior line doesn't have content
            (matlab-ts-mode--delete-whitespace-if-blank-line)
            (insert "\n")
            (matlab-ts-mode--indent-line))))
    ;; When in middle of a line, just split it w/o "region" indent because region indent will likely
    ;; be off due to a syntax errors.
    (let ((indent-line-function #'matlab-ts-mode-tab-indent))
      (insert "\n")
      (treesit-indent))))

(defun matlab-ts-mode--indent-region (beg end)
  "Call `treesit-indent-region' on BEG END, then do electric indent."
  (if matlab-ts-mode--electric-indent
      ;; This will widen, then electric indent, then indent-region
      (matlab-ts-mode--ei-indent-region beg end)
    ;; else just indent-region
    (treesit-indent-region beg end)))

;;; Thing settings for movement, etc.

(defvar matlab-ts-mode--statements-type-re
  (rx (seq
       bos
       (or "arguments_statement"
           "assignment"
           "lambda"
           "class_definition"
           "enumeration"
           "events"
           "for_statement"
           "function_call"
           "function_definition"
           "if_statement"
           "methods"
           "property"
           "properties"
           "spmd_statement"
           "switch_statement"
           "try_statement"
           "while_statement")
       eos))
  "MATLAB command statements.")

(cl-defun matlab-ts-mode-beginning-of-statement (&optional
                                                 goto-end statement-type-re
                                                 find-outermost-statement
                                                 no-message)
  "Move to the beginning of a statement.
If optional GOTO-END is \\='end, move to end of the current statement.

We define a command statement to be a complete syntactic unit that has
a start and end.  For example, if point is in an assignment statement
    var = ...
        1;
move point to the \"v\" when GOTO-END is nil, otherwise move to the
point after \";\".  Likewise for other command statements.

The point is moved to the start or end of the innermost statement that
the point is on.  No movement is performed if point is not in a
statement.  This can occur when there are syntax errors or the buffer
has no content.

Optional STATEMENT-TYPE-RE is a regular expression matching the type of
statement to look for.  For example, to move to the beginning of the
current assignment statement, use

  (matlab-ts-mode-beginning-of-statement nil
    (rx (seq bos \"assignment\" eos))

If STATEMENT-TYPE-RE is not specified, `matlab-ts-mode--statements-type-re'
is used.

Optional FIND-OUTERMOST-STATEMENT, if t locates the outermost statement.
For example, if point is on the 11 below will move to the beginning of
the assignment and not the inner function_call statement.

 var = ...
     fun(11, ...
         22);

Optional NO-MESSAGE if non-nil will prevent messages from being displayed.

Returns nil if not in a statement, otherwise the anchor node
used to do the movement."
  (interactive)

  (cl-assert (or (not goto-end) (eq goto-end 'end)))

  (when (not statement-type-re)
    (setq statement-type-re matlab-ts-mode--statements-type-re))

  (let ((start-point (point))
        (search-node (treesit-node-at (point)))
        statement-node)

    ;; When on a newline, back up to prior statement
    (when (and (> (point) 1)
               (equal (treesit-node-type search-node) "\n")
               (re-search-backward "[^ \t\n\r]" nil t))
      (setq search-node (treesit-node-at (point))))

    ;; When at ";" use prev-sibling
    (when (equal (treesit-node-type search-node) ";")
      (setq search-node (treesit-node-prev-sibling search-node)))

    ;; find nearest ancestor that matches statement-type-re
    (while search-node

      ;; Find inner statement node
      (while (and search-node
                  (let ((type (treesit-node-type search-node)))
                    (when (equal type "ERROR")
                      ;; No movement if we have a syntax error
                      (unless no-message
                        (message "Not in statement due to syntax error"))
                      (cl-return-from matlab-ts-mode-beginning-of-statement))
                    (not (string-match-p statement-type-re type))))
        (setq search-node (treesit-node-parent search-node)))

      (when search-node
        (setq statement-node search-node)
        (if find-outermost-statement
            (setq search-node (treesit-node-parent search-node))
          (setq search-node nil))))

    (when (not statement-node)
      (unless no-message
        (message "Not in a statement"))
      (cl-return-from matlab-ts-mode-beginning-of-statement))

    (let* ((statement-start-point (treesit-node-start statement-node))
           (end-node (or
                      ;; Use next sibling node if it's a ";" for the an
                      ;; assignment or function_call.
                      (and (string-match-p (rx (seq bos (or "assignment"
                                                            "function_call"
                                                            "field_expression")
                                                    eos))
                                           (treesit-node-type statement-node))
                           (let ((next-node (treesit-node-next-sibling statement-node)))
                             (when (and next-node
                                        (string= ";" (treesit-node-type next-node)))
                               next-node)))
                      statement-node))
           (statement-end-point (treesit-node-end end-node)))
      (when (and (>= start-point statement-start-point)
                 (<= start-point statement-end-point))
        (cond
         ((eq goto-end 'end)
          (goto-char statement-end-point)
          end-node)
         (t
          (goto-char statement-start-point)
          statement-node))))))

(defun matlab-ts-mode-end-of-statement (&optional statement-type)
  "Move to the end of a command statement.
This is the opposite of `matlab-ts-mode-beginning-of-statement'.
Optional STATEMENT-TYPE is the type of statement to look for.  If
not specified statement in `matlab-ts-mode--statements-ht' are used."
  (interactive)
  (matlab-ts-mode-beginning-of-statement 'end statement-type))

;; TODO - use these for M-a, M-e?

(defun matlab-ts-mode-beginning-of-command ()
  "Move to the beginning of the command at point.
Commands are either assignment or function_call statements that can be
evaluated at the matlab prompt.  Movement occurs only if the point is in
an assignment or function call.  Note array indexing is considered a
function call."
  (when (matlab-ts-mode-beginning-of-statement nil
                                               (rx (seq bos (or "assignment"
                                                                "function_call"
                                                                "field_expression")
                                                        eos))
                                               t)
    (point)))

(defun matlab-ts-mode-end-of-command ()
  "Move to the end of the command at point.
Commands are either assignment or function_call statements that can be
evaluated at the matlab prompt.  Movement occurs only if the point is in
an assignment or function call.  Note array indexing is considered a
function call."
  (when (matlab-ts-mode-beginning-of-statement 'end
                                               (rx (seq bos (or "assignment"
                                                                "function_call"
                                                                "field_expression")
                                                        eos))
                                               t)
    (point)))

;; matlab-ts-mode--thing-settings used for:
;;   C-M-a  - M-x beginning-of-defun
;;   C-M-e  - M-x end-of-defun
;;
;;   C-M-b  - M-x backward-sexp
;;   C-M-f  - M-x forward-sexp
;;   C-M-t  - M-x transpose-sexps (transpose right sexp with left sexp)
;;   C-M-k  - M-x kill-sexp
;;
;;   M-a    - M-x backward-sentence
;;   M-e    - M-x forward-sentence
;;   M-k    - M-x kill-sentence (kill from point to end of sentence)
;;
;;   C-M-u  - M-x backward-up-list
;;
(defvar matlab-ts-mode--thing-settings
  `((matlab
     (defun ,(rx bos "function_definition" eos))
     (sexp ,(rx bos (or "function_definition"
                        "arguments_statement"

                        ;; "property" of "arguments_statement" and "properties"
                        ;;
                        ;; TODO: the property node includes a newline, which means C-M-f jumps to
                        ;; an odd location.  Should the matlab tree sitter grammar be updated or
                        ;; should we special case this?
                        ;;     function method1(in1, in2)
                        ;;         arguments
                        ;;             in1 (1,1) double
                        ;;             ^                         % <- point here, C-M-f
                        ;;             in2 (1,2) double
                        ;;     ^                                 % -> moves point here.
                        ;;         end
                        ;;
                        ;; Here's the nodes:
                        ;;     (property name: (identifier)
                        ;;      (dimensions ( (number) , (number) ))
                        ;;      (identifier) \n)
                        ;;     (property name: (identifier)
                        ;;      (dimensions ( (number) , (number) ))
                        ;;      (identifier) \n)
                        ;;
                        ;; See: https://github.com/acristoffers/tree-sitter-matlab/issues/91
                        "property"

                        "function_arguments"
                        "function_call"

                        "assignment"
                        "break_statement"
                        "cell"
                        "continue_statement"
                        "command"
                        "for_statement"
                        "global_operator"
                        "if_statement"
                        "matrix"
                        "persistent_operator"
                        "return_statement"
                        "switch_statement"
                        "case"
                        "otherwise"
                        "catch"
                        "try_statement"
                        "while_statement"

                        "class_definition"
                        "classdef"
                        "properties"
                        "methods"
                        "events"
                        "enumeration"
                        "enum"

                        "identifier")
                eos))

     (sentence ,(rx bos (or "function_definition"
                            "assignment"
                            "arguments_statement"
                            "for_statement"
                            "if_statement"
                            "switch_statement"
                            "try_statement"
                            "while_statement"
                            "class_definition"
                            "properties"
                            "methods"
                            "events"
                            "enumeration")))

     (text ,(rx bos (or "comment" "string" "line_continuation") eos))

     ))
  "Tree-sitter things for movement.")

(defun matlab-ts-mode--forward-sexp (&optional arg)
  "Use `treesit-forward-sexp' when matching code only.
ARG is described in the docstring of `forward-sexp'.  When we are
matching a parenthesis, bracket, brace, or when point is in a comment do
the normal s-expression movement by calling
`forward-sexp-default-function'."
  (interactive "^p")
  (let* ((move-back (and (numberp arg) (< arg 0)))
         (match-paren (if move-back
                          (member (char-before) '(?\] ?\) ?\}))
                        (member (char-after) '(?\[ ?\( ?\{))))
         node
         node-type)

    (if (or match-paren
            (progn
              (setq node (let ((pt-and-node (matlab-ts-mode--real-node-at-point)))
                           (cdr pt-and-node)))
              (setq node-type (treesit-node-type node))
              (equal node-type "comment")))
        ;; See tests/test-matlab-ts-mode-thing-settings-files/thing_forward_sexp1.m
        (forward-sexp-default-function arg)

      ;; ELSE if in string handle that locally, else use `treesit-forward-sexp'

      (let ((string-node (and node
                              (or (when (string-match-p (rx bos (or "'"
                                                                    "\""
                                                                    "string_content"
                                                                    "formatting_sequence"
                                                                    "escape_sequence")
                                                            eos)
                                                        node-type)
                                    ;; For "'", the parent could be a postfix_operator, e.g.
                                    ;; a transpose: [1 2; 3 4]'
                                    (let ((parent-node (treesit-node-parent node)))
                                      (when (string= (treesit-node-type parent-node) "string")
                                        parent-node)))
                                  (when (string= "string" node-type)
                                    node)))))
        (if string-node
            ;; See: tests/test-matlab-ts-mode-thing-settings-files/thing_mark_sexp.m
            (goto-char (if move-back
                           (treesit-node-start string-node)
                         (treesit-node-end string-node)))

          ;; Use treesit-forward-sexp
          ;;
          ;; Treesit doesn't behave well when point is not at the end or start of the node,
          ;; so fix that. Consider
          ;;     function foo
          ;;         ^          <== point here and C-M-f
          ;;     end
          ;;      ^             <== point here and C-M-b
          ;; See: tests/test-matlab-ts-mode-thing-settings-files/thing_fun_sexp.m
          (when (and node
                     (>= (point) (treesit-node-start node))
                     (< (point) (treesit-node-end node)))
            (if move-back
                (when (string= (treesit-node-type node) "end")
                  (goto-char (treesit-node-end node)))
              (goto-char (treesit-node-start node))))
          (treesit-forward-sexp arg))))))

;;; Change Log

(defun matlab-ts-mode--defun-name (node)
  "Return the defun name of NODE for Change Log entries."
  (when (string-match-p
         (rx bos (or "function_definition" "class_definition") eos)
         (treesit-node-type node))
    (treesit-node-text (treesit-node-child-by-field-name node "name"))))

;;; imenu

(defun matlab-ts-mode--imenu-create-index ()
  "Return index for imenu.
This will return alist of functions and classes in the current buffer:
   \\='((\"function1\" . start-point1)
     (\"function2\" . start-point2)"
  (let ((root (treesit-buffer-root-node))
        class-index
        fcn-index
        section-index)

    ;; Find classdef and function entries
    (treesit-search-subtree
     root
     (lambda (node)
       (let ((type (treesit-node-type node)))
         (pcase type

           ("class_definition"
            (let* ((name (treesit-node-text (treesit-node-child-by-field-name node "name")))
                   (start-pt (treesit-node-start node)))
              (push `(,name . ,start-pt) class-index)))

           ("function_definition"
            (let* ((name-node (treesit-node-child-by-field-name node "name"))
                   (prev-type (treesit-node-type (treesit-node-prev-sibling name-node)))
                   (name (treesit-node-text name-node))
                   (start-pt (treesit-node-start node))
                   (parent (treesit-node-parent node)))

              ;; classdef get.propName or set.propName
              (when (and prev-type
                         (string-match-p (rx bos (or "get." "set.") eos) prev-type))
                (setq name (concat prev-type name)))

              ;; If nested function prefix with that by looking to parent
              (while parent
                (when (string= "function_definition" (treesit-node-type parent))
                  (let ((parent-name
                         (treesit-node-text (treesit-node-child-by-field-name parent "name"))))
                    (setq name (concat parent-name "." name))))
                (setq parent (treesit-node-parent parent)))
              (push `(,name . ,start-pt) fcn-index))))
         nil)))

    (setq class-index (reverse class-index))
    (setq fcn-index (reverse fcn-index))

    ;; comment section headings
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward matlab-ts-mode--comment-section-heading-re nil t)
        (let* ((heading-start (match-beginning 1))
               (heading (match-string 1))
               (start-point (let ((node (treesit-node-at heading-start)))
                              (goto-char (treesit-node-end node))
                              ;; (when (looking-at "[ \t\r\n]")
                              ;;   (re-search-forward "[^ \t\r\n]" nil t))
                              (point))))
          (setq heading (replace-regexp-in-string "^%%[ \t]*" "" (string-trim heading)))
          (when (string= heading "")
            (setq heading (format "Section at line %d" (line-number-at-pos))))
          (push `(,heading . ,start-point) section-index)))
      (setq section-index (reverse section-index)))

    ;; Return the index
    (let (index)
      (when section-index
        (push `("Section" . ,section-index) index))
      (when fcn-index
        (push `("Function" . ,fcn-index) index))
      (when class-index
        (push `("Class" . ,class-index) index))
      index)))

(defun matlab-ts-mode--imenu-setup ()
  "Configure imenu to use tree-sitter, even when lsp-mode is available."

  ;; Test, see: ./tests/test-matlab-ts-mode-imenu.el
  ;;
  ;; When lsp-mode is active, it takes over imenu-create-index-function. To prevent this,
  ;; we could have people do:
  ;;   (add-hook 'matlab-ts-mode-hook (lambda ()
  ;;                                     (setq-local lsp-enable-imenu nil)
  ;;                                     (lsp) ;; or (lsp-deferred)
  ;;                                     ))
  ;; However, tree-sitter is faster and more robust, so always use that.
  ;;
  ;; See:
  ;; https://www.reddit.com/r/emacs/comments/1c216kr/experimenting_with_tree_sitter_and_imenulist
  (setq-local lsp-enable-imenu nil)
  (setq-local imenu-create-index-function #'matlab-ts-mode--imenu-create-index))

;;; Outline minor mode, M-x outline-minor-mode

(defun matlab-ts-mode--outline-predicate (node)
  "Outline headings for `outline-minor-mode' with MATLAB.
Returns t if tree-sitter NODE defines an outline heading."
  (let ((node-type (treesit-node-type node)))
    (or (string-match-p (rx bos (or "function_definition" "class_definition") eos) node-type)
        (and (string= "comment" node-type)
             (save-excursion
               (goto-char (treesit-node-start node))
               (forward-line 0)
               (looking-at matlab-ts-mode--comment-section-heading-re t))))))

;;; Save hooks

(defun matlab-ts-mode--highlight-ask (begin end prompt)
  "Highlight from BEGIN to END while asking PROMPT as a yes-no question."
  (let ((mo (make-overlay begin end (current-buffer)))
        (show-paren-mode nil) ;; this will highlight things we often ask about.  disable.
        ans)
    (condition-case nil
        (progn
          (overlay-put mo 'face 'matlab-ts-region-face)
          (setq ans (y-or-n-p prompt))
          (delete-overlay mo))
      (quit (delete-overlay mo)
            (user-error "Quit")))
    ans))

(defun matlab-ts-mode-on-save-fix-name (&optional no-prompt)
  "If file name and function/classdef name are different, offer to fix.
If optional NO-PROMPT is t, fix the name if needed without prompting."
  ;; See ./tests/test-matlab-ts-mode-on-save-fixes.el
  (interactive)
  (when (or no-prompt (not noninteractive)) ;; can only prompt if in interactive mode
    (let* ((root (treesit-buffer-root-node))
           (children (treesit-node-children root)))
      (cl-loop for child in children do
               (let ((child-type (treesit-node-type child)))
                 (cond
                  ;; Case: function_definition or class_definition
                  ((string-match-p (rx bos (or "function_definition" "class_definition") eos)
                                   child-type)
                   (when (treesit-search-subtree child (rx bos "ERROR" eos) nil t)
                     ;; Don't try to fix code if there's an error, e.g. don't change in1:
                     ;;   function ...
                     ;;       [    ...
                     ;;        in1,  ... comment about in1
                     ;; See: test-matlab-ts-mode-on-save-fixes-files/on_save_no_fix_syntax_error.m
                     (cl-return))

                   (let* ((def-name-node (treesit-node-child-by-field-name child "name"))
                          (def-name (treesit-node-text def-name-node))
                          (file-name (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
                          (base-name-no-ext (replace-regexp-in-string "\\.[^.]+\\'" "" file-name)))
                     ;; When base-name-no-ext is a valid name, MATLAB will use that.
                     ;; Invalid file names result in an error in MATLAB, so don't try to fix
                     ;; the function/classdef name in that case.
                     (when (and (string-match-p "\\`[a-zA-Z][a-zA-Z0-9_]*\\'" base-name-no-ext)
                                (not (string= def-name base-name-no-ext)))
                       (let ((start-pt (treesit-node-start def-name-node))
                             (end-pt (treesit-node-end def-name-node)))
                         (when (or no-prompt
                                   (matlab-ts-mode--highlight-ask
                                    start-pt end-pt
                                    (concat (if (string= child-type "function_definition")
                                                "Function"
                                              "Classdef")
                                            " name and file names are different. Fix?")))
                           (save-excursion
                             (goto-char start-pt)
                             (delete-region start-pt end-pt)
                             (insert base-name-no-ext))))))
                   (cl-return))

                  ;; Case: any other code means this is a script, so no "on save fix name"
                  ((not (string-match-p (rx bos (or "comment"
                                                    "line_continuation"
                                                    "\n")
                                            eos)
                                        child-type))
                   (cl-return))))))))

(defun matlab-ts-mode--write-file-callback ()
  "Called from `write-contents-functions'.
When `matlab-verify-on-save-flag' is true, run `matlab-mode-verify-fix-file'.
Enable/disable `matlab-sections-minor-mode' based on file content."
  (mapc (lambda (fix-function)
          (funcall fix-function))
        matlab-ts-mode-on-save-fixes)
  (matlab-sections-auto-enable-on-mfile-type-fcn (matlab-ts-mode--mfile-type))
  ;; `write-contents-functions' expects this callback to return nil to continue with other hooks and
  ;; the final save. See `run-hook-with-args-until-success'.
  nil)

;;; Electric Pair Mode, M-x electric-pair-mode

(declare-function electric-pair-default-inhibit "elec-pair")

(defun matlab-ts-mode--electric-pair-inhibit-predicate (char)
  "Return non-nil if `electric-pair-mode' should not pair this CHAR.
Do not pair the transpose operator, (\\='), but pair it when used as a
single quote string."

  ;; (point) is just after CHAR. For example, if we type a single quote:
  ;;   x = '
  ;;        ^--(point)

  (cond
   ;; Case: Single quote
   ;;  str = '      : start of single quote string => nil
   ;;  mat'         : transpose operator => t
   ;;  str = " ' "  : add single quote in string => t
   ((eq char ?')
    (let* ((node-back1 (treesit-node-at (- (point) 1)))
           (type-back1 (treesit-node-type node-back1)))
      (cond
       ;; Case: in comment, return t if it looks like a transpose, e.g. A' or similar.
       ((string-match-p (rx bos (or "comment" "line_continuation") eos) type-back1)
        (save-excursion
          (forward-char -1)
          (looking-at "\\w\\|\\s_\\|\\." t)))

       ;; Case: string delimiter
       ;;    double up if starting a new string => return nil
       ;; For:     s = '
       ;; we have:         (source_file
       ;;                   (ERROR (identifier) = (ERROR)))
       ((string= "ERROR" type-back1)
        nil)

       ;; For:             s = '
       ;; We've also seen: (source_file
       ;;                   (ERROR (identifier) = '))
       ((string= "'" type-back1)
        (not (or (equal (treesit-node-type (treesit-node-prev-sibling node-back1)) "ERROR")
                 (equal (treesit-node-type (treesit-node-parent node-back1)) "ERROR"))))

       ;; Case: inside a single quote string
       ;;    s = 'foobar'
       ;;            ^ insert here
       ;;    s = 'foo''bar'
       ((and (string= "string_content" type-back1)
             (string= "'" (treesit-node-type (treesit-node-prev-sibling node-back1))))
        nil)

       ;; Case: not a string delimiter, return t
       ;;   transpose:                  A'
       ;;   inside double quote string: "foo'bar"
       (t
        t))))

   ;; Case: Not a single quote, defer to the standard electric pair handling
   (t
    (funcall #'electric-pair-default-inhibit char))))

;;; show-paren-mode

(declare-function show-paren--default "paren")

(defun matlab-ts-mode--show-paren-or-block ()
  "Function to assign to `show-paren-data-function'.
Highlight MATLAB pairs in addition to standard items paired by
`show-paren-mode'.  Returns a list: \\='(HERE-BEGIN HERE-END THERE-BEGIN
THERE-END MISMATCH) or nil."
  (let* (here-begin
         here-end
         there-begin
         there-end
         mismatch
         (pt-and-node (matlab-ts-mode--real-node-at-point))
         (pt (car pt-and-node))
         (node (cdr pt-and-node)))

    ;; If point is in whitespace, (treesit-node-at (point)) returns the nearest node. For
    ;; paired matching we want the point on either a start or end paired item.
    (let ((node-start (treesit-node-start node))
          (node-end (treesit-node-end node)))
      (when (and (>= pt node-start)
                 (<= pt node-end))
        (let* ((start-to-parent '(("classdef"    . "class_definition")
                                  ("methods"     . "methods")
                                  ("properties"  . "properties")
                                  ("enumeration" . "enumeration")
                                  ("events"      . "events")
                                  ("function"    . "function_definition")
                                  ("arguments"   . "arguments_statement")
                                  ("if"          . "if_statement")
                                  ("switch"      . "switch_statement")
                                  ("for"         . "for_statement")
                                  ("parfor"      . "for_statement")
                                  ("while"       . "while_statement")
                                  ("try"         . "try_statement")
                                  ("spmd"        . "spmd_statement")
                                  ))
               (start-node-types (mapcar (lambda (pair) (car pair)) start-to-parent))
               (start-re (rx-to-string `(seq bos (or ,@start-node-types) eos) t))
               (node-type (treesit-node-type node))
               (parent-node (treesit-node-parent node))
               (parent-type (treesit-node-type parent-node)))

          (cond

           ;; Case: on a start node, e.g. "function"
           ((string-match-p start-re node-type)
            (let* ((expected-parent-type (cdr (assoc node-type start-to-parent)))
                   (expected-end-type "end") ;; all blocks terminate with an "end" statement
                   (end-node (matlab-ts-mode--child-last-node parent-node)))
              (setq here-begin (treesit-node-start node)
                    here-end (treesit-node-end node))
              (if (and (equal (treesit-node-type parent-node) expected-parent-type)
                       (equal (treesit-node-type end-node) expected-end-type))
                  (setq there-begin (treesit-node-start end-node)
                        there-end (treesit-node-end end-node))
                ;; A missing end keyword will result in parent being "ERROR".
                ;; See: tests/test-matlab-ts-mode-show-paren-files/show_paren_classdef_missing_end.m
                (setq mismatch t))))

           ;; Case: on a "end" node or an inner block that should be matched with parent
           ((and (string-match-p (rx bos (or "end" "elseif" "else" "case" "otherwise" "catch") eos)
                                 node-type)
                 ;; and we have a matching parent node
                 (progn
                   (when (not (equal node-type "end"))
                     (setq parent-node (treesit-node-parent parent-node))
                     (setq parent-type (treesit-node-type parent-node)))
                   (let ((parents (mapcar (lambda (pair)
                                            (cdr pair))
                                          start-to-parent)))
                     (member parent-type parents))))
            (let ((parent-to-start (mapcar (lambda (pair)
                                             (cons (cdr pair) (car pair)))
                                           start-to-parent)))

              (setq here-begin (treesit-node-start node))
              (setq here-end (treesit-node-end node))
              (setq there-begin (treesit-node-start parent-node))
              (setq there-end (+ there-begin (length (cdr (assoc parent-type parent-to-start)))))))

           ;; Case: on a single or double quote for a string.
           ((and (or (equal "'" node-type)
                     (equal "\"" node-type))
                 (equal "string" parent-type))
            (let (q-start-node
                  q-end-node)
              (if (= (treesit-node-start parent-node) (treesit-node-start node))
                  ;; looking at start quote
                  (setq q-start-node node
                        q-end-node parent-node)
                ;; else looking at end quote
                (setq q-start-node parent-node
                      q-end-node node))

              (setq here-begin (treesit-node-start q-start-node))
              (setq here-end (1+ here-begin))

              (let* ((candidate-there-end (treesit-node-end q-end-node))
                     (candidate-there-begin (1- candidate-there-end)))
                (cond
                 ;; Case: Have starting quote of a string, but no content or closing quote.
                 ((= here-begin candidate-there-begin)
                  (setq mismatch t))
                 ;; Case: Have starting quote, have string content, but no closing quote
                 ((not (equal (char-after here-begin) (char-after candidate-there-begin)))
                  (setq mismatch t))
                 (t
                  (setq there-begin candidate-there-begin)
                  (setq there-end candidate-there-end))))))

           ))))

    (if (or here-begin here-end)
        (list here-begin here-end there-begin there-end mismatch)
      (funcall #'show-paren--default))))

;;; post-self-command-hook

(cl-defun matlab-ts-mode--is-electric-end-missing (node)
  "Is statement NODE missing an \"end\"?
Point is at start of NODE which is the first non-whitespace character in
the current line for which to add the missing end."

  ;; Return nil if we already have a matching "end" at the NODE line indent level?
  ;; Consider - we have a matching end even though the parse tree has a syntax error.
  ;;    if a && ...            RET
  ;;    end
  ;; We *assume* spaces and not TABs are used for the indent level (MATLAB standard)
  ;; because TAB characters have different meanings for the "tab stops", e.g. 4 or 8
  ;; are inconsistently used.
  (save-excursion
    (let* ((indent-spaces (make-string (current-indentation) ? ))
           (start (replace-regexp-in-string " " " ?" indent-spaces))
           (indent-rx (concat "^" start "[^ \t\n\r]")))
      (forward-line 1)
      (when (and (not (eobp))
                 (re-search-forward indent-rx nil t)
                 (progn
                   (forward-line 0)
                   (looking-at (rx-to-string `(seq bol ,indent-spaces "end" word-end) t))))
        ;; Found a matching "end" at same indent level as NODE
        (cl-return-from matlab-ts-mode--is-electric-end-missing))))

  (let ((last-child (treesit-node-child (treesit-node-parent node) -1)))
    (cond
     ;; Case: no end keyword for the node in the parse tree
     ((or (not last-child)
          (not (string-match-p (rx bos (or "end" "end_keyword") eos)
                               (treesit-node-type last-child))))
      t)

     ;; Case: see if this is an end for a different statement. Consider:
     ;;   function foo(a)
     ;;       if a > 0            <== When RET typed, we want the electric end to be inserted
     ;;   end
     ;; The parse tree is:
     ;;   (function_definition function name: (identifier)
     ;;    (function_arguments ( arguments: (identifier) ))
     ;;    \n
     ;;    (block
     ;;     (if_statement if
     ;;      condition: (comparison_operator (identifier) > (number))
     ;;      \n end)
     ;;     \n))
     ;; Notice the end is attached to the if_statement.  Therefore, we use indent level
     ;; to see if it is really attached.
     ;; This works well assuming that the code is indented when one is editing it.
     ((let ((node-indent-level (save-excursion
                                 (goto-char (treesit-node-start node))
                                 (current-indentation)))
            (end-indent-level (save-excursion
                                (goto-char (treesit-node-start last-child))
                                (current-indentation))))
        (not (= node-indent-level end-indent-level)))
      t)

     ;; Case: newly added statement causes a parse error, so it's missing the end, Consider:
     ;;   classdef foo
     ;;       properties                   <== properties typed, followed by RET
     ;;       methods
     ;;       end
     ;;   end
     ;; which gives us parse tree:
     ;;   (source_file
     ;;    (class_definition classdef name: (identifier) \n
     ;;     (properties properties \n
     ;;      (ERROR , methods)
     ;;      \n end)
     ;;     end)
     ;;    \n)
     ((let* ((statement-node (treesit-node-parent node))
             (child-idx 0)
             (child (treesit-node-child statement-node child-idx))
             (node-type (treesit-node-type node)))

        (while (and child
                    (string-match-p (rx-to-string `(seq bos (or ,node-type "\n" "comment") eos) t)
                                    (treesit-node-type child)))
          (setq child-idx (1+ child-idx))
          (setq child (treesit-node-child statement-node child-idx)))
        (and child (string= (treesit-node-type child) "ERROR")))
      ;; An ERROR immediately after node
      t)

     ;; Otherwise: statement has an end
     (t
      nil
      ))))

(defun matlab-ts-mode--insert-electric-ends ()
  "A RET was typed at EOL, insert \"end\" or \"% \" for doc comment if needed."

  ;; We could also insert the end of a block comment, "%}", when "%{" is seen but that would likely
  ;; be troublesome when trying to comment out a block of code. Before the code, you type "%{" and
  ;; if we were to insert "%}" we'd need to delete that, then move to the end-of-code and type "%}".

  (let ((statement-with-end-re (rx (or "function" "arguments" "if" "switch" "while" "for" "parfor"
                                       "spmd" "try" "classdef" "enumeration" "properties"
                                       "methods" "events")))
        spaces-to-insert extra-insert item-to-insert move-to-end-of-extra-insert)
    (save-excursion
      (matlab-ts-mode--ei-fast-back-to-indentation)
      (let* ((node (treesit-node-at (point)))
             (node-type (treesit-node-type node)))
        (when node
          (cond
           ;; Case: Was a statement entered that requires and end?
           ((and (string-match-p (concat "\\`" statement-with-end-re "\\'") node-type)
                 ;; must be at a keyword and not in whitespace before it
                 ;; See: tests/test-matlab-ts-mode-electric-ends-files/electric_ends_in_ws.m
                 (looking-at statement-with-end-re))
            (when (and (or (> matlab-ts-mode--function-indent-level 0)
                           (not (string= node-type "function")))
                       (matlab-ts-mode--is-electric-end-missing node)) ;; Missing an end?
              ;; Statement for the RET doesn't have an end, so add one at end-indent-level

              ;; We need a blank line after the electric end statement to get correct indent level
              (save-excursion
                (forward-line)
                (when (not (looking-at (rx bol (0+ (or " " "\t")) eol)))
                  (insert "\n")))

              (treesit-indent) ;; to get indent level for electric end
              (setq spaces-to-insert (concat "\n" (make-string (current-indentation) ? ))
                    item-to-insert "end")

              ;; Extra insert, e.g. case for the switch statement.
              (pcase node-type
                ("switch"
                 (setq extra-insert "case "
                       move-to-end-of-extra-insert t))
                ("try"
                 (setq extra-insert (concat spaces-to-insert "catch me"))))))

           ;; Case: Single-line documentation comment, `font-lock-doc-face'? => insert line "% "
           ((and (string= node-type "comment")
                 (eq (get-char-property (point) 'face) 'font-lock-doc-face)
                 (looking-at "%[ \t]*[^ \t\r\n]"))
            (setq spaces-to-insert (make-string (current-indentation) ? ))
            (setq item-to-insert "% "))))))

    (when spaces-to-insert
      (save-excursion
        (insert spaces-to-insert item-to-insert))
      (cons extra-insert move-to-end-of-extra-insert))))

(defun matlab-ts-mode--add-final-newline (&optional restore-readonly)
  "Add a final newline to non-empty buffer to make tree-sitter happy.
If a final new-line is missing, matlab tree-sitter can generate a parse
error because it looks for the newline to finish statements.

If optional RESTORE-READONLY is non-nil, then if the buffer is read-only
upon calling this function, it will be restored back to read-only if a
final newline was inserted."
  (save-excursion
    (goto-char (point-max))
    (when (and (< (point-min) (point-max))
               (not (= (char-before) ?\n)))
      (let ((is-read-only buffer-read-only))
        (when is-read-only
          (read-only-mode 0))
        (insert "\n")
        (when (and restore-readonly
                   is-read-only)
          (set-buffer-modified-p nil))
        (when is-read-only
          (read-only-mode 1))))))

(defun matlab-ts-mode--post-insert-callback ()
  "Callback attached to `post-self-insert-hook'.

The matlab tree-sitter requires a final newline.  Setting
`require-final-newline' to \\='visit-save doesn't guarantee we have a
newline when typing code into the buffer, so we leverage
`post-self-command-hook' to insert a newline if needed.  See
https://github.com/acristoffers/tree-sitter-matlab/issues/34"

  (when (eq major-mode 'matlab-ts-mode)
    (matlab-ts-mode--add-final-newline)))

;;; MLint Flycheck

(defvar flycheck-checkers) ;; incase flycheck is not on the path

(declare-function flycheck-define-command-checker "flycheck")
(declare-function flycheck-buffer-saved-p "flycheck")

(when (and matlab-ts-mode-enable-mlint-flycheck
           (require 'flycheck nil 'noerror))
  (let* ((mlint (matlab--get-mlint-exe)))
    ;; If we have mlint, activate.
    ;; We could display a message here or in matlab-ts-mode if we don't have mlint, but
    ;; this would be just noise and cause problems when running tests with emacs -q.

    (when mlint
      (flycheck-define-command-checker
          'matlab-mlint
        "MATLAB mlint code analyzer"
        :command `(,mlint "-id" "-all" source-original)
        ;; Example mlint messages.
        ;; L 588 (C 46-49): LOAD: To avoid conflicts with functions ....
        :error-patterns
        '((warning line-start "L " line " (C " column "-" column "): " (id (* alnum)) ":" (message))
          (warning line-start "L " line " (C " column "): " (id (* alnum)) ":" (message)))
        :modes '(matlab-ts-mode)
        :predicate #'(lambda () (flycheck-buffer-saved-p)))
      ;; Register matlab-mlint with flycheck
      (add-to-list 'flycheck-checkers 'matlab-mlint))))

;;; MATLAB Code sections, `matlab-sections-minor-mode'

(defun matlab-ts-mode--mfile-type ()
  "Get *.m type, \\='empty, \\='script, \\='function, \\='class."
  (if (save-excursion
        (= (progn (forward-comment (point-max)) (point)) (point-max)))
      ;; Case: 'empty - No code (blanks and comments are considered empty)
      'empty
    (let* ((first-code-node (let* ((root (treesit-buffer-root-node))
                                   (child-idx 0)
                                   (child (treesit-node-child root child-idx)))
                              (while (and child
                                          (string-match-p (rx bos (or "comment"
                                                                      "line_continuation"
                                                                      "\n")
                                                              eos)
                                                          (treesit-node-type child)))
                                (setq child-idx (1+ child-idx))
                                (setq child (treesit-node-child root child-idx)))
                              child))
           (first-code-type (or (treesit-node-type first-code-node) "comment")))
      (pcase first-code-type
        ("class_definition" 'class)
        ("function_definition" 'function)
        (_ 'script)))))

;;; Comment markers

(defun matlab-ts-mode-comment-marker-help ()
  "Display help on triple-x, fix me, and to do comment markers."
  (interactive)
  (with-help-window "*Comment Marker Help*"
    (with-current-buffer "*Comment Marker Help*"
      (setq-local revert-buffer-function (lambda (&rest _)))
      (insert (format "\
Within comments, the following markers will be highlighted:

%-5s : Triple-x markers indicate coding tasks that must be completed
        prior to committing code to your repository.

%-5s : Fix-me markers should be fixed as soon as possible, but are not
        considered coding tasks that must be addressed prior to
        committing code to your repository.

%-5s : To-do markers represent future coding tasks and therefore code
        with these comments can be committed to your repository.
"
                      (propertize (nth 0 matlab-ts-mode--comment-markers)
                                  'face font-lock-warning-face)
                      (propertize (nth 1 matlab-ts-mode--comment-markers)
                                  'face font-lock-warning-face)
                      (propertize (nth 2 matlab-ts-mode--comment-markers)
                                  'face font-lock-warning-face))))))

(defun matlab-ts-mode-grep-comment-markers ()
  "Run grep on current file to find the triple-x, fix-me, and to do markers."
  (interactive)
  (when (not (buffer-file-name))
    (user-error "Buffer %s is not associated with a file" (buffer-name)))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save %s? " (buffer-name)))
        (save-buffer)
      (user-error "Save %s before grep'ing for comment markers" (buffer-name))))
  (let ((pattern (mapconcat #'identity matlab-ts-mode--comment-markers "\\|")))
    (grep (concat grep-command "-wie \"" pattern "\" "
                  (file-name-nondirectory (buffer-file-name))))))

;;; View parse errors

(defun matlab-ts-mode--get-parse-errors (&optional file-name-for-error)
  "Return a string of parse errors in matlab-ts-mode current buffer.
Returns nil if there are no errors.

Optional FILE-NAME-FOR-ERROR is used instead of the `buffer-name' if
provided."

  ;; See: tests/test-matlab-ts-mode-view-parse-errors.el

  (let ((capture-errors (treesit-query-capture (treesit-buffer-root-node) '((ERROR) @e)))
        (result-list '())
        (buf-name (or file-name-for-error
                      (if (buffer-file-name)
                          (file-name-nondirectory (buffer-file-name))
                        (buffer-name)))))
    (dolist (capture-error capture-errors)
      (let* ((error-node (cdr capture-error))
             (start-point (treesit-node-start error-node))
             (start-line (line-number-at-pos start-point))
             (start-col (save-excursion ;; error messages are one based columns
                          (goto-char start-point)
                          (1+ (current-column))))
             (end-point (treesit-node-end error-node))
             (end-line (line-number-at-pos end-point))
             (end-col (save-excursion
                        (goto-char end-point)
                        (1+ (current-column)))))
        (push (format
               "%s:%d:%d: error: parse error from line %d:%d to line %d:%d (point %d to %d)
%5d | %s
      | %s^
"
               buf-name
               start-line start-col
               start-line start-col
               end-line end-col
               start-point
               end-point
               start-line
               ;; error line
               (buffer-substring (save-excursion
                                   (goto-char start-point)
                                   (pos-bol))
                                 (save-excursion
                                   (goto-char start-point)
                                   (pos-eol)))
               ;; space padding for the pointer (^)
               (save-excursion
                 (goto-char start-point)
                 (let ((n-spaces (- start-point (pos-bol))))
                   (make-string n-spaces ? ))))
              result-list)))
    (let ((errs (mapconcat #'identity (reverse result-list))))
      (if (string= errs "")
          (setq errs nil)
        (setq errs (concat "Tree-sitter parse errors.\n" errs)))
      errs)))

(defvar-local matlab-ts-parse-errors-compilation--buffer nil)

(defun matlab-ts-mode--view-parse-errors-recompile ()
  "Get latest parse errors."
  (interactive)
  (unless matlab-ts-parse-errors-compilation--buffer
    (error "No previously parsed buffer"))
  (when (not (buffer-live-p matlab-ts-parse-errors-compilation--buffer))
    (error "Previously parsed buffer was killed"))
  (with-current-buffer matlab-ts-parse-errors-compilation--buffer
    (matlab-ts-view-parse-errors 'no-pop-to-buffer)))

(defvar-keymap matlab-ts-parse-errors-mode-map
  "g" #'matlab-ts-mode--view-parse-errors-recompile)

;; Note, we are using a slightly shorter names (no -mode)
;;   matlab-ts-view-parse-errors
;;   matlab-ts-parse-errors-mode
;; so the mode line indicator, "matlab-ts-parse-errors" is slightly shorter

(define-compilation-mode matlab-ts-parse-errors-mode "m-ts-parse-errors"
  "Variant of `compilation-mode' used for `matlab-ts-view-parse-errors'.")

(defun matlab-ts-view-parse-errors (&optional no-pop-to-buffer)
  "View parse errors in matlab-ts-mode current buffer.
Optional NO-POP-TO-BUFFER, if non-nil will not run `pop-to-buffer'.
The errors are displayed in a \"*parse errors in BUFFER-NAME*\" buffer
and this buffer is returned."

  ;; Note, matlab tree-sitter can detect errors that it seems MATLAB cannot. For example, the
  ;; codeIssues() command in MATLAB R2026a doesn't detect that
  ;;   1 + @foo
  ;; is a syntax error (you cannot add a number to a function handle).

  (interactive)

  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Current buffer major-mode, %s, is not matlab-ts-mode"
                (symbol-name major-mode)))

  (let ((m-buf (current-buffer))
        (m-buf-dir default-directory)
        parse-errors-buf
        (errs (or (matlab-ts-mode--get-parse-errors)
                  "No tree-sitter errors\n"))
        (err-buf-name (concat "*parse errors in " (buffer-name) "*")))

    (with-current-buffer (setq parse-errors-buf (get-buffer-create err-buf-name))
      (read-only-mode -1)
      (auto-revert-mode 0) ;; no need to save history
      (erase-buffer)
      (insert errs)
      (matlab-ts-parse-errors-mode)
      (goto-char (point-min))
      (when (re-search-forward ": error: " nil t)
        (forward-line 0))
      (setq default-directory m-buf-dir)
      (setq-local matlab-ts-parse-errors-compilation--buffer m-buf)
      (read-only-mode 1)
      (when (not no-pop-to-buffer)
        (pop-to-buffer (current-buffer) 'other-window)))
    parse-errors-buf))

(defun matlab-ts--face-section (heading)
  "Insert stylized section HEADING into current buffer."
  (insert "\n" (propertize (concat "[" heading "]") 'face 'custom-group-tag) "\n"))

(defun matlab-ts--face-button (face sample &optional extra-doc)
  "Insert FACE doc in current buffer and make it a button.
SAMPLE is text that is inserted with FACE property.
EXTRA-DOC is extra doc info to add to the face documentation."

  (let ((face-str (symbol-name face))
        (face-doc (replace-regexp-in-string (rx bos "*") "" (face-documentation face))))

    (insert "\n")

    (insert "* ")
    (insert-text-button face-str 'action (lambda (_button)
                                           (customize-face face)))
    (insert "\n")

    (insert "  " (mapconcat #'identity (split-string face-doc "\n") "\n  "))
    (insert "\n")
    (when extra-doc
      (insert "  " (mapconcat #'identity (split-string extra-doc "\n") "\n  "))
      (insert "\n"))

    (insert "  Sample:\n")
    (insert "    " (propertize
                    (mapconcat #'identity (split-string (string-trim sample) "\n") "\n    ")
                    'face face))
    (insert "\n")))

(defun matlab-ts-describe-faces ()
  "Describe the faces used by matlab-ts-mode."
  (interactive)
  (with-help-window "*matlab-ts-mode faces*"
    (with-current-buffer "*matlab-ts-mode faces*"
      (setq-local revert-buffer-function (lambda (&rest _)))

      (insert "The following faces are used by matlab-ts-mode when displaying code.
A face includes the font, style, color, etc.\n")

      (matlab-ts--face-section "Comments")
      (matlab-ts--face-button 'font-lock-doc-face
                              "% documentation comment for function's and classdef's\n")
      (matlab-ts--face-button 'font-lock-comment-face
                              "% a single line comment\n")
      (matlab-ts--face-button 'matlab-ts-mode-pragma-face
                              (concat "%#ok<*AGROW>\n"
                                      "%#function myFcn\n"))
      (matlab-ts--face-button 'matlab-ts-mode-comment-to-do-marker-face
                              (mapconcat #'identity matlab-ts-mode--comment-markers " "))

      (matlab-ts--face-section "Sections")
      (matlab-ts--face-button 'matlab-ts-mode-comment-heading-face
                              "%% My Code Section\n")
      (matlab-ts--face-button 'matlab-sections-highlight-face
                              "code\n")

      (matlab-ts--face-section "Operators")
      (matlab-ts--face-button 'matlab-ts-mode-operator-face
                              "* / + -\n")

      (matlab-ts--face-section "Brackets")
      (matlab-ts--face-button 'font-lock-bracket-face
                              "( ) [ ] { }\n")

      (matlab-ts--face-section "Delimiters")
      (matlab-ts--face-button 'font-lock-delimiter-face
                              ". , : ;\n")

      (matlab-ts--face-section "Strings")
      (matlab-ts--face-button 'font-lock-string-face
                              "string")
      (matlab-ts--face-button 'matlab-ts-mode-string-delimiter-face
                              "\" \\'")
      (matlab-ts--face-button 'font-lock-escape-face
                              "\\n \\t %d %f %g\n"
                              (concat "This includes escape sequences (\\n, etc.) and fprintf "
                                      "format specs (%d, etc.) within strings."))

      (matlab-ts--face-section "Functions")
      (matlab-ts--face-button 'font-lock-function-name-face
                              "myFunction"
                              (concat "Face used in function definitions.\n"
                                      "For example myFunction will get this face:\n"
                                      "    function out1 = myFunction(in1)\n"
                                      "        out1 = in1 * 2;\n"
                                      "    end\n"))
      (matlab-ts--face-button 'font-lock-function-call-face
                              "myFunction"
                              (concat "\n"
                                      "For example myFunction will get this face:\n"
                                      "    x = myFunction(4);\n"
                                      "This is used for both functions and matrices because\n"
                                      "id1(2) can refer to either a function named id1 or\n"
                                      "a matrix named id1.\n"))
      (matlab-ts--face-button 'matlab-function-signature-face
                              "fcn1")

      (matlab-ts--face-section "Command Dual")
      (matlab-ts--face-button 'matlab-ts-mode-command-arg-face
                              "argument")

      (matlab-ts--face-section "System Command")
      (matlab-ts--face-button 'matlab-ts-mode-system-command-face
                              "! ls *.m *.txt")

      (matlab-ts--face-section "Builtins")

      (matlab-ts--face-button 'font-lock-builtin-face
                              "disp"
                              (concat "This includes any builtin identifier, function, etc.\n"
                                      "provided by MATLAB and add-on products"))

      (matlab-ts--face-section "Classdef")
      (matlab-ts--face-button 'matlab-ts-mode-property-face
                              "prop1")

      (matlab-ts--face-section "Variables")
      (matlab-ts--face-button 'matlab-ts-mode-variable-override-builtin-face
                              "disp")

      (matlab-ts--face-button 'font-lock-variable-name-face
                              "var1"
                              (concat "Face used in variable assignment, function inputs, and "
                                      "function outputs\n"
                                      "Example: this face will be used on var1 in: var1 = 1;\n"
                                      "Example: this face will be used on in1 and out1 in: "
                                      " function out1 = fcnName(in1)"))

      (matlab-ts--face-section "Keywords")
      (matlab-ts--face-button 'font-lock-keyword-face
                              "if else elseif end")

      (matlab-ts--face-section "Code")
      (matlab-ts--face-button 'default
                              "sample"
                              (concat "Face used in code\n"
                                      "Example: this face will be used on thing1 and thing2 in: "
                                      "x = thing1 + thing2;\n"
                                      "where thing1 or thing2 can be variables or functions"))

      (matlab-ts--face-section "Type Functions")
      (matlab-ts--face-button 'font-lock-type-face
                              "int8 int16")

      (matlab-ts--face-section "Numbers")
      (matlab-ts--face-button 'matlab-ts-mode-number-face
                              "1234")
      (matlab-ts--face-button 'matlab-ts-mode-end-number-face
                              "end")

      (matlab-ts--face-section "Prompting")
      (matlab-ts--face-button 'matlab-ts-region-face
                              "myFunction")

      (matlab-ts--face-section "Syntax Errors")
      (matlab-ts--face-button 'font-lock-warning-face
                              "a*/*b"
                              (concat "Used when `matlab-ts-mode-font-lock-level' is set to "
                                      "show syntax errors"))

      )))

;;; Our M-q matlab-ts-mode-prog-fill-reindent-defun

(defun matlab-ts-mode-prog-fill-reindent-defun (&optional justify)
  "MATLAB refill or reindent the paragraph or defun at point.

If the point is in a string or a comment, fill the paragraph that
contains point or follows point.  Optional JUSTIFY is passed
to `fill-paragraph'.

Otherwise, reindent the function definition that contains point
or follows point.

This exists because ellipsis line continuations cause
`prog-fill-reindent-defun' to not behave well.  Thus, we handle
these locally."
  (interactive "P")

  (let ((treesit-text-node
         (and (treesit-available-p)
              (treesit-parser-list)
              (let ((node (treesit-node-at (point))))
                (and (treesit-node-match-p node 'text t)
                     (not (equal (treesit-node-type node) "line_continuation")))))))
    (if (or treesit-text-node
            ;; We can't fill strings because doing so introduces syntax errors
            (let ((sp (syntax-ppss)))
              (and (nth 8 sp) ;; string or comment
                   (not (nth 3 sp)))) ;; not string
            (let ((comment-start-pt
                   (save-excursion
                     (when (and (re-search-forward "\\s-*\\s<" (pos-eol) t)
                                (not (equal (treesit-node-type (treesit-node-at (point)))
                                            "line_continuation")))
                       (point)))))
              (when comment-start-pt
                (goto-char comment-start-pt))))
        (save-excursion
          (fill-paragraph justify (region-active-p)))

      ;; else "prog fill reindent"
      (let ((beg (save-excursion
                   (beginning-of-defun)
                   (point)))
            (end (save-excursion
                   (end-of-defun)
                   (point))))
        (indent-region beg end nil)
        (when (looking-at "[ \t]")
          (matlab-ts-mode--ei-fast-back-to-indentation))))))

;;; Keymap

(defvar-keymap matlab-ts-mode-map
  :doc "Keymap for `matlab-ts-mode' buffers."
  :parent prog-mode-map

  ;; Editing commands
  "M-q" #'matlab-ts-mode-prog-fill-reindent-defun
  "C-i" #'matlab-ts-mode-tab-indent
  "C-m" #'matlab-ts-mode-newline
  "C-c C-j" #'matlab-justify-line

  ;; Integration with `matlab-shell'
  "C-c C-s" 'matlab-shell-save-and-go
  "C-c C-r" 'matlab-shell-run-region
  "C-<return>" 'matlab-shell-run-region-or-line
  "C-c ?" 'matlab-shell-locate-fcn
  "C-h C-m" matlab--shell-help-map
  "M-s" 'matlab-show-matlab-shell-buffer
  "C-M-<mouse-2>" 'matlab-shell-find-file-click)

;;; Menu

(easy-menu-define matlab-mode-menu matlab-ts-mode-map
  "Menu for `matlab-ts-mode'."
  '("MATLAB"
    "---" "MATLAB shell"
    ["     Start MATLAB (M-x matlab-shell)" matlab-shell
     :active (not (matlab-shell-active-p))
     :visible (not (matlab-shell-active-p))
     :help "Run MATLAB in a *MATLAB* shell buffer"]
    ["     Switch to MATLAB (M-x matlab-shell)" matlab-shell
     :active (matlab-any-shell-active-p)
     :visible (matlab-any-shell-active-p)
     :help "Switch to the *MATLAB* shell buffer"]
    ["     Save and go" matlab-shell-save-and-go
     :active (matlab-any-shell-active-p)
     :help "Save this *.m file and evaluate it in the *MATLAB* shell"]
    ["     Run region" matlab-shell-run-region
     :active (matlab-any-shell-active-p)
     :help "Evaluate the active region in the *MATLAB* shell buffer"]
    ["     Run region or line" matlab-shell-run-region-or-line
     :active (matlab-any-shell-active-p)
     :help "Evaluate active region or current line in the *MATLAB* shell buffer"]
    ["     Run command" matlab-shell-run-command
     :active (matlab-shell-active-p)
     :help "Prompt for a command and run it in the *MATLAB* shell buffer.
Result is shown in a *MATLAB Run Command Result* buffer."]
    ["     Describe command" matlab-shell-describe-command
     :active (matlab-shell-active-p)
     :help "Run \"help COMMAND\" in the *MATLAB* shell buffer
and display in a help buffer."]
    ["     Describe variable" matlab-shell-describe-variable
     :active (matlab-shell-active-p)
     :help "Evaluate VARIABLE in the *MATLAB* shell buffer and
display result in a buffer"]
    ["     Command Apropos" matlab-shell-apropos
     :active (matlab-shell-active-p)
     :help "Look for active command in *MATLAB* shell buffer matching a regex"]
    ["     Locate MATLAB function" matlab-shell-locate-fcn
     :active (matlab-shell-active-p)
     :help "Run 'which FCN' in the *MATLAB* shell, and if it's a *.m file open it in a buffer"]
    ["     Switch to *MATLAB* shell" matlab-show-matlab-shell-buffer
     :active (matlab-shell-active-p)
     :help "Switch to the buffer containing the MATLAB process"]
    "---"
    ("Code Sections"
     ["Run section" matlab-sections-run-section
      :active matlab-sections-minor-mode
      :help "Run the current \"%% section\" in
matlab-shell (Unix) or matlab-netshell (Windows)"]
     ["Run prior sections" matlab-sections-run-prior-sections
      :active matlab-sections-minor-mode
      :help "Run all \"%% sections\" prior to the current section in
matlab-shell (Unix) or matlab-netshell (Windows)"]
     ["Move to beginning" matlab-sections-beginning-of-section
      :active matlab-sections-minor-mode
      :help "Move point to the beginning of the current \"%% section\""]
     ["Move to end" matlab-sections-end-of-section
      :active matlab-sections-minor-mode
      :help "Move point to the end of the current \"%% section\""]
     ["Backward section" matlab-sections-backward-section
      :active matlab-sections-minor-mode
      :help "Move point backward to the prior \"%% section\""]
     ["Forward section" matlab-sections-forward-section
      :active matlab-sections-minor-mode
      :help "Move point forward to the next \"%% section\""]
     ["Mark/select section" matlab-sections-mark-section
      :active matlab-sections-minor-mode
      :help "Select the current code selection region by placing the
mark at the beginning of the \"%% section\" and point at the end of the section"]
     ["Move section up" matlab-sections-move-section-up
      :active matlab-sections-minor-mode
      :help "Move the current \"%% section\" up."]
     ["Move section down" matlab-sections-move-section-down
      :active matlab-sections-minor-mode
      :help "Move the current \"%% section\" down."]
     "--"
     ["Sections help" matlab-sections-help])
    "----"
    ("Debug"
     ["Edit File (toggle read-only)" matlab-shell-gud-mode-edit
      :help "Exit MATLAB debug minor mode to edit without exiting MATLAB's K>> prompt."
      :visible gud-matlab-debug-active ]
     ["Add Breakpoint (ebstop in FILE at point)" mlgud-break
      :active (matlab-shell-active-p)
      :help "When MATLAB debugger is active, set break point at current M-file point"]
     ["Remove Breakpoint (ebclear in FILE at point)" mlgud-remove
      :active (matlab-shell-active-p)
      :help "When MATLAB debugger is active, remove break point in FILE at point." ]
     ["List Breakpoints (ebstatus)" mlgud-list-breakpoints
      :active (matlab-shell-active-p)
      :help "List active breakpoints."]
     ["Step (dbstep in)" mlgud-step
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, step into line"]
     ["Next (dbstep)" mlgud-next
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, step one line"]
     ["Finish function  (dbstep out)" mlgud-finish
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, run to end of function"]
     ["Continue (dbcont)" mlgud-cont
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, run to next break point or finish"]
     ["Evaluate Expression" matlab-shell-gud-show-symbol-value
      :active (matlab-any-shell-active-p)
      :help "When MATLAB is active, show value of the symbol under point."]
     ["Show Stack" mlg-show-stack
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, show the stack in a buffer."]

     ;; TODO [future] gud up/down
     ;;  Advertise these more if we can get them working w/ mlgud's frame show.
     ;;      ["Up Call Stack (dbup)" mlgud-up
     ;;       :active gud-matlab-debug-active
     ;;       :help "When MATLAB debugger is active and at break point, go up a frame"]
     ;;      ["Down Call Stack (dbdown)" mlgud-down
     ;;       :active gud-matlab-debug-active
     ;;       :help "When MATLAB debugger is active and at break point, go down a frame"]

     ["Quit debugging (dbquit)" mlgud-stop-subjob
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, stop debugging"]
     )

    "----"
    ["View mlint code analyzer messages" flycheck-list-errors
     :help "View mlint code analyzer messages.
Click FlyC in the mode-line for more options."]
    ["View tree-sitter parse errors" matlab-ts-view-parse-errors
     :help "The MATLAB tree-sitter is the engine behind matlab-ts-mode.
Parse errors are not as detailed as mlint code analyzer messages."]
    "----"
    ["Jump to function" imenu]
    "----"
    ("Editing"
     ["Grep comment markers" matlab-ts-mode-grep-comment-markers
      :help "Run grep to find triple-x, fix-me, and to do comment markers."]
     ["Comment marker help" matlab-ts-mode-comment-marker-help]
     ["Fill comment / string / indent function" prog-fill-reindent-defun]
     ["Indent region" indent-region
      :help "Indent active region"]
     ["Justify line" matlab-justify-line]
     ["Comment DWIM" comment-dwim
      :help "Comment Do What I Mean
If region is active comment or uncomment it,
Else insert comment if line is empty,
Else call comment-indent.
See `comment-dwim' for more capabilities."]
     ["Comment/uncomment current line " comment-line
      :help "Comment or uncomment current line and leave point after it."]
     ["Set comment column to point" comment-set-column
      :help "Set the column for when M-; inserts a column"])

    "----"
    ["Check setup" matlab-ts-mode-check-setup]
    ["Describe faces" matlab-ts-describe-faces
     :help "Describe faces (color, font, etc.) used by matlab-ts-mode."]
    ("Customize"
     ["Customize matlab-ts-mode" (lambda ()
                                   (interactive)
                                   (customize-group 'matlab-ts))]
     ["Customize matlab-shell" (lambda ()
                                 (interactive)
                                 (require 'matlab-shell)
                                 (customize-group 'matlab-shell))
      :help "Customize \\[matlab-shell] which is used by matlab-ts-mode for code evaluation."]
     )
    ))

;;; matlab-ts-mode

;;;###autoload
(define-derived-mode matlab-ts-mode prog-mode "MATLAB:ts"
  "Major mode for editing MATLAB files, powered by tree-sitter.

If you have the MATLAB tree-sitter grammar installed,
  (treesit-ready-p \\='matlab)
is t

1. Tell Emacs to use matlab-ts-mode for MATLAB files by adding the
   following to your `user-init-file' which is typically ~/.emacs, or
   add it to your `site-run-file'

    (add-to-list \\='major-mode-remap-alist \\='(matlab-mode . matlab-ts-mode))

2. Tell `org-mode' that #+begin_src matlab ... #end_src blocks should use
   matlab-ts-mode:

     \\[customize-variable] RET org-src-lang-modes RET

   and map matlab to matlab-ts:

      Language name: matlab
      Major mode: matlab-ts

This mode is independent from the classic matlab-mode.el, `matlab-mode',
so configuration variables of that mode, do not affect this mode.

\\{matlab-ts-mode-map}"

  (matlab-ts-mode--check-file-encoding)

  (when (treesit-ready-p 'matlab)

    (matlab-ts-mode--add-final-newline 'restore-readonly)

    (treesit-parser-create 'matlab)

    ;; Syntax table - think of this as a "language character descriptor". It tells us what
    ;; characters belong to word like things giving us movement commands e.g. C-M-f, matching
    ;; parens, `show-paren-mode', etc.
    ;; See: ./tests/test-matlab-ts-mode-syntax-table.el
    (set-syntax-table matlab-ts-mode--syntax-table)
    (setq-local syntax-propertize-function #'matlab-ts-mode--syntax-propertize)

    ;; Comments.
    ;; See: tests/test-matlab-ts-mode-comments.el
    (setq-local comment-start      "% ")
    (setq-local comment-end        "")
    (setq-local comment-start-skip "%\\s-*")

    ;; Setup `forward-page' and `backward-page' to use ^L or "%% heading" comments
    ;; See: ./tests/test-matlab-ts-mode-page.el
    (setq-local page-delimiter "^\\(?:\f\\|\\s-*%%\\(?:\\s-\\|\n\\)\\)")

    ;; Font-lock.
    ;; See: ./tests/test-matlab-ts-mode-font-lock.el
    (setq-local treesit-font-lock-level matlab-ts-mode-font-lock-level)
    (setq-local treesit-font-lock-settings matlab-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment comment-special comment-marker definition fcn-name-value)
                  (keyword operator string type command-name command-arg)
                  (variable builtins namespace-builtins number bracket delimiter)
                  (syntax-error)))

    ;; Indent.
    ;; See: ./tests/test-matlab-ts-mode-indent.el
    (matlab-ts-mode--set-function-indent-level) ;; Set function indent level on file load
    (setq-local indent-tabs-mode nil) ;; For consistency between Unix and Windows we don't use TABs.
    (setq-local treesit-simple-indent-rules
                (if treesit--indent-verbose ;; add debugging print as first rule?
                    (list (append `,(list (caar matlab-ts-mode--indent-rules))
                                  (list matlab-ts--indent-debug-rule)
                                  (cdar matlab-ts-mode--indent-rules)))
                  matlab-ts-mode--indent-rules))

    ;; Thing settings for movement, etc.
    ;; See: tests/test-matlab-ts-mode-thing-settings.el
    (setq-local treesit-thing-settings matlab-ts-mode--thing-settings)

    ;; Change Logs.
    ;; See: tests/test-matlab-ts-mode-treesit-defun-name.el
    (setq-local treesit-defun-name-function #'matlab-ts-mode--defun-name)

    ;; M-x imenu
    (matlab-ts-mode--imenu-setup)

    ;; M-x outline-minor-mode
    ;; See: ./tests/test-matlab-ts-mode-outline.el
    (setq-local treesit-outline-predicate #'matlab-ts-mode--outline-predicate)

    ;; Save hook.
    ;; See: ./tests/test-matlab-ts-mode-on-save-fixes.el
    (add-hook 'write-contents-functions #'matlab-ts-mode--write-file-callback -99 t)

    ;; Electric pair mode.
    ;; See tests/test-matlab-ts-mode-electric-pair.el
    (setq-local electric-pair-inhibit-predicate #'matlab-ts-mode--electric-pair-inhibit-predicate)

    ;; show-paren-mode. Highlight parens OR function/end, if/end, etc. type blocks.
    ;; See: tests/test-matlab-ts-mode-show-paren.el
    (setq-local show-paren-data-function #'matlab-ts-mode--show-paren-or-block)

    ;; Final newline and electric ends.
    (setq-local require-final-newline 'visit-save)
    (add-hook 'post-self-insert-hook #'matlab-ts-mode--post-insert-callback -99 t)

    ;; give each file it's own parameter history
    (setq-local matlab-shell-save-and-go-history '("()"))

    ;; Activate MATLAB script ";; heading" matlab-sections-minor-mode if needed
    (matlab-sections-auto-enable-on-mfile-type-fcn (matlab-ts-mode--mfile-type))

    ;; TODO [future] Indent - complex for statement
    ;;         function a = foo(inputArgument1)
    ;;             for (idx = (a.b.getStartValue(((inputArgument1 + someOtherFunction(b)) * 2 - ...
    ;;                                            offset))) ...
    ;;     TAB>         : ...
    ;;                  2 * (a.b.myFcn(inputArgument1, ...
    ;;                                 'end') + ...
    ;;                       100))
    ;;                 disp(idx)
    ;;             end
    ;;         end
    ;;
    ;; TODO [future] Improve semantic movement
    ;;      thing-settings doesn't work well. Directly implement C-M-f, M-e, etc.
    ;;
    ;; TODO [future] add matlab-sections-minor-mode indicator in mode line and make it clickable so
    ;;      it can be turned off
    ;;
    ;; TODO [future] add error indicator in mode line and make it clickable so one can see the
    ;;      errors.  Also underline them.

    (treesit-major-mode-setup)

    (setq-local indent-line-function #'matlab-ts-mode--indent-line)
    (setq-local indent-region-function #'matlab-ts-mode--indent-region)

    ;; Correct forward-sexp setup created by `treesit-major-mode' so that for parenthesis, brackets,
    ;; braces, and comments we do normal s-expression matching using parenthesis. This fix is need
    ;; for our tests work. We need to evaluate (t-utils-NAME ....) expressions from within comments
    ;; using C-x C-e and this leverages forward-sexp to match up the parentheses.
    (setq-local forward-sexp-function #'matlab-ts-mode--forward-sexp)

    ;; M-x matlab-shell Debugger interconnect
    (let ((km (current-local-map)))
      (substitute-key-definition 'read-only-mode #'matlab-toggle-read-only km global-map))

    ))

;;; Check setup

(defvar flycheck-mode) ;; from flycheck.el
(declare-function flycheck-list-errors "flycheck.el")

(defun matlab-ts-mode-check-setup ()
  "Check that Emacs has been setup fully for use with MATLAB."
  (interactive)

  ;; TODO [future] check that we have the latest versions of packages

  (let ((msg ""))

    ;;---------;;
    ;; company ;;
    ;;---------;;
    (if (not (featurep 'flycheck))
        (setq msg (concat msg "\
- [ ] Package company is not installed (needed for TAB completions).
      To install: M-x package-install RET company RET\n"))
      (setq msg (concat msg "\
- [x] Package company is installed (needed for TAB completions)\n")))

    ;;--------------------;;
    ;; mlint and flycheck ;;
    ;;--------------------;;
    (cond
     ((not matlab-ts-mode-enable-mlint-flycheck)
      (setq msg (concat msg "\
- MATLAB mlint is disabled.  To enable,
  M-x customize-variable RET matlab-ts-mode-enable-mlint-flycheck\n")))

     ((not (featurep 'flycheck))
      (setq msg (concat msg "\
- [ ] Package flycheck is not installed (needed to view code issues as you type).
      To install, see C-h v matlab-ts-mode-enable-mlint-flycheck\n")))

     ((not (matlab--get-mlint-exe))
      (setq msg (concat msg "\
- [ ] MATLAB mlint is not found, to fix place /path/to/MATLAB-install on your system path\n")))

     ((not flycheck-mode)
      (setq msg (concat msg "\
- [ ] M-x flycheck-mode is not enabled.  A fix is to add to ~/.emacs
      (global-flycheck-mode)\n")))

     (t
      (setq msg (concat msg (format "\
- [x] matlab-mlint flycheck is setup.
      Use \"%s\" to view mlint errors or click FlyC on the mode line.\n"
                                    (substitute-command-keys "\\[flycheck-list-errors]"))))))

    ;;----------;;
    ;; lsp-mode ;;
    ;;----------;;

    ;; TODO [future] check lsp-matlab configuration, location of matlabls index.js
    ;; TODO [future] check that we have latest matlabls version
    ;; TODO [future] add auto-install of the language server, perhaps using the vscode bundle?

    (cond
     ((not (featurep 'lsp-mode))
      (setq msg (concat msg "\
- [ ] Package lsp-mode is not installed.
      lsp-mode provides code navigation, symbol rename, and more.
      To install, see
         https://github.com/mathworks/Emacs-MATLAB-Mode/blob/default/doc/\
matlab-language-server-lsp-mode.org\n")))

     ((and (not (member 'lsp          matlab-ts-mode-hook))
           (not (member 'lsp-deferred matlab-ts-mode-hook)))
      (setq msg (concat msg "\
- The matlab-ts-mode-hook does not contain #'lsp or #'lsp-deferred.  To fix, see
  https://github.com/mathworks/Emacs-MATLAB-Mode/blob/default/doc/\
matlab-language-server-lsp-mode.org\n"
                        )))

     (t
      (setq msg (concat msg "\
- [x] lsp-mode for code navigation, symbol rename, and more is setup\n"))))

    ;; Display check result
    (message "%s" msg)))

(provide 'matlab-ts-mode)
;;; matlab-ts-mode.el ends here

;; LocalWords:  SPDX gmail dylib libtree treesit builtins defface defcustom flycheck MLint MELPA LF
;; LocalWords:  defun progn setq MEC propertize varname eobp mcm defmacro sexp defconst bos eos prev
;; LocalWords:  classdef's Fontify fontified fontify gethash pragma's multioutput pred dir's bol cdr
;; LocalWords:  NPS BUF myfcn pcase xr repeat:nil docstring numberp imenu alist nondirectory mapc ws
;; LocalWords:  funcall mfile elec foo'bar mapcar lsp noerror alnum featurep grep'ing mapconcat wie
;; LocalWords:  Keymap keymap netshell gud ebstop mlgud ebclear ebstatus mlg mlgud's subjob reindent
;; LocalWords:  DWIM dwim parens caar cdar utils fooenum mcode CRLF cmddual lang nconc listify kbd
;; LocalWords:  matlabls vscode buf dolist sp ppss bobp sexps pragmas curr ei isstring eol listp
;; LocalWords:  nums setf tmp fubar linenum anychar linenums
