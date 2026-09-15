;; matlab-ts-mode--ei.el --- MATLAB electric indent -*- lexical-binding: t -*-

;; Version: 8.2.1
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Dec-29-2025
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
;; Electric indent: standardize language element spacing, align consecutive statements, columns,
;; align trailing comments, add missing commas to arrays, aligns multi-line matrices columns, and
;; remove tabs.

;;; Code:

(require 'treesit)

(defvar matlab-ts-mode--array-indent-level)
(defvar matlab-ts-mode--indent-level)
(declare-function matlab-ts-mode "matlab-ts-mode.el")

(defgroup matlab-ts nil
  "MATLAB(R) tree-sitter mode."
  :prefix "matlab-ts-mode-"
  :group 'languages)

(defvar matlab-ts-mode--electric-indent t) ;; This shouldn't be disabled - it's here for testing.

(defvar matlab-ts-mode--electric-indent-verbose nil)
(defvar matlab-ts-mode--indent-assert) ;; from matlab-ts-mode.el

(defvar matlab-ts-mode--ei-keywords-re
  (rx bos (or "arguments" ;; not technically a keyword (can be a variable), but treat as a keyword
              "break"
              "case"
              "catch"
              "classdef"
              "continue"
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
              "return"
              "set." ;; when used in a classdef method, e.g. function obj = set.propName(obj, val)
              "spmd"
              "switch"
              "try"
              "while")
      eos))

(defvar matlab-ts-mode--ei-pad-op-re
  (rx bos (or "+" "-" "*" "/" ".*" "./" ".\\" "\\"
              "==" "~=" ">" ">=" "<" "<="
              "&" "|" "&&" "||"
              "="
              "^" ".^"
              "'" ".'"
              ":")
      eos))

(defvar matlab-ts-mode--ei-val-re (rx bos (or "identifier" "number") eos))

(defvar matlab-ts-mode--ei-spacing
  ;; In a given line, we walk across the nodes adjusting spaces between NODE and NEXT-NODE to
  ;; have N-SPACES-BETWEEN them.
  ;;
  ;; NODE-RE                          NEXT-NODE-RE                                 N-SPACES-BETWEEN
  `(

    ("."                              ,(rx bos (or "dim-(" "comment" "line_continuation") eos)   1)

    ;; Case: property dimension
    ;;         foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
    ("."                              ,(rx bos "prop-dim" eos)                                   0)

    ;; Case;  obj@myUtils.super;
    ;; TopTester: tests/test-matlab-ts-mode-electric-indent-files/electric_indent_call_super.m
    ("."                              ,(rx bos "@-fcn-call" eos)                                 0)
    (,(rx bos "@-fcn-call" eos)       "."                                                        0)

    ;; Case: classdef property get/set
    ;; TopTester: electric_indent_classdef_prop_get_set.m
    (,(rx bos (or "get." "set.") eos) "."                                                        0)

    ;; Case: a.?b, M', M.'
    ("."                              ,(rx bos (or ".?" "'" ".'") eos)                           0)
    (,(rx bos ".?" eos)               "."                                                        0)

    ;; Case 10 .^ a   (must have a space between a number an dot to avoid changing node type)
    (,(rx bos "number" eos)           ,(rx bos ".^" eos)                                         1)

    ;; Case: power and transpose: a^b a.^b
    (,matlab-ts-mode--ei-val-re       (,(rx bos (or "^" ".^") eos) . ,matlab-ts-mode--ei-val-re) 0)

    ;; Case: anything followed by ",", etc.: [a, b]
    ("."                              ,(rx bos (or "," ";" ".") eos)                             0)

    ;; Case lambda: @(x), metaclass operator, ?
    (,(rx bos (or "@" "?") eos)       "."                                                        0)

    ;; Case: anything after a ".", e.g. foo.bar, s.(fieldName)
    (,(rx bos "." eos)                "."                                                        0)

    (,(rx bos (or "," ";" "command_argument" "command_name" "enum-id") eos)  "."                 1)

    ;; Case: open bracket etc., not (~var), unary operator (+123, -123)
    ;;       Example: [123
    ;;                 ^
    (,(rx bos (or "[" "{" "(" "~" "unary-op") eos)  "."                                          0)

    ;; Case: close bracket, etc.
    (,(rx bos "]" eos)                ,(rx bos (or "," ";") eos)                                 0)
    (,(rx bos "]" eos)                ,(rx bos "[" eos)                                          1)
    ("."                              ,(rx bos (or "]" ")" "}" "lambda-)") eos)                  0)

    ;; Case: ") identifier" as in: propName (1, 1) double
    ;;       arguments:  g (1,1) {mustBeNumeric, mustBeReal}
    (,(rx bos "dim-)" eos)            "."                                                        1)
    (,(rx bos "prop-dim" eos)         ,(rx bos "dim-)" eos)                                      0)

    ;; Case: @(x) ((ischar(x) || isstring(x)));
    ;;          ^
    (,(rx bos "lambda-)" eos)         "."                                                        1)

    ;; Case: property identifier (the prop or class): propName (1,1) double
    (,(rx bos (or "prop-id" "prop-class-id") eos)   "."                                          1)

    ;; Case: padded operators, e.g.: a || b     c * d
    (,matlab-ts-mode--ei-pad-op-re    "."                                                        1)
    ("."                              ,matlab-ts-mode--ei-pad-op-re                              1)

    ;; Case: string followed by anything, e.g. ["string1" foo(1)]
    (,(rx bos "string" eos)           "."                                                        1)

    ;; Case: anything before string, e.g. [foo(1) "string1"]
    ("."                              ,(rx bos "string" eos)                                     1)

    ;; Case: c3 = {b [c '%']};
    ("."                              ,(rx bos "[" eos)                                          1)

    ;; Case: foo(1) or foo.bar
    ;; Case: events(thing), enumeration(thing), methods(thing)
    (,(rx bos (or (seq (1+ (in "a-z")) "-fcn") "identifier") eos)  ,(rx bos (or "(" "{") eos)    0)
    (,(rx bos "identifier" eos)       "."                                                        1)
    (,(rx "-fcn" eos)                 "."                                                        1)

    ;; Case: number in matrix: [123 456]
    (,(rx bos "number" eos)           "."                                                        1)

    ;; Case: subclass
    (,(rx bos "<" eos)                "."                                                        1)

    ;; Case: keywords, e.g. if condition
    ;;                         ^
    (,matlab-ts-mode--ei-keywords-re  "."                                                        1)

    ;; Case: c = {['freq' '%'] num2str(2)};
    (,(rx bos "]" eos)                "."                                                        1)

    ;; Case: c4{1} = [1 2; 3 4];
    ;;       v4 = [c4{1}(1,1), c4{1}(1,1)];
    ;;       {s.('field1')(3)}
    (,(rx bos (or "}" ")") eos)       ,(rx bos (or "(" "{") eos)                                 0)

    ;; Case: ")": m3 = uint8([ones(20,1); 2*ones(8,1)]);
    ;;                                 ^  ^
    ;; Case: c3 = {{[17.50 0] [17.50 0]} {[120 0] [120 20]}};
    ;;                                 ^ ^
    (,(rx bos (or ")" "}") eos)       "."                                                        1)

    ;; We shouldn't hit the ERROR node because matlab-ts-mode--ei-no-elements-to-indent
    ;; says to skip lines with ERROR nodes, but be safe in case an ERROR node spans
    ;; multiple lines without inner ERROR nodes.
    (,(rx bos "ERROR" eos)            "."                                                        1)

    ))

;; matlab-ts-mode--ei-spacing-fast
;;    This is used to avoid scanning the matlab-ts-mode--ei-spacing rules.  This fast lookup
;;    resolves the vast majority of pairs (especially the number-number case dominant in matrix
;;    data) in O(1) via hash, falling back to the original linear scan only for rare unmatched
;;    pairs.
(defvar matlab-ts-mode--ei-spacing-fast
  (let ((ht (make-hash-table :test 'equal))
        ;; all-types are the MODIFIED-NODE-TYPE values returned by
        ;; `matlab-ts-mode--ei-line-nodes-in-region'. This will need to be updated
        ;; as `matlab-ts-mode--ei-line-nodes-in-region' evolves. We assert this below.
        (all-types '(
                     ;; Non-modified types that match capture nodes
                     "identifier" "number" "string" "comment" "line_continuation"
                     "command_argument" "command_name"
                     "," ";" "." "(" ")" "[" "]" "{" "}"
                     "+" "-" "*" "/" "\\" "^" ":" "="
                     ".*" "./" ".\\" ".^" ".?" ".'"
                     "<" ">" "<=" ">=" "==" "~="
                     "&" "|" "&&" "||"
                     "~" "@" "?" "'"
                     ;; Non-modified type keywords
                     "arguments" "break" "case" "catch" "classdef" "continue"
                     "else" "elseif" "end" "enumeration" "events" "for" "function"
                     "get." "global" "if" "methods" "otherwise" "parfor" "persistent"
                     "properties" "return" "set." "spmd" "switch" "try" "while"
                     ;; Modified types
                     "prop-id" "prop-class-id" "enum-id" "prop-dim"
                     "unary-op" "@-fcn-call" "dim-(" "dim-)" "lambda-)"
                     ;; Keyword-command modified types
                     "events-fcn" "enumeration-fcn" "methods-fcn" "arguments-fcn"
                     ;; Safety
                     "ERROR")
                   )
        (matched-node-re-ht (make-hash-table :test 'equal)))

    (dolist (tuple matlab-ts-mode--ei-spacing)
      (let* ((node-re   (nth 0 tuple))
             (next-spec (nth 1 tuple))
             (is-3node  (and (listp next-spec) (consp next-spec)))
             (next-node-re (if is-3node (car next-spec) next-spec))
             (n-spaces  (nth 2 tuple))

             matched-node-re)

        (if is-3node
            ;; Skip the 3-node rule — it is handled specially in the lookup function
            (setq matched-node-re node-re)
          (dolist (node-type all-types)
            (when (string-match-p node-re node-type)
              (setq matched-node-re node-re)
              (let (found-next-node-re)
                (dolist (next-node-type all-types)
                  (when (string-match-p next-node-re next-node-type)
                    (setq found-next-node-re t)
                    (let ((key (cons node-type next-node-type)))
                      ;; First match rule wins
                      (unless (gethash key ht)
                        (puthash key n-spaces ht)))))
                (cl-assert found-next-node-re)))))
        (cl-assert matched-node-re)
        (puthash matched-node-re t matched-node-re-ht)))
    ;; Assert we've covered all tuples in the ht
    (dolist (tuple matlab-ts-mode--ei-spacing)
      (let ((node-re (nth 0 tuple)))
        (cl-assert (gethash node-re matched-node-re-ht))))
    ;; Result
    ht)
  "Hash (node-type . next-node-type) -> n-spaces for fast exact-pair dispatch.
Enumerates all concrete (node-type . next-node-type) pairs implied by
the spacing rules.  Rules are processed in order so the first match wins.")

(defun matlab-ts-mode--assert-msg (msg)
  "Call error with MSG for code that shouldn't be hit."
  (error "Assert: %s" msg))

(defun matlab-ts-mode--ei-fast-back-to-indentation ()
  "Fast back to indentation.
The `back-to-indentation' function uses the syntax table causes
slowdowns.  In MATLAB mode, the only whitespace characters we are
concerned with are SPC and TAB.  Thus, we can be fast.  Returns t if
there are non-whitespace characters on the line, nil otherwise."
  (forward-line 0)
  (if (re-search-forward "[^ \t]" (pos-eol) t)
      (progn
        (backward-char)
        t)
    (let ((inhibit-field-text-motion t)) (end-of-line))
    nil))

(defun matlab-ts-mode--ei-get-node-to-use (node)
  "Given NODE, adjust it for electric indent.
Returns (NODE . MODIFIED-NODE-TYPE) pair.
For example, when node parent is a string, we return the parent.  When
node is a \"+\" and parent is a unary_operator, we return MODIFIED-NODE-TYPE to
be unary-op even though the node type is \"+\"."

  (let* ((node-type (treesit-node-type node))
         (parent (treesit-node-parent node))
         (parent-type (or (treesit-node-type parent) "")))

    (cond
     ;; Case: Use string and not the elements of the string
     ((string= parent-type "string")
      (setq node parent
            node-type parent-type))

     ;; Case: prop-id, prop-class-id, enum-id
     ((string= node-type "identifier")
      (cond ((string= parent-type "property") ;; propertyWithOutDot?
             (if (equal (treesit-node-child parent 0) node)
                 (setq node-type "prop-id")
               (setq node-type "prop-class-id")))
            ((string= parent-type "property_name") ;; property.nameWithDot?
             (if (equal (treesit-node-child (treesit-node-parent parent) 0) parent)
                 (setq node-type "prop-id")
               (setq node-type "prop-class-id")))
            ((string= parent-type "enum")
             (setq node-type "enum-id"))
            ))

     ;; Case: unary operator sign, + or -, e.g. [0 -e] or g = - e
     ((and (string= parent-type "unary_operator")
           (equal (treesit-node-child parent 0) node))
      (setq node-type "unary-op"))

     ;; Case: super-class constructor call
     ;;  obj@myUtils.super;
     ((and (string= node-type "@")
           (string= parent-type "function_call"))
      (setq node-type "@-fcn-call"))

     ;; Case: property dimensions
     ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
     ((and (or (string= node-type "number") (string= node-type ":"))
           (or (string= parent-type "dimensions")
               (and (string= parent-type "spread_operator")
                    (string= (treesit-node-type (treesit-node-parent parent)) "dimensions"))))
      (setq node-type "prop-dim"))

     ;; Case: events, enumeration, methods
     ((and (string-match-p (rx bos (or "events" "enumeration" "methods" "arguments") eos) node-type)
           (string= parent-type "identifier"))
      ;; TopTester: electric_indent_inspect_keyword_commands.m
      ;; TopTester: electric_indent_inspect_keyword_commands2.m
      (setq node-type (concat node-type "-fcn")))

     ;; Case: lambda:     @(x) ((ischar(x) || isstring(x)))
     ;;                      ^
     ;;       properties: foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
     ;;                             ^
     ((string= node-type ")")
      (if (string= parent-type "lambda")
          (setq node-type "lambda-)")
        (if (string= parent-type "dimensions")
            (setq node-type "dim-)"))
        ))

     ;; Case: arguments fcn keyword: arguments (1, :) {mustBeNumeric}
     ;;                                        ^
     ((and (string= node-type "(")
           (string= parent-type "dimensions"))
      (setq node-type "dim-(")))

    (cons node node-type)))

;; matlab-ts-mode--ei-bol2loc-map:
;;   Key `pos-bol', value is a location, loc, in line-nodes result from
;;   `matlab-ts-mode--ei-line-nodes-in-region' such that loc is the first location that satisfies:
;;   pos-bol <= (treesit-node-start (caar loc))
;;
;;   Given foo.m
;;      L1: a = 1;
;;      L2:
;;      L3:
;;      L4: % comment line 1
;;      L5: % comment line 2
;;      L6:
;;      L7: b = 2;
;;   M-x matlab-ei-utils-print-bol2loc-map
;;      -->matlab-ts-mode--ei-bol2loc-map
;;         L1  : point     1 (#<treesit-node identifier in 1-2> . "identifier")
;;         L2  : point     8 (#<treesit-node comment in 10-43> . "comment")
;;         L3  : point     9 (#<treesit-node comment in 10-43> . "comment")
;;         L4  : point    10 (#<treesit-node comment in 10-43> . "comment")
;;         L5  : point    27 (#<treesit-node identifier in 45-46> . "identifier")
;;         L6  : point    44 (#<treesit-node identifier in 45-46> . "identifier")
;;         L7  : point    45 (#<treesit-node identifier in 45-46> . "identifier")

(defvar-local matlab-ts-mode--ei-bol2loc-map nil)

(defun matlab-ts-mode--ei-bol2loc-setup (beg end region-nodes)
  "Use BEG END REGION-NODES to populate `matlab-ts-mode--ei-bol2loc-map'."
  (setq matlab-ts-mode--ei-bol2loc-map (make-hash-table))
  (save-excursion
    (let ((ptr region-nodes))
      (goto-char beg)
      (while (and ptr (< (point) end) (not (eobp)))

        (let* ((bol-pt (pos-bol))
               (node (cdar ptr))
               (node-pt (treesit-node-start node)))
          (while (and ptr (< node-pt bol-pt))
            (setq ptr (cdr ptr)
                  node (cdar ptr)
                  node-pt (treesit-node-start node)))
          (when ptr
            (puthash bol-pt ptr matlab-ts-mode--ei-bol2loc-map))
          (forward-line))))))

(defvar-local matlab-ts-mode--ei-errors-map nil) ;; Key (pos-bol), value t on an error line.

(defconst matlab-ts-mode--ei-non-numeric-re
  "[^a-zA-Z0-9_.~ \t,;+\n-]"
  "Regexp matching a character that cannot appear in a numeric matrix row.
Numeric rows contain word tokens (identifiers, numbers including hex
and binary literals), unary operators, commas, and semicolons.
Expression-forming characters such as brackets, quotes, and `@'
indicate non-numeric content.")

(defconst matlab-ts-mode--ei-numeric-entry-re
  "[a-zA-Z0-9][a-zA-Z0-9_.]*"
  "Regexp matching a single numeric entry word token in a matrix row.
Matches alphanumeric tokens with dots and underscores.  Exponent
signs (e.g. the + in 1.23e+5 or the - in 5.0d-15) are extended by
the scanning loop in `matlab-ts-mode--ei-classify-matrix' after the
initial match.  Does not include leading unary operators; those are
also handled by the scanning loop.")

(defun matlab-ts-mode--ei-row-entry-count (row-node)
  "Count the number of entries in ROW-NODE.
Entries are children that are not separators (comma, semicolon),
comments, or line continuations."
  (let ((child-count (treesit-node-child-count row-node))
        (idx 0)
        (entry-count 0))
    (while (< idx child-count)
      (let ((child-type (treesit-node-type (treesit-node-child row-node idx))))
        (unless (or (string= child-type ",")
                    (string= child-type ";")
                    (string= child-type "comment")
                    (string= child-type "line_continuation"))
          (setq entry-count (1+ entry-count))))
      (setq idx (1+ idx)))
    entry-count))

(defun matlab-ts-mode--ei-valid-non-numeric-p (matrix-node)
  "Return non-nil if MATRIX-NODE is a valid non-numeric m-matrix.
A valid non-numeric m-matrix has uniform column counts across all
rows, each row on its own line, and no row spanning multiple lines.
Uses tree-sitter children to inspect the matrix structure."
  (let ((child-count (treesit-node-child-count matrix-node))
        (idx 0)
        (expected-cols nil)
        (prev-row-eol-pos nil)
        (valid t))
    (while (and valid (< idx child-count))
      (let* ((child (treesit-node-child matrix-node idx))
             (child-type (treesit-node-type child)))
        (when (string= child-type "row")
          (let ((cols (matlab-ts-mode--ei-row-entry-count child))
                (row-start-eol-pos (save-excursion (goto-char (treesit-node-start child))
                                                   (pos-eol)))
                (row-end-eol-pos (save-excursion (goto-char (treesit-node-end child))
                                                 (pos-eol))))
            ;; Check uniform column count
            (if (null expected-cols)
                (setq expected-cols cols)
              (when (/= cols expected-cols)
                (setq valid nil)))
            ;; Check row is on a single line
            (when (and valid (/= row-start-eol-pos row-end-eol-pos))
              (setq valid nil))
            ;; Check no two rows share a line
            (when (and valid prev-row-eol-pos (= row-start-eol-pos prev-row-eol-pos))
              (setq valid nil))
            (setq prev-row-eol-pos row-start-eol-pos))))
      (setq idx (1+ idx)))
    valid))

(defun matlab-ts-mode--ei-classify-matrix (matrix-node)
  "Get classification of MATRIX-NODE.
Returns one of:
  (list \\='not-a-m-matrix)
     Single-line matrix, multi-line with multiple rows on one line,
     or rows with non-uniform column counts.
  (list \\='numeric-m-matrix FIRST-COL-OFFSET COLUMN-WIDTHS)
     Multi-line matrix assignment where each row is on its own line and
     contains only non-expression entries (numbers, identifiers,
     unary operators).
     COLUMN-WIDTHS is a list of per-column maximum entry widths.
  (list \\='non-numeric-m-matrix FIRST-COL-OFFSET)
     Multi-line matrix assignment where each row is on its own line but
     contains non-numeric entries.

Where FIRST-COL-OFFSET is either 1 or 0 computed by
`matlab-ts-mode--ei-m-matrix-first-col-offset'.

Uses `re-search-forward' on the buffer text to identify
\\='numeric-m-matrix for performance.  When the regexp scan does not
confirm numeric, falls back to `matlab-ts-mode--ei-valid-non-numeric-p'
which uses tree-sitter children nodes to determine
\\='non-numeric-m-matrix vs \\='not-a-m-matrix."

  (let ((mat-start (treesit-node-start matrix-node))
        (mat-end (treesit-node-end matrix-node)))
    ;; Check if single-line matrix or if the first/last line has extra language elements after the
    ;; matrix delimiter, "]" and on the same line.  For example both of these are 'not-a-m-matrix:
    ;;    m1 = [1 2; 3 4];
    ;;    m2 = [1 2;
    ;;          3 4]; m3 = [1 2; 3 4];        // Simplification: don't align
    (if (or
         ;; Single-line matrix, e.g. m1 = [1 2; 3 4];
         (save-excursion (= (progn (goto-char mat-start) (pos-bol))
                            (progn (goto-char mat-end) (pos-bol))))
         ;; OR extra content after "]" on the last line? We shouldn't align these because
         ;;    it wouldn't look nice (and be complex to implement). Example:
         ;;       m2 = [1 2;
         ;;             3 4]; m3 = [1 2; 3 4];
         (not (matlab-ts-mode--ei-matrix-ends-on-line matrix-node))
         ;; OR has ERROR or "]" nodes within the matrix-node
         (not (matlab-ts-mode--ei-matrix-is-clean matrix-node)))

        '(not-a-m-matrix)

      ;; Else: see if multi-line matrix: scan buffer text line by line to classify.  The text scan
      ;; is only reliable for numeric content.  When non-numeric content is detected, stop the text
      ;; scan and let tree-sitter validate the matrix structure, since string literals and other
      ;; expressions can contain characters like ";", "...", "[", "]" that confuse text-based
      ;; checks.
      (save-excursion
        (goto-char mat-start)
        (let ((is-numeric t)    ;; assume
              (is-valid t)      ;; assume
              (num-cols nil)    ;; expected column count (set from first row with entries)
              (col-widths nil)) ;; list of max widths per column
          (while (and is-valid is-numeric (< (point) mat-end))
            (let* ((lstart (point))
                   (lend (min (pos-eol) mat-end)))
              ;; Find effective end of code on this line (before % comment)
              (goto-char lstart)
              ;; TODO - can we simplify the locating of code-end by only one regex search
              (let* ((eff-end (if (re-search-forward "%" lend t)
                                  (match-beginning 0)
                                lend))
                     ;; Find line continuation (...)
                     (cont-pos (progn
                                 (goto-char lstart)
                                 (when (re-search-forward "\\.\\.\\." eff-end t)
                                   (match-beginning 0))))
                     (code-end (if cont-pos cont-pos eff-end)))
                ;; Compute scan boundaries that skip the matrix's own
                ;; "[" on the first line and "]" on the last line.
                (let ((scan-start (if (= lstart mat-start) (1+ lstart) lstart))
                      (scan-end (if (and (>= (1- code-end) lstart)
                                         (eq (char-after (1- code-end)) ?\]))
                                    (1- code-end)
                                  code-end)))
                  ;; Check for non-numeric content first.  When detected,
                  ;; stop the text scan immediately.  Text-based validity
                  ;; checks below are unreliable for non-numeric content.
                  (goto-char scan-start)
                  (if (re-search-forward matlab-ts-mode--ei-non-numeric-re
                                         scan-end t)
                      (setq is-numeric nil)
                    ;; Numeric line: perform text-based validity checks.
                    ;; Check for multiple rows on one line:
                    ;; A ";" followed by non-whitespace content (other than "]") means
                    ;; two rows share this line.
                    (goto-char lstart)
                    (when (re-search-forward ";[ \t]*[^ \t\n]" code-end t)
                      (unless (eq (char-before) ?\])
                        (setq is-valid nil)))
                    ;; Collect entry widths for this row.
                    ;; Each numeric entry is an optional unary +/-/~
                    ;; prefix followed by a word token.  A +/- is unary
                    ;; when there is no space after it and the preceding
                    ;; non-whitespace character is a separator (space,
                    ;; comma, semicolon, "[") or it is at scan-start.
                    ;; Otherwise, +/- is a binary operator and the row
                    ;; is non-numeric.
                    (when (and is-valid is-numeric)
                      (goto-char scan-start)
                      (let ((col 0)
                            (row-widths nil))
                        (while (and is-numeric
                                    (progn (skip-chars-forward " \t,;" scan-end)
                                           (< (point) scan-end)))
                          (let ((entry-start (point))
                                (ch (char-after)))
                            (cond
                             ;; Unary +/-/~: no space after, preceded by
                             ;; separator or at scan-start.
                             ((and (memq ch '(?+ ?- ?~))
                                   (< (1+ (point)) scan-end)
                                   (not (memq (char-after (1+ (point))) '(?\s ?\t)))
                                   (or (= (point) scan-start)
                                       (memq (char-before) '(?\s ?\t ?, ?\; ?\[))))
                              (forward-char)
                              (if (looking-at matlab-ts-mode--ei-numeric-entry-re)
                                  (progn
                                    (goto-char (match-end 0))
                                    ;; Extend for exponent sign: e.g. 1.23e+5, 5.0d-15
                                    (when (and (memq (char-before) '(?e ?E ?d ?D))
                                               (< (point) scan-end)
                                               (memq (char-after) '(?+ ?-))
                                               (looking-at "[+-][a-zA-Z0-9][a-zA-Z0-9_.]*"))
                                      (goto-char (match-end 0)))
                                    (let ((w (- (point) entry-start)))
                                      (push w row-widths)
                                      (setq col (1+ col))))
                                (setq is-numeric nil)))
                             ;; Binary +/-: mark non-numeric
                             ((memq ch '(?+ ?-))
                              (setq is-numeric nil))
                             ;; Word token without unary prefix
                             ((looking-at matlab-ts-mode--ei-numeric-entry-re)
                              (goto-char (match-end 0))
                              ;; Extend for exponent sign: e.g. 1.23e+5, 5.0d-15
                              (when (and (memq (char-before) '(?e ?E ?d ?D))
                                         (< (point) scan-end)
                                         (memq (char-after) '(?+ ?-))
                                         (looking-at "[+-][a-zA-Z0-9][a-zA-Z0-9_.]*"))
                                (goto-char (match-end 0)))
                              (let ((w (- (point) entry-start)))
                                (push w row-widths)
                                (setq col (1+ col))))
                             ;; Unmatched content
                             (t (setq is-numeric nil)))))
                        (when (and is-numeric (> col 0))
                          ;; Check for multi-line row: a line with entries
                          ;; and "..." but no ";" may continue onto the next
                          ;; line.  Allow it when the entry count matches the
                          ;; established column count (row is complete and
                          ;; "..." is just a trailing continuation/comment).
                          (when (and cont-pos
                                     (save-excursion
                                       (goto-char lstart)
                                       (not (re-search-forward ";" cont-pos t))))
                            (when (or (null num-cols) (/= col num-cols))
                              (setq is-valid nil)))
                          ;; Check uniform column count
                          (when is-valid
                            (if (null num-cols)
                                (setq num-cols col)
                              (when (/= col num-cols)
                                (setq is-valid nil))))
                          (unless (not is-valid)
                            (setq row-widths (nreverse row-widths))
                            ;; Merge row-widths into col-widths (element-wise max)
                            (if (null col-widths)
                                (setq col-widths row-widths)
                              (let ((cw col-widths)
                                    (rw row-widths))
                                (while (and cw rw)
                                  (when (> (car rw) (car cw))
                                    (setcar cw (car rw)))
                                  (setq cw (cdr cw)
                                        rw (cdr rw))))))))))
                  )) ;; end of let scan-start/scan-end, if non-numeric check
              ;; Advance to the next line
              (goto-char lend)
              (forward-line)))
          ;; result
          (if (and is-numeric is-valid)
              ;; Text scan confirmed numeric with valid structure.
              (list 'numeric-m-matrix
                    (matlab-ts-mode--ei-m-matrix-first-col-offset matrix-node)
                    col-widths)
            ;; Not numeric: use tree-sitter to validate matrix structure.
            (if (matlab-ts-mode--ei-valid-non-numeric-p matrix-node)
                (list 'non-numeric-m-matrix
                      (matlab-ts-mode--ei-m-matrix-first-col-offset matrix-node))
              '(not-a-m-matrix))))))))

(defun matlab-ts-mode--ei-mark-m-matrix-lines (matrix-node)
  "Classify MATRIX-NODE and mark matrix lines.
For MATRIX-TYPE, \\='numeric-m-matrix or \\='non-numeric-m-matrix,
add text property \\='m-matrix-info to newline of each matrix line:
  (list MATRIX-TYPE FIRST-COL-OFFSET COLUMN-WIDTHS
        ROW-ON-FIRST-LINE N-BRACKETS-BEFORE)

MATRIX-TYPE FIRST-COL-OFFSET COLUMN-WIDTHS are from
`matlab-ts-mode--ei-classify-matrix'.  COLUMN-WIDTHS is nil for
\\='non-numeric-m-matrix types.

ROW-ON-FIRST-LINE is present on the first line of the matrix and is t if
the line containing the matrix start, \"[\" has row content.

N-BRACKETS-BEFORE is number of brackets before the \"[\" of the matrix
start.

\\='numeric-m-matrix examples:
  n1 = [1,   2      // \n prop (list \\='numeric-m-matrix 0 \\='(1 3) t 0)
        % comment   // \n prop (list \\='numeric-m-matrix 0)
        3,   4      // \n prop (list \\='numeric-m-matrix 0 \\='(1 3))
        5, 600];    // \n prop (list \\='numeric-m-matrix 0 \\='(1 3))

  n2 = [            // \n prop (list \\='numeric-m-matrix 1 nil nil 0)
         1,   2     // \n prop (list \\='numeric-m-matrix 1 \\='(1 3))
         % comment  // \n prop (list \\='numeric-m-matrix 1)
         3,   4     // \n prop (list \\='numeric-m-matrix 1 \\='(1 3))
         5, 600     // \n prop (list \\='numeric-m-matrix 1 \\='(1 3))
       ];           // \n prop (list \\='numeric-m-matrix 1)

  N-BRACKETS-BEFORE is 1:

  n3([1 2; 3 4])=[1 2;  // \n prop (list \\='numeric-m-matrix 0 \\='(1 1) t 1)
                  3, 4] // \n prop (list \\='numeric-m-matrix 0 \\='(1 1))

\\='non-numeric-m-matrix example, notice no column widths:
  e1 = [  1,  a+b   // \n prop (list \\='non-numeric-m-matrix 0 nil t 0)
          % comment // \n prop (list \\='non-numeric-m-matrix 0)
          c- d, 1   // \n prop (list \\='non-numeric-m-matrix 0)
          5, 600];  // \n prop (list \\='non-numeric-m-matrix 0)

  e2 = [            // \n prop (list \\='non-numeric-m-matrix 1 nil nil 0)
         1,  a+b    // \n prop (list \\='non-numeric-m-matrix 1)
         % comment  // \n prop (list \\='non-numeric-m-matrix 1)
         c- d, 1    // \n prop (list \\='non-numeric-m-matrix 1)
         5, 600     // \n prop (list \\='non-numeric-m-matrix 1)
       ];           // \n prop (list \\='non-numeric-m-matrix 1)"
  ;; TopTester: tests/test-matlab-ts-mode--ei-classify-matrix.el
  (let* ((classify-result (matlab-ts-mode--ei-classify-matrix matrix-node))
         (first-line t) ;; line containing the start of the matrix, "["
         (matrix-type (car classify-result)))
    (when (not (eq matrix-type 'not-a-m-matrix))
      (let ((mat-start (treesit-node-start matrix-node))
            (mat-end (treesit-node-end matrix-node))
            (classify-pair (list (nth 0 classify-result) (nth 1 classify-result)))
            (n-brackets-before 0))
        (save-excursion
          (goto-char mat-start)
          (forward-line 0)
          (while (re-search-forward "\\[" mat-start t)
            (setq n-brackets-before (1+ n-brackets-before)))
          (goto-char mat-start)

          (forward-line 0)
          (with-silent-modifications
            (while (and (<= (point) mat-end) (not (eobp)))
              (let* ((have-row (save-excursion
                                 (when (< (point) mat-start)
                                   (goto-char (1+ mat-start)))
                                 ;; have content that is not a comment or a line_continuation
                                 (and (looking-at (rx (0+ (or " " "\t")) (not (or " " "\t" "\n"))))
                                      (not (looking-at (rx (0+ (or " " "\t"))
                                                           (or eol "%" "..." "]")))))))
                     (m-matrix-info (if first-line
                                        (append (if have-row
                                                    classify-result
                                                  classify-pair)
                                                (if (and have-row
                                                         (eq matrix-type 'numeric-m-matrix))
                                                    `(,have-row ,n-brackets-before)
                                                  `(nil ,have-row ,n-brackets-before)))
                                      (if have-row
                                          classify-result
                                        (list (nth 0 classify-result) (nth 1 classify-result)))))
                     (eol-pt (pos-eol)))
                (put-text-property eol-pt (1+ eol-pt) 'm-matrix-info m-matrix-info))
              (setq first-line nil)
              (forward-line))))))))

(defun matlab-ts-mode--ei-mark-error-lines (error-node)
  "Add lines of ERROR-NODE to `matlab-ts-mode--ei-errors-map'."
  (let* ((error-start-pt (treesit-node-start error-node))
         (error-end-pt (treesit-node-end error-node)))
    (save-excursion
      (goto-char error-start-pt)
      (while (and (<= (point) error-end-pt) (not (eobp)))
        (puthash (pos-bol) t matlab-ts-mode--ei-errors-map)
        (forward-line)))))

(defvar matlab-ts-mode--ei-all-nodes-query
  (when (treesit-available-p)
    (treesit-query-compile
     'matlab
     `(
       ;; Matrix nodes to identify multi-row matrices (m-matrices) for alignment
       ((matrix) @matrix)

       ;; Named nodes
       ((identifier) @identifier)
       ((number) @number)
       ((string) @string)
       ((comment) @comment)
       ((line_continuation) @line_continuation)
       ((command_argument) @command_argument)
       ((command_name) @command_name)
       ((dimensions) @dimensions)
       ((ERROR) @error)

       ;; Punctuation & delimiters
       ("," @comma) (";" @semicolon) ("." @dot)
       ("(" @lparen) (")" @rparen)
       ("[" @lbrack) ("]" @rbrack) ("{" @lbrace) ("}" @rbrace)

       ;; Arithmetic & assignment operators
       ("+" @plus) ("-" @minus) ("*" @star) ("/" @slash) ("\\" @backslash)
       ("^" @caret) (":" @colon) ("=" @assign)

       ;; Dot operators
       (".*" @dotStar) ("./" @dotSlash) (".\\" @dotBackslash) (".^" @dotCaret)
       (".?" @dotQuestion) (".'" @dotTranspose)

       ;; Comparison operators
       ("<" @lt) (">" @gt) ("<=" @leq) (">=" @geq) ("==" @eqeq) ("~=" @neq)

       ;; Logical operators
       ("&" @amp) ("|" @pipe) ("&&" @and2) ("||" @or2)

       ;; Other operators
       ("~" @tilde) ("@" @at) ("?" @question)

       ;; String or transpose
       ("'" @singleTick)

       ;; Keywords
       ("arguments" @kw_arguments)
       ("break" @kw_break)
       ("case" @kw_case)
       ("catch" @kw_catch)
       ("classdef" @kw_classdef)
       ("continue" @kw_continue)
       ("else" @kw_else)
       ("elseif" @kw_elseif)
       ("end" @kw_end)
       ("enumeration" @kw_enumeration)
       ("events" @kw_events)
       ("for" @kw_for)
       ("function" @kw_function)
       ("global" @kw_global)
       ("if" @kw_if)
       ("methods" @kw_methods)
       ("otherwise" @kw_otherwise)
       ("parfor" @kw_parfor)
       ("persistent" @kw_persistent)
       ("properties" @kw_properties)
       ("return" @kw_return)
       ("spmd" @kw_spmd)
       ("switch" @kw_switch)
       ("try" @kw_try)
       ("while" @kw_while)

       ;; classdef property get and set method start nodes
       ;; (https://www.mathworks.com/help/matlab/matlab_oop/property-set-methods.html)
       ("get." @kw_get_dot)
       ("set." @kw_set_dot)
       ))))

;; Map capture symbols to their node-type strings for the spacing table.
;; Only symbols whose type string differs from (symbol-name sym) need entries;
;; for the rest we derive the string from the grammar node type at init time.
(defvar matlab-ts-mode--ei-capture-type-map
  (let ((ht (make-hash-table :test 'eq :size 80)))
    ;; Named nodes — capture symbol matches type string
    (dolist (sym '(identifier number string comment line_continuation
                              command_argument command_name))
      (puthash sym (symbol-name sym) ht))
    ;; Punctuation & delimiters
    (puthash 'comma "," ht)     (puthash 'semicolon ";" ht)
    (puthash 'dot "." ht)
    (puthash 'lparen "(" ht)    (puthash 'rparen ")" ht)
    (puthash 'lbrack "[" ht)    (puthash 'rbrack "]" ht)
    (puthash 'lbrace "{" ht)    (puthash 'rbrace "}" ht)
    ;; Arithmetic & assignment
    (puthash 'plus "+" ht)      (puthash 'minus "-" ht)
    (puthash 'star "*" ht)      (puthash 'slash "/" ht)
    (puthash 'backslash "\\" ht)
    (puthash 'caret "^" ht)     (puthash 'colon ":" ht)
    (puthash 'assign "=" ht)
    ;; Dot operators
    (puthash 'dotStar ".*" ht)       (puthash 'dotSlash "./" ht)
    (puthash 'dotBackslash ".\\" ht) (puthash 'dotCaret ".^" ht)
    (puthash 'dotQuestion ".?" ht)   (puthash 'dotTranspose ".'" ht)
    ;; Comparison
    (puthash 'lt "<" ht)        (puthash 'gt ">" ht)
    (puthash 'leq "<=" ht)      (puthash 'geq ">=" ht)
    (puthash 'eqeq "==" ht)     (puthash 'neq "~=" ht)
    ;; Logical
    (puthash 'amp "&" ht)       (puthash 'pipe "|" ht)
    (puthash 'and2 "&&" ht)     (puthash 'or2 "||" ht)
    ;; Other operators
    (puthash 'tilde "~" ht)     (puthash 'at "@" ht)
    (puthash 'question "?" ht)  (puthash 'singleTick "'" ht)
    ;; Keywords
    (puthash 'kw_arguments "arguments" ht)
    (puthash 'kw_break "break" ht)
    (puthash 'kw_case "case" ht)
    (puthash 'kw_catch "catch" ht)
    (puthash 'kw_classdef "classdef" ht)
    (puthash 'kw_continue "continue" ht)
    (puthash 'kw_else "else" ht)
    (puthash 'kw_elseif "elseif" ht)
    (puthash 'kw_end "end" ht)
    (puthash 'kw_enumeration "enumeration" ht)
    (puthash 'kw_events "events" ht)
    (puthash 'kw_for "for" ht)
    (puthash 'kw_function "function" ht)
    (puthash 'kw_global "global" ht)
    (puthash 'kw_if "if" ht)
    (puthash 'kw_methods "methods" ht)
    (puthash 'kw_otherwise "otherwise" ht)
    (puthash 'kw_parfor "parfor" ht)
    (puthash 'kw_persistent "persistent" ht)
    (puthash 'kw_properties "properties" ht)
    (puthash 'kw_return "return" ht)
    (puthash 'kw_spmd "spmd" ht)
    (puthash 'kw_switch "switch" ht)
    (puthash 'kw_try "try" ht)
    (puthash 'kw_while "while" ht)
    (puthash 'kw_get_dot "get." ht)
    (puthash 'kw_set_dot "set." ht)
    ht))

(defun matlab-ts-mode--ei-line-nodes-in-region (beg end)
  "Get line nodes in region BEG to END.
Line nodes are the language elements that we electric indent in the
line.  These are typically leaf nodes except for nodes like strings
which are not leaf nodes.

Returns list with elements (MODIFIED-NODE-TYPE . NODE) where
MODIFIED-NODE-TYPE is the NODE type adjusted.  For example, when NODE
parent is a string and NODE is the string start characters, we return
the parent string node.  Another example: when NODE is a \"+\" and
parent is a unary_operator, we return MODIFIED-NODE-TYPE to be unary-op
even though the node type is \"+\"."

  (setq matlab-ts-mode--ei-errors-map (make-hash-table))

  ;; Removing the properties is helpful when debugging and low cost for production code
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(m-matrix-info nil)))

  (let* ((region-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                              matlab-ts-mode--ei-all-nodes-query beg end))
         dimensions-node ;; we use this to track the "number" and ":" with in properties dimension
         (prev-ptr region-nodes)
         (ptr prev-ptr))

    (while ptr
      (let* ((capture (caar ptr))
             (node (cdar ptr))
             (node-type (gethash capture matlab-ts-mode--ei-capture-type-map)))

        (cond
         ;; Case: ERROR node
         ((eq capture 'error)
          (matlab-ts-mode--ei-mark-error-lines node)
          (setq node nil))

         ;; Case: matrix node to identify multi-line matrices for alignment
         ((eq capture 'matrix)
          (matlab-ts-mode--ei-mark-m-matrix-lines node)
          (setq node nil))

         ;; Case: track when in a property dimensions node
         ((eq capture 'dimensions)
          (setq dimensions-node node
                node nil))

         ;; Case: arguments fcn keyword: arguments (1, :) {mustBeNumeric}
         ;;                                        ^
         ((and dimensions-node (eq capture 'lparen))
          (setq node-type "dim-("))

         ;; Case: property dimensions
         ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ((and dimensions-node (or (eq capture 'number) (eq capture 'colon)))
          (setq node-type "prop-dim"))

         ;; Case: lambda:     @(x) ((ischar(x) || isstring(x)))
         ;;                      ^
         ;;       properties: foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ;;                             ^
         ((eq capture 'rparen)
          (if dimensions-node
              (setq dimensions-node nil ;; close of dimension-node
                    node-type "dim-)")
            (when (string= (treesit-node-type (treesit-node-parent node)) "lambda")
              (setq node-type "lambda-)"))))

         ;; Case: ' string delimiter — ignore (transpose is kept)
         ((and (eq capture 'singleTick)
               (string= (treesit-node-type (treesit-node-parent node)) "string"))
          (setq node nil))

         ;; Case: prop-id, prop-class-id, enum-id
         ((eq capture 'identifier)
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (cond
             ;; arguments
             ;;     p1              % property
             ;; end
             ;; classdef
             ;;     properties
             ;;         p1          % property
             ;;     end
             ;; end
             ((string= parent-type "property") ;; propertyWithOutDot?
              (if (equal (treesit-node-child parent 0) node)
                  (setq node-type "prop-id")
                (setq node-type "prop-class-id")))

             ;; arguments
             ;;     opts.foo string                               % property_name
             ;;     opts.onOff (1,1) matlab.lang.OnOffSwitchState % two property_name's
             ;; end
             ;;
             ;; classdef foo < a & b.c.d                          % two property_name's to skip
             ;; end
             ((string= parent-type "property_name") ;; property.nameWithDot?
              (if (equal (treesit-node-child (treesit-node-parent parent) 0) parent)
                  (setq node-type "prop-id")
                (setq node-type "prop-class-id")))

             ;; enumeration
             ;;     e1 (1)   % e1 is an identifier node that we give modified type: enum-id
             ;; end
             ((string= parent-type "enum")
              (setq node-type "enum-id")))))

         ;; Case: unary operator sign, + or -, e.g. [0 -e] or g = - e
         ((or (eq capture 'plus) (eq capture 'minus))
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (and (string= parent-type "unary_operator")
                       (equal (treesit-node-child parent 0) node))
              (setq node-type "unary-op"))))

         ;; Case: super-class constructor call
         ;;  obj@myUtils.super;
         ((eq capture 'at)
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "function_call")
              (setq node-type "@-fcn-call"))))

         ;; Case: events, enumeration, methods, arguments "commands" and not keywords
         ((memq capture '(kw_events kw_enumeration kw_methods kw_arguments))
          (when (= (treesit-node-child-count node) 0) ;; no children == command
            (let* ((parent (treesit-node-parent node))
                   (parent-type (treesit-node-type parent)))
              (when (string= parent-type "identifier")
                ;; TopTester: electric_indent_inspect_keyword_commands.m
                ;; TopTester: electric_indent_inspect_keyword_commands2.m
                (setq node-type (concat node-type "-fcn")))))))

        (if (and node
                 ;; See: electric_indent_xr_prop_hidden_nodes.m: empty nodes in { }
                 ;; See: electric_indent_example.m: figure       ;% Create a new figure
                 ;;                                              ^ <== empty command_argument
                 (or (not (= (treesit-node-start node) (treesit-node-end node)))
                     (eq capture 'comma)))
            ;; Keep the node and change the capture name to the node-type string
            (progn
              (setcar (car ptr) node-type)
              (setq prev-ptr ptr ;; advance
                    ptr (cdr ptr)))
          ;; Else remove the node
          (if (eq ptr region-nodes)
              (setq region-nodes (cdr ptr)
                    ptr region-nodes)
            (setq ptr (cdr ptr))
            (setcdr prev-ptr ptr)))))

    (matlab-ts-mode--ei-bol2loc-setup beg end region-nodes)

    ;; Result may be nil (consider a region of only blank lines)
    region-nodes))

;; This is setup before `matlab-ts-mode--ei-move-to-and-get-node-pair' is used
(defvar-local matlab-ts-mode--ei-line-nodes nil)

(defvar-local matlab-ts-mode--ei-line-nodes-loc nil)
(defvar-local matlab-ts-mode--ei-line-nodes-eat-comma nil)

(cl-defun matlab-ts-mode--ei-move-to-and-get-node-pair ()
  "Move to and return (MODIFIED-NODE-TYPE . NODE) in line.
Assumes point is at end of an existing node or at beginning of line.
Leverages `matlab-ts-mode--ei-line-nodes-loc' and updates it.

Returned MODIFIED-NODE-TYPE and NODE will be nil if no next node before
end-of-line.  MODIFIED-NODE-TYPE is computed by
`matlab-ts-mode--ei-line-nodes-in-region' and is used in
`matlab-ts-mode--ei-spacing'."

  ;; Move point to next node start
  (when (looking-at "[ \t]")
    (if (re-search-forward "[^ \t]" nil (pos-eol))
        (backward-char)
      (cl-return-from matlab-ts-mode--ei-move-to-and-get-node-pair)))

  (let* ((head-pair (car matlab-ts-mode--ei-line-nodes-loc))
         (pt (point)))

    (cl-loop while (let* ((node (cdr head-pair))
                          (node-end (treesit-node-end node)))
                     (and node
                          (or (> pt node-end)
                              (and (= pt node-end)
                                   ;; Are we at a hidden node, e.g.:
                                   ;;   a = [1 2 -3]
                                   ;;          ^    hidden comma node here where start == end
                                   (if (= (treesit-node-start node) node-end)
                                       ;; Keep the hidden node (e.g. comma) unless we are eating it.
                                       ;; See: electric_indent_eat_hidden_comma.m
                                       ;;   a = [1 ...  // we use the hidden comma after the 1 node
                                       ;;        ...
                                       ;;        2];    // hidden comma is before the 2 node
                                       (when (and matlab-ts-mode--ei-line-nodes-eat-comma
                                                  (string= (treesit-node-type node) ","))
                                         (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)
                                         t)
                                     ;; Else node start != end, so move forward.
                                     t)))))
             do
             (setq matlab-ts-mode--ei-line-nodes-loc (cdr matlab-ts-mode--ei-line-nodes-loc)
                   head-pair (car matlab-ts-mode--ei-line-nodes-loc)))

    (if-let* ((head-node (cdr head-pair)))
        (let ((head-node-start (treesit-node-start head-node)))
          (if (= head-node-start (treesit-node-end head-node))
              ;; Hidden comma has start == end and we just used it, so move our location
              (setq matlab-ts-mode--ei-line-nodes-loc (cdr matlab-ts-mode--ei-line-nodes-loc))
            ;; Else, see if we moved to the next line
            (when (> head-node-start (pos-eol))
              (setq head-pair nil)))))

    ;; Result which may be nil
    head-pair))

(defun matlab-ts-mode--ei-assert-line-match (new-line orig-line)
  "Assert NEW-LINE matches ORIG-LINE ignoring spaces, tabs, and commas."
  ;; Spaces may be added or removed. Commas may be added to new-line, so ignore them.
  (let* ((new-line-no-spaces-or-commas (replace-regexp-in-string "[ \t,]" "" new-line))
         (orig-line-no-spaces-or-commas (replace-regexp-in-string "[ \t,]" "" orig-line)))

    (when (not (string= new-line-no-spaces-or-commas orig-line-no-spaces-or-commas))
      (matlab-ts-mode--assert-msg
       (format "line-content-no-space-mismatch new: \"%s\" !EQ orig: \"%s\" at line %d in %s"
               new-line-no-spaces-or-commas orig-line-no-spaces-or-commas
               (line-number-at-pos (point)) (buffer-name))))))

(defun matlab--ei-offset (&optional pt)
  "Character based column offset in line, starting at 0.
If optional point, PT is specified, return line offset for PT, otherwise
the current `point'.  This is character based and is equal
to `current-column' when there are no TAB characters."
  (if pt
      (- pt (save-excursion (goto-char pt) (pos-bol)))
    (- (point) (pos-bol))))

(defun matlab--ei-move-to-offset (offset)
  "Character based column OFFSET move.
This is equal to `move-to-column' when current line does not have TAB
characters.  Unlike `move-to-column' this returns nil."
  (forward-line 0)
  (forward-char offset))

(defvar-local matlab--eilb nil) ;; Buffer used to create new electric indent line, ei-line

(defmacro matlab--eilb-length ()
  "Current ei-line length."
  '(with-current-buffer matlab--eilb (1- (point))))

(defmacro matlab--eilb-content ()
  "Current ei-line ."
  '(with-current-buffer matlab--eilb (buffer-string)))

(defun matlab--eilb-kill ()
  "Kill the `matlab--eilb' buffer."
  (when matlab--eilb
    (kill-buffer matlab--eilb)
    (setq-local matlab--eilb nil)))

(defun matlab--eilb-setup ()
  "Initialize `matlab-ts-mode--eilb' buffer used to electric indent.
Return t."
  (if matlab--eilb
      (with-current-buffer matlab--eilb
        (erase-buffer))
    ;; Else create matlab--eilb
    (let ((eilb-name (let ((bn (buffer-name)))
                       (concat (when (not (string-match (rx bos " *") bn)) " *")
                               bn "-matlab-ts-mode--ei-line*"))))
      (setq-local matlab--eilb (get-buffer-create eilb-name))
      (with-current-buffer matlab--eilb
        (buffer-disable-undo))))
  t)

(defun matlab--eilb-add-node-text (node eol-c-pt extra-chars &optional n-spaces-to-append)
  "Update `matlab--eilb' with NODE text and more.
EXTRA-CHARS string is appended to EL-LINE after NODE text.  EXTRA-CHARS,
when \\='(string), the string is appended to last non-whitespace in
EL-LINE, then NODE text truncated by EOL-C-PT as needed is appended.
EOL-C-PT is the end-of-line-content which is `pos-eol' when there's no
trailing whitespace, otherwise it's the end of content ignoring the
trailing whitespace.  N-SPACES-TO-APPEND is the number of spaces to
append between nodes."

  (let* ((node-end (treesit-node-end node))
         (last-pt (if (< node-end eol-c-pt) node-end eol-c-pt))
         (node-text (buffer-substring-no-properties (treesit-node-start node) last-pt)))

    (with-current-buffer matlab--eilb

      (when (listp extra-chars)
        ;; Consider: foo1 = {'one', 'two' ...
        ;;                  ...
        ;;              'three' 'four' 'five'};
        ;; After the 'two' node, we have two line_continuation's, then the invisible comma (",")
        ;; This is detected and extra-chars will be '(",")
        ;; TopTester: electric_indent_cell_no_comma_before_ellipsis.m
        (goto-char (point-min))
        (if (re-search-forward (rx (1+ " ") eol))
            (progn
              (replace-match (concat (car extra-chars) "\\&") t)
              (goto-char (point-max)))
          (goto-char (point-max))
          (insert (car extra-chars)))

        (setq extra-chars ""))

      (insert node-text extra-chars)

      (when (and n-spaces-to-append (> n-spaces-to-append 0))
        (insert (make-string n-spaces-to-append ? ))))))

;; matlab-ts-mode--ei-tmp-buf-indent is non-nil if doing m-matrix indent (in this case we know there
;; are no ERROR nodes).
(defvar-local matlab-ts-mode--ei-tmp-buf-indent nil)

(defun matlab-ts-mode--ei-no-elements-to-indent ()
  "Return t if no elements in the current line to indent.
Assumes that current point is at `back-to-indentation'."
  (or
   ;; (1) Nothing to indent in line if it's a comment or ellipsis-line.
   (let ((first-node-type (or (treesit-node-type (treesit-node-at (point) 'matlab)) "")))
     (string-match-p (rx bos (or "line_continuation" "comment") eos) first-node-type))
   ;; (2) Nothing to indent if syntax error on the line.
   (gethash (pos-bol) matlab-ts-mode--ei-errors-map)))

(defun matlab-ts-mode--ei-node-extra-chars (node node-end next-node-start)
  "Get extra chars for NODE after NODE-END and before NEXT-NODE-START."

  (let ((node-type (treesit-node-type node)))
    (cond

     ;; Case: invisible comma, make it visible by returning the comma (",")
     ((and (string= node-type ",")
           (= (treesit-node-start node) (treesit-node-end node)))
      ;; Make invisible comma visible by returning it.
      ",")

     ;; Case: invisible comma to insert at end of array row
     ;;           foo = {'one', 'two' ...
     ;;                              ^   missing comma, return a comma to have it inserted
     ((and (string= node-type "line_continuation")
           (string= (treesit-node-type (treesit-node-parent node)) "row"))
      (let ((next-node (treesit-node-next-sibling node))  ;; after the line continuation
            (ans ""))
        (while (and next-node
                    (string= (treesit-node-type next-node) "line_continuation"))
          (setq next-node (treesit-node-next-sibling next-node)))

        (when next-node
          (let ((row-item next-node)
                (next-parent (treesit-node-parent next-node)))
            (while (and next-parent
                        (not (string= (treesit-node-type next-parent) "row")))
              (setq row-item next-parent
                    next-parent (treesit-node-parent next-parent)))

            (when next-parent ;; next parent is a row
              ;; Emacs 30.1 build 1 returns the hidden comma when using
              ;; treesit-node-next-sibling. Emacs 30.1 build 2 doesn't, but does for
              ;; treesit-node-prev-sibling.
              (let ((row-item-type (treesit-node-type row-item)))
                (when (not (string= row-item-type ","))
                  (setq row-item (treesit-node-prev-sibling row-item))
                  (setq row-item-type (treesit-node-type row-item)))
                (when (and (equal row-item-type ",")
                           (= (treesit-node-start row-item) (treesit-node-end row-item)))
                  (setq matlab-ts-mode--ei-line-nodes-eat-comma t)
                  (setq ans '(","))  ;; list means prefix to NODE text
                  )))))
        ans))

     ;; Case: Handle ignored characters, e.g. ";" in matrices where node="]", next-node="["
     ;;   [[1, 2]; [3, 4]]
     ;;         ^  ^
     (t
      (let ((extra-chars ""))
        (goto-char node-end)
        (when (or ;; [[1,2];[3,4]]?
               (and (< node-end next-node-start)
                    (looking-at "[^ \t]"))
               ;; [[1,2] ; [3,4]]?
               ;; or a multiline matrix:
               ;; x = [ 1 , 2 ;
               (save-excursion (and (when (re-search-forward "[^ \t]" next-node-start t)
                                      (backward-char)
                                      t)
                                    (< (point) next-node-start))))
          (while (< (point) next-node-start)
            (while (and (< (point) next-node-start)
                        (looking-at "[^ \t]"))
              (setq extra-chars (concat extra-chars (match-string 0)))
              (forward-char)
              (setq node-end (point)))
            (re-search-forward "[ \t]+" next-node-start t)))
        extra-chars)))))

(defun matlab-ts-mode--ei-update-line-node-types (line-node-types node node-type)
  "Append NODE-TYPE of NODE to LINE-NODE-TYPES."
  (if (and (string= node-type ";")
           (string= (treesit-node-type (treesit-node-parent node)) "matrix"))
      ;; Ignore ';' row separator in matrix because these may be ignored by tree-sitter
      line-node-types
    (concat line-node-types (when line-node-types " ") node-type)))

(defun matlab-ts-mode--ei-insert-indent-level-spaces ()
  "Insert indent-level spaces for current line expanding tabs."
  (let ((spaces (buffer-substring-no-properties (pos-bol) (point))))
    (when (string-match "\t" spaces)
      (setq spaces (with-temp-buffer
                     (insert spaces)
                     (untabify (point-min) (point-max))
                     (buffer-string))))
    (with-current-buffer matlab--eilb
      (insert spaces))))

;; Internal variable that shouldn't be altered. It's used to avoid infinite recursion.
(defvar matlab-ts-mode--ei-align-enabled t)

(defconst matlab-ts-mode--ei-matrix-syntax-nodes
  (rx bos (or "," ";" "comment" "line_continuation" "[" "]") eos)
  "Node types that are used in the definition of a matrix.")

(defun matlab-ts-mode--ei-is-multi-line-matrix (matrix-node)
  "Is MATRIX-NODE multi-line matrix?
Note, this does not mean it's a multi-row matrix or an m-matrix.
It means the start line and end line are different."
  (save-excursion
    (not (= (progn (goto-char (treesit-node-start matrix-node)) (pos-bol))
            (progn (goto-char (treesit-node-end matrix-node)) (pos-bol))))))

(cl-defun matlab-ts-mode--ei-get-nm-matrix-line (bol-pt eol-pt nm-col-widths
                                                        nm-first-col-offset
                                                        start-node start-node-offset)
  "Build new line for a numeric m-matrix interior row.
BOL-PT and EOL-PT delimit the line.  NM-COL-WIDTHS is the list of
per-column max entry widths.  NM-FIRST-COL-OFFSET is 0 or 1.
START-NODE and START-NODE-OFFSET are used to compute PT-OFFSET.

Scans the line for three classes of items:
  1. Matrix elements - non-whitespace tokens that are not punctuation
     or decorators.
  2. Punctuation - commas and semicolons between elements.
  3. Decorators - comments (%), line continuations (...), and closing
     bracket (]) optionally followed by commas/semicolons then a
     comment or line continuation.

Uses the eilb buffer, switching to it only when a mismatch is detected.

Returns result for `matlab-ts-mode--ei-get-new-line':
  (list NEW-LINE PT-OFFSET)

This does not return the ORIG-LINE-NODE-TYPES FIRST-NODE-PAIR items that
`matlab-ts-mode--ei-get-new-line' returns because these two items are
used by the functions called by `matlab-ts-mode--ei-align' and
`matlab-ts-mode--ei-align' doesn't do any additional alignment
for \\='numeric-m-matrix lines.

Returns nil to fall back to the general alignment algorithm."
  (save-excursion
    (goto-char bol-pt)
    (let* ((indent-pt (progn (matlab-ts-mode--ei-fast-back-to-indentation) (point)))
           (indent-level-spaces (buffer-substring-no-properties bol-pt indent-pt))
           (code-start (point))
           (start-node-start (when start-node (treesit-node-start start-node)))
           using-eilb
           pt-offset
           running-offset
           (col-idx 0))

      ;; Replace TABs in indent-level-spaces
      (when (string-match-p "\t" indent-level-spaces)
        (setq indent-level-spaces (with-temp-buffer (insert indent-level-spaces)
                                                    (untabify (point-min) (point-max))
                                                    (buffer-string)))
        (setq using-eilb (matlab--eilb-setup))
        (with-current-buffer matlab--eilb (insert indent-level-spaces)))

      (setq running-offset (length indent-level-spaces))

      ;; Forward scan from code-start.
      (goto-char code-start)
      (cl-loop
       while (< (point) eol-pt)
       do
       ;; Skip whitespace and punctuation between entries.
       (let ((gap-start (point)))

         (skip-chars-forward " \t,;" eol-pt)
         (when (= (point) eol-pt)
           ;; Trailing punctuation at end of line: collect and emit.
           (let ((trail-punct ""))
             (save-excursion
               (goto-char gap-start)
               (while (< (point) eol-pt)
                 (let ((c (char-after)))
                   (when (or (eq c ?,) (eq c ?\;))
                     (setq trail-punct (concat trail-punct (string c)))))
                 (forward-char)))
             (when (> (length trail-punct) 0)
               (let ((actual (buffer-substring-no-properties gap-start eol-pt)))
                 (when (not using-eilb)
                   (when (not (string= trail-punct actual))
                     (let ((prefix (buffer-substring-no-properties bol-pt gap-start)))
                       (setq using-eilb (matlab--eilb-setup))
                       (with-current-buffer matlab--eilb (insert prefix)))))
                 (when using-eilb
                   (with-current-buffer matlab--eilb (insert trail-punct))))))
           (cl-return))

         ;; ** Found something on the matrix line, ch is what we are looking-at **
         (let ((ch (char-after)))

           ;; --- Decorator: comment or line continuation ---
           (when (or (eq ch ?%)
                     (and (eq ch ?.) (looking-at "\\.\\.\\.")))
             (let* ((decorator-start (point))
                    (punct "")
                    (decorator-text (buffer-substring-no-properties decorator-start eol-pt))
                    expected actual)
               ;; Collect punctuation from gap, removing spaces.
               (save-excursion
                 (goto-char gap-start)
                 (while (< (point) decorator-start)
                   (let ((c (char-after)))
                     (when (or (eq c ?,) (eq c ?\;))
                       (setq punct (concat punct (string c)))))
                   (forward-char)))
               (setq expected (concat punct " " decorator-text)
                     actual (buffer-substring-no-properties gap-start eol-pt))
               ;; PT-OFFSET for decorator.
               (when (and start-node-start (not pt-offset)
                          (>= start-node-start decorator-start))
                 (setq pt-offset (+ running-offset (length punct) 1 start-node-offset)))
               ;; PT-OFFSET for gap punctuation.
               (when (and start-node-start (not pt-offset)
                          (>= start-node-start gap-start)
                          (< start-node-start decorator-start))
                 (setq pt-offset running-offset))
               (when (not using-eilb)
                 (when (not (string= expected actual))
                   (let ((prefix (buffer-substring-no-properties bol-pt gap-start)))
                     (setq using-eilb (matlab--eilb-setup))
                     (with-current-buffer matlab--eilb (insert prefix)))))
               (when using-eilb
                 (with-current-buffer matlab--eilb (insert expected))))
             (cl-return))

           ;; --- Decorator: closing bracket "]" ---
           (when (eq ch ?\])
             (let ((bracket-pos (point))
                   (pre-punct ""))
               ;; Collect punctuation from gap before "]".
               (save-excursion
                 (goto-char gap-start)
                 (while (< (point) bracket-pos)
                   (let ((c (char-after)))
                     (when (or (eq c ?,) (eq c ?\;))
                       (setq pre-punct (concat pre-punct (string c)))))
                   (forward-char)))
               ;; Scan past "]".
               (forward-char)
               ;; Collect commas/semicolons after "]", skipping spaces.
               (let ((post-punct ""))
                 (skip-chars-forward " \t" eol-pt)
                 (while (and (< (point) eol-pt)
                             (let ((c (char-after)))
                               (or (eq c ?,) (eq c ?\;))))
                   (setq post-punct (concat post-punct (string (char-after))))
                   (forward-char)
                   (skip-chars-forward " \t" eol-pt))
                 ;; What follows must be eol, comment, or line continuation.
                 (skip-chars-forward " \t" eol-pt)
                 (let (suffix)
                   (cond
                    ((>= (point) eol-pt)
                     (setq suffix nil))
                    ((eq (char-after) ?%)
                     (setq suffix (buffer-substring-no-properties (point) eol-pt)))
                    ((looking-at "\\.\\.\\.")
                     (setq suffix (buffer-substring-no-properties (point) eol-pt)))
                    ;; Unexpected content: fall back.
                    (t (cl-return-from
                           matlab-ts-mode--ei-get-nm-matrix-line)))
                   (let* ((tail (concat pre-punct "]" post-punct (when suffix (concat " " suffix))))
                          (actual (buffer-substring-no-properties gap-start eol-pt)))
                     ;; PT-OFFSET.
                     (when (and start-node-start (not pt-offset)
                                (>= start-node-start gap-start))
                       (cond
                        ((and suffix
                              (>= start-node-start (- eol-pt (length suffix))))
                         (setq pt-offset (+ running-offset (length pre-punct) 1
                                            (length post-punct) 1 start-node-offset)))
                        ((>= start-node-start bracket-pos)
                         (setq pt-offset (+ running-offset (length pre-punct) start-node-offset)))
                        (t
                         (setq pt-offset running-offset))))
                     (when (not using-eilb)
                       (when (not (string= tail actual))
                         (let ((prefix (buffer-substring-no-properties bol-pt gap-start)))
                           (setq using-eilb (matlab--eilb-setup))
                           (with-current-buffer matlab--eilb (insert prefix)))))
                     (when using-eilb
                       (with-current-buffer matlab--eilb (insert tail)))
                     (setq running-offset (+ running-offset (length tail)))))))
             (cl-return))

           ;; --- Matrix element ---
           ;; Since classification already validated this as a numeric-m-matrix,
           ;; a matrix element is simply a run of non-whitespace, non-delimiter
           ;; characters (includes unary operators, digits, letters, dots, etc.).
           (let ((entry-start (point))
                 entry-end)
             (skip-chars-forward "-+~0-9a-zA-Z_." eol-pt)
             (if (> (point) entry-start)
                 (setq entry-end (point))
               ;; Unmatched: skip to avoid infinite loop.
               (forward-char))

             (when entry-end
               (let* ((entry-width (- entry-end entry-start))
                      (raw-width (nth col-idx nm-col-widths))
                      (col-width (if (and (= col-idx 0)
                                          nm-first-col-offset)
                                     (+ raw-width nm-first-col-offset)
                                   raw-width))
                      (pad (if col-width
                               (max 0 (- col-width entry-width))
                             0))
                      (expected-gap (if (> col-idx 0)
                                        (concat "," (make-string (1+ pad) ? ))
                                      ""))
                      (actual-gap (buffer-substring-no-properties gap-start entry-start)))

                 ;; Check gap match and switch to eilb if needed.
                 (when (not using-eilb)
                   (when (not (string= expected-gap actual-gap))
                     (let ((prefix (buffer-substring-no-properties bol-pt gap-start)))
                       (setq using-eilb (matlab--eilb-setup))
                       (with-current-buffer matlab--eilb (insert prefix)))))

                 ;; PT-OFFSET for gap punctuation.
                 (when (and start-node-start (not pt-offset)
                            (>= start-node-start gap-start)
                            (< start-node-start entry-start))
                   (setq pt-offset running-offset))

                 ;; Emit gap.
                 (when using-eilb
                   (with-current-buffer matlab--eilb (insert expected-gap)))
                 (setq running-offset (+ running-offset (length expected-gap)))

                 ;; PT-OFFSET for entry.
                 (when (and start-node-start (not pt-offset)
                            (>= start-node-start entry-start)
                            (< start-node-start entry-end))
                   (setq pt-offset (+ running-offset start-node-offset)))

                 ;; Emit entry.
                 (when using-eilb
                   (let ((entry-text (buffer-substring-no-properties entry-start entry-end)))
                     (with-current-buffer matlab--eilb (insert entry-text))))
                 (setq running-offset (+ running-offset entry-width))
                 (setq col-idx (1+ col-idx))))))))

      ;; Result for matlab-ts-mode--ei-get-nm-matrix-line
      (list (if using-eilb
                (matlab--eilb-content)
              (buffer-substring-no-properties bol-pt eol-pt))
            pt-offset))))

(cl-defun matlab-ts-mode--ei-get-general-new-line (start-node start-node-offset)
  "Get new line via general node-walking path.
START-NODE and START-NODE-OFFSET are used to compute PT-OFFSET.
Assumes point is at the first non-whitespace character and
`matlab-ts-mode--ei-line-nodes-loc' is set up.

Returns (list NEW-LINE PT-OFFSET ORIG-LINE-NODE-TYPES FIRST-NODE-PAIR)."
  ;; Optimization: defer eilb setup.  Walk nodes in the current buffer and check
  ;; whether existing content already matches.  Only when a mismatch is detected,
  ;; set up the eilb buffer, copy the already-verified prefix, and continue in eilb.
  (let* (pt-offset ;; used in restoring point
         using-eilb ;; non-nil when we've switched to building in eilb
         (bol-pt (pos-bol))
         (pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
         (node-type (or (car pair)
                        ;; no nodes on line
                        (cl-return-from matlab-ts-mode--ei-get-general-new-line)))
         (node (cdr pair))
         (first-node-pair pair)
         orig-line-node-types
         next2-pair ;; used when we have: (NODE-RE (NEXT-NODE-RE NEXT2-NODE-RE) N-SPACES-BETWEEN)
         next2-n-spaces-between
         (eol-pt (pos-eol))
         (eol-c-pt (if (memq (char-before eol-pt) '(?\t ?\ )) ;; eol content pt w/o trailing space
                       (save-excursion (goto-char eol-pt)
                                       (re-search-backward "[^ \t]" bol-pt)
                                       (1+ (point)))
                     eol-pt))
         (indent-has-tabs (save-excursion
                            (let ((first-non-ws (point)))
                              (goto-char bol-pt)
                              (re-search-forward "\t" first-non-ws t))))
         ;; Numeric m-matrix column alignment (for the "[" line only)
         (nm-col-idx 0)
         (m-matrix-info (when matlab-ts-mode--ei-align-enabled
                          (get-text-property eol-pt 'm-matrix-info)))
         (nm-first-col-offset (nth 1 m-matrix-info))
         (nm-col-widths (nth 2 m-matrix-info))
         (nm-row-on-first-line (nth 3 m-matrix-info))
         (nm-inside-matrix (and nm-col-widths (not nm-row-on-first-line))))

    (when indent-has-tabs ;; Tabs in leading whitespace?  Tabs require eilb for untabify.
      (setq using-eilb (matlab--eilb-setup))
      (matlab-ts-mode--ei-insert-indent-level-spaces))

    (cl-loop
     while (and (< (point) eol-pt)
                (< (treesit-node-end node) eol-pt))
     do
     (let* ((next-pair (progn
                         (goto-char (treesit-node-end node))
                         (or next2-pair
                             (matlab-ts-mode--ei-move-to-and-get-node-pair))))
            (next-node-type (or (car next-pair) (cl-return)))
            (next-node (cdr next-pair))
            (n-spaces-between next2-n-spaces-between))

       (setq next2-pair nil
             next2-n-spaces-between nil)

       (when matlab-ts-mode--indent-assert
         (setq orig-line-node-types
               (matlab-ts-mode--ei-update-line-node-types orig-line-node-types node node-type)))

       ;; --- Compute n-spaces-between ---
       (when (not n-spaces-between)
         ;; Fast path: compiled hash dispatch — O(1) for known type pairs.
         (setq n-spaces-between (gethash (cons node-type next-node-type)
                                         matlab-ts-mode--ei-spacing-fast))

         ;; Fast path: 3-node rule for power/transpose: val (^|.^) val -> 0 spaces.
         (when (and (or (not n-spaces-between) (= n-spaces-between 1))
                    (or (string= node-type "identifier") (string= node-type "number"))
                    (or (string= next-node-type "^") (string= next-node-type ".^"))
                    (not (and (string= node-type "number") (string= next-node-type ".^"))))
           (save-excursion
             (goto-char (treesit-node-end next-node))
             (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
                    (next2-node-type (car pair)))
               (when (and next2-node-type
                          (or (string= next2-node-type "identifier")
                              (string= next2-node-type "number")))
                 (setq next2-pair pair
                       next2-n-spaces-between 0
                       n-spaces-between 0)))))

         ;; Slow path fallback: linear scan (should be rare with a complete type list).
         (when (not n-spaces-between)
           (cl-loop for tuple in matlab-ts-mode--ei-spacing do
                    (let* ((node-re (nth 0 tuple))
                           (next-spec (nth 1 tuple))
                           (next-node-re (if (listp next-spec) (car next-spec) next-spec))
                           (next2-node-re (when (listp next-spec) (cdr next-spec))))

                      (when (and
                             (string-match-p node-re node-type)
                             (string-match-p next-node-re next-node-type)
                             (or (not next2-node-re)
                                 (save-excursion
                                   (goto-char (treesit-node-end next-node))
                                   (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
                                          (next2-node-type
                                           (or (car pair)
                                               (cl-return-from
                                                   matlab-ts-mode--ei-get-general-new-line))))

                                     (when (string-match-p next2-node-re next2-node-type)
                                       (setq next2-pair pair)
                                       (setq next2-n-spaces-between (nth 2 tuple))
                                       t)))))

                        (setq n-spaces-between (nth 2 tuple))
                        (when matlab-ts-mode--electric-indent-verbose
                          (message
                           "-->ei-matched: %S for node=<\"%s\" %S> next-node=<\"%s\" %S>"
                           tuple node-type node next-node-type next-node))
                        (cl-return))))))

       (when (not n-spaces-between)
         (matlab-ts-mode--assert-msg
          (format "ei, unhandled node <\"%s\" %S> and next-node <\"%s\" %S>"
                  node-type node next-node-type next-node)))

       ;; Numeric m-matrix column alignment for the "[" line with a first row.
       (when nm-col-widths
         (when (and (not nm-inside-matrix)
                    (string= node-type "[")
                    ;; Consider: v([1,2;3,4]) = [ 101,   21     // following matches 2nd "["
                    ;;                           3001, 4001];
                    (matlab-ts-mode--ei-is-multi-line-matrix (treesit-node-parent node)))
           (setq nm-inside-matrix t))
         (when nm-inside-matrix
           (when (not (or (string-match-p matlab-ts-mode--ei-matrix-syntax-nodes node-type)
                          (string= node-type "unary-op")))
             (setq nm-col-idx (1+ nm-col-idx)))
           (when (and n-spaces-between
                      (not (string-match-p matlab-ts-mode--ei-matrix-syntax-nodes
                                           next-node-type))
                      (not (string= node-type "unary-op")))
             (let* ((next-col (1+ nm-col-idx))
                    (raw-width (nth (1- next-col) nm-col-widths))
                    (col-width (if (and (= next-col 1) nm-first-col-offset)
                                   (+ raw-width nm-first-col-offset)
                                 raw-width))
                    (entry-width
                     (if (string= next-node-type "unary-op")
                         ;; Unary entry: width spans the parent unary_operator node
                         ;; which includes both the sign and the operand.
                         (let ((unary-parent (treesit-node-parent next-node)))
                           (- (treesit-node-end unary-parent)
                              (treesit-node-start unary-parent)))
                       (- (treesit-node-end next-node)
                          (treesit-node-start next-node)))))
               (when (and col-width (< entry-width col-width))
                 (setq n-spaces-between (+ n-spaces-between
                                           (- col-width entry-width))))))))

       ;; --- Extra chars and gap matching ---
       ;; When not yet using eilb, check if existing content matches what we'd produce.
       ;; Switch to eilb when a mismatch is found.
       (let* ((node-end (treesit-node-end node))
              (next-node-start (treesit-node-start next-node))
              (extra-chars (matlab-ts-mode--ei-node-extra-chars
                            node node-end next-node-start)))

         (when (not using-eilb)
           (let* ((gap-str (buffer-substring-no-properties node-end next-node-start))
                  (gap-matches (and (if (listp extra-chars)
                                        nil ;; list extra-chars require eilb retroactive insert
                                      (string= extra-chars ""))
                                    n-spaces-between
                                    (= (length gap-str) n-spaces-between)
                                    (not (string-match-p "[^ ]" gap-str)))))
             (when (not gap-matches)
               ;; Mismatch found: copy content from BOL to node-start into eilb and continue
               (let ((prefix (buffer-substring-no-properties
                              bol-pt (treesit-node-start node))))
                 (setq using-eilb (matlab--eilb-setup))
                 (with-current-buffer matlab--eilb
                   (insert prefix))))))

         (when (equal start-node node)
           (setq pt-offset (if using-eilb
                               (+ (matlab--eilb-length) start-node-offset)
                             (+ (- (treesit-node-start node) bol-pt) start-node-offset))))

         (when using-eilb
           (matlab--eilb-add-node-text node eol-c-pt extra-chars n-spaces-between)))

       (setq node next-node
             node-type next-node-type)))

      ;; --- Last node on the line ---
      (when node
        (let ((extra-chars (matlab-ts-mode--ei-node-extra-chars
                            node (min (treesit-node-end node) eol-c-pt) eol-c-pt)))
          (when (not using-eilb)
            (when (or (listp extra-chars) (not (string= extra-chars "")))
              (let ((prefix (buffer-substring-no-properties bol-pt (treesit-node-start node))))
                (setq using-eilb (matlab--eilb-setup))
                (with-current-buffer matlab--eilb
                  (insert prefix)))))

          (when (equal start-node node)
            (setq pt-offset (if using-eilb
                                (+ (matlab--eilb-length) start-node-offset)
                              (+ (- (treesit-node-start node) bol-pt) start-node-offset))))
          (when matlab-ts-mode--indent-assert
            (setq orig-line-node-types
                  (matlab-ts-mode--ei-update-line-node-types orig-line-node-types node node-type)))
          (when using-eilb
            (matlab--eilb-add-node-text node eol-c-pt extra-chars))))

      (setq matlab-ts-mode--ei-line-nodes-loc nil)
      (list (if using-eilb
                (matlab--eilb-content)
              (buffer-substring-no-properties bol-pt eol-c-pt))
            pt-offset orig-line-node-types first-node-pair)))

(cl-defun matlab-ts-mode--ei-get-new-line (&optional start-node start-node-offset)
  "Get new line content with element spacing adjusted.
Optional START-NODE and START-NODE-OFFSET are used to compute new pt-offset,
the point offset in the line used to restore point after updating line.
Note, new line content may be same as current line.  Also computes
ORIG-LINE-NODE-TYPES which is a string containing the original line node types.

If no new line (e.g. blank, comment, or syntax error on line), nil is
returned, otherwise returns electric indent info, ei-info:

  (list NEW-LINE-CONTENT PT-OFFSET ORIG-LINE-NODE-TYPES FIRST-NODE-PAIR-IN-LINE)
        0                1         2                    3
where:
 - NEW-LINE-CONTENT is the indented line.  Note, during `indent-region',
   does not contain the final amount of leading whitespace because we do
   electric indent before `treesit-indent-region'.
 - PT-OFFSET is the new point offset in the NEW-LINE-CONTENT
   It will be nil if START-NODE and START-NODE-OFFSET are not provided.
 - ORIG-LINE-NODE-TYPES is the nodes in the original line when
   `matlab-ts-mode--indent-assert' is active, otherwise nil.
 - FIRST-NODE-PAIR-IN-LINE is (node-type . node)"
  (save-excursion
    ;; Move to first non-whitespace character on the line
    (let ((have-non-empty-line (matlab-ts-mode--ei-fast-back-to-indentation)))
      (when (or (not have-non-empty-line)
                (matlab-ts-mode--ei-no-elements-to-indent))
        (cl-return-from matlab-ts-mode--ei-get-new-line)))

    ;; Check for numeric m-matrix interior row: use regexp-based fast path
    ;; that avoids tree-sitter node walking entirely.  Returns nil when
    ;; the line has unexpected trailing content and we should fall back.
    (if-let* ((eol-pt (pos-eol))
              (m-matrix-info (when matlab-ts-mode--ei-align-enabled
                               (get-text-property eol-pt 'm-matrix-info)))
              (nm-col-widths (nth 2 m-matrix-info))
              (nm-row-on-first-line (nth 3 m-matrix-info)))
      (when (not nm-row-on-first-line)
        (if-let* ((result (matlab-ts-mode--ei-get-nm-matrix-line
                           (pos-bol) eol-pt nm-col-widths (nth 1 m-matrix-info)
                           start-node start-node-offset)))
            (cl-return-from matlab-ts-mode--ei-get-new-line result))))

    ;; General path: setup node list and walk nodes.
    (setq matlab-ts-mode--ei-line-nodes-loc (gethash (pos-bol) matlab-ts-mode--ei-bol2loc-map))
    (when (not matlab-ts-mode--ei-line-nodes-loc)
      (matlab-ts-mode--assert-msg "No pos-bol in matlab-ts-mode--ei-bol2loc-map"))

    (matlab-ts-mode--ei-get-general-new-line start-node start-node-offset)))

(defun matlab-ts-mode--ei-m-matrix-first-col-offset (matrix-node)
  "Get matrix first-col-offset for indent alignment.
MATRIX-NODE is either a tree-sitter matrix node or a `pos-bol'
position (integer) of the matrix node's start line.  Result is 0 or
`matlab-ts-mode--array-indent-level' divided by 2.  Consider the
following where `matlab-ts-mode--array-indent-level' is 2.

This will return 1
  m1 = [
         1 2
         3 4
       ];
This will return 0
  m2 = [1 2
        3 4];"

  (save-excursion
    ;; Move to point after the matrix open, "["
    (goto-char (treesit-node-start matrix-node))
    (forward-char) ;; step over the "["

    ;; Result is 0 or 1
    (if (and (re-search-forward "[^ \t]" (pos-eol) t)
             (progn (backward-char)
                    ;; have element (not a comment or ellipsis)
                    (not (looking-at (rx (or "%" "..."))))))
        0
      1)))

;; KEY: mat-start-bol-pt: VALUE column-widths
(defvar-local matlab-ts-mode--ei-m-matrix-col-widths-cache nil)

(defun matlab-ts-mode--ei-m-matrix-col-widths (matrix first-col-offset &optional first-col-only)
  "Get multi-line MATRIX column widths adding in FIRST-COL-OFFSET to first column.
If optional FIRST-COL-ONLY is non-nil, then return only the width of the
first column in MATRIX.
Returns alist where each element in the alist is (COLUMN-NUM . WIDTH)"

  (let* ((mat-start-bol-pt (save-excursion (goto-char (treesit-node-start matrix)) (pos-eol)))
         (column-widths (when matlab-ts-mode--ei-m-matrix-col-widths-cache
                          (gethash mat-start-bol-pt matlab-ts-mode--ei-m-matrix-col-widths-cache))))
    (when (not column-widths)
      (dolist (m-child (treesit-node-children matrix))
        (when (string= (treesit-node-type m-child) "row")
          (let ((column-num 0))
            (cl-loop
             for entry in (treesit-node-children m-child)
             do
             (when (not (string= (treesit-node-type entry) ","))
               (setq column-num (1+ column-num))
               (let ((width (or (alist-get column-num column-widths) 0))) ;; matrix element width
                 (let ((el-width (- (treesit-node-end entry) (treesit-node-start entry))))
                   (when (> el-width width)
                     (if (= width 0)
                         (push `(,column-num . ,el-width) column-widths)
                       (setf (alist-get column-num column-widths) el-width)))))
               (when first-col-only
                 (cl-return))
               )))))
      (when (and column-widths ;; non-empty matrix
                 (> first-col-offset 0))
        (let ((col1-width (+ (alist-get 1 column-widths) first-col-offset)))
          (setf (alist-get 1 column-widths) col1-width)))

      (when matlab-ts-mode--ei-m-matrix-col-widths-cache
        (puthash mat-start-bol-pt column-widths matlab-ts-mode--ei-m-matrix-col-widths-cache)))

    column-widths))

(defun matlab-ts-mode--ei-get-m-matrix-row-in-line (t-matrix-node)
  "Given point within a matrix assignment statement, return row node or nil.
Example:  v = [1 2; ...
               ...
               3 4];
when on the 2nd continuation only line, nil is returned otherwise a row node.
If tmp T-MATRIX-NODE is non-nil, we use that to locate the first row."
  (save-excursion
    (if t-matrix-node
        ;; [1, 2, 3;      // child 0 is the '[', child 1 is the first row
        ;;  4, 5, 6]
        ;; [ ...          // child 0 is the '[', child 1 is line_continuation
        ;;  1, 2, 3;
        ;;  4, 5, 6]
        (let ((child1 (treesit-node-child t-matrix-node 1)))
          (when (and (string= (treesit-node-type child1) "row")
                     (= (save-excursion (goto-char (treesit-node-start t-matrix-node)) (pos-eol))
                        (save-excursion (goto-char (treesit-node-start child1)) (pos-eol))))
            ;; row-now on same line as use-matrix-node
            child1))
      ;; else on 2nd or later line, find the row
      (matlab-ts-mode--ei-fast-back-to-indentation)
      (when (looking-at "[^ \t\n]")
        (let (row-node
              found-ans)
          (cl-loop
           while (not found-ans) do

           (let* ((node-at-pt (treesit-node-at (point) 'matlab))
                  (node node-at-pt))
             (while (and node
                         (not (string-match-p (rx bos (or "row" "matrix" "assignment") eos)
                                              (treesit-node-type node))))
               (setq node (treesit-node-parent node)))

             (if (and node
                      (string= (treesit-node-type node) "row"))
                 (setq found-ans t
                       row-node node)
               (goto-char (min (treesit-node-end node-at-pt) (pos-eol)))
               (when (not (re-search-forward "[^ \t]" (pos-eol) t))
                 (setq found-ans t)))))
          row-node)))))

(defun matlab-ts-mode--ei-indent-matrix-in-tmp-buf (assign-node
                                                    t-buf-start-pt-linenum start-pt-offset)
  "Insert ASSIGN-NODE in to current tmp-buf and indent.
T-BUF-START-PT-LINENUM and START-PT-OFFSET are used to place the point
prior to electric indent without alignment in the tmp buf.

Point is left at beginning of line containing the ASSIGN-NODE text.
Returns the line number after the ASSIGN-NODE in the tmp-buf."
  (let (assign-str
        assign-end-linenum
        t-buf-start-pt-offset
        n-levels
        (n-extra-levels 0)
        (is-prop (treesit-parent-until assign-node (rx bos (or "properties" "arguments") eos))))

    (setq matlab-ts-mode--ei-tmp-buf-indent t)

    (with-current-buffer (treesit-node-buffer assign-node)
      (let* ((assign-start-pos (save-excursion (goto-char (treesit-node-start assign-node))
                                               (pos-bol)))
             (assign-end-pos (save-excursion (goto-char (treesit-node-end assign-node))
                                             (pos-eol)))
             (indent-spaces (- (treesit-node-start assign-node) assign-start-pos)))
        (setq assign-str (buffer-substring-no-properties assign-start-pos assign-end-pos)
              n-levels (if (= (mod indent-spaces matlab-ts-mode--indent-level) 0)
                           (/ indent-spaces matlab-ts-mode--indent-level)
                         ;; else: not at a standard level so no need to add conditionals as the
                         ;; indent level will be corrected later.
                         0))))
    (when is-prop
      (setq n-extra-levels (max (- n-levels 2) 0)
            n-levels 2))

    (if is-prop
        (insert "classdef foo\n" "properties\n")
      (cl-loop for level from 1 to n-levels do
               (insert "if 1\n")))

    (insert assign-str "\n")

    (setq assign-end-linenum (line-number-at-pos))

    (cl-loop for level from 1 to n-levels do
             (insert "end\n"))

    (matlab-ts-mode)

    ;; Indent to adjust spacing among operators, but don't do other alignment items
    (let ((matlab-ts-mode--ei-align-enabled nil)
          ;; t-utils-test-indent captures messages using treesit--indent-verbose and we don't
          ;; want to capture the messages from this temp indent-region.
          (treesit--indent-verbose nil))
      (matlab--eilb-setup)
      (when t-buf-start-pt-linenum
        (goto-char (point-min))
        (forward-line (1- t-buf-start-pt-linenum))
        (matlab--ei-move-to-offset start-pt-offset))

      (matlab-ts-mode--ei-indent-region (point-min) (point-max))

      (when t-buf-start-pt-linenum
        (setq t-buf-start-pt-offset (matlab--ei-offset))))

    (goto-char (point-min))
    (forward-line n-levels) ;; Leave point at assignment start
    (when (> n-extra-levels 0) ;; Add additional whitespace on left?
      (save-excursion
        (let ((start-point (point))
              (end-point (progn
                           (goto-char (point-min))
                           (forward-line assign-end-linenum))))
          (string-rectangle start-point end-point (make-string (* n-extra-levels 4) ? )))))
    (cons assign-end-linenum t-buf-start-pt-offset)))

;; matlab-ts-mode--ei-align-nnm-matrix-cache
;;   - This is used to cache 'non-numeric-m-matrix alignments for indent-region
;;     It will be non-nil when called from indent-region.
;; TODO - improve performance
;;   - We indent from start to end and thus after indenting a matrix, we don't need to keep the
;;     cache for prior matrices.
;;   - Replace this cache with a "persistent" temp-buf that remains for the duration of the indent.
;;   - Indent in the temp-buf and add text properties to the newlines, pos-eol, in the buffer being
;;     indented (the code buffer) that contain the pos-bol of the aligned line in the temp buf.
;;   - This will avoid copying lines into a hash and growing memory.
;;
;; KEY: linenum; VALUE: ei-info
(defvar-local matlab-ts-mode--ei-align-nnm-matrix-cache nil)

(cl-defun matlab-ts-mode--ei-align-line-in-nnm-matrix (assign-node ei-info)
  "Align current line with EI-INFO in a \\='non-numeric-m-matrix of ASSIGN-NODE.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (let* ((matrix-cache matlab-ts-mode--ei-align-nnm-matrix-cache) ;; non-nil if indenting a region
         (assign-start-linenum (line-number-at-pos (treesit-node-start assign-node)))
         (start-pt-prop-pos (next-single-property-change (point-min) 'm-ei-start-pt-offset))
         (start-pt-linenum (when start-pt-prop-pos
                             (line-number-at-pos start-pt-prop-pos)))
         (start-pt-offset (when start-pt-prop-pos
                            (get-text-property start-pt-prop-pos 'm-ei-start-pt-offset)))
         (t-buf-start-pt-linenum (when start-pt-linenum
                                   (1+ (- start-pt-linenum assign-start-linenum))))
         (t-buf-ei-linenum (1+ (- (line-number-at-pos) assign-start-linenum)))
         (t-buf-row-linenum (if matlab-ts-mode--ei-align-nnm-matrix-cache 1 t-buf-ei-linenum)))

    (with-temp-buffer
      (unwind-protect
          (progn
            (let* (t-curr-linenum
                   (tmp-buf-info (matlab-ts-mode--ei-indent-matrix-in-tmp-buf
                                  assign-node t-buf-start-pt-linenum start-pt-offset))
                   (t-end-linenum (car tmp-buf-info))
                   (t-buf-start-pt-offset (cdr tmp-buf-info))
                   (t-assign-node (save-excursion
                                    (matlab-ts-mode--ei-fast-back-to-indentation)
                                    (treesit-parent-until (treesit-node-at (point) 'matlab)
                                                          "assignment")))
                   (t-mx-node (if t-assign-node
                                  (treesit-node-child-by-field-name t-assign-node "right")
                                (treesit-node-parent (treesit-search-subtree ;; else a property
                                                      (treesit-buffer-root-node)
                                                      (rx bos "[" eos) nil t))))
                   (t-mx-node-linenum (line-number-at-pos (treesit-node-start t-mx-node)))
                   (first-col-offset (let ((mi (get-text-property (pos-eol) 'm-matrix-info)))
                                       (if mi
                                           (nth 1 mi)
                                         ;; TODO - we should be able to eliminate this call
                                         (matlab-ts-mode--ei-m-matrix-first-col-offset t-mx-node))))
                   (col-widths (matlab-ts-mode--ei-m-matrix-col-widths t-mx-node first-col-offset)))
              ;; Move to the line of interest when directly indenting a line rather than a region.
              (when (and (not matrix-cache)
                         (> t-buf-ei-linenum 1))
                (forward-line (1- t-buf-ei-linenum)))

              ;; Adjust column widths
              (while (< (setq t-curr-linenum (line-number-at-pos)) t-end-linenum)
                (matlab-ts-mode--ei-fast-back-to-indentation)
                (let* ((row-node (when (>= t-curr-linenum t-mx-node-linenum)
                                   (matlab-ts-mode--ei-get-m-matrix-row-in-line
                                    (when (= t-curr-linenum t-mx-node-linenum) t-mx-node))))
                       (indent-offset (when (looking-at "[^ \t\n]+") (matlab--ei-offset)))
                       ei-line
                       pt-offset)
                  (matlab--eilb-setup)
                  (if (and row-node indent-offset)
                      (let* ((col-num (length col-widths)) ;; Iterate last col down to first col
                             (indent-start-pt (point))
                             (r-content (buffer-substring-no-properties indent-start-pt (pos-eol)))
                             (matrix-offset (save-excursion
                                              (goto-char (treesit-node-start t-mx-node))
                                              (1+ (- (point) (pos-bol))))))
                        (setq pt-offset (if start-pt-linenum
                                            (when (= t-buf-start-pt-linenum t-curr-linenum)
                                              t-buf-start-pt-offset)
                                          (nth 1 ei-info)))

                        ;; Adjust matrix row in eilb using r-content w/o leading indent-level spaces
                        (with-current-buffer matlab--eilb
                          (insert r-content))
                        (when (< matrix-offset indent-offset)
                          (when pt-offset
                            (setq pt-offset (- pt-offset (- indent-offset matrix-offset))))
                          (setq indent-offset matrix-offset))

                        (dolist (row-el (reverse (treesit-node-children row-node)))
                          (when (not (string-match-p ;; at a column?
                                      (rx bos (or "," "line_continuation" "comment") eos)
                                      (treesit-node-type row-el)))
                            (let ((width (or (alist-get col-num col-widths)
                                             (matlab-ts-mode--assert-msg "no col width")))
                                  (curr-width (- (treesit-node-end row-el)
                                                 (treesit-node-start row-el))))
                              (when (< curr-width width)
                                (let ((offset (- (treesit-node-start row-el) indent-start-pt))
                                      (n-spaces (- width curr-width)))
                                  (when (and pt-offset
                                             (or (= indent-start-pt (point-min))
                                                 (<= (+ offset matrix-offset) pt-offset)))
                                    (setq pt-offset (+ pt-offset n-spaces)))
                                  (with-current-buffer matlab--eilb
                                    ;; left align strings, right align all other matrix elements
                                    (if (string= (treesit-node-type row-el) "string")
                                        (goto-char (+ 1 offset (length
                                                                (treesit-node-text row-el))))
                                      (goto-char (1+ offset)))
                                    (insert (make-string n-spaces ? ))
                                    (goto-char (point-max)))))

                              (setq col-num (1- col-num)))))

                        (with-current-buffer matlab--eilb
                          (goto-char (point-min))
                          (insert (make-string indent-offset ? ))
                          (goto-char (point-max)))

                        (setq ei-line (matlab--eilb-content))
                        (when (= t-buf-row-linenum t-buf-ei-linenum)
                          (setq ei-info (list ei-line pt-offset (nth 2 ei-info) (nth 3 ei-info)))))

                    ;; Else a blank or continuation line w/o matrix content
                    (setq ei-line (buffer-substring-no-properties (pos-bol) (pos-eol))))

                  (when matrix-cache
                    (let* ((buf-linenum (1- (+ assign-start-linenum t-buf-row-linenum))))
                      (puthash buf-linenum (list ei-line pt-offset) matrix-cache)))

                  (if (not matrix-cache)
                      (goto-char (point-max))
                    (forward-line)
                    (setq t-buf-row-linenum (1+ t-buf-row-linenum)))))))
        ;; unwind forms
        (matlab--eilb-kill)))
    )
  ;; ei-info for current line
  ei-info)

(cl-defun matlab-ts-mode--ei-matrix-ends-on-line (matrix)
  "Does MATRIX end on a line by itself?"
  ;; TopTester: test-matlab-ts-mode-electric-indent-files/electric_indent_matrix_ends_on_line.m
  (save-excursion
    (goto-char (treesit-node-end matrix))
    (let ((eol-pt (pos-eol)))
      (while (re-search-forward "[^ \t]" eol-pt t)
        (let ((matched (match-string 0)))
          (cond
           ((or (string= matched "%") ;; comment?
                (and (string= matched ".") (looking-at (rx "..")))) ;; ellipsis?
            (cl-return-from matlab-ts-mode--ei-matrix-ends-on-line t))
           ((not (string-match-p (rx (or "," ";")) matched))
            ;; have content after the matrix
            (cl-return-from matlab-ts-mode--ei-matrix-ends-on-line))))))
    t))

(defun matlab-ts-mode--ei-matrix-is-clean (matrix)
  "Is MATRIX node free of errors and inner matrices?"
  ;; TopTester: tests/test-matlab-ts-mode-electric-indent-files/electric_indent_not_clean.m
  ;; TopTester: tests/test-matlab-ts-mode--ei-classify-matrix-files/ei_classify_matrix_not_clean.m

  ;; TODO - performance improvement.
  ;;   1. use the capture info from matlab-ts-mode--ei-line-nodes-in-region
  ;;   2. Use regex to search for "]" and if found check if in a string or not via a capture
  ;;      Numeric matrices won't have a "]" in a string.
  (let ((s-node (treesit-search-subtree matrix (rx bos (or "ERROR" "]") eos) nil t)))
    (= (treesit-node-end s-node) (treesit-node-end matrix))))

(defun matlab-ts-mode--ei-is-m-matrix (matrix)
  "Is MATRIX node a multi-line matrix, t or nil?
We define a a multi-line matrix has one row per line ignoring blank lines and
comments."
  (let* ((mat-start (treesit-node-start matrix))
         (mat-eol-pt (save-excursion (goto-char mat-start) (pos-eol))))

    (let ((mat-info (get-text-property mat-eol-pt 'm-matrix-info)))
      (when mat-info
        ;; Have 'numeric-m-matrix or 'non-numeric-m-matrix, now see if this is it
        (let ((n-brackets-before (nth 4 mat-info)))
          (or (= n-brackets-before 0)
              (save-excursion
                (goto-char mat-start)
                (forward-line 0)
                (while (re-search-forward "\\[" mat-start t)
                  (setq n-brackets-before (1- n-brackets-before)))
                (= n-brackets-before 0))))))))

(defun matlab-ts-mode--ei-get-mat-first-col-width-for-i-row (matrix)
  "Get first column offset plus width of a MATRIX row if an m-matrix or nil.
This is called from `matlab-ts-mode--i-row-matcher' and is used in
setting the indent level of a row within an m-matrix if current row is
in an m-matrix.  The offset is 0 if there's a matrix row on the line
containing the \"[\":
  m1 = [  1,   2
        300, 400]             // offset=0, width=3 => result 3
  m2 = [
           1,   2
         300, 400             // offset=1, width=3 => result 4
       ];"

  (let ((eol-pt (pos-eol))
        do-cleanup
        first-col-offset-plus-width)
    (when (not (matlab-ts-mode--ei-in-disabled-region eol-pt))
      (unwind-protect
          (progn
            ;; `matlab-ts-mode--ei-errors-map' is setup by `matlab-ts-mode--ei-line-nodes-in-region'
            ;; which calls `matlab-ts-mode--ei-mark-m-matrix-lines' to compute 'm-matrix-info text
            ;; property. If it's not setup, then we are being called by
            ;; `matlab-ts-mode--indent-line'
            (when (not matlab-ts-mode--ei-errors-map)
              ;; Given: m1 = [1 2 3
              ;;              100 200 300]
              ;; (matlab-ts-mode--ei-line-nodes-in-region (pos-bol) (pos-eol)) will setup what we
              ;; need for the matrix node, i.e. the treesit-query-capture finds it.
              (setq do-cleanup t)
              (matlab-ts-mode--ei-line-nodes-in-region (pos-bol) eol-pt))

            (if-let* ((m-matrix-info (get-text-property eol-pt 'm-matrix-info))
                      (first-col-offset (nth 1 m-matrix-info))
                      (first-col-width (let ((column-widths (nth 2 m-matrix-info)))
                                         (if column-widths
                                             (car column-widths)
                                           (let ((col-widths-alist
                                                  (matlab-ts-mode--ei-m-matrix-col-widths
                                                   matrix 0 t)))
                                             (alist-get 1 col-widths-alist))))))
                (setq first-col-offset-plus-width (+ first-col-offset first-col-width))))
        (when do-cleanup
          (matlab-ts-mode--ei-cleanup))))
    ;; Return first-col-width plus 0 or 1. May be nil.
    first-col-offset-plus-width))

(cl-defun matlab-ts-mode--ei-struct-ends-on-line (struct)
  "Does function_call STRUCT node end on a line?"
  (save-excursion
    (goto-char (treesit-node-end struct))
    (let ((pt-eol (pos-eol)))
      (while (re-search-forward "^[ \t]" pt-eol t)
        (backward-char)
        (if (looking-at "%") ;; comment after end?
            (goto-char pt-eol)
          (if (looking-at ";") ;; semicolon okay
              (forward-char)
            (cl-return-from matlab-ts-mode--ei-struct-ends-on-line)))))
    t))

(defun matlab-ts-mode--ei-struct-starts-on-first-line (struct)
  "Is function_call STRUCT node have opening paren on start line?"
  (let ((start-bol (save-excursion (goto-char (treesit-node-start struct)) (pos-bol)))
        paren-node)
    (cl-loop
     for child in (treesit-node-children struct) do
     (when (string= (treesit-node-type child) "(")
       (setq paren-node child)
       (cl-return)))
    (and paren-node
         (= start-bol (save-excursion (goto-char (treesit-node-start paren-node)) (pos-bol))))))

(defvar-local matlab-ts-mode--ei-is-m-struct-cache nil) ;; cache

(cl-defun matlab-ts-mode--ei-is-m-struct (struct)
  "Is function_call STRUCT node a multi-line struct that can be aligned?
If so return `(max-field-width . arguments-node), else nil."

  (let* ((start-bol (save-excursion (goto-char (treesit-node-start struct)) (pos-bol)))
         (cache-value (when matlab-ts-mode--ei-is-m-struct-cache
                        (gethash start-bol matlab-ts-mode--ei-is-m-struct-cache)))
         (max-field-width 0))

    (when cache-value
      (cl-return-from matlab-ts-mode--ei-is-m-struct (when (> (car cache-value) 0) cache-value)))

    (let* ((end-bol (save-excursion (goto-char (treesit-node-end struct)) (pos-bol)))
           arguments-node
           (is-m-struct (and
                         ;; Is candidate for a multi-line struct we can align if more than one line
                         (> end-bol start-bol)
                         ;; AND the opening "(" is on the same line as the struct
                         (matlab-ts-mode--ei-struct-starts-on-first-line struct)
                         ;; AND the closing ")" ends on it's own line
                         (matlab-ts-mode--ei-struct-ends-on-line struct)
                         ;; AND we have an arguments node struct(arg1, .....)
                         (setq arguments-node (treesit-search-subtree struct
                                                                      (rx bos "arguments" eos))))))
      (when is-m-struct
        (let (tracking-bol ;; iterate through arguments, tracking how many args on a line
              (n-args-on-tracking-line 0))
          (cl-loop
           for child in (treesit-node-children arguments-node) do
           (when (not (string-match-p (rx bos (or "," "line_continuation" eos))
                                      (treesit-node-type child)))
             ;; either field or value?
             (let ((arg-bol (save-excursion (goto-char (treesit-node-start child)) (pos-bol))))
               (if (and tracking-bol (= tracking-bol arg-bol))
                   (progn (setq n-args-on-tracking-line (1+ n-args-on-tracking-line))
                          (when (> n-args-on-tracking-line 2)
                            (setq is-m-struct nil)
                            (cl-return)))
                 ;; on new line
                 (when (= n-args-on-tracking-line 1) ;; 0 or 2 is good, 1 is bad
                   (setq is-m-struct nil)
                   (cl-return))
                 (when (not (string= (treesit-node-type child) "string"))
                   (setq is-m-struct nil) ;; first arg on line must be the struct field string
                   (cl-return))
                 (when (string-match-p "," (treesit-node-text child))
                   (setq is-m-struct nil) ;; we need the first comma to be end of the field string
                   (cl-return))
                 (let ((field-width (length (treesit-node-text child))))
                   (when (> field-width max-field-width)
                     (setq max-field-width field-width)))
                 (setq tracking-bol arg-bol
                       n-args-on-tracking-line 1)))))
          (when (and n-args-on-tracking-line (= n-args-on-tracking-line 1)) ;; 0 or 2 is good
            (setq is-m-struct nil))))

      (let ((ans (when (and is-m-struct (> max-field-width 0))
                   `(,max-field-width . ,arguments-node))))
        (when matlab-ts-mode--ei-is-m-struct-cache
          ;; When not an align-able multi-line struct, use (0 . nil) to indicate it as such
          (puthash start-bol (if ans ans '(0 . nil))
                   matlab-ts-mode--ei-is-m-struct-cache))
        ans))))

(cl-defun matlab-ts-mode--ei-align-line-in-m-struct (tuple ei-info)
  "Align multi-line struct.
TUPLE = (list struct-assign-node max-field-width arguments-node) where
See `matlab-ts-mode--ei-get-new-line' for EI-INFO."

  (let* ((ei-line (nth 0 ei-info))
         (struct-assign-node (nth 0 tuple))
         (max-field-width (nth 1 tuple))
         (assign-bol (save-excursion (goto-char (treesit-node-start struct-assign-node)) (pos-bol)))
         comma-offset
         new-comma-offset)

    (if (= (pos-bol) assign-bol)
        ;; On "var = struct(........" line?
        (let* ((eq-offset (string-match-p "=" ei-line))
               (open-paren-offset (string-match-p "(" ei-line eq-offset))
               (arg (when (string-match "\\([^ \t]+\\)" ei-line (1+ open-paren-offset))
                      (match-string 0 ei-line))))
          (when (or (not arg) (string-match-p (rx bos "...") arg)) ;; skip continuations
            (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))
          (setq comma-offset (1+ (string-match-p "," ei-line open-paren-offset))
                new-comma-offset (+ 1 open-paren-offset max-field-width)))
      ;; Else on later line
      (when (string-match-p (rx bos (0+ (or " " "\t")) "...") ei-line) ;; skip continuations
        (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))
      (setq comma-offset (string-match-p "," ei-line))
      (when (not comma-offset) ;; Ending ");" by itself on a line
        ;; TopTester: electric_indent_struct_in_prop2.m
        (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))
      (setq comma-offset (1+ comma-offset) ;; point after the ","
            new-comma-offset (+ (string-match-p "[^ \t]" ei-line) max-field-width))
      (let ((first-node-in-line (cdr (nth 3 ei-info)))
            (arguments-node (nth 2 tuple)))
        (when (not (equal (treesit-node-parent first-node-in-line) arguments-node))
          ;; TopTester: electric_indent_struct_with_multiline_field_values.m
          (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))))

    (let ((n-spaces-to-add (1+ (- new-comma-offset comma-offset))))
      (when (not (= n-spaces-to-add 0))
        (setq ei-line (concat (substring ei-line 0 comma-offset)
                              (make-string n-spaces-to-add ? )
                              (substring ei-line comma-offset)))
        (let ((pt-offset (nth 1 ei-info)))
          (when (and pt-offset (> pt-offset comma-offset))
            (setq pt-offset (+ n-spaces-to-add pt-offset)))
          (setq ei-info (list ei-line pt-offset (nth 2 ei-info) (nth 3 ei-info)))))))
  ei-info)

(defun matlab-ts-mode--ei-is-assign (first-node a-type &optional assign-node)
  "Is FIRST-NODE of line for an assignment that matches A-TYPE?
If caller knows FIRST-NODE is in an assignment, ASSIGN-NODE should be
supplied to avoid search ancestors for the assignment node.  Returns
assignment node when A-TYPE is met, else nil.  A-TYPE can be:
 - \\='single-line : varName = value            % on a single line, e.g. v1 = 1
 - \\='m-matrix    : varName = [100   2         % matrix on multiple lines
                                  3 400];
 - \\='m-struct    : varName = struct(...       % struct on multiple lines
                        \\='field1\\', value1, ...
                        \\='otherField2\\', value2);
Note, \\='m-struct returns (list assignment-node max-field-width arguments-node) or nil."
  (when (not assign-node)
    (setq assign-node (treesit-parent-until first-node (rx bos "assignment" eos))))

  (when assign-node
    (save-excursion
      (forward-line 0)
      (when (re-search-forward "=" (pos-eol) t)
        (backward-char)
        (let ((eq-node (treesit-node-at (point) 'matlab)))
          ;; First "=" must be an assignment (assumptions elsewhere require this).
          (when (and (equal (treesit-node-type eq-node) "=")
                     (if (string= (treesit-node-type assign-node) "assignment")
                         (equal (treesit-node-type (treesit-node-parent eq-node))
                                "assignment")
                       (equal (treesit-node-type (treesit-node-parent
                                                  (treesit-node-parent eq-node)))
                              "property")))
            (cond
             ;; Single-line assignment? Example: v1 = [1, 2];
             ((eq a-type 'single-line)
              (when (<= (treesit-node-end assign-node) (pos-eol))
                assign-node))

             ;; Multi-line matrix or struct assignment? Examples:
             ;;   m1 = [100   2
             ;;          3 400];
             ;;   s = struct('field1',      value1, ...
             ;;              'otherField2', value2);
             ((or (eq a-type 'm-matrix) (eq a-type 'm-struct))
              (let ((next-node (treesit-node-next-sibling eq-node)))
                (while (equal (treesit-node-type next-node) "line_continuation")
                  (setq next-node (treesit-node-next-sibling next-node)))
                (when next-node
                  (cond
                   ((eq a-type 'm-matrix)
                    (when (and (string= (treesit-node-type next-node) "matrix")
                               (matlab-ts-mode--ei-is-m-matrix next-node))
                      assign-node))
                   ((eq a-type 'm-struct)
                    (when (and (string= (treesit-node-type next-node) "function_call")
                               (string= (treesit-node-text (treesit-node-child-by-field-name
                                                            next-node "name"))
                                        "struct")
                               ;; TopTester: electric_indent_struct_on_next_line.m
                               (= (save-excursion (goto-char (treesit-node-start assign-node))
                                                  (pos-eol))
                                  (save-excursion (goto-char (treesit-node-start next-node))
                                                  (pos-eol))))
                      (let ((pair (matlab-ts-mode--ei-is-m-struct next-node))) ;; cdr => arguments
                        (when (and pair (> (car pair) 0)) ;; max-field-width > 0?
                          (list assign-node (car pair) (cdr pair))))))))))

             (t
              (matlab-ts-mode--assert-msg (format "bad a-type %S" a-type))))))))))

(defun matlab-ts-mode--ei-point-in-m-type (ei-info m-type)
  "Are we in an M-TYPE line?
1. \\='m-matrix M-TYPE: return the assignment node when in an
   m-matrix else nil.  We define an m-matrix to be an assignment where
   there's more than one line, each row is on the same line, with same
   number of columns.  Example:
     m = [100   2;
            3 400];
2. \\='m-struct M-TYPE: return the assignment node if m-struct else
   nil.  We define an m-matrix to be an assignment where there's more than
   one line, each row is on the same line, with same number of columns:
     m = [100   2;
            3 400];
3. \\='single-line M-TYPE: return assignment node when on a single line,
   else nil.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO."
  (let* ((first-node-in-line (cdr (nth 3 ei-info)))
         (assign-node (treesit-parent-until first-node-in-line
                                            (rx bos (or "property" "assignment") eos))))
    (when assign-node
      (save-excursion
        (goto-char (treesit-node-start assign-node))
        (matlab-ts-mode--ei-fast-back-to-indentation)
        (let ((first-node (treesit-node-at (point) 'matlab)))
          (matlab-ts-mode--ei-is-assign first-node m-type assign-node))))))

(defun matlab-ts-mode--ei-assign-offset (ei-line)
  "Get the assignment offset from the indent-level in EI-LINE."
  (let* ((first-char-offset (or (string-match-p "[^ \t]" ei-line)
                                (matlab-ts-mode--assert-msg "no first char")))
         (offset (- (or (string-match-p "=" ei-line)
                        (matlab-ts-mode--assert-msg "no ="))
                    first-char-offset)))
    offset))

;; This is used to cache aligned assignments for indent-region
;; KEY: `pos-bol', VALUE: assign-offset
(defvar-local matlab-ts-mode--ei-align-assign-cache nil)

(defun matlab-ts-mode--ei-align-assignments (ei-info)
  "Update EI-INFO to align assignments.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (let ((first-node-in-line (cdr (nth 3 ei-info)))
        (eat-comma matlab-ts-mode--ei-line-nodes-eat-comma))
    ;; We are calling `matlab-ts-mode--ei-get-new-line' and hence need to have our own hidden comma
    ;; handling state.
    (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)
    (when (matlab-ts-mode--ei-is-assign first-node-in-line 'single-line)
      (let* ((ei-line (nth 0 ei-info))
             (line-assign-offset (matlab-ts-mode--ei-assign-offset ei-line))
             assign-offset
             assign-bols
             line-start-pt)

        (when (or (not matlab-ts-mode--ei-align-assign-cache)
                  (not (setq assign-offset (gethash (pos-bol)
                                                    matlab-ts-mode--ei-align-assign-cache))))
          (setq assign-offset line-assign-offset)
          (setq assign-bols `(,(pos-bol)))
          (save-excursion
            (forward-line 0)
            (setq line-start-pt (point))

            ;; Look backwards and then forwards for single-line assignments
            (cl-loop
             for direction in '(-1 1) do
             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let* ((l-info (matlab-ts-mode--ei-get-new-line))
                     (l-first-node (cdr (nth 3 l-info))))
                (if (and l-first-node
                         (matlab-ts-mode--ei-is-assign l-first-node 'single-line))
                    (let ((l-offset (matlab-ts-mode--ei-assign-offset (nth 0 l-info))))
                      (push (pos-bol) assign-bols)
                      (when (> l-offset assign-offset)
                        (setq assign-offset l-offset)))
                  (cl-return))))))
          (when matlab-ts-mode--ei-align-assign-cache
            (dolist (assign-bol assign-bols)
              (puthash assign-bol assign-offset matlab-ts-mode--ei-align-assign-cache))))

        (let ((diff (- assign-offset line-assign-offset)))
          (when (> diff 0)
            (let* ((loc (1- (string-match "=" ei-line)))
                   (new-pt-offset (let ((pt-offset (nth 1 ei-info)))
                                    (when pt-offset
                                      (if (<= loc pt-offset)
                                          (+ pt-offset diff)
                                        pt-offset)))))
              (setq ei-line (concat (substring ei-line 0 loc)
                                    (make-string diff ? )
                                    (substring ei-line loc)))
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info))))))))
    (setq matlab-ts-mode--ei-line-nodes-eat-comma eat-comma))
  ei-info)

(defun matlab-ts-mode--ei-get-prop-node (ei-info)
  "Return property node for first node in EI-INFO.
Returns nil if not a class property, enum field, or argument property
node that ends on same line and has items to align."
  (if-let ((first-node-in-line-pair (nth 3 ei-info)))
      (let* ((modified-node-type (car first-node-in-line-pair))
             (first-node (cdr first-node-in-line-pair))
             (prop-id-node (pcase modified-node-type
                             ((or "prop-id" "enum-id")
                              first-node)
                             ("property_name"
                              (treesit-node-parent first-node))))
             (prop-node (treesit-node-parent prop-id-node)))
        ;; skip multi-line nodes for alignment (properties / arguments can span multiple lines)
        (when (and prop-node
                   (= (save-excursion (goto-char (treesit-node-start prop-node)) (pos-eol))
                      (save-excursion (goto-char (treesit-node-end prop-node)) (pos-eol)))
                   (> (length (treesit-node-children prop-node)) 1))
          prop-id-node))))

(defun matlab-ts-mode--ei-prop-length (ei-info)
  "Get the property length from the electric indented line in EI-INFO."
  ;; We need to use the ei-line and not the current content of the buffer because the ei-line could
  ;; have been adjusted. Example:
  ;;    arguments
  ;;       nameValueArgs .  foo ( 1,1)
  ;;    end
  ;; becomes "nameValueArgs.foo (1,1)" in EI-LINE.
  (let ((ei-line (nth 0 ei-info)))
    (when (not (string-match "[^ \t]+" ei-line))
      (matlab-ts-mode--assert-msg (format "no property in ei-line %s" ei-line)))
    (- (match-end 0) (match-beginning 0))))

;; This is used to cache aligned properties/arguments for indent-region.
(defvar-local matlab-ts-mode--ei-align-prop-cache nil)

(defun matlab-ts-mode--ei-align-properties (ei-info)
  "Align properties and arguments in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (when (matlab-ts-mode--ei-get-prop-node ei-info)
    (let ((ei-info-p-length (matlab-ts-mode--ei-prop-length ei-info))
          (p-length (when matlab-ts-mode--ei-align-prop-cache
                      (gethash (pos-bol) matlab-ts-mode--ei-align-prop-cache)))
          (eat-comma matlab-ts-mode--ei-line-nodes-eat-comma))

      (when (not p-length)
        (save-excursion
          (forward-line 0)
          (let* ((line-start-pt (point))
                 (line-bols `(,line-start-pt)))

            (setq p-length ei-info-p-length)

            ;; Look backwards and then forwards for properties/arguments
            (cl-loop
             for direction in '(-1 1) do

             ;; We are calling `matlab-ts-mode--ei-get-new-line' and hence need to have our own
             ;; hidden comma handling state.
             (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)

             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let* ((l-ei-info (matlab-ts-mode--ei-get-new-line)))
                (if (matlab-ts-mode--ei-get-prop-node l-ei-info)
                    (let ((l-p-length (matlab-ts-mode--ei-prop-length l-ei-info)))
                      (when matlab-ts-mode--ei-align-prop-cache
                        (push (pos-bol) line-bols))
                      (when (> l-p-length p-length)
                        (setq p-length l-p-length)))
                  (cl-return)))))

            (when matlab-ts-mode--ei-align-prop-cache
              (dolist (line-bol line-bols)
                (puthash line-bol p-length matlab-ts-mode--ei-align-prop-cache))))))

      (when (not (= p-length ei-info-p-length))
        (let* ((diff (- p-length ei-info-p-length))
               (ei-line (nth 0 ei-info))
               (p-end (when (string-match "\\([^ \t]+\\)[ \t]+[^ \t;]" ei-line)
                        (match-end 1))))
          (when p-end
            (let ((new-pt-offset (let ((pt-offset (nth 1 ei-info)))
                                   (when pt-offset
                                     (if (<= p-end pt-offset)
                                         (+ pt-offset diff)
                                       pt-offset)))))
              (setq ei-line (concat (substring ei-line 0 p-end)
                                    (make-string diff ? )
                                    (substring ei-line p-end)))
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info)))))))
      (setq matlab-ts-mode--ei-line-nodes-eat-comma eat-comma)))
  ei-info)

(defun matlab-ts-mode--ei-trailing-comment-offset (ei-info)
  "Get trailing comment (scope . offset) from first node in line.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.
To simplify implementation, we require that the first \"%\" character in
the line be the start of the trailing comment.  Thus,
  s = \"foo % bar\" % comment
is not identified as having a trailing comment and
  s = \"foo bar\" %comment
is identified as having a trailing comment."
  (when ei-info
    (save-excursion
      (forward-line 0)
      (when (re-search-forward "%" (pos-eol) t)
        (let ((c-node (treesit-node-at (point) 'matlab)))
          (when (equal (treesit-node-type c-node) "comment")
            (if-let* ((first-node (cdr (nth 3 ei-info)))
                      (scope
                       (or
                        ;; Line of matrix/cell? Skip 1st line because its indent level differs.
                        ;; TODO - handle first line, e.g.
                        ;;   cell2 = { % comment 1
                        ;;            [], [], [], []; % comment 2
                        ;;            0, 0, 0, 0;     % comment 3
                        ;;           };
                        (let ((a-node (treesit-parent-until c-node
                                                            (rx bos (or "cell" "matrix") eos))))
                          (when (and a-node
                                     (not (= (pos-bol) (save-excursion
                                                         (goto-char (treesit-node-start a-node))
                                                         (pos-bol)))))
                            a-node))
                        ;; Use scope of first node in the line
                        (or (when (string-match-p (rx bol (or "arguments"
                                                              "end"
                                                              "enumeration"
                                                              "events"
                                                              "function"
                                                              "methods"
                                                              "properties")
                                                      eos)
                                                  (treesit-node-type first-node))
                              first-node)
                            (treesit-parent-until
                             first-node (rx bol (or "block"
                                                    "cell"
                                                    "matrix"
                                                    "source_file"
                                                    (seq (1+ anychar) "_clause")
                                                    (seq (1+ anychar) "_definition")
                                                    (seq (1+ anychar) "_definition")
                                                    (seq (1+ anychar) "_statement"))
                                            eol))))))
                ;; We align trailing comments when in same "scope" (same indent level):
                ;;    x   = [1, 2, 3]; % comment 1 (aligned)
                ;;    xyz = 2;         % comment 2 (aligned)
                ;;
                ;;    foo1 blah % comment (aligned)
                ;;    c = 1;    % comment (aligned)
                ;; We do when not in same "scope":
                ;;    fooBarGooToo = [something                                ...
                ;;                    otherThing(wIndex):theEndIndex(wIndex)-1}]; %#ok<AGROW>
                ;;    P0 = [P0 foo.blah{wIndex}.foobarGooOrig(:,1:end-1)]; %#ok<AGROW>
                ;;
                ;;    if a % comment1                  <= we shouldn't align this
                ;;       b = [1, 2, 3]; % comment2
                ;;       c = 1;         % comment3
                (let* ((new-line (nth 0 ei-info))
                       (indent-col (string-match-p "[^ \t]" new-line))
                       (offset (- (string-match-p "%" new-line) indent-col)))
                  (when (> offset 0)
                    ;; have trailing comment at offset from first char in new-line
                    `(,scope . ,offset))))))))))

;; This is used to cache comment alignments for indent-region
(defvar-local matlab-ts-mode--ei-align-comment-cache nil)

(defun matlab-ts-mode--ei-align-trailing-comments (ei-info)
  "Align trailing comments in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (if-let* ((line-comment-pair (matlab-ts-mode--ei-trailing-comment-offset ei-info)))
      (let ((eat-comma matlab-ts-mode--ei-line-nodes-eat-comma)
            comment-offset)

        ;; Compute desired comment-offset
        (when (or (not matlab-ts-mode--ei-align-comment-cache)
                  (not (setq comment-offset (gethash (pos-bol)
                                                     matlab-ts-mode--ei-align-comment-cache))))
          (let (line-bols
                line-start-pt
                (scope (car line-comment-pair)))

            (setq comment-offset (cdr line-comment-pair))

            (setq line-bols `(,(pos-bol)))
            (save-excursion
              (forward-line 0)
              (setq line-start-pt (point))

              ;; Look backwards and then forwards for lines with trailing comments
              (cl-loop
               for direction in '(-1 1) do
               (goto-char line-start-pt)

               ;; We are calling `matlab-ts-mode--ei-get-new-line' and hence need to have our own
               ;; hidden comma handling state.
               (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)

               (cl-loop
                while (not (if (= direction -1) (bobp) (eobp))) do
                (forward-line direction)
                (let ((ei-l-info (matlab-ts-mode--ei-get-new-line))
                      l-offset-pair)
                  (setq ei-l-info (matlab-ts-mode--ei-align-assignments ei-l-info))
                  (setq ei-l-info (matlab-ts-mode--ei-align-properties ei-l-info))
                  (setq l-offset-pair (matlab-ts-mode--ei-trailing-comment-offset ei-l-info))
                  (if (and l-offset-pair (equal scope (car l-offset-pair)))
                      (let ((l-offset (cdr l-offset-pair)))
                        (push (pos-bol) line-bols)
                        (when (> l-offset comment-offset)
                          (setq comment-offset l-offset)))
                    (cl-return))))))

            (when matlab-ts-mode--ei-align-comment-cache
              (dolist (line-bol line-bols)
                (puthash line-bol comment-offset matlab-ts-mode--ei-align-comment-cache)))))

        (let ((diff (- comment-offset (cdr line-comment-pair))))
          (when (> diff 0)
            (let* ((ei-line (nth 0 ei-info))
                   (loc (1- (string-match "%" ei-line)))
                   (new-pt-offset (let ((pt-offset (nth 1 ei-info)))
                                    (when pt-offset
                                      (if (<= loc pt-offset)
                                          (+ pt-offset diff)
                                        pt-offset)))))
              (setq ei-line (concat (substring ei-line 0 loc)
                                    (make-string diff ? )
                                    (substring ei-line loc)))
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info))))))

        (setq matlab-ts-mode--ei-line-nodes-eat-comma eat-comma)))
  ei-info)

(cl-defun matlab-ts-mode--ei-align (ei-info)
  "Align elements in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.
Optional START-PT-LINENUM and START-PT-OFFSET give the point prior to electric
indent region."
  ;; Skip alignment for numeric-m-matrix lines because column alignment is handled in
  ;; `matlab-ts-mode--ei-get-new-line'.
  (let ((m-matrix-info (get-text-property (pos-eol) 'm-matrix-info)))
    (when (and m-matrix-info
               (eq (nth 0 m-matrix-info) 'numeric-m-matrix))
      (cl-return-from matlab-ts-mode--ei-align ei-info)))

  (let ((matrix-assign-node (matlab-ts-mode--ei-point-in-m-type ei-info 'm-matrix)))
    (if matrix-assign-node
        (setq ei-info (matlab-ts-mode--ei-align-line-in-nnm-matrix matrix-assign-node ei-info))
      (let ((tuple (matlab-ts-mode--ei-point-in-m-type ei-info 'm-struct)))
        (if tuple
            (setq ei-info (matlab-ts-mode--ei-align-line-in-m-struct tuple ei-info))
          ;; else do single-line alignments
          (setq ei-info (matlab-ts-mode--ei-align-assignments ei-info))
          (setq ei-info (matlab-ts-mode--ei-align-properties ei-info))
          (setq ei-info (matlab-ts-mode--ei-align-trailing-comments ei-info))))))
  ei-info)

(defun matlab-ts-mode--ei-get-start-info (is-indent-region)
  "Get start node and start node offset in the current line.

If IS-INDENT-REGION is t, we look for \\='m-ei-start-pt-offset on the
newline.  If its not present, this will return nil.

If IS-INDENT-REGION is nil, we use the current point to get the
start point offset.  If point isn't on the current line, nil is returned.

When we have a start point offset (the character column offset on the
current line), we return

 (cons START-NODE START-NODE-OFFSET)

- START-NODE is non-nil if the point is at a node and start-node will
  be the modified node from `matlab-ts-mode--ei-get-node-to-use'

- START-NODE-OFFSET is the offset of the point from the beginning of start-node.

Example:
    area = width * length;
             ^  <== point
START-NODE is the identifier node for \"width\" and START-NODE-OFFSET is 2
corresponding to the \"d\" in \"width\".

If start point is at end-of-line, nil is returned because we need to
just move the point to the end of the electric indented line."
  (let (start-node
        start-node-offset
        (start-pt-offset (cond
                          (is-indent-region
                           (get-text-property (pos-eol) 'm-ei-start-pt-offset))
                          (t
                           (when (and (>= (point) (pos-bol))
                                      (<= (point) (pos-eol)))
                             (matlab--ei-offset))))))
    (when start-pt-offset
      (save-excursion
        (matlab--ei-move-to-offset start-pt-offset)

        (when (not (looking-at "[ \t]*$")) ;; when not at EOL
          (let ((new-start-pt (save-excursion
                                (let ((start-pt (point)))
                                  (forward-line 0)
                                  (when (and (re-search-forward "[^ \t]" (pos-eol) t)
                                             (> (point) start-pt))
                                    (backward-char)
                                    (point))))))
            (when new-start-pt
              (goto-char new-start-pt)))

          ;; TODO - would using matlab-ts-mode--ei-line-nodes to get the node be faster?
          ;;        we track where we are then walk the nodes.
          (let ((node (car (matlab-ts-mode--ei-get-node-to-use (treesit-node-at (point) 'matlab)))))
            (when (not (and node
                            (>= (point) (treesit-node-start node))
                            (<= (point) (treesit-node-end node))))
              (when (re-search-forward "[^ \t]" (pos-eol) t)
                (backward-char)
                (setq node (car (matlab-ts-mode--ei-get-node-to-use
                                 (treesit-node-at (point) 'matlab))))))

            (when (and node
                       (>= (point) (treesit-node-start node))
                       (<= (point) (treesit-node-end node)))
              (setq start-node node)
              (setq start-node-offset (- (point) (treesit-node-start node)))))))
      (when start-node
        (cons start-node start-node-offset)))))

(defun matlab-ts-mode--ei-workaround-143 (beg-pos-bol end-pos-eol)
  "Workaround https://github.com/acristoffers/tree-sitter-matlab/issues/143.
Between BEG-POS-BOL, a `pos-bol' and END-POS-EOL a `pos-eol', insert a
space between numbers and dot operators.  For example,

  123./  =>  123 ./
  123.*  =>  123 .*
  123.\\  =>  123 .\\

Buffer point is logically maintained:
      x = 123./1 + 567
                   ^         <= buffer point
TAB>  x = 123 ./1 + 567
                    ^        <= updated buffer point
Returns updated end point."
  (let ((buffer-pt (point)))
    (goto-char beg-pos-bol)
    (while (re-search-forward "[0-9]\\.[/\\*\\\\]" end-pos-eol t)
      (backward-char 2) ;; on the "."
      (let ((node (treesit-node-at (point) 'matlab)))
        (when (equal (treesit-node-type node) "number")
          (when (<= (point) buffer-pt)
            (setq buffer-pt (1+ buffer-pt)))
          (insert " ")
          (setq end-pos-eol (1+ end-pos-eol))))
      (forward-char 2))
    (goto-char buffer-pt)
    end-pos-eol))

(defvar-local matlab-ts-mode--ei-orig-line-node-types-cache nil)

(cl-defun matlab-ts-mode--ei-assert-nodes-types-match (curr-line-node-types
                                                       orig-line-node-types
                                                       linenum)
  "Validate CURR-LINE-NODE-TYPES eq ORIG-LINE-NODE-TYPES for LINENUM."

  ;; We insert comma's in arrays, so curr-line-node-types may be larger, thus ignore commas
  (setq curr-line-node-types (replace-regexp-in-string " , " " " curr-line-node-types))
  (setq orig-line-node-types (replace-regexp-in-string " , " " " orig-line-node-types))

  (when (not (string= curr-line-node-types orig-line-node-types))

    ;; See https://github.com/acristoffers/tree-sitter-matlab/issues/149
    (dolist (keyword '("events" "enumeration" "methods" "arguments"))
      (let ((orig-modified (replace-regexp-in-string (rx "identifier line_continuation" eos)
                                                     (concat keyword "-fcn line_continuation")
                                                     orig-line-node-types)))
        (when (string= curr-line-node-types orig-modified)
          (cl-return-from matlab-ts-mode--ei-assert-nodes-types-match))))

    (matlab-ts-mode--assert-msg (format "line-node-types mismatch
  new:  \"%s\"
  orig: \"%s\"
  at line %d in %s"
                                        curr-line-node-types
                                        orig-line-node-types
                                        linenum
                                        (buffer-name)))))

(defun matlab-ts-mode--ei-assert-line-nodes-match (beg)
  "Assert that original line node types match modified line node types.
We examine lines between BEG and the line with the \\='m-ei-end text
property on its newline."

  (let ((end (next-single-property-change (point-min) 'm-ei-end))
        (linenum (line-number-at-pos beg))
        done)

    (setq matlab-ts-mode--ei-line-nodes (matlab-ts-mode--ei-line-nodes-in-region beg end)
          matlab-ts-mode--ei-line-nodes-loc matlab-ts-mode--ei-line-nodes)

    (save-excursion
      (goto-char beg)
      (while (not done)
        (let ((orig-line-node-types (when matlab-ts-mode--ei-orig-line-node-types-cache
                                      (gethash linenum
                                               matlab-ts-mode--ei-orig-line-node-types-cache))))
          (when orig-line-node-types ;; line was updated

            (matlab-ts-mode--ei-fast-back-to-indentation)
            (let (curr-line-node-types
                  (eol-pt (pos-eol)))
              (cl-loop
               while (< (point) eol-pt)
               do
               (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
                      (node-type (or (car pair)
                                     (cl-return)))
                      (node (cdr pair)))
                 (setq curr-line-node-types
                       (matlab-ts-mode--ei-update-line-node-types curr-line-node-types
                                                                  node node-type))
                 (let ((node-end (treesit-node-end node)))
                   (if (< node-end eol-pt)
                       (goto-char node-end)
                     (goto-char eol-pt)))))

              (matlab-ts-mode--ei-assert-nodes-types-match curr-line-node-types
                                                           orig-line-node-types
                                                           linenum)))
          (when (not (setq done (get-text-property (pos-eol) 'm-ei-end)))
            (forward-line)
            (setq linenum (1+ linenum)))))


      (setq matlab-ts-mode--ei-line-nodes nil
            matlab-ts-mode--ei-line-nodes-loc nil))))

(defun matlab-ts-mode--ei-get-disabled-regions ()
  "Return regions disabled by %-indent-mode=minimal comments.
Electric indent can be disabled then enabled using comments:
  %-indent-mode=minimal
    <code and comments>
  %-indent-mode=full
Code in the minimal region has the indent-level (spaces to the left)
modified as required.  The elements within the lines are not
modified.  For example,

  %-indent-mode=minimal
  if a >    1
  disp( \"a > 1\")
      end
  %-indent-mode=full

is indented to the following.  Notice that whitespace in between the
line elements is not modified.

  %-indent-mode=minimal
  if a >    1
      disp( \"a > 1\")
  end
  %-indent-mode=full

If we remove the %-indent-mode=* comments, indent produces:

  if a > 1
      disp(\"a > 1\")
  end

Returns nil if no disabled regions, else returns:
    \\='((REG-BOL-POS1 . REG-EOL-POS1) (REG-BOL-POS2 . REG-EOL-POS2) ...)
where
  REG-BOL-POS1 corresponds to the `pos-bol' of the first %-indent-mode=minimal
  REG-EOL-POS1 corresponds to the `pos-eol' of the first %-indent-mode=full
and so on."
  (let (result
        reg-bol-pos)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward (rx bol (0+ (or " " "\t"))
                                      (group (or "%-indent-mode=minimal" "%-indent-mode=full"))
                                      word-end)
                                  nil t)
          (let ((directive (match-string 1)))
            (pcase directive
              ("%-indent-mode=minimal"
               (when (not reg-bol-pos)
                 (setq reg-bol-pos (pos-bol))))
              ("%-indent-mode=full"
               (when reg-bol-pos
                 (push `(,reg-bol-pos . ,(pos-eol)) result)
                 (setq reg-bol-pos nil)))
              (_
               (matlab-ts-mode--assert-msg (format "bad directive, %s" directive))))))
        (when reg-bol-pos
          ;; Last character is a newline, point-max is one past that.
          (push `(,reg-bol-pos . ,(1- (point-max))) result))))
    (reverse result)))

;; Cache has value 'have-disabled-regions or 'no-disabled-regions
(defvar-local matlab-ts-mode--ei-disabled-regions-cache nil)

(defun matlab-ts-mode--ei-set-disabled-regions-cache (init)
  "If INIT is t, setup disabled regions cache, else clear it.
The cache consists of `matlab-ts-mode--ei-disabled-regions-cache' and
\\='m-ei-disabled t property on the newline characters.
Assumes there are no buffer restrictions in place."
  (with-silent-modifications
    (if init
        (let ((disabled-regions (matlab-ts-mode--ei-get-disabled-regions)))
          (if disabled-regions
              (save-excursion
                (dolist (region disabled-regions)
                  (let ((reg-bol-pos (car region))
                        (reg-eol-pos (cdr region)))
                    (goto-char reg-bol-pos)
                    (while (< (point) reg-eol-pos)
                      (let ((eol-pt (pos-eol)))
                        (put-text-property eol-pt (1+ eol-pt) 'm-ei-disabled t))
                      (forward-line))))
                (setq matlab-ts-mode--ei-disabled-regions-cache 'have-disabled-regions))
            (setq matlab-ts-mode--ei-disabled-regions-cache 'no-disabled-regions)))
      ;; Else clear the cache
      (setq matlab-ts-mode--ei-disabled-regions-cache nil)
      (remove-text-properties (point-min) (point-max) '(m-ei-disabled nil)))))

(defun matlab-ts-mode--ei-in-disabled-region (&optional eol-pt)
  "Is EOL-PT, defaulting to `pos-eol' within a disabled region?"
  (if matlab-ts-mode--ei-disabled-regions-cache
      (if (eq matlab-ts-mode--ei-disabled-regions-cache 'no-disabled-regions)
          nil ;; no disabled regions in the buffer, so not in a disabled region.
        (get-text-property (or eol-pt (pos-eol)) 'm-ei-disabled))
    (let ((disabled-regions (matlab-ts-mode--ei-get-disabled-regions))
          (pt (or eol-pt (point))))
      (when disabled-regions
        (cl-loop for region in disabled-regions do
                 (let ((reg-bol-pos (car region))
                       (reg-eol-pos (cdr region)))
                   (when (and (>= pt reg-bol-pos)
                              (<= pt reg-eol-pos))
                     (cl-return t))))))))

(defun matlab-ts-mode--ei-setup (beg end)
  "Setup for indent in region BEG to END."

  ;; Expand region to enable alignments, e.g. consecutive statements, matrix alignment
  (save-excursion
    ;; Move beg "up" based on consecutive lines
    (goto-char beg)
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at (rx (0+ (or " " "\t")) (not (or " " "\t")))))
      (setq beg (point))
      (forward-line -1))

    ;; Move up to handle matrix with blank lines
    (when (not (bobp))
      (goto-char beg)
      (when (re-search-forward "[^ \t\n]" nil t)
        (backward-char)
        (let ((matrix-node (treesit-parent-until (treesit-node-at (point)) (rx bol "matrix" eol))))
          (when matrix-node
            (goto-char (treesit-node-start matrix-node))
            (setq beg (pos-bol))))))

    ;; Move end "down" based on consecutive lines
    (goto-char end)
    (forward-line -1)
    (while (and (not (eobp))
                (looking-at (rx (0+ (or " " "\t")) (not (or " " "\t")))))
      (forward-line)
      (setq end (point)))

    ;; Move down to handle matrix with blank lines
    (when (not (eobp))
      (goto-char end)
      (when (re-search-backward "[^ \t\n]" nil t)
        (let ((matrix-node (treesit-parent-until (treesit-node-at (point)) (rx bol "matrix" eol))))
          (when matrix-node
            (goto-char (treesit-node-end matrix-node))
            (forward-line)
            (setq end (point)))))))

  (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)
  (setq matlab-ts-mode--ei-line-nodes (matlab-ts-mode--ei-line-nodes-in-region beg end))
  nil)

(defun matlab-ts-mode--ei-cleanup ()
  "Free memory used during indent."
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(m-matrix-info nil)))
  (setq matlab-ts-mode--ei-line-nodes nil
        matlab-ts-mode--ei-errors-map nil
        matlab-ts-mode--ei-bol2loc-map nil))

(cl-defun matlab-ts-mode--ei-indent-elements-in-line (&optional is-indent-region start-pt-area)
  "Indent current line by adjusting spacing around elements.

When IS-INDENT-REGION is t, we return tuple

  (list NEW-LINE UPDATED NEW-START-PT-OFFSET)

  If START-PT-AREA is \\='start-pt-area-in-region and
  \\='m-ei-start-pt-offset property exists on the newline of the current
  line, NEW-START-PT-OFFSET is the offset for the logical point in the
  new line, otherwise NEW-START-PT-OFFSET will be nil.

When IS-INDENT-REGION is nil, we update the line and adjust the point
to maintain its logical location."

  (let ((bol-pt (pos-bol))
        (eol-pt (pos-eol)))

    (when (not is-indent-region)
      (matlab-ts-mode--ei-setup bol-pt eol-pt))

    (when (matlab-ts-mode--ei-in-disabled-region)
      (cl-return-from matlab-ts-mode--ei-indent-elements-in-line))

    (let* ((start-node-and-offset (when (eq start-pt-area 'start-pt-area-in-region)
                                    (matlab-ts-mode--ei-get-start-info is-indent-region)))
           (start-node (car start-node-and-offset)) ;; may be nil
           (start-node-offset (cdr start-node-and-offset))
           (orig-line (buffer-substring-no-properties bol-pt eol-pt))
           cached-ei-info
           ei-info
           result)

      (if (and (let ((m-matrix-info (get-text-property eol-pt 'm-matrix-info)))
                 ;; 'numeric-m-matrix lines are fully handled by
                 ;; `matlab-ts-mode--ei-get-new-line' and there's no caching for them
                 (and m-matrix-info (eq (car m-matrix-info) 'non-numeric-m-matrix)))
               matlab-ts-mode--ei-align-nnm-matrix-cache
               (setq cached-ei-info
                     (gethash (line-number-at-pos) matlab-ts-mode--ei-align-nnm-matrix-cache)))
          ;; TopTester: electric_indent_non_numeric_m_matrix.m
          (setq ei-info cached-ei-info)
        (setq ei-info (matlab-ts-mode--ei-get-new-line start-node start-node-offset)))

      (if ei-info
          (setq result
                (progn
                  (when (and (not cached-ei-info) matlab-ts-mode--ei-align-enabled)
                    (setq ei-info (matlab-ts-mode--ei-align ei-info)))
                  (let* ((ei-line (or (nth 0 ei-info) orig-line))
                         ;; new-start-pt-offset is non-nil if start-node-offset is non-nil
                         (new-start-pt-offset (nth 1 ei-info))
                         (orig-line-node-types (nth 2 ei-info))
                         (updated (and ei-info (not (string= orig-line ei-line)))))
                    (when (and updated new-start-pt-offset start-node (looking-at "[ \t]*$"))
                      ;; TODO - is this needed? Can we hit this?
                      (setq new-start-pt-offset (length ei-line))) ;; at eol

                    (when (and matlab-ts-mode--indent-assert updated)
                      (when matlab-ts-mode--ei-orig-line-node-types-cache
                        (puthash (line-number-at-pos) orig-line-node-types
                                 matlab-ts-mode--ei-orig-line-node-types-cache))
                      (matlab-ts-mode--ei-assert-line-match ei-line orig-line))

                    (if is-indent-region
                        (list ei-line updated new-start-pt-offset) ;; result
                      ;; Else updated the line if needed (TAB on a line to electric indents it).
                      (when updated
                        (delete-region bol-pt eol-pt)
                        (insert ei-line)
                        (when new-start-pt-offset
                          (goto-char (+ (pos-bol) new-start-pt-offset))))
                      nil ;; return nil for TAB indent
                      ))))
        ;; else nothing updated
        (when is-indent-region
          (setq result (list orig-line))))

      (when (not is-indent-region)
        (matlab-ts-mode--ei-cleanup)
        (matlab--eilb-kill))
      result)))

(defun matlab-ts-mode--ei-move-to-logical-start-pt (beg end start-pt-area start-pt-offset)
  "Move point to its logical start point accounting for indent.
BEG and END are the final points of the indent region.

When START-PT-AREA is

\\='start-pt-area-before-beg : no point movement.

\\='start-pt-area-at-beg : point is moved to BEG.

\\='start-pt-area-in-region: point is moved to start line at column
START-PT-OFFSET where the start line is found by looking for text
property \\='m-ei-start-pt-offset.

\\='start-pt-area-at-end : point is moved to END.

\\='start-pt-area-past-end : no point movement.

When point is moved, it is also returned, otherwise returns nil."


  (cond

   ((eq start-pt-area 'start-pt-area-before-beg)
    ;; TopTester: electric_indent_xr_start_pt_area_before_beg.m
    nil)

   ((eq start-pt-area 'start-pt-area-at-beg)
    ;; TopTester: electric_indent_xr_start_pt_area_at_beg.m
    (goto-char beg))

   ((eq start-pt-area 'start-pt-area-in-region)
    ;; TopTester: electric_indent_xr_start_pt_area_in_region.m
    (let ((prop-loc (next-single-property-change (point-min) 'm-ei-start-pt-offset)))
      (goto-char prop-loc) ;; gets us to our start line
      (when (not start-pt-offset)
        (setq start-pt-offset (get-text-property (point) 'm-ei-start-pt-offset)))
      (forward-line 0)
      (let ((eol-col (- (pos-eol) (pos-bol))))
        (when (> start-pt-offset eol-col) ;; be robust too big a start-pt-offset
          ;; TopTester: electric_indent_start_pt_offset.m
          (setq start-pt-offset eol-col)))
      (forward-char start-pt-offset)
      (point)))

   ((eq start-pt-area 'start-pt-area-at-end)
    ;; TopTester: electric_indent_xr_start_pt_area_at_end.m
    (goto-char end))

   ((eq start-pt-area 'start-pt-area-past-end)
    ;; TopTester: electric_indent_xr_start_pt_area_past_end.m
    nil)

   (t
    (matlab-ts-mode--assert-msg (format "bad start-pt-area %S" start-pt-area)))))

(defun matlab-ts-mode--ei-set-region-caches (init &optional start-pt-area beg end)
  "Setup electric indent caches.
Optional START-PT-AREA, and region BEG END must be provided
when INIT is t.  If INIT is non-nil, set to initial value, otherwise set
to nil."

  (if init
      (matlab-ts-mode--ei-setup beg end)
    (matlab-ts-mode--ei-cleanup))

  (with-silent-modifications

    ;; Under normal operations, removing the text properties is a nop.  Removing the properties is
    ;; helpful when debugging and low cost for production code.
    (remove-text-properties (point-min) (point-max) '(m-ei-start-pt-offset nil m-ei-end nil))

    (when init
      (when (eq start-pt-area 'start-pt-area-in-region) ;; beg < point < end
        ;; character column number of the starting point and electric indent uses this to maintain
        ;; the logical point. For example
        ;;   a = [1000 2
        ;;        3 4];
        ;;          ^       <== point here
        ;; Electric indent yields the following where the logical point is maintained:
        ;;   a = [1000, 2
        ;;           3, 4];
        ;;              ^   <== point here
        (let ((eol-pt (pos-eol)))
          (put-text-property eol-pt (1+ eol-pt) 'm-ei-start-pt-offset (matlab--ei-offset))))

      ;; Mark the newline that end points so we know when to stop electric indent
      (put-text-property end (1+ end) 'm-ei-end t)))

  (setq
   matlab-ts-mode--ei-align-assign-cache             (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-align-prop-cache               (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-align-comment-cache            (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-m-matrix-col-widths-cache      (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-align-nnm-matrix-cache         (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-is-m-struct-cache              (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-orig-line-node-types-cache     (when init
                                                       (make-hash-table :test 'eql))

   matlab-ts-mode--ei-disabled-regions-cache (matlab-ts-mode--ei-set-disabled-regions-cache init)))

(defun matlab-ts-mode--ei-indent-region-impl (new-content-buf start-pt-area beg end)
  "Implementation for `matlab-ts-mode--ei-indent-region'.
NEW-CONTENT-BUF is used to electric indent BEG to END region.
Current buffer `point' is logically maintained.  For example,
   a = [1 2,3];
            ^      <== buffer point here
indent yields:
   a = [1, 2, 3];
              ^    <== updated point here is returned.
START-PT-AREA is used to move the point to its logical location
after indent."

  (cl-assert (save-excursion (goto-char beg) (= (point) (pos-bol))) t "beg at pos-bol")
  (cl-assert (save-excursion (goto-char end) (= (point) (pos-eol))) t "end at pos-eol")

  ;; We need to run electric indent before treesit-indent-region. Consider
  ;;    l2 = @(x)((ischar(x) || isstring(x) || isnumeric(x)) && ...
  ;;                 ~strcmpi(x, 'fubar'));
  ;; If we indent-region first, we'll get
  ;;    l2 = @(x)((ischar(x) || isstring(x) || isnumeric(x)) && ...
  ;;              ~strcmpi(x, 'fubar'));
  ;; then when we adjust spacing, we'll have the following where the 2nd line is not
  ;; indented correctly (the 2nd line should be shifted left one character).
  ;;    l2 = @(x) ((ischar(x) || isstring(x) || isnumeric(x)) && ...
  ;;              ~strcmpi(x, 'fubar'));

  (let* (region-updated
         done
         start-pt-offset)

    (goto-char beg)
    (while (not done)
      (let* ((eol-pt (pos-eol))
             (line-ending (buffer-substring eol-pt (1+ eol-pt)))) ;; newline w/our text properties
        (if (matlab-ts-mode--ei-in-disabled-region eol-pt)
            (let ((curr-line (buffer-substring-no-properties (pos-bol) eol-pt)))
              (with-current-buffer new-content-buf
                (insert curr-line line-ending)))
          ;; else: electric indent the line
          (let* ((tuple (matlab-ts-mode--ei-indent-elements-in-line 'indent-region start-pt-area))
                 (new-line-content (nth 0 tuple))
                 (line-updated (nth 1 tuple)))
            (with-current-buffer new-content-buf
              (insert new-line-content line-ending))
            (when (and (eq start-pt-area 'start-pt-area-in-region)
                       (get-text-property eol-pt 'm-ei-start-pt-offset))
              (if-let* ((new-start-pt-offset (nth 2 tuple)))
                  (if new-start-pt-offset
                      (setq start-pt-offset new-start-pt-offset))
                (when line-updated
                  (setq start-pt-offset (length new-line-content)))))
            (when line-updated
              (setq region-updated t))))
        (when (not (setq done (get-text-property eol-pt 'm-ei-end)))
          (forward-line))))

    (when region-updated
      (save-excursion
        (goto-char beg)
        (delete-region beg (1+ end))
        (insert (with-current-buffer new-content-buf (buffer-string)))
        (when matlab-ts-mode--indent-assert
          (matlab-ts-mode--ei-assert-line-nodes-match beg))
        ;; Update END point accounting for electric indent buffer modifications
        (setq end (next-single-property-change (point-min) 'm-ei-end))))

    (let ((moved-point (matlab-ts-mode--ei-move-to-logical-start-pt
                        beg end start-pt-area start-pt-offset)))
      (treesit-indent-region beg end)
      (when moved-point
        (point)))))

(defun matlab-ts-mode--ei-clean-cr (beg end)
  "Convert CRLF to LF and convert lone CR to LF in whole buffer.
May alter BEG, END, and `point'.
Returns (cons NEW-BEG NEW-END).
Assumes no buffer restrictions.
Logical point is maintained, i.e. point is updated as needed."
  ;; TopTester: tests/test-matlab-ts-mode-electric-indent-files/electric_indent_mat_with_cr_chars.m
  (let ((buffer-pt (point)))

    (when (string-match "dos$" (symbol-name buffer-file-coding-system))
      ;; File has CRLF and Emacs loaded them as LF's. If we leave coding as dos, then Emacs will
      ;; write them back-out as CRLF. Therefore, we switch to unix coding, which tells emacs to
      ;; write out the LF's out as is and lets us see the CR (\r) below.
      ;;
      ;; Note, setting the coding-system to utf-8-unix marks the buffer as dirty because when the
      ;; file is save the CRLF's will become LF's.
      (set-buffer-file-coding-system 'utf-8-unix))

    ;; Can have a file whos first 50 lines are LF, then 51 - 55 are CRLF, then remainder are LF
    ;; (this occurs when editing files on different platforms).
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (if (looking-at "$" t) ;; CRLF => LF?
          (progn
            (when (<= (point) buffer-pt)
              (setq buffer-pt (1- buffer-pt)))
            (replace-match "")
            (when (<= (point) beg)
              (setq beg (1- beg)))
            (when (<= (point) end)
              (setq end (1- end))))
        ;; Else handle the case of \r only, which MATLAB treats as a newline, e.g.
        ;;   foo1.m
        ;;   ------
        ;;   m = [10^M20]^J         // man ascii: ^M == \r  and ^J == \n
        ;;
        ;;   >> foo1
        ;;   m =
        ;;       10
        ;;       20
        ;;
        ;;   hexdump -C foo1.m
        ;;   00000000  6d 20 3d 20 5b 31 30 0d  32 30 5d 0a              |m = [10.20].|
        ;;                                  \r           \n
        (replace-match "\n")))

    ;; Add final newline if missing
    (goto-char (point-max))
    (when (not (looking-at "^$"))
      (when (= end (point))
        (setq end (1+ end)))
      (insert "\n"))

    (goto-char buffer-pt) ;; maintain logical buffer point
    ;; Result: updated beg end indent region points
    (cons beg end)))

(defun matlab-ts-mode--ei-indent-region (beg end)
  "Indent BEG END region by adjusting spacing around elements.
If BEG is not at start of line, it is moved to start of the line.  If
END is not at start or end of line, it is moved to corresponding end of
the line.  This expansion of the region is done to simplify electric
indent."

  (let (buffer-pt)

    (save-excursion
      (save-restriction
        (widen)

        (let ((updated-beg-end (matlab-ts-mode--ei-clean-cr beg end)))
          (setq beg (car updated-beg-end)
                end (cdr updated-beg-end)))

        (let ((new-content-buf (get-buffer-create
                                (generate-new-buffer-name " *temp-matlab-indent-region*"))))
          (unwind-protect
              (let* ((start-disabled (matlab-ts-mode--ei-in-disabled-region))
                     (end-disabled (matlab-ts-mode--ei-in-disabled-region
                                    (save-excursion (goto-char end)
                                                    (pos-eol)))))

                (if (and start-disabled end-disabled)

                    (treesit-indent-region beg end) ;; adjust indent-level only

                  ;; Else electric indent
                  (save-excursion
                    (goto-char beg)
                    (setq beg (pos-bol)))

                  (save-excursion
                    (goto-char end)
                    (if (and (> end beg) (looking-at (rx bol)))
                        (setq end (1- (pos-bol)))
                      (setq end (pos-eol))))

                  (setq end (matlab-ts-mode--ei-workaround-143 beg end))

                  (when (not (= beg end))
                    ;; start-pt-area is where the point is in relation to the indent region.
                    (let ((start-pt-area (cond ((< (point) beg)
                                                'start-pt-area-before-beg)
                                               ((= (point) beg)
                                                'start-pt-area-at-beg)
                                               ((and (> (point) beg) (< (point) end))
                                                'start-pt-area-in-region)
                                               ((= (point) end)
                                                'start-pt-area-at-end)
                                               ((> (point) end)
                                                'start-pt-area-past-end)
                                               (t
                                                (matlab-ts-mode--assert-msg
                                                 (format "no start-pt-area"))))))
                      (matlab-ts-mode--ei-set-region-caches t start-pt-area beg end)

                      (setq buffer-pt
                            (matlab-ts-mode--ei-indent-region-impl new-content-buf start-pt-area
                                                                   beg end))))))

            ;; unwind-protect cleanup:
            (matlab--eilb-kill)
            (matlab-ts-mode--ei-set-region-caches nil)
            (kill-buffer new-content-buf)))))

    ;; Logical buffer point maintained (adjusted as appropriate)
    (when buffer-pt
      (goto-char buffer-pt))))

(provide 'matlab-ts-mode--ei)
;;; matlab-ts-mode--ei.el ends here

;; LocalWords:  SPDX gmail treesit defcustom bos eos isstring defun eol eobp setq curr cdr xr progn
;; LocalWords:  listp alist dolist setf tmp buf utils linenum nums bobp pcase untabify SPC eilb prev
;; LocalWords:  linenums reindent bol fubar repeat:ans defmacro bn impl puthash caadr caar gethash
;; LocalWords:  ERROR's repeat:nil lang xyz cdar lparen rparen lbrack rbrack lbrace rbrace eql consp
;; LocalWords:  geq eqeq neq memq bols ridx rchild defconst FFs stmt lstart nreverse rw CRLF LF LF's
;; LocalWords:  setcar setcdr anychar CRLF's hexdump ws nnm
