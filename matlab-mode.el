;;; matlab-mode.el --- Major mode for MATLAB(R) dot-m files -*- lexical-binding: t -*-

;; Version: 8.2.1
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Matt Wette <mwette@alumni.caltech.edu>,
;;         Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>, Uwe Brauer <oub@mat.ucm.es>, John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: 04 Jan 91
;; Keywords: MATLAB(R)
;; Package-Requires: ((emacs "27.2"))

;; Copyright (C) 1991-2025 Free Software Foundation, Inc.
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

;; This major mode for GNU Emacs provides support for editing MATLAB(R)
;; dot-m files.  It automatically indents for block structures (including
;; nested functions), line continuations (e.g., ...), and comments.

;; Additional features include auto-fill including auto-additions of
;; ellipsis for commands, and even strings.  Block/end construct
;; highlighting as you edit.  Primitive code-verification and
;; identification.  Templates and other code editing functions.
;; Advanced symbol completion.  Code highlighting via font-lock.
;; There are many navigation commands that let you move across blocks
;; of code at different levels.

;; Lastly, there is support for running MATLAB(R) in an Emacs buffer,
;; with full shell history and debugger support (when used with the db
;; commands.)  The shell can be used as an online help while editing
;; code, providing help on functions, variables, or running arbitrary
;; blocks of code from the buffer you are editing.

;;; Code:

(defconst matlab-version (package-get-version)
  "Version of Emacs MATLAB Mode.")

(require 'matlab)

(provide 'matlab-mode)

;;; matlab-mode.el ends here

;; LocalWords:  SPDX Wette mwette edu Ludlam eludlam Uwe Brauer oub ucm gmail defconst
