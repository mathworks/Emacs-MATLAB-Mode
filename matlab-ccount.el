;;; matlab-ccount.el --- character count minor mode
;;
;; Copyright (C) 2021 
;;
;; Author:  Eric Ludalm
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
;; Make it easier to participate in the mini-hack contest by showing how many
;; code characters are in your script.
;; https://www.mathworks.com/matlabcentral/contests.html
;;
;; Based on contest rules, comments, newlines, and spaces at EOL don't
;; count toward character counts.

;;; Code:
(easy-mmode-define-minor-mode matlab-character-count-minor-mode
  "Toggle character count minor-mode.
When active, this mode shows the # of characters in this buffer.
MATLAB Character counter ignores comments and indentation spaces."
  nil
  (:eval (format " %d" (matlab-count-characters)))
  nil ; empty mode-map
  nil ; empty body
  )

;;; Character Counting
;;
(defun matlab-count-characters ()
  "Return a number that is the number of characters in this buffer.
MATLAB character count will ignore comments, indentation spaces,
and trailing semi-colons so long as there are no chars after the ;"
  (save-excursion
    (goto-char (point-min))
    (let ((all (- (point-max) 1))
	  (comments 0)
	  (indents 0)
	  (trail-semi 0)
	  (last-land -1)
	  )
      ;; The contest starts with a fresh figure every time.  Our
      ;; script needs to reset any figure we're going to use, so skip that up front.
      (while (looking-at "\\(clf\\|figure\\|reset(gcf)\\|clear\\);?")
	(goto-char (match-end 0))
	(skip-chars-forward "\n\t "))
      (setq all (- all (point) -1))
      
      ;; Scan for bum syntax across the rest of the system
      (while (and (not (eobp)) (re-search-forward "\\s<\\|;?\\s-*$" nil t)
		  (not (= last-land (point))))
	(goto-char (match-beginning 0))
	(setq last-land (point))
	(cond
	 ((looking-at ";?$")
	  (setq indents (+ indents (- (match-end 0) (match-beginning 0))))
	  (goto-char (match-end 0))
	  (when (not (eobp))
	    (setq indents (- indents (- (point)
					(progn
					  ;; Also skip leading indent on next line
					  (skip-chars-forward "\n\t ")
					  (point)))))
	    )
	  )
	 ((and (looking-at "\\s-+") (beginning-of-line))
	  (setq indents (+ indents (- (match-end 0) (match-beginning 0))))
	  (goto-char (match-end 0))
	  )
	 ((looking-at "\\s<")
	  (setq comments (- comments (- (point)
					(progn
					  (forward-comment 100000)
					  (point))))))
	 (t
	  ;; Some stuff like . triggers big regex, but isn't in the above.
	  ;; Skip over whatever it is.
	  (goto-char (match-end 0)))
	 ))
      (- all comments indents trail-semi)
      )))

(provide 'matlab-ccount)

;;; matlab-ccount.el ends here
