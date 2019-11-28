;;; matlab-shell-gud.el --- GUD support in matlab-shell.
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <eludlam@emacsvm>
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
;; GUD (grand unified debugger) support for MATLAB shell.
;;
;; Includes setting up gud mode in the shell, and all filters, etc specific
;; to supporting gud.

(require 'matlab-shell)

(eval-and-compile
  (require 'gud)
  )

;;; Code:

;;;###autoload
(defun matlab-shell-mode-gud-enable-bindings ()
  "Enable GUD features for `matlab-shell' in the current buffer."

  ;; Make sure this is safe to use gud to debug MATLAB
  (when (not (fboundp 'gud-def))
    (error "Your emacs is missing `gud-def' which means matlab-shell won't work correctly.  Stopping"))

  (gud-def gud-break  "dbstop in %d/%f at %l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "dbclear in %d/%f at %l" "\C-d" "Remove breakpoint at current line.")
  (gud-def gud-step   "dbstep in"           "\C-s" "Step one source line, possibly into a function.")
  (gud-def gud-next   "dbstep %p"           "\C-n" "Step over one source line.")
  (gud-def gud-cont   "dbcont"              "\C-r" "Continue with display.")
  (gud-def gud-stop-subjob "dbquit"         nil    "Quit debugging.") ;; gud toolbar stop
  (gud-def gud-finish "dbquit"              "\C-f" "Finish executing current function.")
  (gud-def gud-up     "dbup"                "<"    "Up N stack frames (numeric arg).")
  (gud-def gud-down   "dbdown"              ">"    "Down N stack frames (numeric arg).")
  ;; using (gud-def gud-print  "%e" "\C-p" "Eval expression at point") fails
  (gud-def gud-print  "% gud-print not available" "\C-p" "gud-print not available.")

  (if (fboundp 'gud-make-debug-menu)
      (gud-make-debug-menu))
  )

;;;###autoload
(defun matlab-shell-gud-startup ()
  "Configure GUD when a new `matlab-shell' is initialized."
  (gud-mode)

  ;; TODO - the filter and stuff was setup in 2 diff ways.
  ;; Pick one and stick with it.
  
  (make-local-variable 'gud-marker-filter)
  (setq gud-marker-filter 'gud-matlab-marker-filter)
  (make-local-variable 'gud-find-file)
  (setq gud-find-file 'gud-matlab-find-file)

  (if (fboundp 'gud-overload-functions)
      (gud-overload-functions
       '((gud-massage-args . gud-matlab-massage-args)
	 (gud-marker-filter . gud-matlab-marker-filter)
	 (gud-find-file . gud-matlab-find-file))))
  

  ;; XEmacs doesn't seem to have this concept already.  Oh well.
  (make-local-variable 'gud-marker-acc)
  (setq gud-marker-acc nil)

  
  (gud-set-buffer))

;;; GUD Functions
(defun gud-matlab-massage-args (file args)
  "Argument message for starting matlab file.
I don't think I have to do anything, but I'm not sure.
FILE is ignored, and ARGS is returned."
  args)

(defun gud-matlab-find-file (f)
  "Find file F when debugging frames in MATLAB."
  (save-excursion
    (let* ((realfname (if (string-match "\\.\\(p\\)$" f)
			  (progn
			    (aset f (match-beginning 1) ?m)
			    f)
			f))
	   (buf (find-file-noselect realfname)))
      (set-buffer buf)
      (if (fboundp 'gud-make-debug-menu)
	  (gud-make-debug-menu))
      buf)))


;;; GUD Filter Function
;;
;; MATLAB's process filter handles output from the MATLAB process and
;; interprets it for formatting text, and for running the debugger.

(defvar gud-matlab-marker-regexp-plain-prompt "^K?>>"
  "Regular expression for finding a prompt.")

(defvar gud-matlab-marker-regexp-1 "^K>>"
  "Regular expression for finding a file line-number.")

(defvar gud-matlab-marker-regexp-2
  (concat "^> In \\(" matlab-anchor-beg
          "\\|\\)\\([-.a-zA-Z0-9_>/@]+\\) \\((\\w+) \\|\\)at line \\([0-9]+\\)[ \n]+")
  "Regular expression for finding a file line-number.
Please note: The leading > character represents the current stack frame, so if
there are several frames, this makes sure we pick the right one to popup.")

(defvar gud-matlab-dbhotlink nil
  "Track if we've sent a dbhotlink request.")
(make-variable-buffer-local 'gud-matlab-dbhotlink)

(defun gud-matlab-marker-filter (string)
  "Filters STRING for the Unified Debugger based on MATLAB output."

  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output "") (frame nil))

    ;; ERROR DELIMITERS
    ;; Newer MATLAB's wrap error text in {^H  }^H characters.
    ;; Convert into something COMINT won't delete so we can scan them.
    (while (string-match "{" gud-marker-acc)
      (setq gud-marker-acc (replace-match matlab-shell-errortext-start-text t t gud-marker-acc 0)))

    (while (string-match "}" gud-marker-acc)
      (setq gud-marker-acc (replace-match matlab-shell-errortext-end-text t t gud-marker-acc 0)))
    
    ;; DEBUG PROMPTS
    (when (string-match gud-matlab-marker-regexp-1 gud-marker-acc)

      ;; Look for any frames for case of a debug prompt.
      (let ((url gud-marker-acc)
	    ef el)

	;; We use dbhotlinks to create the below syntax.  If we see it we have a frame,
	;; and should tell gud to go there.
	
	(when (string-match "opentoline('\\([^']+\\)',\\([0-9]+\\),\\([0-9]+\\))" url)
	  (setq ef (substring url (match-beginning 1) (match-end 1))
		el (substring url (match-beginning 2) (match-end 2)))

	  (setq frame (cons ef (string-to-number el)))))

      ;; Newer MATLAB's don't print useful info.  We'll have to
      ;; search backward for the previous line to see if a frame was
      ;; displayed.
      (when (and (not frame) (not gud-matlab-dbhotlink))
	(let ((dbhlcmd (if matlab-shell-echoes
			   "dbhotlink()%%%\n"
			 ;; If no echo, force an echo
			 "disp(['dbhotlink()%%%' newline]);dbhotlink();\n")))
	  ;;(when matlab-shell-io-testing (message "!!> [%s]" dbhlcmd))
	  (process-send-string (get-buffer-process gud-comint-buffer) dbhlcmd)
	  )
	(setq gud-matlab-dbhotlink t)
	)
      )

    ;; If we're forced to ask for a stack hotlink, we will see it come in via the
    ;; process output.  Don't output anything until a K prompt is seen after the display
    ;; of the dbhotlink command.
    (when gud-matlab-dbhotlink
      (let ((start (string-match "dbhotlink()%%%" gud-marker-acc))
	    (endprompt nil))
	(if start
	    (progn
	      (setq output (substring gud-marker-acc 0 start)
		    gud-marker-acc (substring gud-marker-acc start))

	      ;; The hotlink text will persist until we see the K prompt.
	      (when (string-match "^K?>> " gud-marker-acc)
		(setq endprompt (match-end 0))

		;; (when matlab-shell-io-testing (message "!!xx [%s]" (substring gud-marker-acc 0 endprompt)))

		;; We're done with the text!  Remove it from the accumulator.
		(setq gud-marker-acc (substring gud-marker-acc endprompt))
		;; If we got all this at the same time, push output back onto the accumulator for
		;; the next code bit to push it out.
		(setq gud-marker-acc (concat output gud-marker-acc)
		      output ""
		      gud-matlab-dbhotlink nil)
		))
	  ;; Else, waiting for a link, but hasn't shown up yet.
	  ;; TODO - what can I do here to fix var setting if it gets
	  ;; locked?
	  (when (string-match "^>> " gud-marker-acc)
	    ;; A non-k prompt showed up.  We're not going to get out request.
	    (setq gud-matlab-dbhotlink nil))
	  )))
    
    ;; This if makes sure that the entirety of an error output is brought in
    ;; so that matlab-shell-mode doesn't try to display a file that only partially
    ;; exists in the buffer.  Thus, if MATLAB output:
    ;;  error: /home/me/my/mo/mello.m,10,12
    ;; All of that is in the buffer, and it goes to mello.m, not just
    ;; the first half of that file name.
    ;; The below used to match against the prompt, not \n, but then text that
    ;; had error: in it for some other reason wouldn't display at all.
    (if (and matlab-prompt-seen ;; don't pause output if prompt not seen
	     gud-matlab-dbhotlink ;; pause output if waiting on debugger
	     )
	;; We could be collecting debug info.  Wait before output.
	nil
      ;; Finish off this part of the output.  None of our special stuff
      ;; ends with a \n, so display those as they show up...
      (while (string-match "^[^\n]*\n" gud-marker-acc)
	(setq output (concat output (substring gud-marker-acc 0 (match-end 0)))
	      gud-marker-acc (substring gud-marker-acc (match-end 0))))

      (if (string-match "^K?>> $" gud-marker-acc)
	  (setq output (concat output gud-marker-acc)
		gud-marker-acc ""))
      
      ;; Check our output for a prompt, and existence of a frame.
      ;; If this is true, throw out the debug arrow stuff.
      (if (and (string-match "^>> $" output)
	       gud-last-last-frame)
	  (progn
	    (setq overlay-arrow-position nil
		  gud-last-last-frame nil
		  gud-overlay-arrow-position nil)
	    (sit-for 0)
	    )))

    (if frame (setq gud-last-frame frame))

    (when matlab-shell-io-testing
      (message "-->[%s] [%s]" output gud-marker-acc))

    ;;(message "Looking for prompt in %S" output)
    (when (and (not matlab-shell-suppress-prompt-hooks)
	       (string-match gud-matlab-marker-regexp-plain-prompt output))
      ;; Now that we are about to dump this, run our prompt hook.
      ;;(message "PROMPT!")
      (setq matlab-shell-prompt-hook-cookie t))
    
    output))



(provide 'matlab-shell-gud)

;;; matlab-shell-gud.el ends here
