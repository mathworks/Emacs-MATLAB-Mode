;;; mstest.el --- MATLAB Shell test suite
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@ballista>
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
;; Test MATLAB shell by running MATLAB in an inferior process, and sending it
;; commands, and making sure we can read it's errors, and send it commands.

(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (d (file-name-directory (directory-file-name d1)))
       )
  (defvar mst-testfile-path d1
    "Location of test MATLAB code.")
  
  (add-to-list 'load-path (expand-file-name d) t))

(require 'matlab)
(require 'matlab-shell)
(require 'comint)

;;; Code:
(defun mstest-run-all-tests ()
  "Run all the tests in this test file."

  ;; When debugging the test suite, make sure we can access the debugger.
  (unless noninteractive
    (toggle-debug-on-error)
    (toggle-debug-on-quit))

  (mstest-start)
  (mstest-completion)
  (mstest-error-parse)
  )

;;; Startup Tests
(defun mstest-start ()
  "Test that we can start MATLAB, and that it runs the correct init file."
  (let ((process-connection-type nil))
    ;; Use PIPE to prevent MATLAB from identifying the actual width of the terminal.
    ;; For some reason, batch mode emacs will choose a pty width of 10, which breaks up
    ;; text and makes it hard to test against.
    ;; A PIPE won't echo, so turn off that feature.
    (setq matlab-shell-echoes nil)
    (matlab-shell))
  
  (let ((msb (matlab-shell-active-p)))
    (when (not msb) (error "MATLAB Shell command failed to create a shell buffer."))
    (accept-process-output nil 1)
    (save-excursion
      (set-buffer msb)
      (when (not (get-buffer-process msb)) (error "MATLAB Shell buffer failed to start process."))

      ;; Check full startup.
      (let* ((start (current-time))
	     (elapsed nil))
	(while (not matlab-prompt-seen)
	  (setq elapsed (float-time (time-subtract nil start)))
	  (when (> elapsed 20)
	    (error "MATLAB Shell took to long (20s) to produce a prompt."))
	  (accept-process-output nil 1)
	  (redisplay)
	  (sit-for 1)))
      
      ;; Make sure MATLAB things we have enough columns to display the rest of our tests.
      ;; Without the right number of columns, future tests will fail.
      (let ((txt (mstest-get-command-output "disp(get(0,'CommandWindowSize'))")))
	(when (< (string-to-number txt) 80)
	  (mstest-savestate)	  
	  (error "COLUMNS TEST: Expecting a minimum of 80 columns, found %S" txt)))
      
      ;; Check that our path was added.
      (let ((txt (mstest-get-command-output "P=split(path,':');disp(P{1});"))
	    (tbxdir (expand-file-name "toolbox" (file-name-directory
						 (locate-library "matlab")))))
	(message "PATH TEST: Expecting %S" tbxdir)
	(when (not (string= txt tbxdir))
	  (message "PATH TEST: Found %S" txt)
	  (mstest-savestate)
	  (error "MATLAB Shell failed to initialize with matlab-emacs as first entry on path."))
	(message "PASS")
	)

      ;; Make sure we have a ROOT
      (message "MATLABROOT TEST")
      (let ((txt (matlab-shell-matlabroot)))
	(message "Computed MATLAB ROOT as : %S" txt)
	(when (or (not txt) (string= txt ""))
	  (mstest-savestate)
	  (error "Failed to find MATLABROOT."))
	)
      (message "PASS")

      ;; Turn off beeps, because during tests they are annoying.
      (message "BEEP OFF TEST: ")
      (let ((txt (mstest-get-command-output "beep off; stat = beep; disp(stat)")))
	(when (or (not txt) (not (string= txt "off")))
	  (mstest-savestate)
	  (error "Expected BEEPS to be off, but found %S" txt)))
      (message "PASS")
      
      ;; Make sure that 'WHICH' works correctly.
      (message "WHICH TEST: ls")
      (let ((txt (car (matlab-shell-which-fcn "ls")))
	    (exp (expand-file-name "toolbox/matlab/general/ls.m" (matlab-shell-matlabroot))))
	(if (string= txt exp)
	    (message "PASS")
	  (mstest-savestate)
	  (error "Expected %s, but found %s" exp txt)))
      
      )))

;;; Command Sending Tests
(defun mstest-completion ()
  "Test emacsdocomplete, and make sure it returns what we expect."
  (let ((msb (matlab-shell-active-p)))
    (when (not msb) (error "mstest-completion must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      ;; TEST completion fcn
      (message "COMPLETION TEST: emacs")
      (let* ((CLO
	      (condition-case ERR
		  (matlab-shell-completion-list "emacs")
		(error
		 (mstest-savestate)
		 (error "%S" ERR))))
	     (CL (cdr (nth 2 CLO)))
	     (EXP '("emacs" "emacsdocomplete" "emacsinit" "emacsnetshell" "emacsrunregion"))
	     (cnt 1))
	(while (and CL EXP)
	  (when (not (string= (car EXP) (car (car CL))))
	    (error "Expected %S /= actual TS for %d completion"
		   (car EXP) (car (car CL)) cnt))
	  (setq cnt (1+ cnt)
		CL (cdr CL)
		EXP (cdr EXP))))
      (message "PASS")

      )))

;;; Error Parsing
(defun mstest-error-parse ()
  "Test various errors, and if we can parse them."
  
  (let ((msb (matlab-shell-active-p)))
    (when (not msb) (error "mstest-completion must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      (mstest-error-command-check "buggy err" "buggy.m" 7)

      (mstest-error-command-check "buggy cmderr" "ls.m" 39)

      (mstest-error-command-check "buggy warn" "buggy.m" 15)

      (mstest-error-command-check "eltest.utils.testme" "testme.m" 7)

      (mstest-error-command-check "eltest.utils.testme(true)" "testme.m" 14)

      (mstest-error-command-check "et=eltest.EmacsTest; et.throwerr()" "EmacsTest.m" 17)

      ;; This must occur after assignment into variable et.
      (mstest-error-command-check "et.throwprop()" "EmacsTest.m" 22)

      (mstest-error-command-check "syntaxerr" "syntaxerr.m" 8)
      
      )))

(defun mstest-error-command-check (command file line)
  "Check that COMMAND produces an error that visits FILE at LINE.
Assume we are in the MATLAB process buffer."

  (message "TEST ERRORS: %s" command)
  (let ((txt (mstest-get-command-output command)))
    (goto-char (point-max))
  
    (save-window-excursion
      (condition-case ERR
	  (matlab-shell-last-error)
	(error
	 (mstest-savestate)
	 (message "matlab-shell-last-error produced n error:")
	 (error "%S" ERR))
	(t (error "%S" ERR)))
    
      (let* ((bfn (buffer-file-name))
	     (bfnd (if bfn (file-name-nondirectory bfn)
		     (buffer-name)))
	     (ln (count-lines (point-min) (min (1+ (point)) (point-max))))
	     )
      
	(when (not (string= bfnd file))
	  (mstest-savestate)
	  (message "Err Text Generated:\n%S" txt)
	  (error "Expected last error in %s.  Found myself in %s" file (buffer-name)))
	(when (not (= ln line))
	  (mstest-savestate)
	  (message "Err Text Generated:\n\n%S\n" txt)
	  (error "Expected last error in %s on line %d.  Found on line %d" file line ln))
      
	))
    (message "PASS")

    ;; Now CD someplace where these files are no longer on the path.
    (message "TEST ERRORS NOT ON PATH ANYMORE: %s" command)
    (mstest-get-command-output "cd('..')")

    ;; Re-do our last-error test to make sure it works when not on path.
    (save-window-excursion
      (condition-case ERR
	  (matlab-shell-last-error)
	(error
	 (mstest-savestate)
	 (error "Error not found"))
	(t (error "%S" ERR)))
    
      (let* ((bfn (buffer-file-name))
	     (bfnd (if bfn (file-name-nondirectory bfn)
		     (buffer-name)))
	     (ln (count-lines (point-min) (min (1+ (point)) (point-max))))
	     )
      
	(when (not (string= bfnd file))
	  (mstest-savestate)
	  (error "Expected last error in %s.  Found myself in %s" file (buffer-name)))
	(when (not (= ln line))
	  (mstest-savestate)
	  (error "Expected last error in %s on line %d.  Found on line %d" file line ln))
      
	))

    (mstest-get-command-output "cd('tests')")
  
    (message "PASS")
  
    ))
  

      
;;; UTILITIES

(defun mstest-get-command-output (command)
  "Wait for any pending output, and then return the text from lst command.
Searches for the text between the last prompt, and the previous prompt."
  (with-current-buffer (matlab-shell-active-p)

    ;; Send the command.
    (let ((start (point)))
      (cond
       ;; For a string, send it ourselves
       ((stringp command)
	(matlab-shell-send-command command))
       ;; For a function, call it, expect it to send some command.
       ((functionp command)
	(funcall command))
       ;; What is this?
       (t
	(error "Unkown command for mtest-get-command-output"))
       )
    
      ;; Wait.
      (let ((starttime (current-time))
	    (totaltime nil)
	    (matlab-shell-cco-testing t))
	(while (or (= (point) start)
		   (not (matlab-on-empty-prompt-p)))
	  (setq totaltime (float-time (time-subtract nil starttime)))
	  (when (> totaltime 10)
	    (mstest-savestate)
	    (error "Timeout waiting for prompt. (%d elapsed seconds)" totaltime))
	  (redisplay)
	  ;; Some filters also call accept-process-output inside this one
	  ;; which causes it to not time out.
	  (accept-process-output nil .5)
	  (goto-char (point-max)))))
    
    ;; Get the text from last prompt.
    (save-excursion
      (let ((inhibit-field-text-motion t)
	    (endpt nil)
	    (startpt nil))
	(goto-char (point-max))
	(beginning-of-line)
	(forward-char -1)
	(setq endpt (point))
	(re-search-backward comint-prompt-regexp)
	(beginning-of-line)
	(forward-line 1)
	(setq startpt (point))
	(buffer-substring-no-properties startpt endpt)
	))))

(defun mstest-savestate ()
  "Save output from *MATLAB* buffer when a test fails."
  (with-current-buffer (matlab-shell-active-p)
    (let* ((td (if (fboundp 'temporary-file-directory)
		   (temporary-file-directory)
		 temporary-file-directory))
	   (fn (expand-file-name "MATLABSHELL-BUFFER-CONTENST.txt" td)))
      (write-region (point-min) (point-max) fn)
      (message "Content of *MATLAB* buffer saved in %s" fn))))

(provide 'mstest)

;;; mstest.el ends here
