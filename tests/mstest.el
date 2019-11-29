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

(require 'matlab-load)
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

  ;; Enable this to see how the input/output is interacting with the
  ;; test harness.
  (when (getenv "TESTDEBUG")
    (setq matlab-shell-io-testing t))
    
  (mstest-start)
  (mstest-completion)
  (mstest-error-parse)
  (mstest-debugger)
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

      ;; During boot, we need to scrape the version number and release so we can load the
      ;; history file.  Make sure that happend.
      (if matlab-shell-running-matlab-version
	  (message "VERSION SCRAPE: Successfully found MATLAB Version %s" matlab-shell-running-matlab-version)
	(error "VERSION SCRAPE: Failed to find MATLAB Version number during startup."))
      (message "PASS")
      
      ;; Make sure MATLAB thinks we have enough columns to display the rest of our tests.
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
    (when (not msb) (error "mstest-error-parse must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      (mstest-error-command-check "buggy err" "buggy.m" 7)

      (mstest-error-command-check "buggy cmderr" "ls.m" -1)

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
Assume we are in the MATLAB process buffer.
If LINE is negative then do not test the line number."

  (message "ERRORS: %s" command)
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
	(when (and (> line  0) (not (= ln line)))
	  (mstest-savestate)
	  (message "Err Text Generated:\n\n%S\n" txt)
	  (error "Expected last error in %s on line %d.  Found on line %d" file line ln))
      
	))
    (message "PASS")

    ;; Now CD someplace where these files are no longer on the path.
    (message "NO PATH ERRORS: %s" command)
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
	(when (and (> line  0) (not (= ln line)))
	  (mstest-savestate)
	  (error "Expected last error in %s on line %d.  Found on line %d" file line ln))
      
	))

    (mstest-get-command-output "cd('tests')")
  
    (message "PASS")
  
    ))

;;; Debugging: Breakpoints, stopping, visiting files
(defun mstest-debugger ()
  "Test debugging commands, and how MATLAB outputs state."
  (let ((msb (matlab-shell-active-p)))
    (when (not msb) (error "mstest-debugger must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      ;; Basic create/clear cycle.
      (mstest-debugger-breakpoint "dbstop in dbtester" "dbtester" "4")
      (mstest-debugger-breakpoint "dbclear all" nil nil))

    (save-excursion
      (find-file (expand-file-name "dbtester.m" mst-testfile-path))
      (goto-line 6)
      ;; Use gud fcn
      (mstest-debugger-breakpoint #'gud-break "dbtester" "6")
      (mstest-debugger-navto "dbtester" "dbtester.m" "6")

      (mstest-debugger-navto #'gud-next "dbtester.m" 8)
      
      (mstest-debugger-navto #'gud-next "dbtester.m" 10)

      (mstest-debugger-navto #'gud-cont "dbtester.m" -1)
    
    )))

(defun mstest-debugger-navto (command fileexp lineexp &optional skipchecktxt)
  "Run some dbugger nav command, and verify we ended up in FILE at LINE.
Command can be anything that will end with a K>> prompt, such as running
the file that will hit a breakpoint, or dbstep, etc.
COMMAND can be a string to run on the ML command prompt, or it can be
a function."

  ;; In case test writer makes a mistake
  (when (stringp lineexp) (setq lineexp (string-to-number lineexp)))
  
  (message "DEBUG: Running %s until breakpoint" command)
  ;; Run the command, then call dbstatus to see what is there.
  (let ((txt (mstest-get-command-output command)))

    ;; TODO - test contents - should see a line with a # on it indicating the line
    ;; of text we stopped at.

    (let ((fname (file-name-nondirectory (buffer-file-name (current-buffer))))
	  (line (line-number-at-pos)))

      (if (<= lineexp 0)
	  ;; Neg means we aren't debugging anymore.
	  (when (matlab-on-debug-prompt-p)
	    ;; on a debug prompt, this is a problem.
	    (mstest-savestate)
	    (error "DEBUG: Expected to have exited debug mode, but still on K>> prompt."))

	;; else, we should have jumped to some src code.
	(when (or (not (string= fname fileexp))
		  (and (not (= line lineexp)) (< lineexp 0)))
	  (message "DEBUG: Expected %s line %d, ended up at %s line %d"
		   fileexp lineexp fname line)
	  (mstest-savestate)
	  (error "DEBUG test failed"))


	;; Text should start w/ a number, then some code
	(if (not skipchecktxt)
	    (if (not (string-match "^\\s-*\\([0-9]+\\)\\>" txt))
		(progn
		  (mstest-savestate)
		  (message "Command produced output : [%s]" txt)
		  (error "DEBUG: Expected ML dugger to produce a line number.  It did not."))
	      (let ((dbln (string-to-number (match-string 1 txt))))
		(when (not (= dbln line))
		  (message "DEBUG: Expected %s line %d, ended up at %s %d"
			   fileexp lineexp fname line)
		  (mstest-savestate)
		  (error "DEBUG test failed"))))
	  (message "Skipping ML output line check.")
	  )))

    (message "PASS")))

(defun mstest-debugger-breakpoint (command fileexplst lineexplst)
  "Test setting a breakpoint with COMMAND.
COMMAND can be a string (command to send) or a function to run (such as a gud command).
If a function, it will be called interactively.
It should create a breakpoint in file FILEEXP on line LINEEXP.
FILEEXP and LINEEXP can be lists.  If a list, then there should be breakpoints
set in the same order as specified."

  ;;(message "MSDB: %S -> %S %S" command fileexplst lineexplst)
  
  (when (stringp fileexplst)
    (setq fileexplst (list fileexplst))
    (setq lineexplst (list lineexplst)))

  ;; Run the command, then call dbstatus to see what is there.
  (cond
   ((and (functionp command) (commandp command))
    ;; We don't want to get command output.  gud commands don't leave
    ;; anything behind.
    ;; (mstest-get-command-output command t)
    (call-interactively command)
    (accept-process-output nil 1) ;; don't care what output was.
    )
   (t
    (mstest-get-command-output command))
   )

  ;; Get the breakpoint status to see what's there.
  (let ((txt (mstest-get-command-output "dbstatus")))

    (if (not fileexplst)
	;; this means no breakpoints.  Check TXT is empty
	(progn
	  (message "DEBUG: Expecting no breakpoints.")
	  (when (not (string= txt "\n"))
	    (message "DEBUG: Expected no breakpoints.  Found '%S'." txt)
	    (mstest-savestate)
	    (error "DEBUG test failed"))
	  (message "PASS"))

      ;; We are expecting breakpoints.  Make sure they all match.
      (while (and fileexplst lineexplst)

	(let ((fileexp (car fileexplst))
	      (lineexp (car lineexplst)))
    
	  (message "DEBUG: Expecting Breakpoint for %s line %s..." fileexp lineexp)
	  (when (not (string-match "Breakpoint for \\([.>a-zA-Z0-9]+\\) is on line \\([0-9]+\\)" txt))
	    (message "DEBUG: No breakpoints found.  dbstatus returned %S" txt)
	    (mstest-savestate)
	    (error "DEBUG test failed"))
	  (let ((file (match-string-no-properties 1 txt))
		(line (match-string-no-properties 2 txt)))
	    (when (not (string= file fileexp))
	      (message "DEBUG: Breakpoints in wrong place.  Expected %S, found %S" fileexp file)
	      (mstest-savestate)
	      (error "DEBUG test failed"))
	    (when (not (string= line lineexp))
	      (message "DEBUG: Breakpoints in wrong place.  Expected Line %S, found %S" lineexp line)
	      (mstest-savestate)
	      (error "DEBUG test failed"))
	  
	    (message "PASS")))

	(setq fileexplst (cdr fileexplst)
	      lineexplst (cdr lineexplst)))))
  )
  

      
;;; UTILITIES

(defun mstest-cap-context (&optional start)
  "Return a string that represents context around point at this time."
  (when (not start) (setq start (point)))
  
  (concat (buffer-substring-no-properties
	   (max (point-min) (- start 15))
	   start)
	  "<!>"
	  (buffer-substring-no-properties
	   start
	   (min (point-max) (+ start 15)))))

(defun mstest-get-command-output (command)
  "Wait for any pending output, and then return the text from lst command.
Searches for the text between the last prompt, and the previous prompt."
  ;; Send the command.
  (let* ((ctxt nil)
	 (start (with-current-buffer (matlab-shell-active-p)
		  (goto-char (point-max))
		  (setq ctxt (mstest-cap-context))
		  (point)))
	 )
;;    (when matlab-shell-io-testing
;;      (message "Start CTXT: [%s]" ctxt))
    (cond
     ;; For a string, send it ourselves
     ((stringp command)
      (matlab-shell-send-command command))
     ;; For a command, call it interactively
     ((and (functionp command) (commandp command))
      (call-interactively command))
     ;; For a function, call it, expect it to send some command.
     ((functionp command)
      (funcall command))
     ;; What is this?
     (t
      (error "Unkown command for mtest-get-command-output"))
     )
    
    (with-current-buffer (matlab-shell-active-p)
      ;; Wait.
      (let ((starttime (current-time))
	    (totaltime nil)
	    (matlab-shell-cco-testing t))
	(when matlab-shell-io-testing
	  (message "!!>"))
	(while (or (= (point) start)
		   (not (matlab-on-empty-prompt-p)))
	  (setq totaltime (float-time (time-subtract nil starttime)))
	  (when (> totaltime 10)
	    (let ((inhibit-field-text-motion t)
		  (ctxte (mstest-cap-context)))
	      (message "timeout: Start Ctxt: %S" ctxt)
	      (message "timeout: End Ctxt: %S" ctxte)
	      (mstest-savestate)
	      (error "Timeout waiting for prompt. (%d elapsed seconds)" totaltime)))
	  (redisplay)
	  ;; Some filters also call accept-process-output inside this one
	  ;; which causes it to not time out.
	  (accept-process-output nil .5)
	  (goto-char (point-max)))))
    (when matlab-shell-io-testing
      (message "!!<"))
    
    ;; Get the text from last prompt.
    (with-current-buffer (matlab-shell-active-p)
      (let ((inhibit-field-text-motion t)
	    (endpt nil)
	    (startpt nil))
	;; end pt is the end of the previous line from the last prompt.
	(goto-char (point-max))
	(beginning-of-line)
	(forward-char -1)
	(setq endpt (point))
	;; start pt is at the line after the start pt.
	(goto-char start)
	(beginning-of-line)
	(if (looking-at "^K?>>\\s-*")
	    (progn
	      ;; The command was inserted.  Skip it.
	      (end-of-line)
	      (forward-char 1))
	  ;; Any output text was deleted.  Don't move the curosr
	  ;; so we can grab the output.
	  nil)
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
