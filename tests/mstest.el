;;; mstest.el --- MATLAB Shell test suite -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019-23 Eric Ludlam
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

(defvar mst-testfile-path) ;; quiet compiler

(require 'comint)

(require 'matlab)
(require 'matlab-shell)
(require 'matlab-topic)

;;; Code:
(defun mstest-run-all-tests ()
  "Run all the test cases in this test file."

  ;; When debugging the test suite, make sure we can access the debugger.
  (unless noninteractive
    (toggle-debug-on-error)
    (toggle-debug-on-quit))

  ;; Enable this to see how the input/output is interacting with the
  ;; test harness.
  (when (getenv "TESTDEBUG")
    (toggle-debug-on-error)
    (setq matlab-shell-io-testing t))

  (mstest-start)
  (mstest-capture)
  (mstest-completion)
  (mstest-error-parse)
  (mstest-debugger)
  )

;;; Startup Tests
(defun mstest-start ()
  "Test that we can start MATLAB, using the correct init file."
  (let ((process-connection-type nil))
    ;; Use PIPE to prevent MATLAB from identifying the actual width of the terminal.
    ;; For some reason, batch mode emacs will choose a pty width of 10, which breaks up
    ;; text and makes it hard to test against.
    ;; A PIPE won't echo, so turn off that feature.
    (setq matlab-shell-echoes nil)
    (matlab-shell))

  (let ((msb (matlab-shell-active-p)))
    (when (not msb) (user-error "MATLAB Shell command failed to create a shell buffer"))
    (accept-process-output nil 1)
    (with-current-buffer msb
      (when (not (get-buffer-process msb))
        (user-error "MATLAB Shell buffer failed to start process"))

      ;; Check full startup.
      (let* ((start (current-time))
             (elapsed nil))
        (while (not matlab-prompt-seen)
          (setq elapsed (float-time (time-subtract nil start)))
          (when (> elapsed 120)
            (user-error "MATLAB Shell took too long (120s) to produce a prompt"))
          (accept-process-output nil 1)
          (redisplay)
          (sit-for 1)))

      ;; During boot, we need to scrape the version number and release so we can load the
      ;; history file.  Make sure that happend.
      (if matlab-shell-running-matlab-version
          (message "VERSION SCRAPE: Successfully found MATLAB Version %s"
                   matlab-shell-running-matlab-version)
        (user-error "VERSION SCRAPE: Failed to find MATLAB Version number during startup"))
      (message "PASS")

      ;; Make sure MATLAB thinks we have enough columns to display the rest of our tests.
      ;; Without the right number of columns, future tests will fail.
      (let ((txt (mstest-get-command-output "disp(get(0,'CommandWindowSize'))")))
        (when (< (string-to-number txt) 80)
          (mstest-savestate)
          (user-error "COLUMNS TEST: Expecting a minimum of 80 columns, found %S" txt)))

      ;; Check that our path was added.
      (let ((txt (mstest-get-command-output "P=split(path,':');disp(P{1});"))
            (tbxdir (expand-file-name "toolbox" (file-name-directory
                                                 (locate-library "matlab")))))
        (message "PATH TEST: Expecting %S" tbxdir)
        (when (not (string= txt tbxdir))
          (message "PATH TEST: Found %S" txt)
          (mstest-savestate)
          (user-error "MATLAB Shell failed to initialize with matlab-emacs as first entry on path"))
        (message "PASS")
        )

      ;; Make sure we have a ROOT
      (message "MATLABROOT TEST")
      (let ((txt (matlab-shell-matlabroot)))
        (message "Computed MATLAB ROOT as : %S" txt)
        (when (or (not txt) (string= txt ""))
          (mstest-savestate)
          (user-error "Failed to find MATLABROOT"))
        )
      (message "PASS")

      ;; Turn off beeps, because during tests they are annoying.
      (message "BEEP OFF TEST: ")
      (let ((txt (mstest-get-command-output "beep off; stat = beep; disp(stat)")))
        (when (or (not txt) (not (string= txt "off")))
          (mstest-savestate)
          (user-error "Expected BEEPS to be off, but found %S" txt)))
      (message "PASS")

      ;; Make sure that 'WHICH' works correctly.
      (message "WHICH TEST: ls")
      (let ((txt (car (matlab-shell-which-fcn "ls")))
            (exp (expand-file-name "toolbox/matlab/io/filesystem/ls.m" (matlab-shell-matlabroot))))
        (if (string= txt exp)
            (message "PASS")
          (mstest-savestate)
          (message "Expected: [%S]" exp)
          (message "Found: [%S]" txt)
          (user-error "'WHICH TEST: ls' failed"))))))

;;; Command Sending Tests
(defun mstest-completion ()
  "Test emacsdocomplete and verifies result."
  (let ((msb (matlab-shell-active-p)))
    (when (not msb)
      (user-error "Test, mstest-completion, must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      ;; TEST completion fcn
      (message "COMPLETION TEST: emacs")
      (let* ((CLO
              (condition-case ERR
                  (matlab-shell-completion-list "emacs")
                (error
                 (mstest-savestate)
                 (user-error "%S" ERR))))
             (CL (cdr (nth 2 CLO)))
             (EXP '("emacs" "emacscd" "emacsdocomplete" "emacsinit" "emacsnetshell" "emacsrun" "emacsrunregion" "emacstipstring"))
             (cnt 1))
        (while (and CL EXP)
          (when (not (string= (car EXP) (car (car CL))))
            (user-error "Expected %S /= %S TS for %d completion"
                        (car EXP) (car (car CL)) cnt))
          (setq cnt (1+ cnt)
                CL (cdr CL)
                EXP (cdr EXP))))
      (message "PASS")

      )))

(defvar mstest-EVAL-TEST)

;; Command Capture tests
(defun mstest-capture ()
  "Test the Emacs capturing output functionality."
  (save-window-excursion
    (let ((msb (matlab-shell-active-p)))
      (when (not msb) (user-error "Test, mstest-completion, must run after mstest-start"))

      ;; We'll be testing how windows split, etc.
      (switch-to-buffer msb)
      (delete-other-windows)

      (goto-char (point-max))

      ;; TEST completion fcn
      (message "HELP TEST: ls")

      (let ((txt (mstest-get-command-output "help ls")))

        (when (not (string= txt "\n"))
          (mstest-savestate)
          (message "Leftover text: [%s]" txt)
          (user-error "There should be no leftover text from help commands"))

        (when (not (eq (current-buffer) msb))
          (mstest-savestate)
          (user-error "Help command changed current buffer"))

        (when (not (= (length (window-list)) 2))
          (mstest-savestate)
          (user-error "Help command failed to create a 2nd window"))

        (other-window 1)

        (when (not (string= (buffer-name) "*MATLAB Help: ls*"))
          (mstest-savestate)
          (user-error "Help command failed to create MATLAB Help buffer"))

        (goto-char (point-min))
        ;; Should see one of
        ;;    LS List folder contents (R2024a and prior)
        ;;    ls - List folder contents (R2024b)
        (when (not (looking-at "\\s-*ls\\s-+\\(?:\\-\\s-+\\)?list"))

          (mstest-savestate)
          (user-error "Help ls command failed to populate help with LS help"))

        (message "PASS"))

      (message "EVAL OUTPUT: testeeval")

      (let ((txt (mstest-get-command-output "testeeval")))

        (when (not (string= txt "\n"))
          (mstest-savestate)
          (message "Leftover text: [%s]" txt)
          (user-error "There should be no leftover text from testeeval command"))

        (when (or (not (stringp mstest-EVAL-TEST))
                  (not (string= mstest-EVAL-TEST "evaluate this")))
          (mstest-savestate)
          (user-error "Emacs failed to evaluate command sent from testeeval MATLAB command"))

        (message "PASS"))

      )))

;;; Error Parsing
(defun mstest-error-parse ()
  "Test various errors, and if we can parse them."

  (let ((msb (matlab-shell-active-p)))
    (when (not msb)
      (user-error "Test, mstest-error-parse, must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      (mstest-error-command-check "buggy err" "buggy.m" 21)

      (mstest-error-command-check "buggy cmderr" "ls.m" -1)

      (mstest-error-command-check "buggy warn" "buggy.m" 29)

      (mstest-error-command-check "eltest.utils.testme" "testme.m" 21)

      (mstest-error-command-check "eltest.utils.testme(true)" "testme.m" 28)

      (mstest-error-command-check "et=eltest.EmacsTest; et.throwerr()" "EmacsTest.m" 31)

      ;; This must occur after assignment into variable et.
      (mstest-error-command-check "et.throwprop()" "EmacsTest.m" 36)

      ;; TODO - this no longer works ...
      ;; (mstest-error-command-check "syntaxerr" "syntaxerr.m" 22)

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
         (user-error "%S" ERR))
        (t (user-error "%S" ERR)))

      (let* ((bfn (buffer-file-name))
             (bfnd (if bfn (file-name-nondirectory bfn)
                     (buffer-name)))
             (ln (count-lines (point-min) (min (1+ (point)) (point-max))))
             )

        (when (not (string= bfnd file))
          (mstest-savestate)
          (message "Err Text Generated:\n%S" txt)
          (user-error "Expected last error in %s.  Found myself in %s" file (buffer-name)))
        (when (and (> line  0) (not (= ln line)))
          (mstest-savestate)
          (message "Err Text Generated:\n\n%S\n" txt)
          (user-error "Expected last error in %s on line %d.  Found on line %d" file line ln))

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
         (user-error "Error not found"))
        (t (user-error "%S" ERR)))

      (let* ((bfn (buffer-file-name))
             (bfnd (if bfn (file-name-nondirectory bfn)
                     (buffer-name)))
             (ln (count-lines (point-min) (min (1+ (point)) (point-max))))
             )

        (when (not (string= bfnd file))
          (mstest-savestate)
          (user-error "Expected last error in %s.  Found myself in %s" file (buffer-name)))
        (when (and (> line  0) (not (= ln line)))
          (mstest-savestate)
          (user-error "Expected last error in %s on line %d.  Found on line %d" file line ln))

        ))

    (mstest-get-command-output "cd('tests')")

    (message "PASS")

    ))

(declare-function mlgud-break "mlgud")
(declare-function mlgud-next "mlgud")
(declare-function mlgud-cont "mlgud")

;;; Debugging: Breakpoints, stopping, visiting files
(defun mstest-debugger ()
  "Test debugging commands, and how MATLAB outputs state."
  (let ((msb (matlab-shell-active-p)))
    (when (not msb)
      (user-error "Test, mstest-debugger, must run after mstest-start"))

    (with-current-buffer msb
      (goto-char (point-max))

      ;; Basic create/clear cycle.
      (mstest-debugger-breakpoint "dbstop in dbtester" "dbtester" "18") ;; B = 1:.2:pi
      (mstest-debugger-breakpoint "dbclear all" nil nil))

    (save-excursion
      (find-file (expand-file-name "dbtester.m" mst-testfile-path))
      (forward-line 19) ;; goto-line 20: OUT = localfunc_1(B);

      ;; Use our mlgud functions
      (mstest-debugger-breakpoint #'mlgud-break "dbtester" "20")

      (mstest-debugger-breakpointlist '(("dbtester" . 20)))

      (mstest-debugger-navto "dbtester" "dbtester.m" "20")

      (mstest-debugger-navto #'mlgud-next "dbtester.m" 22) ;; OUT = OUT + 1;

      (mstest-debugger-navto #'mlgud-next "dbtester.m" 24) ;; end

      (mstest-debugger-navto #'mlgud-cont "dbtester.m" -1)

      (goto-char (point-min))
      (forward-line 54) ;; goto-line 55, A = B;

      ;; TODO following errors, need to fix
      ;; (mstest-debugger-breakpoint #'mlgud-break "dbtester" "20" "dbtester>localfunc_5" "55")
      )

    ;; TODO following errors, need to fix

    ;; (mstest-debugger-breakpointlist '(("dbtester>localfunc_5" . 55)
    ;;                                  ("dbtester" . 20)))

    ;; (find-file (expand-file-name "dbtester.m" mst-testfile-path))

    ;; (mstest-debugger-navto "dbtester" "dbtester.m" "20")

    ;; (mstest-debugger-navto #'mlgud-cont "dbtester.m" 55)

    ;; (mstest-debugger-navto #'mlgud-finish "dbtester.m" 51) ;; A = localfunc_5(B);

    ;; (mstest-debugger-stacklist '(("localfunc_4" . 51) ;; A = localfunc_5(B);
    ;;                             ("localfunc_3" . 47) ;; A = localfunc_4(B);
    ;;                             ("localfunc_2" . 43) ;; A = localfunc_3(B);
    ;;                             ("localfunc_1" . 38) ;; OUT = localfunc_2(OUT_TMP(1:2:end));
    ;;                             ("dbtester" . 20) ;; OUT = localfunc_1(B);
    ;;                             ))

    ;; (mstest-debugger-navto #'mlgud-finish "dbtester.m" 47) ;; A = localfunc_4(B);

    ;; (mstest-debugger-navto #'mlgud-cont "dbtester.m" -1)

    ))

(defun mstest-debugger-stacklist (expectedstack)
  "Run ebstack, check that the expected stack buffer has EXPECTEDSTACK."
  (message "DEBUG: Running ebstatus and checking for breakpoints buffer.")

  (let* ((txt (mstest-get-command-output "ebstack"))
         (buff (get-buffer "*MATLAB stack*"))
         (cnt 1)
         )

    (with-current-buffer buff

      (goto-char (point-min))
      (dolist (SK expectedstack)

        (unless (looking-at (format "\\s-*%d\\s-+\\(>>\\|--\\)\\s-+%s\\s-+%d" cnt (car SK) (cdr SK)))
          (mstest-savestate)
          (message "DEBUG: txt = %S" txt)
          (user-error "DEBUG: Stack buffer did not contain stack frame for %S, found [%s]"
                      SK (buffer-substring (point-at-bol) (point-at-eol))))
        (forward-line 1)
        (setq cnt (1+ cnt)))

      (message "PASS: Found %d matching breakpoints." (1- cnt))

      )))

(defun mstest-debugger-breakpointlist (expectedbreakpoints)
  "Run ebstatus and check breakpoit buffer appeared with EXPECTEDBREAKPOINTS."

  (message "DEBUG: Running ebstatus and checking for breakpoints buffer.")

  (let* ((txt (mstest-get-command-output "ebstatus"))
         (buff (get-buffer "*MATLAB breakpoints*"))
         (cnt 1)
         )

    (with-current-buffer buff

      (goto-char (point-min))
      (dolist (BP expectedbreakpoints)

        (unless (looking-at (format "\\s-*%d\\s-+-\\s-+%s\\s-+%d" cnt (car BP) (cdr BP)))
          (mstest-savestate)
          (message "DEBUG: txt=%S" txt)
          (user-error "DEBUG: Breakpoints buffer did not contain breakpoint for %S, found [%s]"
                      BP (buffer-substring (point-at-bol) (point-at-eol))))
        (forward-line 1)
        (setq cnt (1+ cnt)))

      (message "PASS: Found %d matching breakpoints." (1- cnt))

      )))

(defun mstest-debugger-navto (command fileexp lineexp &optional skipchecktxt)
  "Run some debugger nav command, and verify we ended up in FILEEXP at LINE.
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
            (user-error "DEBUG: Expected to have exited debug mode, but still on K>> prompt"))

        ;; else, we should have jumped to some src code.
        (when (or (not (string= fname fileexp))
                  (and (not (= line lineexp)) (< lineexp 0)))
          (message "DEBUG: Expected %s line %d, ended up at %s line %d"
                   fileexp lineexp fname line)
          (mstest-savestate)
          (user-error "DEBUG test failed"))


        ;; Text should start w/ a number, then some code
        (if (not skipchecktxt)
            (if (not (string-match "^\\s-*\\([0-9]+\\)\\>" txt))
                (progn
                  (mstest-savestate)
                  (message "Command produced output : [%s]" txt)
                  (user-error "DEBUG: Expected ML dugger to produce a line number.  It did not"))
              (let ((dbln (string-to-number (match-string 1 txt))))
                (when (not (= dbln line))
                  (message "DEBUG: Expected %s line %d, ended up at %s %d"
                           fileexp lineexp fname line)
                  (mstest-savestate)
                  (user-error "DEBUG test failed"))))
          (message "Skipping ML output line check.")
          )))

    (message "PASS")))

(defun mstest-debugger-breakpoint (command &rest inputs)
  "Test setting a breakpoint with COMMAND and INPUTS.
COMMAND can be a string (command to send) or a function to run
such as a gud command.If a function, it will be called interactively.
It should create a breakpoint in file FILEEXP on line LINEEXP.
FILEEXP and LINEEXP can be lists.  If a list, then there should be breakpoints
set in the same order as specified."

  ;;(message "MSDB: %S -> %S %S" command fileexplst lineexplst)
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

    (while inputs
      (let ((fileexplst (car inputs))
            (lineexplst (car (cdr inputs))))
        (setq inputs (cdr (cdr inputs)))

        (when (stringp fileexplst)
          (setq fileexplst (list fileexplst)))
        (when (stringp lineexplst)
          (setq lineexplst (list lineexplst)))


        (if (not fileexplst)
            ;; this means no breakpoints.  Check TXT is empty
            (progn
              (message "DEBUG: Expecting no breakpoints.")
              (when (not (string= txt "\n"))
                (message "DEBUG: Expected no breakpoints.  Found '%S'." txt)
                (mstest-savestate)
                (user-error "DEBUG test failed"))
              (message "PASS"))

          ;; We are expecting breakpoints.  Make sure they all match.
          (while (and fileexplst lineexplst)

            (let ((fileexp (car fileexplst))
                  (lineexp lineexplst))

              (message "DEBUG: Expecting Breakpoint for %s line %S..." fileexp lineexp)
              (when (not (string-match "Breakpoint for \\([.>a-zA-Z0-9_]+\\) \\(is\\|are\\) on line " txt))
                (message "DEBUG: No breakpoints found.  dbstatus returned %S" txt)
                (mstest-savestate)
                (user-error "DEBUG test failed"))
              (let ((file (match-string-no-properties 1 txt))
                    (txtend (match-end 0)))

                ;; Check file name
                (when (not (string= file fileexp))
                  (message "DEBUG: Breakpoints in wrong place.  Expected %S, found %S" fileexp file)
                  (mstest-savestate)
                  (user-error "DEBUG test failed"))

                ;; Loop over all the line numbers.
                (dolist (LN lineexp)
                  (unless (string-match "[0-9]+" txt txtend)
                    (message "DEBUG: Breakpoints text found.  No line number found for expected %S" LN)
                    (mstest-savestate)
                    (user-error "DEBUG test failed"))

                  (let ((line (match-string-no-properties 0 txt)))
                    (setq txtend (match-end 0))

                    (when (not (string= line LN))
                      (message "DEBUG: Breakpoints in wrong place.  Expected Line %S, found %S" LN line)
                      (mstest-savestate)
                      (user-error "DEBUG test failed"))))

                (message "PASS")))

            (setq fileexplst (cdr fileexplst)))))

      ;; Trim the string, but only if there is more to do.
      (when inputs
        (unless (string-match "^\\s-*$" txt)
          (message "DEBUG: Expected multiple breakpoint sets, but found no separator before exptected sets: %S" inputs)
          (mstest-savestate)
          (user-error "DEBUG test failed"))

        (setq txt (substring txt (match-end 0)))
        )

      )))



;;; UTILITIES

(defun mstest-cap-context (&optional start)
  "Return a string that represents context around point at START time."
  (when (not start) (setq start (point)))

  (concat (buffer-substring-no-properties
           (max (point-min) (- start 15))
           start)
          "<!>"
          (buffer-substring-no-properties
           start
           (min (point-max) (+ start 15)))))

(defun mstest-get-command-output (command)
  "Wait for any pending output, and then return the text from lst COMMAND.
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
      (user-error "Unknown command, %S, for mtest-get-command-output" command))
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
              (user-error "Timeout waiting for prompt (%d elapsed seconds)" totaltime)))
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
