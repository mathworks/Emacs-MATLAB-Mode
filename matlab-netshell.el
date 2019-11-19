;;; matlab-netshell.el --- Control MATLAB from a network port.
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
;; Form a back-channale for Emacs to chat with a running MATLAB.
;; Allows you to edit for and with a MATLAB sessions, even if it is not in a
;; matlab-shell buffer.

(require 'matlab)

;;; Code:


(defvar matlab-netshell-listen-port 32475
  "Port used for the Emacs server listening for MATLAB connections.")

(defvar matlab-netshell-name "*MATLAB netshell*"
  "Name used for the Netshell server")

(defvar matlab-netshell-clients nil
  "List of clients created from the MATLAB netshell server.")

(defun matlab-netshell-server-start nil
    "Start the MATLAB netshell server."
    (interactive)
    (make-network-process :name matlab-netshell-name
			  :buffer "*MATLAB Netshell*" :family 'ipv4 :host 'local
			  :service matlab-netshell-listen-port
			  :filter #'matlab-netshell-filter
			  :sentinel #'matlab-netshell-sentinel
			  :server t)
    (setq matlab-netshell-clients nil)
    

    )

(defun matlab-netshell-server-stop nil
  "Stop the MATLAB Netshell server."
  (interactive)
  (dolist (C matlab-netshell-clients)
    (delete-process C))
  (setq matlab-netshell-clients nil)
  (delete-process matlab-netshell-name)
  )

(defun matlab-netshell-filter (proc string)
  "Filter used for MATLAB Netshell processes."
  ;; We recieved a command from PROC.  Interpret the command.
  (cond ((string-match "^init" string)
	 nil ; Yep, thanks for letting me know
	 (message "MATLAB connection initialized.")
	 )
	(t
	 (message "Unknown command from matlab: %S" string)
	 )))

(defun matlab-netshell-sentinel (proc msg)
  "Sentinel used for MATLAB Netshell processes.
Identify when a connection is lost, and close down services."
  (cond ((string-match "^open from " msg)
	 ;; New connection - set it up.
	 (setq matlab-netshell-clients (cons proc matlab-netshell-clients))
	 (message "MATLAB Has connected!"))
  
	((string= msg "connection broken by remote peer\n")
	 (setq matlab-netshell-clients (delq proc matlab-netshell-clients))
	 (message (format "MATLAB has dropped its connecction" proc)))

	(t
	 (message "Unhandle event."))))

(defun matlab-netshell-send (msg)
  "Send MSG to the active MATLAB shell connection."
  (interactive "sMsg: ")
  (let ((C (car matlab-netshell-clients)))
    (if C
	(process-send-string C (concat msg "\n"))
      (error "No MATLAB network connection to send to.")
      )))

;;(matlab-netshell-server-start)
;;(sleep-for 300)
;;(matlab-netshell-server-stop)


(provide 'matlab-netshell)

;;; matlab-netshell.el ends here
