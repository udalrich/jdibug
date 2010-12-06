;;; jdibug.el --- Fully elisp java debugger

;; Copyright (C) 2010 Troy Daniels

;; Author: Troy Daniels < udalrich.schermer@gmail.com >
;; Maintainer: Troy Daniels < udalrich.schermer@gmail.com >
;; Created: 2 April 2010
;; Keywords: lisp tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; http://code.google.com/p/jdibug/

;; This class provides the menu support in jdibug.

(require 'easymenu)
(require 'cc-mode)
(require 'jdibug-ui)
(require 'jdibug-run)

(defvar jdibug-menu-spec
  (list "JDIbug"

		[ "Step Over"
		  jdibug-step-over
		  :active jdibug-active-thread
		  :help "Step over the next method."]

		["Step Into"
		 jdibug-step-into
		 :active jdibug-active-thread
		 :help "Step into the next method."]

		["Step Out"
		 jdibug-step-out
		 :active jdibug-active-thread
		 :help "Step out of the current method." ]

		["Resume Thread"
		 jdibug-resume
		 :active jdibug-active-thread
		 :help "Resume execution of the current thread."]

		["Resume All Threads"
		 jdibug-resume-all
		 :active (jdibug-connected-p)
		 :help "Resume execution of the current thread."]

		"-"

		["Run and connect"
		 jdibug-run
		 :active (and (not (jdibug-connected-p))
					  jdibug-run-main-class)
		 :help "Start a new process and connect to it."]

		["Connect"
		 jdibug-connect
		 :active (not (jdibug-connected-p))
		 :help "Connect to a running process."]

		["Disconnect"
		 jdibug-disconnect
		 :active (jdibug-connected-p)
		 :help "Disconnect from all running processes."]

		["Kill JVM"
		 jdibug-exit-jvm
		 :active (jdibug-connected-p)
		 :help "Disconnect from all running processes."]
		"-"

		["Toggle Breakpoint"
		 jdibug-toggle-breakpoint
		 :help "Cycle breakpoint through set, disabled and not set."]

		["Add Watchpoint"
		 jdibug-add-watchpoint
		 :help "Evaluate an expression whenever the debuggee is suspended." ]
		"-"

		["Refresh all windows"
		jdibug-refresh-all-windows
		:help "Refresh the contents of all the JDIbug windows"
		:active (jdibug-connected-p) ]
		))

(defun jdibug-refresh-all-windows ()
  "Refresh all of the dedicated window used by the debugger to display output"
  (interactive)
  (jdibug-refresh-watchpoints-buffer-now)
  (jdibug-refresh-threads-buffer-now)
  (jdibug-refresh-locals-buffer-now)
  (jdibug-refresh-frames-buffer-now))

(easy-menu-define jdibug-menu java-mode-map "JDIbug Menu"
  jdibug-menu-spec)
;; (easy-menu-define jdibug-menu jde-mode-map "JDIbug Menu"
;;   jdibug-menu-spec)

(provide 'jdibug-menu)