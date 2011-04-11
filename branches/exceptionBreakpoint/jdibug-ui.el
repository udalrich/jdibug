;;; jdibug.el --- Fully elisp java debugger

;; Copyright (C) 2008 Phuah Yee Keat

;; Author: Phuah Yee Keat <ykphuah@gmail.com>
;; Maintainer: Troy Daniels <udalrich.schermer@gmail.com>
;; Created: 20 May 2008
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

;; If you are using JDEE, you only need to customize two variables
;;     jdibug-connect-hosts
;; which is a list of  hostname:port that you want to connect to.
;; Also ensure the
;;		jdibug-use-jde-source-paths
;; is set to t.  This causes jdibug to use the jde sourcepath.
;;
;;
;; After that, make sure you have loaded the JDEE project with the
;; property variables (namely jde-sourcepath)
;; and execute C-c C-c C-c in a JDEE buffer, wait for a while
;; until you see "Done" in the echo area.
;;
;; Then just go into any line in a java source code and do
;; C-c C-c C-b to break at that line.
;; Just run your application until you hit the breakpoint
;; after that stuffs are pretty much self explanatory
;;
;; The rest of the functions are almost similiar to that of jdb and jdebug

(defgroup jdibug nil
  "JDIbug options"
  :prefix "jdibug"
  :group 'languages)

(defcustom jdibug-connect-hosts '("localhost:6001")
  "Host:Port of the debuggees to connect to."
  :group 'jdibug
  :type '(repeat (string :tag "Host:Port")))

(defcustom jdibug-source-paths nil
  "Paths of the source codes. This will be ignored if
`jdibug-use-jde-source-paths' is t.  This must be a list like '(\"~/src\") not astring like \"~/src\""
  :group 'jdibug
  :type 'list)

(defcustom jdibug-refresh-delay 0.5
  "The delay in seconds between when a breakpoint/step was hit
and the frames/locals buffer are refreshed. Set to 0 for instant update.
Set to more for speeding up quick multi step when the frames/locals buffer
need not be refreshed."
  :group 'jdibug
  :type 'float)

(defcustom jdibug-locals-max-array-size 20
  "The maximum number of entries shown when expanding an array.
Expanding large numbers of arrays is slow.  Setting this to a
reasonably small number allows the arrays to be expanded quickly."
  :group 'jdibug
  :type 'integer)

(defcustom jdibug-locals-min-sub-array-size 10
  "The minium number of entries shown when expanding an
sub-array.  This prevents the subarrays from being exceedingly
short."
  :group 'jdibug
  :type 'integer)

(defcustom jdibug-use-jde-source-paths t
  "Set to t to use the jde-sourcepath as the source paths.
`jdibug-source-paths' will be ignored if this is set to t."
  :group 'jdibug
  :type 'boolean)

(defcustom jdibug-threads-buffer-name "*JDIbug-Threads*"
  "Name of the buffer to display the tree of threads."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-locals-buffer-name "*JDIbug-Locals*"
  "Name of the buffer to display the tree of local variables."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-frames-buffer-name "*JDIbug-Frames*"
  "Name of the buffer to display the list of active frames."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-breakpoints-buffer-name "*JDIbug-Breakpoints*"
  "Name of the buffer to display the list of breakpoints."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-watchpoints-buffer-name "*JDIbug-Watchpoints*"
  "Name of the buffer to display the list of watchpoints."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-break-on-class-regexp "^public\\s-+class"
  "The regexp that will be used to identify whether you want to break on all the methods in the class."
  :group 'jdibug
  :type 'string)

(defvar jdibug-connected-hook nil
  "Hook to run when we are connected to the debuggee.")

(defvar jdibug-breakpoint-hit-hook nil
  "Hook to run when a breakpoint is hit.")

(defvar jdibug-detached-hook nil
  "Hook to run when debuggee is detached, either from jdibug-disconnect or from vm-death event.")

(defvar jdibug-resumed-hook nil
  "Hook to run when debuggee is resumed.")

(require 'bindat)
(require 'elog)
(require 'jdwp)
(require 'jdi)
(require 'jdibug-expr)
(require 'jdibug-util)
(require 'tree-mode)
(require 'eieio)

(elog-make-logger jdibug)

(defvar jdibug-virtual-machines nil
  "list of jdi-virtual-machine")

(defvar jdibug-active-frame nil
  "the jdi-frame that
1. we are looking at in the locals browser
2. we have selected in the frames buffer
3. our resume, step commands will go to")

(defvar jdibug-active-thread nil
  "the thread that is suspended")

(defvar jdibug-others-suspended nil
  "When a breakpoint event happens and we are already on a breakpoint. The (thread location request-id) will be pushed into this list.
When the user resumes, we will switch to this thread and location.")

(defvar jdibug-current-line-overlay nil)

(defvar jdibug-threads-tree nil)

(defvar jdibug-threads-buffer nil)

(defvar jdibug-breakpoints-buffer nil)

(defvar jdibug-watchpoints nil
  "List of watchpoints")

(defvar jdibug-locals-buffer nil
  "buffer the shows a tree of the local variables")

(defvar jdibug-watchpoints-buffer nil
  "buffer the shows a tree of the local variables")

(defvar jdibug-locals-tree nil
  "the variable that hold the tree-widget of the locals buffer")

(defvar jdibug-frames-buffer nil)
(defvar jdibug-frames-tree nil)

(defvar jdibug-refresh-threads-buffer-timer nil)
(defvar jdibug-refresh-frames-buffer-timer nil)
(defvar jdibug-refresh-locals-buffer-timer nil)
(defvar jdibug-refresh-watchpoints-buffer-timer nil)

(defvar jdibug-breakpoints-mode-map nil
  "Local keymap for breakpoints buffer.")

(setq jdibug-breakpoints-mode-map (make-keymap))
(suppress-keymap jdibug-breakpoints-mode-map t)
(define-key jdibug-breakpoints-mode-map "e" 'jdibug-breakpoints-toggle)
(define-key jdibug-breakpoints-mode-map "d" 'jdibug-breakpoints-delete)
(define-key jdibug-breakpoints-mode-map "c" 'jdibug-breakpoints-add-condition)
(define-key jdibug-breakpoints-mode-map "\C-m" 'jdibug-breakpoints-goto)
(define-key jdibug-breakpoints-mode-map [mouse-1] 'jdibug-breakpoints-goto)

;;; Customized display and expanders:
(defvar jdibug-object-custom-set-strings nil
  "a list of (class setter) where

class is a string which hold the class name of the object to be matched.
The matching will be done using something like instance of.

setter is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-string of the jdi-value. If setter
is a string, it will be the method that will be invoked on the java object
and the value that is returned is shown.")

(setq jdibug-object-custom-set-strings
      '(("java.lang.Boolean"      "toString")
		("java.lang.Number"       "toString")
		("java.lang.StringBuffer" "toString")
		("java.util.Date"         "toString")
		("java.util.Collection"   jdibug-object-custom-set-string-with-size)
		("java.util.Map"          jdibug-object-custom-set-string-with-size)))

(defvar jdibug-object-custom-expanders nil
  "a list of (instance expander-func) where

instance is a string that is matched with jdi-object-instance-of-p with the
value

expander-func is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-values of the jdi-value.")

(setq jdibug-object-custom-expanders
      '(("java.util.Collection" jdibug-value-custom-expand-collection)
		("java.util.Map"        jdibug-value-custom-expand-map)))

(defclass jdibug-breakpoint nil
  ((status :initform unresolved :type symbol :accessor jdibug-breakpoint-status)
   (event-requests :initform nil :type list ;; :allow-nil-initform t (probably need this)
		   :accessor jdibug-breakpoint-event-requests
		   :documentation
		   "List jdi-event-request.  This is a list
because we might be installing 2 breakpoints for a single vm
because it have two class loaders, or we have two different vm!")

   (condition :type list :initform nil
	      :accessor jdibug-breakpoint-condition
	      :documentation
	      "The condition for the breakpoint.  If non-nil,
	      only step if the condition evaluates to true.  Any
	      other value (or an error) causes the breakpoing to
	      be ignored.")
   (condition-text :type string :initform ""
				   :accessor jdibug-breakpoint-condition-text
				   :documentation "Unparsed text of the condition.  Useful for display."))
  :abstract t
  :documentation "Underlying infrastructure for all types of conditions that can
suspend the JVM (even those that are not considered breakpoints
by JDWP).

Subclasses must define a static method `breakpoint-list' that
specifies the list that holds breakpoints after they are
created.")



;; Exception based breakpoint
(defclass jdibug-breakpoint-exception (jdibug-breakpoint)
  ;; Fields
  ((name :type string :initarg :name :accessor jdibug-breakpoint-name
			:documentation
			"Name of the exception in java format (e.g., java.lang.Exception)")
   (caught :type boolean :initarg :caught :accessor jdibug-breakpoint-caught)
   (uncaught :type boolean :initarg :uncaught :accessor jdibug-breakpoint-uncaught)
	(list :type list :initform nil :accessor breakpoint-list :allocation :class))
  :documentation "Breakpoint based on an exception being thrown")

(defclass jdibug-breakpoint-location (jdibug-breakpoint)
  ((source-file :type string :initarg :source-file :accessor jdibug-breakpoint-source-file)
   (line-number :type integer :initarg :line-number :accessor jdibug-breakpoint-line-number)
   (overlay :type (or overlay null) :initform nil ; :allow-nil-initform t
	    :accessor jdibug-breakpoint-overlay
	    :documentation
	    "Overlay within the file to indicate the status of the breakpoint.")
	(list :type list :initform nil :accessor breakpoint-list :allocation :class))

  :documentation "Traditional breakpoint based on file and line number.")

(defmethod pretty-print ((bp jdibug-breakpoint))
  "Convert to a string for developers to understand what is in the object"
  (concat (symbol-name (class-of bp)) " is "
			 (symbol-name (jdibug-breakpoint-status bp))
			 " with event requests of ("
			 (mapconcat (lambda (er)
							  (number-to-string (jdi-event-request-id er)))
							(jdibug-breakpoint-event-requests bp)
							", ")
			 ")"
			 (if (jdibug-breakpoint-condition bp)
				  (format "\n\twhen %s" (jdibug-breakpoint-condition-text bp))
				"")
			 "\n\t"
			 (pretty-print-extra bp)))

(defmethod pretty-print-extra ((bp jdibug-breakpoint-location))
  "Extra information when pretty-printing a breakpoint"
  (concat "at line " (number-to-string (jdibug-breakpoint-line-number bp))
			 " in " (jdibug-breakpoint-source-file bp)))

(defmethod pretty-print-extra ((bp jdibug-breakpoint-exception))
  "Extra information when pretty-printing a breakpoint"
  (concat "when " (jdibug-breakpoint-name bp)
			 " is thrown"
			 (if (jdibug-breakpoint-caught bp)
				  (if (jdibug-breakpoint-uncaught bp)
						;; always caught
						""
					 " and caught")
				(if (jdibug-breakpoint-uncaught bp)
					 " but not caught"
				  "but neither caught nor not caught?!?"))))

(defun jdibug-list-breakpoints (&rest ignore)
  "List all of the current breakpoints and provide a reasonably
legible description of the breakpoint.  The resulting buffer has
static contents, but `revert-buffer' in the resulting buffer will
update the list of breakpoints based on the current breakpoints."
  (interactive "d")
  (switch-to-buffer "*JDIbug Breakpoint List*")
  (erase-buffer)
  (mapc (lambda (bp)
			 (insert (pretty-print bp))
			 (newline))
		  (jdibug-all-breakpoints))
  (setq revert-buffer-function 'jdibug-list-breakpoints))

(defmethod cleanup-on-disconnect ((bp jdibug-breakpoint))
  "Do any cleanup related to this BP when the jvm disconnects"
  (setf (jdibug-breakpoint-status bp) 'unresolved))

(defmethod cleanup-on-disconnect ((bp jdibug-breakpoint-location))
  "Do any cleanup related to this BP when the jvm disconnects"
  (call-next-method)
  (if (jdibug-breakpoint-overlay bp)
		(delete-overlay (jdibug-breakpoint-overlay bp))))



(defun jdibug-all-breakpoints nil
  "Return all instances of jdibug breakpoints"
  (loop for class in (class-children jdibug-breakpoint)
		  append (breakpoint-list class)))

(defstruct jdibug-watchpoint
  ;; Basic expression to evaluate
  expression

  ;; Parsed form of the expression
  parse)

(defmethod short-source-file ((bp jdibug-breakpoint-location))
  (let ((buf (jdibug-breakpoint-source-file bp)))
	(setf buf (replace-regexp-in-string ".*/" "" buf))
	buf))

(defvar jdibug-current-message "")

(defun jdibug-message (message &optional append)
  (if append
	  (setf jdibug-current-message (concat jdibug-current-message message))
	(setf jdibug-current-message message))
  (jdibug-info jdibug-current-message)
  (message jdibug-current-message))

(add-hook 'jdi-breakpoint-hooks 'jdibug-handle-breakpoint)
;; Currently, no difference in breakpoint and exception handling,
;; since exception events occur when we want to break on the exception
;; being thrown
(add-hook 'jdi-exception-hooks 'jdibug-handle-breakpoint)
(add-hook 'jdi-step-hooks 'jdibug-handle-step)
(add-hook 'jdi-detached-hooks 'jdibug-handle-detach)
(add-hook 'jdi-class-prepare-hooks 'jdibug-handle-class-prepare)
(add-hook 'jdi-thread-start-hooks 'jdibug-handle-thread-start)
(add-hook 'jdi-thread-end-hooks 'jdibug-handle-thread-end)

(defun jdibug-connect ()
  "Create a new debugger and connect to a running process.  See
`jdibug-connect-hosts' for where it will look for the debuggee
processes"
  (interactive)

  ;; (unless (byte-code-function-p (symbol-function 'jdibug-connect))
  ;; 	(error "You must byte-compile jdibug before using it."))

  (jdibug-debug "jdibug-connect")

  (if (jdibug-connected-p)
	  (message "JDIbug already connected")

	(jdwp-uninterruptibly
	  (jdibug-message "JDIbug connecting... ")

	  (jdibug-debug "setting jdibug-active-thread to nil in jdibug-connect")

	  (setq jdibug-threads-buffer     (get-buffer-create jdibug-threads-buffer-name)
			jdibug-locals-buffer      (get-buffer-create jdibug-locals-buffer-name)
			jdibug-watchpoints-buffer (get-buffer-create jdibug-watchpoints-buffer-name)
			jdibug-frames-buffer      (get-buffer-create jdibug-frames-buffer-name)
			jdibug-breakpoints-buffer (get-buffer-create jdibug-breakpoints-buffer-name)

			jdibug-active-frame       nil
			jdibug-active-thread      nil
			jdibug-others-suspended    nil)

	  (with-current-buffer jdibug-breakpoints-buffer
		(use-local-map jdibug-breakpoints-mode-map)
		(toggle-read-only 1))

	  (setq jdibug-virtual-machines
			(remove nil
					(mapcar (lambda (connect-host-and-port)
							  (let* ((host (nth 0 (split-string connect-host-and-port ":")))
									 (port (string-to-number (nth 1 (split-string connect-host-and-port ":"))))
									 (jdwp (make-jdwp))
									 (vm (make-jdi-virtual-machine :host host :port port :jdwp jdwp)))
								(jdibug-message (format "%s:%s" host port) t)
								(condition-case err
									(progn
									  (jdi-virtual-machine-connect vm)
									  (jdibug-message "(connected)" t)
									  vm)
								  (file-error (jdibug-message "(failed)" t) nil))))
							jdibug-connect-hosts)))

	  (when (find-if 'identity jdibug-virtual-machines)
		 (mapc 'set-breakpoint (jdibug-all-breakpoints))

		 (run-hooks 'jdibug-connected-hook)
		 (jdibug-refresh-frames-buffer)
		 (jdibug-refresh-threads-buffer)))))

(defun jdibug-disconnect ()
  (interactive)
  (jdwp-uninterruptibly
	(jdibug-trace "jdibug-disconnect")
	(jdibug-cleanup-on-disconnect)

	(jdibug-message "JDIbug disconnecting... ")
	(mapc (lambda (vm)
			(jdibug-message (format "%s:%s"
									(jdi-virtual-machine-host vm)
									(jdi-virtual-machine-port vm)) t)
			(jdi-virtual-machine-disconnect vm)
			(jdibug-message "(disconnected) " t))
		  jdibug-virtual-machines)
	(setq jdibug-virtual-machines nil)
	(run-hooks 'jdibug-detached-hook)))

(defun jdibug-cleanup-on-disconnect nil
  (if jdibug-current-line-overlay
	  (delete-overlay jdibug-current-line-overlay))
  (mapc (lambda (bp)
			 (cleanup-on-disconnect bp))
		(jdibug-all-breakpoints))

  (and (timerp jdibug-refresh-locals-buffer-timer)
	   (cancel-timer jdibug-refresh-locals-buffer-timer))

  (and (timerp jdibug-refresh-watchpoints-buffer-timer)
	   (cancel-timer jdibug-refresh-watchpoints-buffer-timer))

  (and (timerp jdibug-refresh-frames-buffer-timer)
	   (cancel-timer jdibug-refresh-frames-buffer-timer))

  (and (timerp jdibug-refresh-threads-buffer-timer)
	   (cancel-timer jdibug-refresh-threads-buffer-timer))

  (mapc (lambda (buf)
			 (when buf (kill-buffer buf)))
		  (list jdibug-locals-buffer jdibug-watchpoints-buffer
				  jdibug-frames-buffer jdibug-threads-buffer))

  (setq jdibug-locals-tree    nil
		  jdibug-locals-buffer  nil

		  jdibug-frames-tree    nil
		  jdibug-frames-buffer  nil

		  jdibug-threads-buffer nil
		  jdibug-threads-tree   nil

		  jdibug-watchpoints-buffer nil

		  jdibug-active-thread  nil
		  jdibug-active-frame   nil))

(defun jdibug-exit-jvm ()
  "End the debugging session and kill all attached JVMs."
  (interactive)
  (jdwp-uninterruptibly
	(jdibug-trace "jdibug-exit-jvm")
	(jdibug-cleanup-on-disconnect)

	(jdibug-message "JDIbug disconnecting and killing JVM... ")
	(mapc (lambda (vm)
			(jdibug-message (format "%s:%s"
									(jdi-virtual-machine-host vm)
									(jdi-virtual-machine-port vm)) t)
			(jdi-virtual-machine-exit vm -1)
			(jdibug-message " (killed) " t))
		  jdibug-virtual-machines)
	(setq jdibug-virtual-machines nil)
	(run-hooks 'jdibug-detached-hook)))

(defun jdibug-have-class-source-p (class)
  (jdibug-debug "jdibug-have-class-source-p")
  (let ((signature (jdi-class-get-signature class)))
	(file-exists-p (jdibug-class-signature-to-source-file signature))))

(defun jdibug-find-buffer (file-name)
  "Return the buffer that is viewing this file."
  (catch 'found
	(dolist (buf (buffer-list))
	  (if (equal file-name (buffer-file-name buf))
		  (throw 'found buf)))))

(defun jdibug-raise-window (window)
  "Raise the frame that is showing this window."
  (jdibug-debug "jdibug-raise-window")
  (let ((frame (window-frame window)))
	(make-frame-visible frame)
	(raise-frame frame)
	(select-frame frame)
	(select-window window)))

(defun jdibug-show-file-and-line-number (file-name line-number &optional highlight)
  "Show the buffer containing the file, or open the file if there are no open buffer for this file.
And position the point at the line number."
  (jdibug-debug "jdibug-show-file-and-line-number:%s:%s" file-name line-number)
  (let* ((buffer-name (jdibug-find-buffer file-name))
		 (win (if buffer-name (get-buffer-window buffer-name t) nil))
		 (frame (if win (window-frame win) nil)))
	(jdibug-debug "buffer-name=%s win=%s frame=%s" buffer-name win frame)
	(if win
		;; file is already open in a buffer, just show it
		(progn
		  (jdibug-raise-window win)
		  (goto-char (point-min))
		  (forward-line (1- line-number))
		  (if highlight
			  (with-current-buffer buffer-name
				(jdibug-highlight-current-line line-number))))
	  ;; file is not open, try to find it
	  (if (file-exists-p file-name)
		  (if line-number
			  (let ((win (catch 'done
						   (walk-windows (lambda (win)
										   (if (not (window-dedicated-p win))
											   (throw 'done win)))))))
				(select-window win)
				(find-file file-name)
				(jdibug-raise-window win)
				(goto-char (point-min))
				(forward-line (1- line-number))
				(if highlight
					(with-current-buffer (window-buffer win)
					  (jdibug-highlight-current-line line-number))))
			(message "JDIbug file %s does not have line number information" file-name))
		(message "JDIbug file %s not in source path" file-name)))))

(defun jdibug-goto-location (location)
  (jdibug-debug "jdibug-goto-location")
  (let ((signature (jdi-class-get-signature (jdi-location-class location))))
	(let ((line-number (jdi-location-get-line-number location)))
	  (jdibug-show-file-and-line-number (jdibug-class-signature-to-source-file signature)
										line-number
										t))))


(defun jdibug-handle-breakpoint (thread location request-id)
  (jdibug-debug "jdibug-handle-breakpoint: active-frames is %s"
				(and jdibug-active-frame (jdi-frame-id jdibug-active-frame)))

  ;; Check if this is a conditional breakpoint with an unsatisfied condition
  (if (jdibug-breakpoint-condition-p thread location request-id)
	  (progn
		(if (null jdibug-active-frame)
			(progn
			  (jdibug-goto-location location)
			  (setq jdibug-active-frame (car (jdi-thread-get-frames thread)))
			  (jdibug-refresh-locals-buffer)
			  (jdibug-refresh-watchpoints-buffer)
			  ;; 	(jdibug-debug "setting jdibug-active-thread to %s in jdibug-handle-breakpoint"
			  ;; 			   thread)

			  (setq jdibug-active-thread thread))
		  (setq jdibug-others-suspended (append jdibug-others-suspended (list `(,thread ,location ,request-id)))))

		(jdibug-refresh-frames-buffer)
		(jdibug-refresh-threads-buffer)

		(run-hooks 'jdibug-breakpoint-hit-hook))
	;; Condition not satisfied, so continue
	(jdibug-trace "Condition not satisfied so resuming thread")
	(jdi-thread-resume thread)))



(defun jdibug-breakpoint-condition-p (thread location request-id)
  "Check if the breakpoint corresponding to REQUEST-ID that has
been hit in THREAD at LOCATION either has no condition or that
the condition is satisfied."
  (let ((bp (find-if (lambda (bp)
							  (find-if (lambda (er)
											 (= (jdi-event-request-id er) request-id))
										  (jdibug-breakpoint-event-requests bp)))
							(jdibug-all-breakpoints)))
		  condition)
    (if (not bp)
		  (progn
			 (jdibug-error "Unable to find breakpoint with request id %d" request-id)
			 nil)
		(setq condition (jdibug-breakpoint-condition bp))
		(if condition
			 (let* ((frame (car (jdi-thread-get-frames thread)))
					  (jdwp (jdi-virtual-machine-jdwp (jdi-frame-virtual-machine frame)))
					  (result (catch jdibug-expr-bad-eval (jdibug-expr-eval-expr jdwp condition frame))))
				;; We pass only if the result is a boolean that is true
				(when (jdi-value-p result)
				  (if (= (jdi-value-type result) jdwp-tag-boolean)
						(not (equal (jdi-primitive-value-value result) 0))
					 (jdibug-warn "Breakpoint condition is not a boolean but a %s" (jdwp-type-name (jdi-value-type result)))
					 nil)))
		  t))))

(defun jdibug-handle-step (thread location)
  (jdibug-debug "jdibug-handle-step")

  ;; 	(jdibug-debug "setting jdibug-active-thread to %s in jdibug-handle-step"
  ;; 			   thread)

  (setq jdibug-active-thread thread)

  (jdibug-goto-location location)

  (jdibug-refresh-frames-buffer)
  (jdibug-refresh-threads-buffer)

  (unless jdibug-active-frame
	(setq jdibug-active-frame (car (jdi-thread-get-frames jdibug-active-thread)))
	(jdibug-refresh-locals-buffer)
	(jdibug-refresh-watchpoints-buffer)))

(defun jdibug-handle-change-frame (frame)
  (jdibug-debug "jdibug-handle-change-frame")

  (jdibug-goto-location (jdi-frame-location frame))
  (jdibug-refresh-frames-buffer)
  (jdibug-refresh-watchpoints-buffer)
  (jdibug-refresh-locals-buffer))

(defun jdibug-handle-class-prepare (class thread)
  (jdibug-debug "jdibug-handle-class-prepare")
  ;; TODO: refactor this to call an overridden method?
  (dolist (bp (breakpoint-list jdibug-breakpoint-location))
	 (jdibug-handle-class-prepare-breakpoint class thread bp))
  (dolist (exc (breakpoint-list jdibug-breakpoint-exception))
	(jdibug-handle-class-prepare-exception class thread exc))

  (jdibug-debug "jdibug-handle-class-prepare finishing, resuming thread, thread status is %s" (jdi-thread-get-status thread))
  (jdi-thread-resume thread))

(defun jdibug-handle-class-prepare-exception (class thread exc)
  "Check loaded class against exception breaks."
  (jdibug-debug "jdibug-handle-class-prepare-exception: class=%s, exc=%s" (jdi-class-get-signature class) (jdibug-breakpoint-name exc))

  (when (jdibug-signature-matches-name-with-wildcards
			(jdi-class-get-signature class)
			(jdibug-breakpoint-name exc))
	(jdibug-debug "Found exception matching %s" (jdibug-breakpoint-name exc))
	(if (jdi-class-instance-of-p
		  class
		  (jdi-class-name-to-class-signature "java.lang.Throwable"))
		 (progn
			(let* ((vm (jdi-mirror-virtual-machine class)))
			  (set-breakpoint exc (list vm))
			  (jdibug-refresh-breakpoints-buffer)))
	  (jdibug-warn "%s is not a subclass of Throwable.  Not setting break when thrown."
					 (jdi-jni-to-print (jdi-class-get-signature class))))))

(defun jdibug-signature-matches-name-with-wildcards (signature name)
  "Checks if the JNI SIGNATURE matches NAME, which might contain * as a wildcard"
  (jdibug-debug "jdibug-signature-matches-name-with-wildcards %s %s"
					 signature name)
  (let ((sig-as-name (car (jdi-jni-to-print signature)))
		  (name-regexp (mapconcat 'regexp-quote
										  (split-string name "*")
										  ".*")))
	 (jdibug-debug "Checking if %s matches %s" sig-as-name name-regexp)
	 (string-match (concat "^" name-regexp "$") sig-as-name)))




(defun jdibug-handle-class-prepare-breakpoint (class thread bp)
  "Check loaded class against breakpoints"
  (when (equal (jdibug-breakpoint-source-file bp)
					(jdibug-class-signature-to-source-file (jdi-class-get-signature class)))
	(jdibug-debug "Found breakpoint in %s" (jdibug-breakpoint-source-file bp))
	(let (found)
	  (dolist (location (jdi-class-get-locations-of-line class (jdibug-breakpoint-line-number bp)))
		(setq found t)
		(let ((vm (jdi-mirror-virtual-machine class)))
		  (set-breakpoint bp (list vm) class)
		  (jdibug-refresh-breakpoints-buffer))))))


(defun jdibug-handle-detach (vm)
  (if jdibug-current-line-overlay
	  (delete-overlay jdibug-current-line-overlay))
  (mapc (lambda (bp) (handle-detach bp))
		  (jdibug-all-breakpoints))
  (message "JDIbug %s:%s vm detached" (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
  (unless (jdibug-connected-p)
	(run-hooks 'jdibug-detached-hook)))

(defmethod handle-detach ((bp jdibug-breakpoint))
  "Handle detaching from a running JVM.  The default implementation does nothing."
  nil)

(defmethod handle-detach ((bp jdibug-breakpoint-location))
  "Clear the overlay if it exists"
  (if (jdibug-breakpoint-overlay bp)
		(delete-overlay (jdibug-breakpoint-overlay bp))))

(defun jdibug-handle-thread-start (thread)
  ;; We don't request that the thread is suspended in the event
  ;;   request, so don't resume it.  Doing so can cause problems with other
  ;;   events, like class-prepare, which do suspend it and cause the
  ;;   thread to be resumed before a breakpoint can be set.

  ;; (jdi-thread-resume thread)
  (jdibug-refresh-frames-buffer)
  (jdibug-refresh-threads-buffer))

(defun jdibug-handle-thread-end (thread)
  (jdibug-refresh-frames-buffer)
  (jdibug-refresh-threads-buffer))

(defun jdibug-highlight-current-line (line-number)
  (jdibug-debug "jdibug-highlight-current-line:%d:current-buffer=%s" line-number (current-buffer))
  (goto-char (point-min))
  (forward-line (1- line-number))
  (beginning-of-line)
  (if jdibug-current-line-overlay
      (delete-overlay jdibug-current-line-overlay))
  (setq jdibug-current-line-overlay (make-overlay (point) (1+ (line-end-position))))
  (overlay-put jdibug-current-line-overlay 'face 'jdibug-current-line)
  (overlay-put jdibug-current-line-overlay 'priority 10))

;;(custom-set-faces '(jdibug-current-line ((t (:foreground "navajo white" :background "DodgerBlue"))) t))
(defface jdibug-current-line
  `((t (:foreground "navajo white" :background "DodgerBlue")))
  "Face for current executing line"
  :group 'jdibug)

;;(custom-set-faces '(jdibug-breakpoint-enabled ((t (:foreground "navajo white" :background "indian red"))) t))
(defface jdibug-breakpoint-enabled
  `((t (:foreground "navajo white" :background "indian red")))
  "Face for enabled breakpoint"
  :group 'jdibug)

;;(custom-set-faces '(jdibug-breakpoint-disabled ((t (:foreground "navajo white" :background "SpringGreen4"))) t))
(defface jdibug-breakpoint-disabled
  `((t (:foreground "navajo white" :background "SpringGreen4")))
  "Face for disabled breakpoint"
  :group 'jdibug)

;;(custom-set-faces '(jdibug-breakpoint-unresolved ((t (:foreground "navajo white" :background "gold4"))) t))
(defface jdibug-breakpoint-unresolved
  `((t (:foreground "navajo white" :background "gold4")))
  "Face for unresolved breakpoint"
  :group 'jdibug)

(defface jdibug-current-frame
  `((t (:foreground "navajo white" :background "DodgerBlue")))
  "Face for current frame"
  :group 'jdibug)

(defun jdibug-expand-method-node (tree)
  (jdwp-uninterruptibly
	(jdibug-debug "jdibug-expand-method-node")
	(let* ((value (widget-get tree :jdi-value))
		   (method (widget-get tree :jdi-method))
		   (result-value (jdi-value-invoke-method value jdibug-active-thread method nil nil))
		   (result-string (jdibug-value-get-string result-value)))
	  (jdibug-debug "running method %s returned %s" (jdi-method-name method) result-string)
	  (list (jdibug-make-tree-from-value "result" result-value result-string)))))

(defun jdibug-make-method-node (value method)
  (let ((display (format "%s: %s" (jdi-method-name method) (jdi-method-signature method))))
	(if (string= "()" (substring (jdi-method-signature method) 0 2))
		`(tree-widget
		  :node (push-button
				 :tag ,display
				 :format "%[%t%]\n")
		  :open nil
		  :jdi-value  ,value
		  :jdi-method ,method
		  :dynargs jdibug-expand-method-node
		  :expander jdibug-expand-method-node)
	  `(item
		:value
		,display))))

(defun jdibug-expand-methods (tree)
  (jdwp-uninterruptibly
	(let ((value (widget-get tree :jdi-value)))
	  (let ((class (jdi-value-get-reference-type value)))
		(let ((methods (jdi-class-get-all-methods class)))
		  (mapcar (lambda (method)
					(jdibug-make-method-node value method))
				  (sort (remove-duplicates methods
										   :test (lambda (obj1 obj2)
												   (and (equal (jdi-method-name obj1)
															   (jdi-method-name obj2))
														(equal (jdi-method-signature obj1)
															   (jdi-method-signature obj2))))
										   :from-end t)
						(lambda (obj1 obj2)
						  (string< (jdi-method-name obj1)
								   (jdi-method-name obj2))))))))))

(defun jdibug-make-methods-node (value)
  `(tree-widget
	:node (push-button
		   :tag "methods"
		   :format "%[%t%]\n")
	:open nil
	:jdi-value ,value
	:dynargs jdibug-expand-methods
	:expander jdibug-expand-methods))

(defun jdibug-expand-locals (tree)
  (jdibug-debug "jdibug-expand-locals")
  (let ((values (widget-get tree :jdi-values))
		(variables (widget-get tree :jdi-variables)))
	(let ((strings (mapcar 'jdibug-value-get-string values)))
	  (loop for variable in variables
			for value in values
			for string in strings
			collect (jdibug-make-tree-from-variable-value variable value string)))))

(defun jdibug-make-locals-tree (variables values)
  `(tree-widget
	:node (push-button
		   :tag "Locals"
		   :format "%[%t%]\n")
	:open t
	:jdi-variables ,variables
	:jdi-values ,values
	:dynargs jdibug-expand-locals
	:expander jdibug-expand-locals))

(defun jdibug-tree-mode-find-child-path (tree path)
  (when path
	(if (equal (widget-get (tree-widget-node tree) :tag) (car path))
		path
	  (find-if (lambda (path)
				 (jdibug-tree-mode-find-child-path tree path))
			   (cdr path)))))

(defun jdibug-tree-mode-reflesh-tree (tree)
  "Redraw TREE.
If tree has attribute :dynargs, generate new :args from that function.
Otherwise use :old-args which saved by `tree-mode-backup-args'."
  (eval `(jdwp-uninterruptibly (jdibug-tree-mode-reflesh-tree-1 ,tree))))

(defun jdibug-tree-mode-reflesh-tree-1 (tree)
  (let ((path (or (jdibug-tree-mode-find-child-path tree (widget-get tree :jdibug-opened-path))
				  (tree-mode-opened-tree tree))))
	(jdibug-debug "jdibug-tree-mode-reflesh-tree:tag=%s:path=%s" (widget-get (tree-widget-node tree) :tag) path)
	(jdibug-debug "jdibug-tree-mode-reflesh-tree:opened-path=%s" (tree-mode-opened-tree tree))
    (if (widget-get tree :dynargs)
        (widget-put tree :args nil)
      (if (widget-get tree :old-args)
          (widget-put tree :args (widget-get tree :old-args))))
	(jdibug-debug "before widget-value-set")
    (widget-value-set tree (widget-value tree))
	(jdibug-debug "before tree-mode-open-tree")
    (tree-mode-open-tree tree path)))

(defun jdibug-value-expander (tree)
  (jdibug-make-expansion-list 'jdibug-value-expander-1 'jdibug-tree-mode-reflesh-tree tree))

(defun jdibug-value-expander-1 (tree)
  (let ((value (widget-get tree :jdi-value)))
	(jdibug-debug "jdibug-value-expander:type=%s" (jdi-value-type value))

	;; call the custom expander here, so that the custom expander can
	;; make a ArrayList look like an array by changing the value-type
	(let* ((expander (find-if (lambda (custom)
								(jdi-object-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
							  jdibug-object-custom-expanders))
		   (expander2 (if expander (cadr expander))))
	  (cond (expander2
			 (funcall expander2 value))
			((= (jdi-value-type value) jdwp-tag-object)
			 (jdibug-value-expander-object value))
			((= (jdi-value-type value) jdwp-tag-array)
			 (jdibug-value-expander-array value))))))

(defun jdibug-value-expander-object (value)
  (let* ((reference-type (jdi-value-get-reference-type value))
		 (fields (sort (copy-sequence (jdi-reference-type-get-all-fields reference-type)) 'jdibug-field-sorter))
		 (values (jdi-value-get-values value fields)))
	(jdibug-debug "jdibug-value-expander got %s values" (length values))
	(append (loop for v in values
				  for f in fields
				  collect (jdibug-make-tree-from-field-value f v))
			(list (jdibug-make-methods-node value)))))

(defun jdibug-value-expander-array (value &optional first last)
  (let* ((first (or first 0))
		 (last (or last (jdi-array-get-array-length value)))
		 (num-display (- last first)))
    (if (<= num-display jdibug-locals-max-array-size)
		;; Short array, so display all of the values
		(loop with values = (jdi-array-get-values value first last)
			  with strings = (mapcar 'jdibug-value-get-string values)
			  for v in values
			  for s in strings
			  for i from first by 1
			  collect (jdibug-make-tree-from-value (format "[%s]" i) v s))
	  ;; Long array, so create pseudo nodes for subarrays
	  (loop with step = (jdibug-locals-step-size num-display)
			for start from first below last by step
			for end = (min last (+ start step))
			  collect (jdibug-make-array-subtree value start end)))))

(defun jdibug-locals-step-size (num-display)
  "Determine the number of subnodes to create when NUM-DISPLAY entries are present.  This respects `jdibug-locals-max-array-size' and `jdibug-locals-min-sub-array-size' and attempts to find a round value to return."
  (let ((step (max (ceiling num-display jdibug-locals-max-array-size) jdibug-locals-min-sub-array-size))
		(scale 1))
	(if (> step jdibug-locals-min-sub-array-size)
		;; Round up to the nearest power of 10.
		(progn
		  ;; First, find the largest power of 10 less than the current step
		  (while (< (* scale 10) step)
			(setq scale (* scale 10)))
		  ;; Now round up to that value
		  (* scale (ceiling step scale)))
	  ;; We are at the minimum value (which the user can set) so just use that
	  step)))




(defun jdibug-make-map-array-subtree (value start end label expander)
  "Create a pseudo node representing a subset of the array VALUE
from START (inclusive) to END (exclusive).  Use LABEL to format
the text for the button.  It must be a format string that takes
the two parameters (the start and end indices).  EXPANDER is used
to expand the node."
  (let ((display (format label start (1- end))))
	`(tree-widget
	  :node (push-button
		 :tag ,display
		 :format "%[%t%]\n")
	  :open nil
	  :args nil
	  :jdi-value ,value
	  :jdi-start ,start
	  :jdi-end ,end
	  :expander ,expander
	  :dynargs ,expander)))

(defun jdibug-make-array-subtree (value start end)
  "Create a pseudo node representing a subset of the array VALUE
from START (inclusive) to END (exclusive)"
  (jdibug-make-map-array-subtree value start end "[%d-%d]" 'jdibug-array-subtree-expander))

(defun jdibug-make-map-subtree (value start end)
  "Create a pseudo node representing a subset of the array VALUE
from START (inclusive) to END (exclusive)"
  (jdibug-make-map-array-subtree value start end "[kv%d-%d]" 'jdibug-map-subtree-expander))

(defun jdibug-array-subtree-expander (tree)
  (jdwp-uninterruptibly
	(jdibug-debug "jdibug-array-subtree-expander")

	;; Expand the elements in the sub-array
	(let ((value (widget-get tree :jdi-value))
		  (start (widget-get tree :jdi-start))
		  (end (widget-get tree :jdi-end)))
	  (jdibug-value-expander-array value start end))))

(defun jdibug-map-subtree-expander (tree)
  (jdwp-uninterruptibly
	(jdibug-debug "jdibug-map-subtree-expander")

	;; Expand the elements in the sub-array
	(let ((value (widget-get tree :jdi-value))
		  (start (widget-get tree :jdi-start))
		  (end (widget-get tree :jdi-end)))
	  (jdibug-value-custom-expand-map-1 value start end))))

(defun jdibug-make-tree-from-field-value (field value)
  (let ((display (format "%s%s: %s"
						 (if (or (jdi-field-static-p field) (jdi-field-final-p field))
							 (format "(%s%s) "
									 (if (jdi-field-static-p field) "S" "")
									 (if (jdi-field-final-p field) "F" ""))
						   "")
						 (jdi-field-name field)
						 (jdibug-value-get-string value))))
	(if (jdi-value-has-children-p value)
		`(tree-widget
		  :node (push-button
				 :tag ,display
				 :format "%[%t%]\n")
		  :open nil
		  :args nil
		  :jdi-value ,value
		  :expander jdibug-value-expander
		  :dynargs jdibug-value-expander)
	  `(item
		:value
		,display))))

(defun jdibug-make-tree-from-variable-value (variable value string)
  (jdibug-make-tree-from-value (jdi-variable-name variable) value string))

(defun jdibug-make-tree-from-value (name value string)
  (if (jdi-value-has-children-p value)
      `(tree-widget
		:node (push-button
			   :tag ,(format "%s: %s" name string)
			   :format "%[%t%]\n")
		:open nil
		:args nil
		:jdi-value ,value
		:expander jdibug-value-expander
		:dynargs jdibug-value-expander)
    `(item
      :value
      ,(format "%s: %s" name string))))

(defun jdibug-variable-sorter (o1 o2)
  (string< (jdi-variable-name o1) (jdi-variable-name o2)))

(defun jdibug-field-sorter (o1 o2)
  (if (= (jdi-field-mod-bits o1) (jdi-field-mod-bits o2))
	  (string< (jdi-field-name o1) (jdi-field-name o2))
	(> (jdi-field-mod-bits o1) (jdi-field-mod-bits o2))))

(defmacro jdibug-refresh-buffer-now (buffer retry-func must-be-suspended &rest body)
  "Standard boilerplate for updating BUFFER.  If we cannot update
now, call RETRY-FUNC to schedule another try later.  Otherwise,
execute BODY."
  (declare (indent 3))
  `(when (or (jdwp-accepting-process-output-p)
			jdwp-uninterruptibly-running-p
			jdwp-sending-command
			(null (catch 'jdwp-input-pending
					(jdwp-uninterruptibly
					  (let ((jdwp-throw-on-input-pending t))
						(when (buffer-live-p ,buffer)
						  (with-current-buffer ,buffer
							(let ((inhibit-read-only t))
							  (erase-buffer))

							(if (and ,must-be-suspended (null jdibug-active-thread))
								(insert "Not suspended")
							  ,@body)))))
					  t)))
	(,retry-func)))

(defun jdibug-refresh-locals-buffer ()
  (if (timerp jdibug-refresh-locals-buffer-timer)
	  (cancel-timer jdibug-refresh-locals-buffer-timer))
  (setq jdibug-refresh-locals-buffer-timer
		(jdibug-util-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-locals-buffer-now)))

(defun jdibug-refresh-locals-buffer-now ()
  (jdibug-refresh-buffer-now jdibug-locals-buffer jdibug-refresh-locals-buffer t
	(let* ((jdwp-ignore-error (list jdwp-error-absent-information))
		   (variables (sort (copy-sequence (jdi-frame-get-visible-variables jdibug-active-frame)) 'jdibug-variable-sorter)))
	  (if (null variables)
		  (insert "No debug information available")
		(let* ((values (jdi-frame-get-values jdibug-active-frame variables))
			   (tree (jdibug-make-locals-tree variables values)))
		  (jdibug-debug "values.size=" (length values))
		  (setq jdibug-locals-tree (tree-mode-insert tree))
		  (widget-put jdibug-locals-tree :jdibug-opened-path (tree-mode-opened-tree jdibug-locals-tree))
		  (jdi-info "current opened tree path:%s" (tree-mode-opened-tree jdibug-locals-tree))
		  (local-set-key "s" 'jdibug-node-tostring)
		  (local-set-key "c" 'jdibug-node-classname)
		  (message "Locals updated")
		  )))))

(defun jdibug-refresh-watchpoints-buffer ()
  "Start the timer to refresh the watchpoints buffer.  This is
delayed because immediately refreshing slows down the debugger if
we quickly step several times."
  (jdibug-trace "jdibug-refresh-watchpoints-buffer")
  (if (timerp jdibug-refresh-watchpoints-buffer-timer)
	  (cancel-timer jdibug-refresh-watchpoints-buffer-timer))
  (setq jdibug-refresh-watchpoints-buffer-timer
		(jdibug-util-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-watchpoints-buffer-now)))

(defun jdibug-refresh-watchpoints-buffer-now ()
  (jdibug-trace "jdibug-refresh-watchpoints-buffer-now: %s" jdibug-watchpoints)
  (jdibug-refresh-buffer-now jdibug-watchpoints-buffer jdibug-refresh-watchpoints-buffer t
	(if (not jdibug-active-frame)
		(insert "Not suspended")
	  (jdibug-debug "evaluating watchpoints")
	  (let* ((jdwp-ignore-error (list jdwp-error-absent-information))
			 (watchpoint-alist (jdibug-evaluate-watchpoints jdibug-active-frame)))
		(jdibug-debug "watchpoint-alist: %s" watchpoint-alist)
		(mapc 'jdibug-refresh-watchpoints-1 watchpoint-alist)
		(message "Watchpoints updated")
		(jdibug-trace "jdibug-refresh-watchpoints-buffer-now finished")))))

(defun jdibug-evaluate-watchpoints (frame)
  "Evaluate all of the currently defined watch points, using
FRAME as the stack frame for defining the values.  Returns a list
of conses suitable for passing to `jdibug-refresh-watchpoints-1'"
  (mapcar (lambda (watchpoint)
			(let* ((parse (jdibug-watchpoint-parse watchpoint))
				   (jdwp (jdi-virtual-machine-jdwp (jdi-frame-virtual-machine frame)))
				   (object (catch jdibug-expr-bad-eval (jdibug-expr-eval-expr jdwp parse frame))))
			  (cons (jdibug-watchpoint-expression watchpoint) object)))
		  jdibug-watchpoints))


(defun jdibug-refresh-watchpoints-1 (pair)
  "Display the value of a watchpoint in the current buffer.  PAIR must be a cons of the form (expression . value)"
  (let ((expression (car pair))
		(value (cdr pair)))
	(insert (format "%-20s: " expression))
	(cond ((stringp value)
		   ;; TODO: do we want to color the text?
		   (insert (format "%s" value)))
		  ((jdi-value-p value)
		   (insert (format "%s" (jdibug-value-get-string value))))
		  (t
		   (jdwp-error "Unknown data structure for values of %s: %s" expression value)
		   (insert (format "Unknown data structure: %s" value))))
	(insert "\n")))


(defun jdibug-frame-notify (button &rest ignore)
  (jdibug-debug "jdibug-frame-notify")
  (let ((frame (widget-get button :jdi-frame)))
	(eval `(jdwp-uninterruptibly (jdibug-frame-notify-1 ,frame)))))

(defun jdibug-frame-notify-1 (frame)
  (let ((frames (jdi-thread-get-frames (jdi-frame-thread frame))))
	(jdibug-debug "going to class=%s method=%s line-number=%s"
				  (jdi-class-name (jdi-location-class (jdi-frame-location frame)))
				  (jdi-method-name (jdi-location-method (jdi-frame-location frame)))
				  (jdi-location-line-number (jdi-frame-location frame)))
	(let ((frame-index (position frame frames :test (lambda (f1 f2)
													  (equal (jdi-frame-id f1) (jdi-frame-id f2)))))
		  (thread (jdi-frame-thread frame)))
	  (jdibug-debug "looking at frame-id:%s frame-index:%s" (jdi-frame-id frame) frame-index)
	  (if jdibug-debug-flag
		  (loop for frame in frames
				do
				(jdibug-debug "existing frame-id:%s" (jdi-frame-id frame))))

;;	  (jdibug-refresh-locals-buffer (jdi-frame-thread frame) (jdi-frame-location frame)))
	  ;; we need to resolve again and get the frame id from the index
	  ;; if not, we will get into some invalid frame id errors
	  ;; when doing stack-get-values

	  (jdi-thread-get-frames thread)
	  (let ((frame (nth frame-index frames)))
		(jdibug-debug "after reload frame-id:%s" (jdi-frame-id frame))
		(setq jdibug-active-frame frame)

		(jdibug-handle-change-frame frame)))
	(jdibug-goto-location (jdi-frame-location frame))))

(defun jdibug-make-frame-node (frame)
  (jdibug-debug "jdibug-make-frame-node:frame-id=%s" (jdi-frame-id frame))
  (let* ((location (jdi-frame-location frame))
		 (declaring-type-name (jdi-class-name (jdi-location-class location)))
		 (receiving-type-name (jdi-class-name (jdi-value-get-reference-type (jdi-frame-get-this-object frame))))
		 (value (format "%s.%s(%s) line: %s"
						(if (and receiving-type-name
								 (not (string= receiving-type-name "null"))
								 (not (string= declaring-type-name receiving-type-name)))
							(format "%s(%s)" receiving-type-name declaring-type-name)
						  declaring-type-name)
						(jdi-method-get-name (jdi-location-method location))
						(mapconcat 'identity (cdr (jdi-jni-to-print (jdi-method-get-signature (jdi-location-method location)) t)) ", ")
						(jdi-location-get-line-number location)))
		 (active-frame-id (if jdibug-active-frame (jdi-frame-id jdibug-active-frame))))
	(jdibug-info "jdibug-make-frame-node:frame-id=%s:active-frame-id=%s" (jdi-frame-id frame) active-frame-id)
	(if (jdibug-have-class-source-p (jdi-location-class (jdi-frame-location frame)))
		`(push-button
		  :value ,value
		  :jdi-frame ,frame
		  :button-face ,(if (equal (jdi-frame-id frame) active-frame-id) 'jdibug-current-frame)
		  :notify jdibug-frame-notify
		  :format "%[%t%]\n")
	  `(item
		:value ,value))))

(defun jdibug-make-thread-value (thread)
  (jdibug-debug "jdibug-make-thread-value")
  (format "%sThread [%s] (%s) %s"
		  (if (jdi-thread-get-daemon-p thread)
			  "Daemon "
			"")
		  (jdi-thread-get-name thread)
		  (if (jdi-thread-running-p thread)
			  "Running"
			"Suspended")
		  (if (eq jdibug-active-thread thread)
			  "(Current)" "")))

(defun jdibug-make-frames-node (tree)
  (jdibug-debug "jdibug-make-frames-node")
  (let ((thread (widget-get tree :jdi-thread)))
	(when (and thread (not (jdi-thread-running-p thread)))
	  (let ((frames (jdi-thread-get-frames thread)))
		(jdibug-debug "jdibug-make-frames-node: number of frames = %s" (length frames))
		(mapcar 'jdibug-make-frame-node frames)))))

(defun jdibug-make-thread-tree (thread mode)
  (jdibug-debug "jdibug-make-thread-tree")
  (let ((value (jdibug-make-thread-value thread))
		(dynargs (case mode
				   (frames (function jdibug-make-frames-node))
				   (thread-group (function jdibug-make-thread-detail-node))
				   (t (error "Unknown mode for thread tree: %s" mode))))
		(format "%[%t%]\n")
		widget)
	(jdibug-debug "jdibug-make-thread-tree:value=%s" value)
	(jdi-thread-update-status thread)
	(setq widget `(tree-widget
				   :node (push-button
						  :tag ,value
						  :jdi-thread ,thread
						  :notify jdibug-thread-notify
 						  :format ,format
						  )
				   :open ,(eq mode 'frames)
				   :jdi-thread ,thread
				   :args nil
				   :expander ,dynargs
				   :dynargs ,dynargs))))

(defun jdibug-thread-notify (button &rest ignore)
  (jdibug-trace "jdibug-thread-notify" )
  (let ((thread (widget-get button :jdi-thread)))
	(setq jdibug-active-thread thread
		  jdibug-active-frame nil)
	(jdibug-refresh-frames-buffer)))

(defun jdibug-make-threads-tree (tree)
  (jdibug-debug "jdibug-make-threads-tree")
  (let ((mode (widget-get tree :jdi-mode)))
	(jdibug-debug "mode=%s" mode)
	(mapcar (lambda (thread)
			  (jdibug-make-thread-tree thread mode))
			(loop for thread in (jdi-virtual-machine-get-threads (widget-get tree :jdi-virtual-machine))
				  unless (and (eq mode 'frames)
							  (jdi-thread-get-system-thread-p thread))
				  collect thread))))

(defun jdibug-make-virtual-machine-tree (vm which)
  (let ((dynargs (case which
				   (frames (function jdibug-make-threads-tree))
				   (thread-group (function jdibug-make-thread-groups-tree))
				   (t (error "Unknown mode for virtual machine tree: %s" which)))))
  `(tree-widget
    :value ,(format "%s [%s:%s]" (jdi-virtual-machine-name vm) (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
    :open t
	:args nil
	:jdi-virtual-machine ,vm
	:jdi-mode ,which
	:dynargs ,dynargs
	:expander ,dynargs)))

(defun jdibug-make-frames-tree ()
  (jdibug-debug "jdibug-make-frames-tree")
  `(tree-widget
	:node (push-button
		   :tag "Debuggees"
		   :format "%[%t%]\n")
	:open t
	:args ,(mapcar
			(lambda (vm)
			  (jdibug-make-virtual-machine-tree vm 'frames))
			jdibug-virtual-machines)))

(defun jdibug-refresh-frames-buffer ()
  (if (timerp jdibug-refresh-frames-buffer-timer)
	  (cancel-timer jdibug-refresh-frames-buffer-timer))
  (setq jdibug-refresh-frames-buffer-timer
		(jdibug-util-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-frames-buffer-now)))

(defun jdibug-refresh-frames-buffer-now ()
  (jdibug-refresh-buffer-now jdibug-frames-buffer jdibug-refresh-frames-buffer nil
	(setq jdibug-frames-tree
		  (tree-mode-insert (jdibug-make-frames-tree)))
	(tree-mode)
	(let ((active-frame-point (jdibug-point-of-active-frame)))
	  (when active-frame-point
		(goto-char active-frame-point)
		(set-window-point (get-buffer-window jdibug-frames-buffer t) (point))
		(forward-line -1)
		(set-window-start (get-buffer-window jdibug-frames-buffer t) (point))))
	(message "Frames updated")))

(defun jdibug-node-tostring ()
  (interactive)
  (jdwp-uninterruptibly
	(let* ((wid (widget-at))
		   (tree (widget-get wid :parent))
		   (value (widget-get tree :jdi-value)))
	  (if (null value)
		  (message "not an object")
		(let ((class (jdi-value-get-reference-type value)))
		  (let ((methods (jdi-class-get-all-methods class)))
			(let ((tostringmethod (find-if (lambda (obj)
											 (and (equal (jdi-method-name obj)
														 "toString")
												  (equal (jdi-method-signature obj)
														 "()Ljava/lang/String;")))
										   methods)))
			  (if tostringmethod
				  (let ((result-value (jdi-value-invoke-method value jdibug-active-thread tostringmethod nil nil)))
					(let ((result-string (jdibug-string-get-string result-value)))
					  (message "%s" result-string)))
				(message "no toString method")))))))))


(defun jdibug-node-classname ()
  (interactive)
  (jdwp-uninterruptibly
	(let* ((wid (widget-at))
		   (tree (widget-get wid :parent))
		   (value (widget-get tree :jdi-value)))
	  (if (null value)
		  (message "not an object")
		(message "Class: %s" (jdi-class-signature (jdi-value-get-reference-type value)))))))

(defun jdibug-get-source-paths ()
  (or (and jdibug-use-jde-source-paths
	   (if (boundp 'jde-sourcepath)
	       (if (fboundp 'jde-normalize-path)
		   (if (fboundp 'jde-expand-wildcards-and-normalize)
		       (jde-expand-wildcards-and-normalize jde-sourcepath
							   'jde-sourcepath)
		     (mapcar
		      (lambda (path)
			(jdibug-normalize-path path 'jde-sourcepath t))
		      jde-sourcepath)))))
	  (if (stringp jdibug-source-paths)
		  (error "jdibug-source-paths must be a list of strings, not a single string")
		jdibug-source-paths)))

(defun jdibug-file-in-source-paths-p (file)
  (jdibug-debug "jdibug-file-in-source-paths-p:%s" file)
  (let ((result (find-if (lambda (sp)
						   (string-match (expand-file-name sp) file))
						 (jdibug-get-source-paths))))
	(jdibug-debug (if result "found" "not found"))
	result))

(defun jdibug-source-file-to-class-signature (source-file)
  "Converts a source file to a JNI class name."
  (let ((buf source-file))
    (mapc (lambda (sp)
			(setf buf (replace-regexp-in-string (expand-file-name sp) "" buf)))
		  (jdibug-get-source-paths))
    (setf buf (replace-regexp-in-string "^/" "" buf))
    (setf buf (replace-regexp-in-string ".java$" "" buf))
	 (setf buf (jdi-class-name-to-class-signature buf))
    (jdibug-trace "jdi-source-to-class-signature : %s -> %s" source-file buf)
    buf))


(defun jdibug-class-signature-to-source-file (class-signature)
  "Converts a JNI class name to source file."
  (jdibug-debug "jdibug-class-signature-to-source-file:%s" class-signature)
  (let ((buf class-signature))
    (setf buf (replace-regexp-in-string "^L" "" buf))
    (setf buf (replace-regexp-in-string ";$" "" buf))
	(setf buf (replace-regexp-in-string "\\$.*" "" buf))
    (setf buf (concat buf ".java"))
    (mapc (lambda (sp)
			(if (file-exists-p (concat (expand-file-name sp) "/" buf))
				(setf buf (concat (expand-file-name sp) "/" buf))))
		  (jdibug-get-source-paths))
    (jdibug-trace "jdi-class-signature-to-source : %s -> %s" class-signature buf)
    buf))


(defmethod set-breakpoint ((bp jdibug-breakpoint) &optional vm-list class)
  "Set the breakpoint BP.  If VM-LIST, only set it in those
virtual machines.  If CLASS, use that class instead of the one
based on the file name."

  (jdibug-debug "set-breakpoint %s %s" (object-class-name bp) vm-list)
  (setf (jdibug-breakpoint-status bp) 'unresolved)
  (unless (memq bp (breakpoint-list bp))
	 (push bp (breakpoint-list bp)))
  (jdibug-breakpoint-update bp)
  (jdibug-message "JDIbug setting breakpoint...")
  (dolist (vm (or vm-list jdibug-virtual-machines))
	(let ((result (set-breakpoint-in-vm bp vm class)))
	  (if result
		  (progn
			(jdibug-message "done" t)
			(setf (jdibug-breakpoint-status bp) 'enabled)
			(setf (jdibug-breakpoint-event-requests bp)
				  (append result (jdibug-breakpoint-event-requests bp))))
		(jdibug-message "pending" t))))
  (jdibug-breakpoint-update bp))


(defmethod set-breakpoint-in-vm ((bp jdibug-breakpoint-location) vm &optional class)
  (jdibug-debug "set-breakpoint-in-vm %s" (class-name (class-of bp)))
  (let ((source-file (jdibug-breakpoint-source-file bp))
		(line-number (jdibug-breakpoint-line-number bp)))
	(if (not (jdibug-file-in-source-paths-p source-file))
		(progn
		  (jdibug-message (format "file %s is not in source path "
								  source-file) t)
		  nil)

	  (let ((result (jdi-virtual-machine-set-breakpoint
						  vm
						  (if class (jdi-class-signature class)
								(jdibug-source-file-to-class-signature source-file))
						  line-number)))
		;; return values is the events returned above, which can (and
		;; will) be nil if the call failed.
		result))))

(defmethod set-breakpoint-in-vm ((bp jdibug-breakpoint-exception) vm &optional class-signature)
  (jdibug-debug "set-breakpoint-in-vm %s" (class-name (class-of bp)))
  (let* ((name (jdibug-breakpoint-name bp))
			(signature (jdi-class-name-to-class-signature name))
			(caught (jdibug-breakpoint-caught bp))
			(uncaught (jdibug-breakpoint-uncaught bp)))
	(jdi-virtual-machine-set-break-on-exception vm
												signature caught uncaught)))

(defmethod disable-breakpoint ((bp jdibug-breakpoint))
  (mapc (lambda (er)
			 (condition-case err
				  (jdi-event-request-disable er)
				(error (jdibug-error "Unable to disable breakpoint: %s" (cdr err)))
				))
		  (jdibug-breakpoint-event-requests bp))
  (setf (jdibug-breakpoint-status bp) 'disabled)
  (jdibug-breakpoint-update bp)
  (message "breakpoint disabled"))

(defmethod disable-breakpoint ((bp jdibug-breakpoint-location))
  (call-next-method)
  (when (jdibug-breakpoint-overlay bp)
	 (delete-overlay (jdibug-breakpoint-overlay bp))))

(defun jdibug-enable-breakpoint (bp)
  (mapc (lambda (er)
		  (jdi-event-request-enable er))
		(jdibug-breakpoint-event-requests bp))
  (setf (jdibug-breakpoint-status bp) 'enabled)
  (jdibug-breakpoint-update bp)
  (message "breakpoint enabled"))

(defun jdibug-remove-breakpoint (bp)
  (disable-breakpoint bp)
  (setf (breakpoint-list bp) (delete bp (breakpoint-list bp)))

  (jdibug-refresh-breakpoints-buffer)
  (message "breakpoint removed"))

(defun jdibug-remove-all-breakpoints ()
  "Remove all breakpoints"
  (interactive)
  (let ((bps (jdibug-all-breakpoints)))
	(mapc 'jdibug-remove-breakpoint bps)))

(defun jdibug-toggle-breakpoint ()
  "Toggle the breakpoint at the current line between enabled, disabled and removed"
  (interactive)
  (jdwp-uninterruptibly
	 (let* ((current-line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
			  (source-file (buffer-file-name))
			  (line-number (if (string-match jdibug-break-on-class-regexp current-line-text)
									 nil
								  (line-number-at-pos)))
			  (bp (find-if (lambda (bp)
								  (and (equal (jdibug-breakpoint-source-file bp)
												  source-file)
										 (equal (jdibug-breakpoint-line-number bp)
												  line-number)))
								(breakpoint-list jdibug-breakpoint-location))))
		(jdibug-debug "line-number:%s,current-line-text:%s"
						  line-number current-line-text)
		(cond ((and bp (member (jdibug-breakpoint-status bp)
									  (list 'enabled 'unresolved)))
				 (disable-breakpoint bp))
				((and bp (equal (jdibug-breakpoint-status bp) 'disabled))
				 (jdibug-remove-breakpoint bp))
			(t
			 ;; Set the name for help with debugging.  Not actually used anywhere
			 (set-breakpoint (jdibug-breakpoint-location (format "%s:%d"
																 (replace-regexp-in-string ".*/" "" source-file)
																 line-number)
														 :source-file source-file
														 :line-number line-number)))))))

(defvar jdibug--break-on-exception-history nil
  "History of exceptions input in `jdibug-break-on-exception")

(defun jdibug-break-on-exception (name caught uncaught)
  "Break whenever an exception of type NAME (or a subclass) is
thrown.  Breaks can happen for CAUGHT and/or UNCAUGHT exceptions.
If name is not yet loaded, the breakpoint will be enabled when
the class is loaded."
  (interactive (list
				(read-from-minibuffer "Class of exception: "
											 nil nil nil
											 'jdibug--break-on-exception-history)
				(y-or-n-p "Break on caught exception? ")
				(y-or-n-p "Break on uncaught exception? ")))
  ;; Give the breakpoint a helpful name for debugging.  Not actually used anywhere.
  (jdwp-uninterruptibly
	 (eval `(let ((bp (jdibug-breakpoint-exception ,name
																	:name ,name
																	:caught ,caught
																	:uncaught ,uncaught)))
				  (set-breakpoint bp)))))

(defun jdibug-get-class-line-number ()
  "Get the line number of the class declaration."
  (save-excursion
	(goto-char (point-min))
	(search-forward-regexp jdibug-break-on-class-regexp)
	(line-number-at-pos)))

(defun jdibug-breakpoint-update (bp)
  (jdibug-debug "jdibug-breakpoint-update")
  (jdibug-refresh-breakpoints-buffer)
  (breakpoint-update bp))

(defmethod breakpoint-update ((this jdibug-breakpoint))
  "Update any display of the breakpoint.  The default
  implementation does nothing.")

(defmethod breakpoint-update ((this jdibug-breakpoint-location))
  "Update any display of the breakpoint.  Updates the overlays of the breakpoint in the file buffer."

  (let ((buffer (find-if (lambda (buf)
						   (string= (buffer-file-name buf) (jdibug-breakpoint-source-file this)))
						 (buffer-list))))
	(when (and buffer (buffer-live-p buffer))
	  (jdibug-debug "Found buffer")
	  (with-current-buffer buffer
		(goto-char (point-min))
		(forward-line (1- (or (jdibug-breakpoint-line-number this)
							  (jdibug-get-class-line-number))))
		(if (jdibug-breakpoint-overlay this)
			(delete-overlay (jdibug-breakpoint-overlay this)))
		(setf (jdibug-breakpoint-overlay this) (make-overlay (point) (1+ (line-end-position))))
		(overlay-put (jdibug-breakpoint-overlay this) 'priority 5)
		(overlay-put (jdibug-breakpoint-overlay this) 'face
					 (cond ((equal (jdibug-breakpoint-status this) 'enabled)
							'jdibug-breakpoint-enabled)
						   ((equal (jdibug-breakpoint-status this) 'unresolved)
							'jdibug-breakpoint-unresolved)
						   ((equal (jdibug-breakpoint-status this) 'disabled)
							'jdibug-breakpoint-disabled)))))))

(defun jdibug-breakpoints-toggle ()
  (interactive)
  (jdwp-uninterruptibly
	(let ((bp (get-text-property (point) 'breakpoint)))
	  (if (equal (jdibug-breakpoint-status bp) 'enabled)
		  (progn
			(message ":action disable breakpoint")
			(disable-breakpoint bp))
		(message ":action enable breakpoint")
		(jdibug-enable-breakpoint bp)))))

(defun jdibug-breakpoints-add-condition (condition)
  (interactive "sCondition for breakpoint: ")
  (jdwp-uninterruptibly
	(let* ((bp (get-text-property (point) 'breakpoint))
		   (parse (jdibug-expr-parse-expr condition)))
	  (cond
	   ((consp parse)
		(setf (jdibug-breakpoint-condition bp) parse
			  (jdibug-breakpoint-condition-text bp) condition)
		(message "Set condition on breakpoint to %s" condition))
	 ((stringp parse)
	  (message "Unable to parse %s because %s" condition parse))
	 (t (error "Unknown return from jdibug-expr-parse-expr: %s" parse))))))


(defun jdibug-breakpoints-delete ()
  (interactive)
  (jdwp-uninterruptibly
	(let ((bp (get-text-property (point) 'breakpoint)))
	  (jdibug-remove-breakpoint bp))))


(defun jdibug-breakpoints-goto ()
  (interactive)
  (jdwp-uninterruptibly
	(let ((bp (get-text-property (point) 'breakpoint)))
	  (if bp
		  (jdibug-show-file-and-line-number (jdibug-breakpoint-source-file bp)
											(jdibug-breakpoint-line-number bp))))))

(defun jdibug-breakpoints-jde-mode-hook ()
  (mapc (lambda (bp)
			 (jdibug-breakpoint-update bp))
		  (jdibug-all-breakpoints)))

(add-hook 'jde-mode-hook 'jdibug-breakpoints-jde-mode-hook)

(defun jdibug-refresh-breakpoints-buffer ()
  (jdibug-debug "jdibug-refresh-breakpoints-buffer:%s breakpoints" (length (jdibug-all-breakpoints)))
  (if (buffer-live-p jdibug-breakpoints-buffer)
	  (let ((orig-line (line-number-at-pos)))
		(with-current-buffer jdibug-breakpoints-buffer
		  (let* ((header (format "   S %-40s Details" "Breakpoint"))
					(inhibit-read-only t))
			 (erase-buffer)
			 (setq header-line-format header)
			 ;; the breakpoints are added in reverse order, so in order to display the first one added first, we reverse it here
			 (dolist (bp (reverse (jdibug-all-breakpoints)))
				(let ((str (format " %s %s\n"
										 (cond ((equal (jdibug-breakpoint-status bp) 'enabled) " E")
												 ((equal (jdibug-breakpoint-status bp) 'unresolved) " P")
												 (t "  "))
										 (display-string bp))))
				  (insert (propertize str
									  'breakpoint bp
									  'help-echo (if (jdibug-breakpoint-condition bp)
													 (format "when %s" (jdibug-breakpoint-condition-text bp))
												   "always")))))))
		(goto-char (point-min))
		(forward-line (1- orig-line)))))

(defmethod display-string ((bp jdibug-breakpoint))
  "Create a string for displaying BP to the user"
  (jdibug-expr-abstract-method bp))

(defmethod display-string ((bp jdibug-breakpoint-location))
  (format "%-40s %-20s"
			 (short-source-file bp)
			 (or (jdibug-breakpoint-line-number bp)
				  "class")))

(defmethod display-string ((bp jdibug-breakpoint-exception))
  (format "%-40s thrown when %s"
			 (jdibug-breakpoint-name bp)
			 (concat (if (jdibug-breakpoint-caught bp)
							 "caught ")
						(if (jdibug-breakpoint-uncaught bp)
							 "uncaught "))))

(defun jdibug-send-step (depth)
  (jdibug-debug "jdibug-send-step")
  (if jdibug-current-line-overlay
      (delete-overlay jdibug-current-line-overlay))
  (if (null jdibug-active-thread)
      (message "JDIbug Can not step. Not suspended.")
	(let ((active-thread jdibug-active-thread))
	  ;; we can not clear the active-thread after calling send-step
	  ;; because the send-step command is going to trigger the
	  ;; handle-step command first, and then return here
	  ;; and we will be clearing it again
;; 	(jdibug-debug "setting jdibug-active-thread to nil in jdibug-send-step")
	  (setq jdibug-active-thread nil)
	  (setq jdibug-active-frame nil)
	  (jdi-thread-send-step active-thread depth)
	  (jdibug-debug "jdibug-send-step:end"))))

(defun jdibug-step-over ()
  (interactive)
  (jdwp-uninterruptibly
	(jdibug-send-step jdwp-step-depth-over)))

(defun jdibug-step-into ()
  (interactive)
  (jdwp-uninterruptibly
	(jdibug-send-step jdwp-step-depth-into)))

(defun jdibug-step-out ()
  (interactive)
  (jdwp-uninterruptibly
	(jdibug-send-step jdwp-step-depth-out)))

(defun jdibug-resume ()
  "Resume the current thread, as specified by `jdibug-active-thread'"
  (interactive)
  (jdwp-uninterruptibly
	(if jdibug-current-line-overlay
		(delete-overlay jdibug-current-line-overlay))
	(let ((active-thread jdibug-active-thread))
	  (setq jdibug-active-thread nil
			jdibug-active-frame nil)
	  (jdi-thread-resume active-thread)
	  (run-hooks 'jdibug-resumed-hook)

	  (jdibug-refresh-frames-buffer)
	  (jdibug-refresh-threads-buffer)

	  (if jdibug-others-suspended
		  (let ((other (pop jdibug-others-suspended)))
			(apply 'jdibug-handle-breakpoint other)))

	  (message "JDIbug resumed"))))

(defun jdibug-resume-all ()
  "Resume all threads in all JVMs"
  (interactive)
  (jdwp-uninterruptibly
	(if jdibug-current-line-overlay
		(delete-overlay jdibug-current-line-overlay))
	(mapc (lambda (vm)
			(jdi-resume vm))
		  jdibug-virtual-machines)
	(setq jdibug-active-thread nil
		  jdibug-active-frame nil
		  jdibug-others-suspended nil)
	(run-hooks 'jdibug-resumed-hook)

	(jdibug-refresh-frames-buffer)

	(message "JDIbug resumed all threads")))

(defun jdibug-connected-p ()
  (interactive)
  (jdibug-debug "jdibug-connected-p")
  (let ((connected 0))
	(mapc (lambda (vm)
			(let ((proc (jdwp-process (jdi-virtual-machine-jdwp vm))))
			  (if (and proc
					   (equal (process-status proc) 'open))
				  (incf connected))))
		  jdibug-virtual-machines)
	(jdibug-debug "connected = %s" connected)
	(if (equal connected 0)
		nil
	  connected)))

(defun jdibug-debug-view ()
  "Change into debug view."
  (interactive)
  (jdibug-debug "jdibug-debug-view")
  (delete-other-windows)
  (split-window-vertically -20)

  (split-window-horizontally -50)
  (other-window 1)
  (switch-to-buffer jdibug-locals-buffer-name)
  ;; its much easier to see
  (setq truncate-lines t)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)

  (other-window 1)
  (switch-to-buffer jdibug-frames-buffer-name)
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer jdibug-breakpoints-buffer-name)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (other-window 1))

(defun jdibug-debug-view-1 ()
  "Change into an alternate debug view.  This is an alternate
value for jdibug-connected-hook.  It displays the source, frames,
locals and threads buffers."
  (interactive)
  (jdibug-debug "jdibug-debug-view")
  (delete-other-windows)
  (split-window-vertically -20)

  (split-window-horizontally -50)
  (other-window 1)
  (switch-to-buffer jdibug-locals-buffer-name)
  ;; its much easier to see
  (setq truncate-lines t)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)

  (other-window 1)
  (switch-to-buffer jdibug-frames-buffer-name)
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer jdibug-threads-buffer-name)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (other-window 1))

(defun jdibug-debug-view-2 ()
  "Change into an alternate debug view.  This is an alternate
value for jdibug-connected-hook.  It displays the source, frames,
locals and watchpoint buffers."
  (interactive)
  (jdibug-debug "jdibug-debug-view")
  (delete-other-windows)
  (split-window-vertically -20)

  (split-window-horizontally -50)
  (other-window 1)
  (switch-to-buffer jdibug-locals-buffer-name)
  ;; its much easier to see
  (setq truncate-lines t)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)

  (other-window 1)
  (switch-to-buffer jdibug-frames-buffer-name)
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer jdibug-watchpoints-buffer-name)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (other-window 1))

(defun jdibug-undebug-view ()
  (interactive)
  (jdibug-debug "jdibug-undebug-view")
  (mapc (lambda (buffer)
		  (let ((win (get-buffer-window buffer t)))
			(if win (delete-window win)))
		  (if (get-buffer buffer)
			  (kill-buffer buffer)))
		(list jdibug-locals-buffer-name
			  jdibug-frames-buffer-name
			  jdibug-breakpoints-buffer-name
			  jdibug-threads-buffer-name
			  jdibug-watchpoints-buffer-name)))

(add-hook 'jdibug-connected-hook 'jdibug-debug-view-2)
(add-hook 'jdibug-detached-hook 'jdibug-undebug-view)

;; Until when we have our own minor mode, we put it into jde-mode-map.
;; Ideally, we should use java-mode, but then we run into conflicts
;; with C-c C-c where we lose.
(when (boundp 'jde-mode-map)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-c] 'jdibug-connect)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-d] 'jdibug-disconnect)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-b] 'jdibug-toggle-breakpoint)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-n] 'jdibug-step-over)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-i] 'jdibug-step-into)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-o] 'jdibug-step-out)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-r] 'jdibug-resume)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-a] 'jdibug-resume-all)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-k] 'jdibug-exit-jvm)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-w] 'jdibug-add-watchpoint)
  )

(defun jdibug-value-get-string (value)
  "Get a string to be displayed for a value"
  (jdibug-debug "jdibug-value-get-string:type=%s"
				(jdi-value-type value))

  (cond ((or (equal (jdi-value-type value) jdwp-tag-byte)
			 (equal (jdi-value-type value) jdwp-tag-char)
			 (equal (jdi-value-type value) jdwp-tag-short))
		 (format "%d" (jdi-primitive-value-value value)))

		((or (equal (jdi-value-type value) jdwp-tag-long)
			 (equal (jdi-value-type value) jdwp-tag-int))
		 ;; TODO: This will fail for large values (greater than about
		 ;; 24 bits) since emacs integers only have 24-28 bits.  Doing
		 ;; this right will require writing something like
		 ;; number-to-string that works on at least 8 bytes
;; 		 (jdibug-debug "value=%s value-value=%s" value (jdi-primitive-value-value value))
		 (format "%d" (jdwp-vec-to-int (jdi-primitive-value-value value))))

		((equal (jdi-value-type value) jdwp-tag-float)
		 (jdibug-float-to-string (jdwp-vec-to-float (jdi-primitive-value-value value))))

		((equal (jdi-value-type value) jdwp-tag-double)
		 (jdibug-double-to-string (jdwp-vec-to-double (jdi-primitive-value-value value))))

		((equal (jdi-value-type value) jdwp-tag-boolean)
		 (if (= 0 (jdi-primitive-value-value value)) "false" "true"))

		((equal (jdi-value-type value) jdwp-tag-void)
		 "void")

		((equal (jdi-value-type value) jdwp-tag-class-object)
		 ;; TODO: put in the class name
		 "class")

		((equal (jdi-value-type value) jdwp-tag-class-loader)
		 "class-loader")

		((equal (jdi-value-type value) jdwp-tag-thread)
		 "thread")

		((equal (jdi-value-type value) jdwp-tag-thread-group)
		 "thread-group")

 		((equal (jdi-value-type value) jdwp-tag-object)
 		 (jdibug-object-get-string value))

		((equal (jdi-value-type value) jdwp-tag-string)
		 (jdibug-string-get-string value))

  		((equal (jdi-value-type value) jdwp-tag-array)
  		 (jdibug-array-get-string value))

		(t
		 (jdi-error "fixme: do not know how to print value of type:%s" (jdi-value-type value))
		 "...")))

(defun jdibug-float-to-string (float)
  "Convert a FLOAT converted by jdwp to a string for display.
The value is probably a number, but it could be a constant for
special cases like infinity."
  (if (numberp float)
	  (format "%.9g" float)
	(format "%s" float)))

(defun jdibug-double-to-string (double)
  "Convert a DOUBLE converted by jdwp to a string for display.
The value is probably a number, but it could be a constant for
special cases like infinity."
  (if (numberp double)
	  (format "%.17g" double)
	(format "%s" double)))

(defun jdibug-object-get-string (object)
  (jdibug-debug "jdibug-object-get-string:object-id=%s" (jdi-object-id object))
  (if (equal (jdi-object-id object) [0 0 0 0 0 0 0 0])
	  "null"

	(let* ((class (jdi-object-get-reference-type object))
		   (pair (find-if (lambda (custom)
							(jdi-class-instance-of-p class (jdi-class-name-to-class-signature (car custom))))
						  jdibug-object-custom-set-strings))
		   (setters (if pair (cdr pair))))
	  (jdibug-debug "jdibug-object-get-string: object-id=%s found-setters:%s" (jdi-object-id object) setters)
	  (format "%s (id=%s)"
			  (if setters
				  (mapconcat (lambda (setter)
							   (cond ((stringp setter)
									  (jdibug-object-custom-set-string-with-method object jdibug-active-thread setter))
									 ((functionp setter)
									  (funcall setter object))))
							 setters
							 ":")
				(jdi-class-name class))
			  (jdwp-vec-to-int (jdi-object-id object))))))

(defun jdibug-array-get-string (array)
  (jdibug-debug "jdibug-array-get-string")
  (let ((length (jdi-array-get-array-length array)))
	(jdibug-array-display-string array length)))

(defun jdibug-array-display-string (array size)
  "for array of three dimension, return i[2][][]."
  (let* ((class-name (jdi-class-name (jdi-object-get-reference-type array))) ;; this will be i[][][]
		 (pos (string-match "\\]" class-name))) ;; just find the first closing bracket and insert the size before that
	(format "%s%s%s (id=%s)"
			(substring class-name 0 pos)
			size
			(substring class-name pos)
			(jdwp-vec-to-int (jdi-array-id array)))))

(defun jdibug-string-get-string (string)
  (jdibug-debug "jdibug-string-get-string")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp string) "string-value" `((:object . ,(jdi-object-id string))))))

	(jdibug-debug "jdibug-string-get-string:%s:%s" (jdwp-get-string reply :value) (jdi-format-string (jdwp-get-string reply :value)))
	(jdi-format-string (jdwp-get-string reply :value))))

(defun jdibug-object-custom-set-string-with-method (object thread method-name)
  (jdibug-debug "jdibug-object-custom-set-string-with-method")
  (let* ((class (jdi-object-get-reference-type object))
		 (methods (jdi-class-get-all-methods class))
		 (method (find-if (lambda (obj)
							(let ((print (jdi-jni-to-print (jdi-method-signature obj))))
							  (and (string= (jdi-method-name obj) method-name)
								   (= (length print) 1)))) ;; doesn't take any argument
						  methods)))
	(if (null method)
		(format "setter %s not found" method-name)

	  (let ((result-value (if (jdi-method-static-p method)
							  (jdi-class-invoke-method (jdi-object-get-reference-type object) thread method nil nil)
							(jdi-object-invoke-method object thread method nil nil))))
		(jdibug-value-get-string result-value)))))

(defun jdibug-object-custom-set-string-with-size (object)
  (jdibug-debug "jdibug-object-custom-set-string-with-size")
  (let ((result-value (jdi-object-invoke-method object jdibug-active-thread "size" nil nil)))
	(if result-value
		(format "%s[%s]"
				(jdi-class-name (jdi-object-get-reference-type object))
				(jdwp-vec-to-int (jdi-primitive-value-value result-value)))
	  (format "%s[nosize]" (jdi-class-name (jdi-object-get-reference-type object))))))

(defun jdibug-value-custom-expand-collection (value)
  (jdi-debug "jdibug-value-custom-expand-collection")
  (let ((result-value (jdi-value-invoke-method value jdibug-active-thread "toArray" nil nil)))
	(if result-value
		(jdibug-value-expander-array result-value))))

(defun jdibug-value-custom-expand-map (value)
  (jdi-debug "jdibug-value-custom-expand-collection")
  (jdibug-value-custom-expand-map-1 value nil nil))

(defun jdibug-value-custom-expand-map-1 (value first last)
  (let ((keyset-value (jdi-value-invoke-method value jdibug-active-thread "keySet" nil nil))
		(values-value (jdi-value-invoke-method value jdibug-active-thread "values" nil nil)))
	(when (and keyset-value values-value)
	  (let ((keyset-array (jdi-value-invoke-method keyset-value jdibug-active-thread "toArray" nil nil))
			(values-array (jdi-value-invoke-method values-value jdibug-active-thread "toArray" nil nil)))
		(when (and keyset-array values-array)
		  (let* ((first (or first 0))
				 (last (or last (jdi-array-get-array-length keyset-array)))
				 (num-display (- last first)))
			(jdibug-debug "Expanding map.  num-display=%d, max-size=%d"
						  num-display jdibug-locals-max-array-size)
			(if (<= num-display jdibug-locals-max-array-size)
				;; Short map, so display all of the values
				(loop for v in (loop for key in (jdi-array-get-values keyset-array first last)
							   for value in (jdi-array-get-values values-array first last)
							   append (list key value))
					  for s = (jdibug-value-get-string v)
					  for i from (* 2 first) by 1
					  collect (jdibug-make-tree-from-value (format "[%s%s]"
																   (if (= 0 (mod i 2)) "k" "v")
																   (/ i 2))
														   v s))
			  ;; Long map, so create pseudo nodes for subarrays
			  (loop with step = (jdibug-locals-step-size num-display)
					for start from first below last by step
					for end = (min last (+ start step))
					collect (jdibug-make-map-subtree value start end)))))))))


(defun jdibug-point-of-active-frame ()
  (catch 'done
	(with-current-buffer jdibug-frames-buffer
	  (goto-char (point-min))
	  (while (not (eobp))
		(let ((plist (overlays-at (point)))
			  (next-change
			   (or (next-overlay-change (point))
				   (point-max))))
		  (dolist (overlay (overlays-at next-change))
			(when (eq (overlay-get overlay 'face) 'jdibug-current-frame)
			  (throw 'done next-change)))
		  (goto-char next-change))))))


(defun jdibug-normalize-path (path symbol cygwin-p)
  "Process PATH and SYMBOL like `jde-normalize-path'.  If CYGWIN-P, leave the result in cygwin form."
  ;; If we want to leave paths in cygwin form, bind the converter to a
  ;; function that does nothing.  Also, if the jdee functions are not
  ;; available, do nothing.
  (let ((jde-cygwin-path-converter
		 (or (if (boundp 'jde-cygwin-path-converter)
				 (unless cygwin-p
				   jde-cygwin-path-converter))
			 (list (lambda (path) path)))))
	(if (fboundp 'jde-normalize-path)
		(jde-normalize-path path symbol)
	  (error "jde-normalize-path is not available"))))

(defun jdibug-refresh-threads-buffer ()
  (if (timerp jdibug-refresh-threads-buffer-timer)
	  (cancel-timer jdibug-refresh-threads-buffer-timer))
  (setq jdibug-refresh-threads-buffer-timer
		(jdibug-util-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-threads-buffer-now)))

(defun jdibug-refresh-threads-buffer-now ()
  (jdibug-refresh-buffer-now jdibug-threads-buffer jdibug-refresh-threads-buffer nil
	(let ((tree  (jdibug-make-top-level-thread-groups-tree)))
	  ;; 	(if (jdibug-threads-tree jdibug-this)
	  ;; 		(tree-mode-reflesh-tree (jdibug-threads-tree jdibug-this))
	  (setq jdibug-threads-tree (tree-mode-insert tree))
										;						(jdibug-debug "jdibug-threads-tree=%s" jdibug-threads-tree)
	  (tree-mode))
	(message "Threads updated")))

(defun jdibug-make-top-level-thread-groups-tree ()
  (jdibug-debug "jdibug-make-top-level-thread-groups-tree")
  `(tree-widget
	:node (push-button
		   :tag "Debuggees"
		   :format "%[%t%]\n")
	:open t
	:args ,(mapcar (lambda (vm)
					 (jdibug-make-virtual-machine-tree vm 'thread-group))
				   jdibug-virtual-machines)))

(defun jdibug-make-thread-groups-tree (tree)
  "Create an list of child nodes for TREE.  This will contain 0+
trees representing the child thread groups of the thread-group
for TREE."
  (jdibug-debug "jdibug-make-thread-groups-tree")
  (mapcar 'jdibug-make-thread-group-tree
		  (jdi-virtual-machine-get-top-level-thread-groups (widget-get tree :jdi-virtual-machine))))

(defun jdibug-make-thread-group-tree (thread-group)
  (jdibug-debug "jdibug-make-thread-group-tree")
  (let ((value (jdibug-make-thread-group-value thread-group))
		widget)
	(jdibug-debug "jdibug-make-thread-group-tree:value=%s" value)
	`(tree-widget
	  :node (push-button
			 :tag ,value
			 :jdi-thread-group ,thread-group
			 :notify jdibug-thread-group-notify
			 :format "%[%t%]\n"
			 )
	  :open t
	  :jdi-thread-group ,thread-group
	  :jdi-mode thread-group ; constant, intentionally no comma
	  :args nil
	  :expander jdibug-make-thread-groups-subtree
	  :dynargs jdibug-make-thread-groups-subtree)))

(defun jdibug-make-thread-group-value (thread-group)
  "Create the string for displaying THREAD-GROUP"
  (jdibug-trace "jdibug-make-thread-group-value")
  (format "Thread Group [%s] %s"
		  (jdi-thread-group-get-name thread-group)
		  (if (jdi-thread-group-get-system-thread-p thread-group)
			  "(System)" "")))

(defun jdibug-make-thread-groups-subtree (tree)
  "Create an list of child nodes for TREE.  This will contain 0+
trees representing the child thread groups of the thread-group
for TREE and 0+ trees representing the threads in the
thread-group for TREE."
  (jdibug-debug "jdibug-make-thread-groups-tree")
  (let ((parent-thread-group (widget-get tree :jdi-thread-group)))
	(jdi-thread-group-get-children parent-thread-group)
	(let ((groups 	 (mapcar 'jdibug-make-thread-group-tree (jdi-thread-group-child-groups parent-thread-group)))
		  (threads 	 (mapcar (lambda (thread)
			   (jdibug-make-thread-tree thread 'thread-group))
			 (jdi-thread-group-child-threads parent-thread-group))))
;; 	  ;; groups is a list in the correct form, but threads is a list
;; 	  ;; of lists that needs to be flattened.
;; 	(append groups (let (result)
;; 					 (mapcar (lambda (list) (setq result (append result list))) threads)
;; 					 result)))))
	  (append groups threads))))


(defun jdibug-make-thread-detail-node (tree)
  (jdibug-make-expansion-list 'jdibug-make-thread-detail-node-1 'jdibug-tree-mode-reflesh-tree tree))

(defun jdibug-make-expansion-list (func reflesh-func tree)
  "Create the child widgets for TREE by calling FUNC.  The call
is wrapped in `jdwp-uninterruptibly'.  If the execution is
delayed, a placeholder widget is returned and REFLESH-FUNC is
called.  REFLESH-FUNC should take a single argument (TREE) and
schedule a call for later execution to perform the actual update."

  (let ((result (jdwp-uninterruptibly
				  ;; If execution is delayed, tree will be unbound, in
				  ;; which case there is no point in buiding the node
				  ;; since we won't be able to use it anyway.
				  (if (and (boundp 'tree) (fboundp func))
					  (apply func (list tree))))))
	(if (eq result 'jdwp-deferred)
		(progn
		  (apply reflesh-func tree)
		  `((item :value "Building...")))
	  (jdibug-debug "jdibug-make-expansion-list:%s returning %s"
					func result)
	  result)))



(defun jdibug-make-thread-detail-node-1 (tree)
	(jdibug-debug "jdibug-make-thread-detail-node")
	(let ((thread (widget-get tree :jdi-thread)))
	  (multiple-value-bind (status suspend-status) (jdi-thread-get-status thread)
		`((item
		   :value ,(format "id: %s" (jdi-thread-id thread)))
		  (item
		   :value ,(format "Status: %s" (jdwp-thread-status-string status)))
		  (item
		   :value ,(format "Suspend count: %d" (jdi-thread-get-suspend-count thread)))
		  (push-button
		   :tag "Suspend"
		   :jdi-thread ,thread
		   :notify jdibug-thread-suspend-notify
		   :format "%[%t%]\n")
		  (push-button
		   :tag "Resume"
		   :jdi-thread ,thread
		   :notify jdibug-thread-resume-notify
		   :format "%[%t%]\n")))))

(defun jdibug-thread-resume-notify (button &rest event)
  (let ((thread (widget-get button :jdi-thread)))
	(eval `(jdwp-uninterruptibly
			 (setq jdibug-active-thread thread)
			 (jdibug-resume)))))

(defun jdibug-thread-suspend-notify (button &rest event)
  (let ((thread (widget-get button :jdi-thread)))
	(eval `(jdwp-uninterruptibly
			 (jdi-thread-suspend ,thread)
			 (jdibug-refresh-threads-buffer)
			 (jdibug-refresh-frames-buffer)))))

(defun jdibug-widget-string (tree)
  "Print TREE but do not include jdi-properties that are
probably either very large or result in infinite loops when
printed."
  (if (widgetp tree)
	  (let ((result (format "(%s" (car tree)))
			(items (cdr tree)))
		(while items
		  (let ((name (symbol-name (nth 0 items)))
				(value (nth 1 items)))
			(unless (or (string-match "^:jdi" name) (string-match "^:parent" name))
			  (cond ((widgetp value)
					 (setq value (jdibug-widget-string value)))
					((and value (listp value))
					 (setq value (format "(%s)"
										 (mapconcat 'jdibug-widget-string value " ")))))
			  (setq result (concat result (format " %s %s" name value))))
			(setq items (cddr items))))
		(setq result (concat result ")")))
	(format "%s" tree)))

(defun jdibug-print-tree (tree)
  "Print TREE but do not include jdi-properties that are
probably either very large or result in infinite loops when
printed."
  (let ((result (jdibug-widget-string tree)))
	(jdibug-info "%s" result)
	(message "%s" result)))


(defun jdibug-add-watchpoint (expression)
  "Create a watchpoint of EXPRESSION.  It's value will be displayed in the `jdibug-watchpoints-buffer' whenever the JVM is suspended."
  (interactive "sExpression to watch: ")
  (let ((parse (jdibug-expr-parse-expr expression)))
	(cond
	 ((consp parse)
	  (push (make-jdibug-watchpoint :expression expression :parse parse)
			jdibug-watchpoints)
	  (jdibug-refresh-watchpoints-buffer-now))
	 ((stringp parse)
	  (jdibug-error "Unable to parse %s because %s"
					expression parse))
	 (t (error "Unknown return from jdibug-expr-parse-expr: %s" parse)))))

(defun jdibug-remove-all-watchpoints (&optional interactive)
  "Remove all currently defined watchpoints"
  (interactive "P")
  (unless (and interactive
			   (not (yes-or-no-p "Remove all watchpoints ")))
	(setq jdibug-watchpoints nil)
	(jdibug-refresh-watchpoints-buffer)))

(provide 'jdibug-ui)
