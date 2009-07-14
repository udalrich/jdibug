;;; jdibug.el --- Fully elisp java debugger

;; Copyright (C) 2008 Phuah Yee Keat

;; Author: Phuah Yee Keat <ykphuah@gmail.com>
;; Maintainer: Phuah Yee Keat <ykphuah@gmail.com>
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
;;     jdibug-connect-host
;; which is the hostname that you want to connect to
;;     jdibug-connect-port
;; which is the port that you want to connect to
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
jdibug-use-jde-source-paths is t."
  :group 'jdibug
  :type 'list)

(defcustom jdibug-refresh-delay 0.5
  "The delay in seconds between when a breakpoint/step was hit
and the frames/locals buffer are refreshed. Set to 0 for instant update.
Set to more for speeding up quick multi step when the frames/locals buffer
need not be refreshed."
  :group 'jdibug
  :type 'float)

(defcustom jdibug-use-jde-source-paths t
  "Set to t to use the jde-sourcepath as the source paths. 
jdibug-source-paths will be ignored if this is set to t."
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

(defcustom jdibug-break-on-class-regexp "^public class"
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
(require 'jdi)
(require 'tree-mode)
(require 'cont)

(elog-make-logger jdibug)

(defstruct jdibug
  virtual-machines ;; list of jdi-virtual-machine

  ;; the jdi-frame that
  ;; 1. we are looking at in the locals browser
  ;; 2. we have selected in the frames buffer
  ;; 3. our resume, step commands will go to
  active-frame
  active-thread

  suspended-frames

  current-line-overlay
  threads-tree 
  threads-buffer

  breakpoints ;; list of jdibug-breakpoint
  breakpoints-buffer

  ;; buffer the shows a tree of the local variables
  locals-buffer
  ;; the variable that hold the tree-widget of the locals buffer
  locals-tree

  frames-buffer
  frames-tree

  refresh-timer
  refresh-proc
  )

(defstruct jdibug-breakpoint
  source-file
  line-number
  status ;; one of 'unresolved 'enabled 'disabled
  overlay

  ;; list jdi-event-request, this is a list because we might be installing 2 breakpoints for a single vm
  ;; because it have two class loaders, or we have two different vm!
  event-requests 
)

(defun jdibug-breakpoint-short-source-file (bp)
  (let ((buf (jdibug-breakpoint-source-file bp)))
	(setf buf (replace-regexp-in-string ".*/" "" buf))
	buf))

(makunbound 'jdibug-this)
(defvar jdibug-this (make-jdibug)
  "The current instance of jdibug, we can only have one jdibug running.")

(defvar jdibug-current-message "")

(defun jdibug-message (message &optional append)
  (if append
	  (setf jdibug-current-message (concat jdibug-current-message message))
	(setf jdibug-current-message message))
  (message jdibug-current-message))
  
(add-hook 'jdi-breakpoint-hooks 'jdibug-handle-breakpoint)
(add-hook 'jdi-step-hooks 'jdibug-handle-step)
(add-hook 'jdi-detached-hooks 'jdibug-handle-detach)
(add-hook 'jdi-class-prepare-hooks 'jdibug-handle-class-prepare)

(defvar jdibug-start-time nil)

(defun jdibug-time-start ()
  (setf jdibug-start-time (current-time)))

(defun jdibug-time-end (message)
  (jdibug-info "benchmark: %s took %s seconds" message (float-time (time-subtract (current-time) jdibug-start-time))))

(defun jdibug-connect ()
  (interactive)

  (jdibug-debug "jdibug-connect")

  (jdibug-message "JDIbug connecting... ")

  (cont-bind (result) (cont-mapcar (lambda (connect-host-and-port)
									 (lexical-let* ((host (nth 0 (split-string connect-host-and-port ":")))
													(port (string-to-number (nth 1 (split-string connect-host-and-port ":"))))
													(jdwp (make-jdwp))
													(vm (make-jdi-virtual-machine :host host :port port :jdwp jdwp)))
									   (cont-bind (result) (jdi-virtual-machine-connect vm)
										 (jdibug-message (format "%s:%s(%s)" host port (if result "connected" "failed")) t)
										 (push vm (jdibug-virtual-machines jdibug-this))
										 (cont-values vm))))
								   jdibug-connect-hosts)

	(lexical-let* ((bps (jdibug-breakpoints jdibug-this))
				   (signatures (loop for bp in bps
									 collect (jdibug-source-file-to-class-signature
											  (jdibug-breakpoint-source-file bp)))))
	  (setf (jdibug-breakpoints jdibug-this) nil)
	  (cont-bind () (cont-mapc 'jdibug-set-breakpoint bps)

		(setf (jdibug-threads-buffer jdibug-this)     (get-buffer-create jdibug-threads-buffer-name)
			  (jdibug-locals-buffer jdibug-this)      (get-buffer-create jdibug-locals-buffer-name)
			  (jdibug-frames-buffer jdibug-this)      (get-buffer-create jdibug-frames-buffer-name)
			  (jdibug-breakpoints-buffer jdibug-this) (get-buffer-create jdibug-breakpoints-buffer-name))

		(with-current-buffer (jdibug-breakpoints-buffer jdibug-this)
		  (use-local-map jdibug-breakpoints-mode-map)
		  (toggle-read-only 1))

		(run-hooks 'jdibug-connected-hook)
		(setf (jdibug-refresh-timer jdibug-this)
			  (jdibug-run-with-timer jdibug-refresh-delay nil 
									 'jdibug-refresh-now))
		(cont-values)))))
  
(defun jdibug-disconnect ()
  (interactive)
  (jdibug-trace "jdibug-disconnect")
  (if (jdibug-current-line-overlay jdibug-this)
	  (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (mapc (lambda (bp) 
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp))))
		(jdibug-breakpoints jdibug-this))

  (cont-kill (jdibug-refresh-proc jdibug-this))
  (and (timerp (jdibug-refresh-timer jdibug-this))
	   (cancel-timer (jdibug-refresh-timer jdibug-this)))

  (setf (jdibug-locals-tree             jdibug-this) nil
		(jdibug-locals-buffer           jdibug-this) nil

		(jdibug-frames-tree             jdibug-this) nil
		(jdibug-frames-buffer           jdibug-this) nil

		(jdibug-active-frame            jdibug-this) nil)

  (jdibug-message "JDIbug disconnecting... ")
  (mapc (lambda (vm)
		  (jdibug-message (format "%s:%s" 
								  (jdi-virtual-machine-host vm) 
								  (jdi-virtual-machine-port vm)) t)
		  (jdi-virtual-machine-disconnect vm)
		  (jdibug-message "(disconnected) " t))
		(jdibug-virtual-machines jdibug-this))
  (run-hooks 'jdibug-detached-hook))

(defun jdibug-have-class-source-p (class)
  (jdibug-debug "jdibug-have-class-source-p")
  (lexical-let ((class class))
	(cont-bind (signature) (jdi-class-get-signature class)
	  (cont-values (file-exists-p (jdibug-class-signature-to-source-file signature))))))

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

(defun jdibug-show-file-and-line-number (jdibug file-name line-number &optional highlight)
  "Show the buffer containing the file, or open the file if there are no open buffer for this file.
And position the point at the line number."
  (jdibug-debug "jdibug-show-file-and-line-number:%s:%s" file-name line-number)
  (let* ((buffer-name (jdibug-find-buffer file-name))
		 (win (if buffer-name (get-buffer-window buffer-name t) nil))
		 (frame (if win (window-frame win) nil)))
	(if win
		;; file is already open in a buffer, just show it
		(progn
		  (jdibug-raise-window win)
		  (goto-line line-number)
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
				(goto-line line-number)
				(if highlight
					(with-current-buffer (window-buffer win)
					  (jdibug-highlight-current-line line-number))))
			(message "JDIbug file %s does not have line number information" file-name))
		(message "JDIbug file %s not in source path" file-name)))))

(defun jdibug-goto-location (location)
  (jdibug-debug "jdibug-goto-location")
  (lexical-let ((location location))
	(cont-bind (signature) (jdi-class-get-signature (jdi-location-class location))
	  (lexical-let ((signature signature))
		(cont-bind (line-number) (jdi-location-get-line-number location)
		  (jdibug-show-file-and-line-number jdibug-this
											(jdibug-class-signature-to-source-file signature)
											line-number
											t))))))

(defun jdibug-handle-breakpoint (thread location)
  (jdibug-debug "jdibug-handle-breakpoint")

  (setf (jdibug-active-thread jdibug-this) thread)

  (if (jdibug-refresh-timer jdibug-this)
	  (cancel-timer (jdibug-refresh-timer jdibug-this)))
  (setf (jdibug-refresh-timer jdibug-this)
		(jdibug-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-now location))
  (run-hooks 'jdibug-breakpoint-hit-hook))

(defun jdibug-handle-step (thread location)
  (jdibug-debug "jdibug-handle-step")

  (setf (jdibug-active-thread jdibug-this) thread)

;;   (let ((class (gethash class-id (jdi-virtual-machine-classes vm))))
;; 	(jdibug-goto-location (jdi-class-find-location class method-id line-code-index)))

  (if (jdibug-refresh-timer jdibug-this)
	  (cancel-timer (jdibug-refresh-timer jdibug-this)))
  (setf (jdibug-refresh-timer jdibug-this)
		(jdibug-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-now location)))

(defun jdibug-handle-change-frame (frame)
  (jdibug-debug "jdibug-handle-change-frame")

  (jdibug-goto-location (jdi-class-find-location (gethash (jdi-location-class-id (jdi-frame-location frame)) 
														  (jdi-virtual-machine-classes (jdi-mirror-virtual-machine frame)))
												 (jdi-location-method-id (jdi-frame-location frame))
												 (jdi-location-line-code-index (jdi-frame-location frame))))

  (if (jdibug-refresh-timer jdibug-this)
	  (cancel-timer (jdibug-refresh-timer jdibug-this)))
  (setf (jdibug-refresh-timer jdibug-this)
		(jdibug-run-with-timer jdibug-refresh-delay nil 
							   'jdibug-refresh-now 
							   (jdi-mirror-virtual-machine frame)
							   (jdi-thread-id (jdi-frame-thread frame))
							   (jdi-class-id (jdi-location-class (jdi-frame-location frame)))
							   (jdi-method-id (jdi-location-method (jdi-frame-location frame)))
							   (jdi-location-line-code-index (jdi-frame-location frame)))))
  
(defun jdibug-refresh-now (&optional location)
  (jdibug-debug "jdibug-refresh-now")
  (lexical-let ((location location))
	(cont-kill (jdibug-refresh-proc jdibug-this))
	(jdibug-time-start)
	(setf (jdibug-refresh-proc jdibug-this)
		  (cont-fork
		   (if location (jdibug-goto-location location))
		   (jdibug-refresh-locals-buffer-now)
		   (jdibug-refresh-frames-buffer-now)))))
  ;; 			   (cont-bind (threads) (jdi-virtual-machine-get-threads vm)
  ;; 				 (if (and thread location)
  ;; 					 (progn
  ;; 					   (jdibug-time-start)
  ;; 					   (cont-bind () (jdi-class-get-signature (jdi-location-class location))
  ;; 						 (jdibug-time-end "jdibug-refresh-now:jdi-class-get-signature")
  ;; 						 (jdibug-time-start)
  ;; 						 (cont-bind () (jdi-location-get-line-number location)
  ;; 						   (jdibug-time-end "jdibug-refresh-now:jdi-location-get-line-number")
  ;; 						   (jdibug-goto-location location)
  ;; 						   (jdibug-time-start)
  ;; 						   (cont-bind () (jdi-thread-get-frames thread)
  ;; 							 (jdibug-time-end "jdibug-refresh-now:jdi-thread-get-frames")
  ;; 							 (when (null (jdi-frame-id (jdibug-active-frame jdibug-this)))
  ;; 							   (setf (jdibug-active-frame jdibug-this) (car (jdi-thread-frames thread))))
  ;; 							 (jdibug-refresh-locals-buffer-now (jdibug-active-frame jdibug-this) location)
  ;; 							 (jdibug-refresh-frames-buffer-now)))))
				   
  ;;				   (jdibug-refresh-frames-buffer-now))))))

(defun jdibug-get-active-frame ()
  (jdibug-debug "jdibug-get-active-frame")
  (if (jdibug-active-frame jdibug-this)
	  (cont-values (jdibug-active-frame jdibug-this))

	(cont-bind (suspended-threads) (cont-mappend 'jdi-virtual-machine-get-suspended-threads (jdibug-virtual-machines jdibug-this))
	  (jdibug-debug "jdibug-get-active-frame:number of suspended-threads:%s" (length suspended-threads))
	  (if suspended-threads
		  (cont-bind (frames) (jdi-thread-get-frames (car suspended-threads))
			(setf (jdibug-active-frame jdibug-this) (car frames))
			(cont-values (jdibug-active-frame jdibug-this)))
		(setf (jdibug-active-frame jdibug-this) nil)
		(cont-values nil)))))

(defun jdibug-handle-class-prepare (class)
  (jdibug-debug "jdibug-handle-class-prepare")
  (lexical-let ((class class)
				(bps-matched-class (loop for bp in (jdibug-breakpoints jdibug-this)
										 if (equal (jdibug-source-file-to-class-signature (jdibug-breakpoint-source-file bp))
												   (jdi-class-signature class))
										 collect bp)))
	(jdibug-debug "bps-matched-class:%s" (length bps-matched-class))
	(when bps-matched-class
	  (cont-bind () (jdi-class-get-all-line-locations class)
		(jdibug-debug "jdibug-handle-class-prepare:after jdi-class-get-all-line-locations")
		(cont-wait 
		  (mapc (lambda (bp)
				  (lexical-let ((bp bp))
					(cont-bind (result) (jdi-virtual-machine-set-breakpoint
										 (jdi-mirror-virtual-machine class)
										 (jdi-class-signature class)
										 (jdibug-breakpoint-line-number bp))
					  (when result
						(push result (jdibug-breakpoint-event-requests bp))
						(setf (jdibug-breakpoint-status bp) 'enabled)
						(jdibug-breakpoint-update bp)))))
				bps-matched-class)
		  (cont-values))))))

(defun jdibug-handle-detach (vm)
  (interactive)
  (if (jdibug-current-line-overlay jdibug-this)
	  (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (mapc (lambda (bp) 
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp))))
		(jdibug-breakpoints jdibug-this))
  (message "JDIbug %s:%s vm detached" (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
  (unless (jdibug-connected-p)
	(run-hooks 'jdibug-detached-hook)))

(defun jdibug-highlight-current-line (line-number)
  (jdibug-debug "jdibug-highlight-current-line:%d:current-buffer=%s" line-number (current-buffer))
  (goto-line line-number)
  (beginning-of-line)
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (setf (jdibug-current-line-overlay jdibug-this) (make-overlay (point) (1+ (line-end-position))))
  (overlay-put (jdibug-current-line-overlay jdibug-this) 'face 'jdibug-current-line)
  (overlay-put (jdibug-current-line-overlay jdibug-this) 'priority 10))

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
  (jdibug-trace "jdibug-expand-method-node")
  (lexical-let* ((tree tree)
				 (value (widget-get tree :jdi-value))
				 (method (widget-get tree :jdi-method)))
	(let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
	  (cont-bind (reply error jdwp id)  (if (jdi-method-static-p method)
											(jdi-method-invoke method)
										  (jdi-value-invoke-method value method))
		(jdibug-debug "type:%s,value:%s" 
					 (bindat-get-field reply :return-value :type)
					 (bindat-get-field reply :return-value :u :value))
		(lexical-let ((value (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
											 ;:name "returns"
											 :type (bindat-get-field reply :return-value :type) 
											 :value (bindat-get-field reply :return-value :u :value))))
		  (cont-bind (result) (jdibug-value-get-string value)
			(jdibug-debug "running method %s returned %s" (jdi-method-name method) (jdi-value-string value))
			(jdibug-tree-set-and-refresh (jdibug-locals-buffer jdibug-this)
										 tree (list (jdibug-make-tree-from-value value))))))))
  (list 
   `(item 
	 :value "loading...")))

(defun jdibug-make-method-node (value method)
  (if (string= "()" (substring (jdi-method-signature method) 0 2))
	  `(tree-widget
		:node (push-button
			   :tag ,(format "%s: %s" (jdi-method-name method) (jdi-method-signature method))
			   :format "%[%t%]\n")
		:open nil
		:jdi-value  ,value
		:jdi-method ,method
		:dynargs jdibug-expand-method-node
		:expander jdibug-expand-method-node)
	`(item 
	  :value 
	  ,(format "%s: %s" (jdi-method-name method) (jdi-method-signature method)))))

(defun jdibug-tree-set-and-refresh (buffer tree args)
  ;; have this run later, so we can call this function in expander, and do not have to worry
  ;; about this executing before we return the "loading..." sign!
  (jdibug-debug "jdibug-tree-set-and-refresh")
  (jdibug-run-with-timer 0 nil 'jdibug-tree-set-and-refresh-now buffer tree args))

(defun jdibug-tree-set-and-refresh-now (buffer tree args)
  (jdibug-debug "jdibug-tree-set-and-refresh-now")
  (progn
	(widget-put tree :args args)
	(widget-put tree :dynargs nil)
	(widget-put tree :expander nil)
	(with-current-buffer buffer
	  (jdibug-tree-mode-reflesh-tree tree))))

(defun jdibug-expand-methods (tree)
  (lexical-let ((tree tree)
				(value (widget-get tree :jdi-value)))
	(let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
	  (cont-bind () (jdi-classes-get-super-r (list (jdi-value-class value)))
		(cont-bind () (jdi-classes-get-methods (jdi-class-all-super (jdi-value-class value)))
		  (jdibug-tree-set-and-refresh (jdibug-locals-buffer jdibug-this)
									   tree (mapcar (lambda (method) 
													  (jdibug-make-method-node value method))
													(sort (remove-duplicates (jdi-class-all-methods (jdi-value-class value))
																			 :test (lambda (obj1 obj2)
																					 (and (equal (jdi-method-name obj1)
																								 (jdi-method-name obj2))
																						  (equal (jdi-method-signature obj1)
																								 (jdi-method-signature obj2))))
																			 :from-end t)
														  (lambda (obj1 obj2)
															(string< (jdi-method-name obj1)
																	 (jdi-method-name obj2))))))))))
  (list 
   `(item 
	 :value "loading...")))


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
  (lexical-let* ((tree tree)
				 (values (widget-get tree :jdi-values))
				 (variables (widget-get tree :jdi-variables))
				 (alist (loop for variable in variables
							  for value in values
							  collect `(,variable . ,value))))
	(let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
	  (cont-bind (strings) (cont-mapcar (lambda (item)
										  (jdibug-value-get-string (car item) (cdr item)))
										alist)
		(jdibug-tree-set-and-refresh (jdibug-locals-buffer jdibug-this)
									 tree 
									 (loop for variable in variables
										   for value in values
										   for string in strings
										   collect (jdibug-make-tree-from-value value (jdi-variable-name variable) string))))))
  (list 
   `(item 
	 :value "loading...")))

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
  (jdibug-debug "jdibug-tree-mode-reflesh-tree")
  (unless tree
	(jdibug-error "tree is null!"))
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
  (jdibug-debug "jdibug-value-expander")
  (jdibug-trace "jdibug-expand-value-node")
  (lexical-let* ((tree tree)
				 (value (widget-get tree :jdi-value)))
	(jdibug-time-start)
	(cond ((= (jdi-value-type value) jdwp-tag-object)
		   (let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
			 (cont-bind (class) (jdi-value-get-class value)
			   (cont-bind (fields) (jdi-class-get-all-fields class)
				 (lexical-let ((fields fields))
				   (cont-bind (values) (jdi-value-get-values value fields)
					 (lexical-let ((values values))
					   (cont-bind (strings) (cont-mapcar (lambda (item)
														   (jdibug-value-get-string (car item) (cdr item)))
														 (loop for field in fields
															   for value in values
															   collect `(,field . ,value)))
						 (jdibug-debug "jdibug-value-expander got %s values" (length values))
						 (jdibug-time-end "expanded")
						 (jdibug-tree-set-and-refresh (jdibug-locals-buffer jdibug-this)
													  tree (append (loop for v in values
																		 for f in fields
																		 for s in strings
																		 collect (jdibug-make-tree-from-value v (jdi-field-name f) s))
																   (list (jdibug-make-methods-node value))))))))))))
		  ((= (jdi-value-type value) jdwp-tag-array)
		   (let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
			 (cont-bind (result) (jdi-value-array-get-values value)
			   (jdibug-time-end "expanded")
			   (jdibug-tree-set-and-refresh (jdibug-locals-buffer jdibug-this)
											tree (mapcar 'jdibug-make-tree-from-value (jdi-value-values value)))))))
	(jdibug-trace "setting into the tree"))

  (list 
   `(item 
	 :value "loading...")))

(defun jdibug-make-tree-from-value (value name string)
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

(defun jdibug-refresh-locals-buffer-now ()
  (condition-case err
	  (cont-bind (frame) (jdibug-get-active-frame)
		(jdibug-debug "jdibug-refresh-locals-buffer-now:active-frame=%s" (if frame (jdi-frame-id frame) "no"))
		(if (null frame)
			(with-current-buffer (jdibug-locals-buffer jdibug-this)
			  (let ((inhibit-read-only t))
				(erase-buffer))
			  (cont-values t))

		  (lexical-let ((frame frame)
						(location (jdi-frame-location frame)))
			(jdibug-debug "jdibug-refresh-locals-buffer-now:type-of frame:%s" (type-of frame))
			(with-current-buffer (jdibug-locals-buffer jdibug-this)
			  (let ((inhibit-read-only t))
				(erase-buffer))
			  (insert "Loading..."))

			(jdibug-time-start)
			(cont-bind (variables) (jdi-frame-get-visible-variables frame)
			  (lexical-let ((variables variables))
				(jdibug-debug "jdi-frame-get-visible-variables returned %s variables" (length variables))
				(cont-bind (values) (jdi-frame-get-values frame variables)
				  (with-current-buffer (jdibug-locals-buffer jdibug-this)
					(let ((inhibit-read-only t))
					  (erase-buffer))
					(setf (jdibug-locals-tree jdibug-this)
						  (tree-mode-insert (jdibug-make-locals-tree variables values)))
					(widget-put (jdibug-locals-tree jdibug-this) :jdibug-opened-path (tree-mode-opened-tree (jdibug-locals-tree jdibug-this)))
					(jdi-info "current opened tree path:%s" (tree-mode-opened-tree (jdibug-locals-tree jdibug-this)))
					(local-set-key "s" 'jdibug-node-tostring)
					(local-set-key "c" 'jdibug-node-classname))
				  (cont-values t)))))))
	(error (jdibug-error "jdibug-refresh-locals-buffer-now:%s" (error-message-string err)))))

(defun jdibug-frame-notify (button &rest ignore)
  (jdibug-debug "jdibug-frame-notify")
  (lexical-let ((button button)
				(frame (widget-get button :jdi-frame)))
	(jdibug-debug "going to class=%s method=%s line-number=%s"
				 (jdi-class-name (jdi-location-class (jdi-frame-location frame)))
				 (jdi-method-name (jdi-location-method (jdi-frame-location frame)))
				 (jdi-location-line-number (jdi-frame-location frame)))
	(lexical-let ((frame-index (position frame (jdi-thread-frames (jdi-frame-thread frame))))
				  (thread (jdi-frame-thread frame)))
	  (jdibug-debug "looking at frame-id:%s frame-index:%s" (jdi-frame-id frame) frame-index)

;;	  (jdibug-refresh-locals-buffer (jdi-frame-thread frame) (jdi-frame-location frame)))
	  ;; we need to resolve again and get the frame id from the index
	  ;; if not, we will get into some invalid frame id errors
	  ;; when doing stack-get-values

	  (setf (jdi-thread-frames thread) nil)
	  (cont-bind () (jdi-thread-get-frames thread)
		(let ((frame (nth frame-index (jdi-thread-frames thread))))
		  (jdibug-debug "after reload frame-id:%s" (jdi-frame-id frame))
		  (setf (jdibug-active-frame jdibug-this) frame)

		  (jdibug-handle-change-frame frame))))
	(jdibug-goto-location (jdi-frame-location frame))))

(defun jdibug-make-frame-value (frame)
  (lexical-let ((frame frame)) 
	(jdibug-debug "jdibug-make-frame-value:frame-id=%s:class-id=%s:method-id=%s" 
				  (jdi-frame-id frame)
				  (jdi-class-id (jdi-location-class (jdi-frame-location frame)))
				  (jdi-method-id (jdi-location-method (jdi-frame-location frame))))
	(cont-bind (class-name) (jdi-class-get-name (jdi-location-class (jdi-frame-location frame)))
	  (lexical-let ((class-name class-name))
		(cont-bind (method-name) (jdi-method-get-name (jdi-location-method (jdi-frame-location frame)))
		  (lexical-let ((method-name method-name))
			(cont-bind (line-number) (jdi-location-get-line-number (jdi-frame-location frame))
			  (cont-values (format "%s.%s:%s" class-name method-name line-number)))))))))

(defun jdibug-make-frame-node (frame)
  (jdibug-debug "jdibug-make-frame-node:frame-id=%s" (jdi-frame-id frame))
  (lexical-let ((frame frame))
	(cont-bind (has-class-source) (jdibug-have-class-source-p (jdi-location-class (jdi-frame-location frame)))
	  (lexical-let ((has-class-source has-class-source))
		(cont-bind (value) (jdibug-make-frame-value frame)
		  (lexical-let ((value value))
			(cont-bind (active-frame) (jdibug-get-active-frame)
			  (jdibug-info "jdibug-make-frame-node:frame-id=%s:active-frame-id=%s" (jdi-frame-id frame) (jdi-frame-id active-frame))
			  (if has-class-source
				  (cont-values `(push-button
								 :value ,value
								 :jdi-frame ,frame
								 :button-face ,(if (equal (jdi-frame-id frame) (jdi-frame-id active-frame)) 'jdibug-current-frame)
								 :notify jdibug-frame-notify
								 :format "%[%t%]\n"))
				(cont-values `(item 
							   :value ,value))))))))))
  
(defun jdibug-make-thread-value (thread)
  (jdibug-debug "jdibug-make-thread-value")
  (lexical-let ((thread thread))
	(cont-bind (name) (jdi-thread-get-name thread)
	  (lexical-let ((name name))
		(cont-bind (status suspend-status) (jdi-thread-get-status thread)
		  (cont-values (format "%s (%s)" name (if (equal suspend-status jdwp-suspend-status-suspended)
												  "Suspended"
												"Running"))))))))

(defun jdibug-make-frames-node (tree)
  (jdibug-debug "jdibug-make-frames-node")
  (lexical-let ((tree tree)
				(thread (widget-get tree :jdi-thread)))
	(cont-bind (suspended) (jdi-thread-get-suspended-p thread)
	  (jdibug-debug "jdibug-make-frames-node:suspended=%s" suspended)
	  (if suspended
		  (cont-bind (frames) (jdi-thread-get-frames thread)
			(jdibug-debug "jdibug-make-frames-node: number of frames = %s" (length frames))
			(cont-bind (nodes) (cont-mapcar 'jdibug-make-frame-node frames)
			  (jdibug-tree-set-and-refresh (jdibug-frames-buffer jdibug-this)
										   tree
										   nodes))))))
  nil)

(defun jdibug-make-thread-tree (thread)
  (jdibug-debug "jdibug-make-thread-tree")
  (lexical-let ((thread thread))
	(cont-bind (value) (jdibug-make-thread-value thread)
	  (jdibug-debug "jdibug-make-thread-tree:value=%s" value)
	  (cont-values `(tree-widget
					 :value ,value
					 :open t
					 :jdi-thread ,thread
					 :args nil
					 :expander jdibug-make-frames-node
					 :dynargs jdibug-make-frames-node)))))

(defun jdibug-make-threads-tree (tree)
  (jdibug-debug "jdibug-make-threads-tree")
  (lexical-let ((tree tree)
				(vm (widget-get tree :jdi-virtual-machine)))
	(cont-bind (threads) (jdi-virtual-machine-get-threads vm)
	  (cont-bind (nodes) (cont-mapcar 'jdibug-make-thread-tree threads)
		(jdibug-debug "jdibug-make-threads-tree: number of nodes = %s" (length nodes))
		(jdibug-tree-set-and-refresh (jdibug-frames-buffer jdibug-this)
									 tree
									 nodes))))
  (jdibug-debug "jdibug-make-threads-tree: return loading")
  (list `(item :value "loading...")))

(defun jdibug-make-virtual-machine-tree (vm)
  `(tree-widget
    :value ,(format "%s:%s" (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
    :open t
	:args nil
	:jdi-virtual-machine ,vm
	:dynargs jdibug-make-threads-tree
	:expander jdibug-make-threads-tree))

(defun jdibug-make-frames-tree ()
  (jdibug-debug "jdibug-make-frames-tree")
  `(tree-widget
	:node (push-button
		   :tag "*"
		   :format "%[%t%]\n")
	:open t
	:args ,(mapcar 'jdibug-make-virtual-machine-tree (jdibug-virtual-machines jdibug-this))))

(defun jdibug-refresh-frames-buffer-now ()
  (jdibug-debug "jdibug-refresh-frames-buffer-now")
  (with-current-buffer (jdibug-frames-buffer jdibug-this)
;; 	(if (jdibug-frames-tree jdibug-this)
;; 		(tree-mode-reflesh-tree (jdibug-frames-tree jdibug-this))
	  (let ((inhibit-read-only t))
		(erase-buffer))
	  (setf (jdibug-frames-tree jdibug-this)
			(tree-mode-insert (jdibug-make-frames-tree)))
	  (tree-mode)))

(defun jdibug-node-tostring ()
  (interactive)
  (lexical-let* ((wid (widget-at))
				 (tree (widget-get wid :parent))
				 (value (widget-get tree :jdi-value))
				 (jdibug (widget-get tree :jdibug)))
    (if (null value)
		(message "not an object")
	  (cont-bind (reply error jdwp id) (jdi-value-invoke-method value "toString" "()Ljava/lang/String;")
		(let ((object-id (bindat-get-field reply :return-value :u :value)))
		  (cont-bind (reply error jdwp id) (jdwp-send-command 
											(jdi-mirror-jdwp value) 
											"string-value" 
											`((:object . ,object-id)))
			(jdibug-trace "tostring-reply:%s" (jdwp-get-string reply :value))
			(message "%s" (jdwp-get-string reply :value))))))))

(defun jdibug-node-classname ()
  (interactive)
  (let* ((wid (widget-at))
		 (tree (widget-get wid :parent))
		 (value (widget-get tree :jdi-value)))
    (if (null value)
		(message "not an object")
	  (message "Class: %s" (jdi-class-signature (jdi-value-class value))))))

(defun jdibug-file-in-source-paths-p (file)
  (jdibug-debug "jdibug-file-in-source-paths-p:%s" file)
  (let ((result (find-if (lambda (sp) 
						   (string-match (expand-file-name sp) file))
						 (if jdibug-use-jde-source-paths
							 (mapcar 
							  (lambda (path)
								(jde-normalize-path path 'jde-sourcepath))
							  jde-sourcepath)
						   jdibug-source-paths))))
	(jdi-debug (if result "found" "not found"))
	result))
		
(defun jdibug-source-file-to-class-signature (source-file)
  "Converts a source file to a JNI class name."
  (let ((buf source-file))
    (mapc (lambda (sp)
			(setf buf (replace-regexp-in-string (expand-file-name sp) "" buf)))
		  (if jdibug-use-jde-source-paths
			  (mapcar 
			   (lambda (path)
				 (jde-normalize-path path 'jde-sourcepath))
			   jde-sourcepath)
			jdibug-source-paths))
    (setf buf (replace-regexp-in-string "^/" "" buf))
    (setf buf (replace-regexp-in-string ".java$" "" buf))
    (setf buf (concat "L" buf ";"))
    (jdibug-trace "jdi-source-to-class-signature : %s -> %s" source-file buf)
    buf))

(defun jdibug-class-signature-to-source-file (class-signature)
  "Converts a JNI class name to source file."
  (jdibug-debug "jdibug-class-signature-to-source-file:%s" class-signature)
  (let ((buf class-signature))
    (setf buf (replace-regexp-in-string "^L" "" buf))
    (setf buf (replace-regexp-in-string ";$" "" buf))
    (setf buf (concat buf ".java"))
    (mapc (lambda (sp)
			(if (file-exists-p (concat (expand-file-name sp) "/" buf))
				(setf buf (concat (expand-file-name sp) "/" buf))))
		  (if jdibug-use-jde-source-paths
			  (mapcar 
			   (lambda (path)
				 (jde-normalize-path path 'jde-sourcepath))
			   jde-sourcepath)
			jdibug-source-paths))
    (jdibug-trace "jdi-class-signature-to-source : %s -> %s" class-signature buf)
    buf))

(defun jdibug-set-breakpoint (bp)
  (jdibug-debug "jdibug-set-breakpoint")
  (lexical-let ((bp bp)
				(source-file (jdibug-breakpoint-source-file bp))
				(line-number (jdibug-breakpoint-line-number bp)))
	(setf (jdibug-breakpoint-status bp) 'unresolved)
	(push bp (jdibug-breakpoints jdibug-this))
	(jdibug-breakpoint-update bp)
	(jdibug-message "JDIbug setting breakpoint...")
	(jdibug-time-start)
	(cont-mapc (lambda (vm)
				 (if (not (jdibug-file-in-source-paths-p source-file))
					 (progn
					   (jdibug-message "file is not in source path" t)
					   (cont-values t))

				   (cont-bind (result) (jdi-virtual-machine-set-breakpoint 
										vm 
										(jdibug-source-file-to-class-signature source-file)
										line-number)
					 (unless (null result)
					   (jdibug-message "done" t)
					   (jdibug-time-end "set-breakpoint")
					   (setf (jdibug-breakpoint-status bp) 'enabled)
					   (setf (jdibug-breakpoint-event-requests bp)
							 (append result (jdibug-breakpoint-event-requests bp)))
					   (jdibug-breakpoint-update bp))
					 (cont-values t))))
			   (jdibug-virtual-machines jdibug-this))))

(defun jdibug-disable-breakpoint (bp)
  (mapc (lambda (er)
		  (jdi-event-request-disable er))
		(jdibug-breakpoint-event-requests bp))
  (setf (jdibug-breakpoint-status bp) 'disabled)
  (jdibug-breakpoint-update bp)
  (message "breakpoint disabled"))

(defun jdibug-enable-breakpoint (bp)
  (mapc (lambda (er)
		  (jdi-event-request-enable er))
		(jdibug-breakpoint-event-requests bp))
  (setf (jdibug-breakpoint-status bp) 'enabled)
  (jdibug-breakpoint-update bp)
  (message "breakpoint enabled"))

(defun jdibug-remove-breakpoint (bp)
  (jdibug-disable-breakpoint bp)
  (setf (jdibug-breakpoints jdibug-this) (delete bp (jdibug-breakpoints jdibug-this)))
  (delete-overlay (jdibug-breakpoint-overlay bp))
  (jdibug-refresh-breakpoints-buffer jdibug-this)
  (message "breakpoint removed"))

(defun jdibug-toggle-breakpoint ()
  (interactive)
  (let* ((current-line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
		 (source-file (buffer-file-name))
		 (line-number (if (string-match jdibug-break-on-class-regexp current-line-text)
						  nil
						(line-number-at-pos)))
		 (bp (find-if (lambda (bp) 
						(and (equal (jdibug-breakpoint-source-file bp) source-file)
							 (equal (jdibug-breakpoint-line-number bp) line-number)))
					  (jdibug-breakpoints jdibug-this))))
	(jdibug-debug "line-number:%s,current-line-text:%s" line-number current-line-text)
	(cond ((and bp (member (jdibug-breakpoint-status bp) (list 'enabled 'unresolved)))
		   (jdibug-disable-breakpoint bp))
		  ((and bp (equal (jdibug-breakpoint-status bp) 'disabled))
		   (jdibug-remove-breakpoint bp))
		  (t 
		   (jdibug-set-breakpoint (make-jdibug-breakpoint :source-file source-file :line-number line-number))))))

(defun jdibug-get-class-line-number ()
  "Get the line number of the class declaration."
  (save-excursion
	(goto-char (point-min))
	(search-forward-regexp jdibug-break-on-class-regexp)
	(line-number-at-pos)))

(defun jdibug-breakpoint-update (bp)
  (jdibug-debug "jdibug-breakpoint-update")
  (jdibug-refresh-breakpoints-buffer jdibug-this)
  (let ((buffer (find-if (lambda (buf)
						   (string= (buffer-file-name buf) (jdibug-breakpoint-source-file bp)))
						 (buffer-list))))
	(if (and buffer (buffer-live-p buffer))
		(with-current-buffer buffer
		  (goto-line (or (jdibug-breakpoint-line-number bp) (jdibug-get-class-line-number)))
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp)))
		  (setf (jdibug-breakpoint-overlay bp) (make-overlay (point) (1+ (line-end-position))))
		  (overlay-put (jdibug-breakpoint-overlay bp) 'priority 5)
		  (overlay-put (jdibug-breakpoint-overlay bp) 'face
					   (cond ((equal (jdibug-breakpoint-status bp) 'enabled)
							  'jdibug-breakpoint-enabled)
							 ((equal (jdibug-breakpoint-status bp) 'unresolved)
							  'jdibug-breakpoint-unresolved)
							 ((equal (jdibug-breakpoint-status bp) 'disabled)
							  'jdibug-breakpoint-disabled)))))))

(defvar jdibug-breakpoints-mode-map nil
  "Local keymap for breakpoints buffer.")

(setq jdibug-breakpoints-mode-map (make-keymap))
(suppress-keymap jdibug-breakpoints-mode-map t)
(define-key jdibug-breakpoints-mode-map "e" 'jdibug-breakpoints-toggle)
(define-key jdibug-breakpoints-mode-map "d" 'jdibug-breakpoints-delete)
(define-key jdibug-breakpoints-mode-map "\C-m" 'jdibug-breakpoints-goto)
(define-key jdibug-breakpoints-mode-map [mouse-1] 'jdibug-breakpoints-goto)

(defun jdibug-breakpoints-toggle ()
  (interactive)
  (let ((bp (get-text-property (point) 'breakpoint)))
	(if (equal (jdibug-breakpoint-status bp) 'enabled)
		(progn
		  (message ":action disable breakpoint")
		  (jdibug-disable-breakpoint bp))
	  (message ":action enable breakpoint")
	  (jdibug-enable-breakpoint bp))))

(defun jdibug-breakpoints-delete ()
  (interactive)
  (message "jdibug-breakpoints-delete"))

(defun jdibug-breakpoints-goto ()
  (interactive)
  (let ((bp (get-text-property (point) 'breakpoint)))
	(if bp
		(jdibug-show-file-and-line-number jdibug-this
										  (jdibug-breakpoint-source-file bp)
										  (jdibug-breakpoint-line-number bp)))))

(defun jdibug-breakpoints-jde-mode-hook ()
  (mapc (lambda (bp)
		  (jdibug-breakpoint-update bp))
		(jdibug-breakpoints jdibug-this)))

(add-hook 'jde-mode-hook 'jdibug-breakpoints-jde-mode-hook)

(defun jdibug-refresh-breakpoints-buffer (jdibug)
  (jdibug-debug "jdibug-refresh-breakpoints-buffer:%s breakpoints" (length (jdibug-breakpoints jdibug)))
  (if (buffer-live-p (jdibug-breakpoints-buffer jdibug))
	  (let ((orig-line (line-number-at-pos)))
		(with-current-buffer (jdibug-breakpoints-buffer jdibug)
		  (let* ((fmt "%s %-40s %-20s")
				 (header (format fmt "  S" "File" "Line"))
				 (inhibit-read-only t))
			(erase-buffer)
			(setq header-line-format header)
			;; the breakpoints are added in reverse order, so in order to display the first one added first, we reverse it here
			(dolist (bp (reverse (jdibug-breakpoints jdibug)))
			  (let ((str (format (concat fmt "\n")
								 (cond ((equal (jdibug-breakpoint-status bp) 'enabled) " E")
									   ((equal (jdibug-breakpoint-status bp) 'unresolved) " P")
									   (t "  "))
								 (jdibug-breakpoint-short-source-file bp) 
								 (or (jdibug-breakpoint-line-number bp)
									 "class"))))
				(insert (propertize str 'breakpoint bp))))))
		(goto-line orig-line))))

(defun jdibug-send-step (depth)
  (jdibug-debug "jdibug-send-step")
  (mapc (lambda (buffer) 
		  (with-current-buffer buffer
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert "not suspended"))))
		(list (jdibug-frames-buffer jdibug-this)))
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (if (null (jdibug-active-thread jdibug-this))
      (message "JDIbug Can not step. Not suspended.")
    (jdi-thread-send-step (jdibug-active-thread jdibug-this) depth)
	(setf (jdibug-active-thread jdibug-this) nil)
	(setf (jdibug-active-frame jdibug-this) nil)))

(defun jdibug-step-over ()
  (interactive)
  (jdibug-send-step jdwp-step-depth-over))

(defun jdibug-step-into ()
  (interactive)
  (jdibug-send-step jdwp-step-depth-into))

(defun jdibug-step-out ()
  (interactive)
  (jdibug-send-step jdwp-step-depth-out))

(defun jdibug-resume ()
  (interactive)
  (mapc (lambda (buffer) 
		  (with-current-buffer buffer
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert "not suspended"))))
		(list (jdibug-frames-buffer jdibug-this)))
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (jdi-thread-resume (jdi-frame-thread (jdibug-active-frame jdibug-this)))
  (run-hooks 'jdibug-resumed-hook)

  (setf (jdibug-suspended-frames jdibug-this) 
		(remove-if (lambda (obj)
					 (and (equal (jdi-thread-id (jdi-frame-thread obj))
								 (jdi-thread-id (jdi-frame-thread (jdibug-active-frame jdibug-this))))
						  (equal (jdi-location-class-id (jdi-frame-location obj))
								 (jdi-location-class-id (jdi-frame-location (jdibug-active-frame jdibug-this))))
						  (equal (jdi-location-method-id (jdi-frame-location obj))
								 (jdi-location-method-id (jdi-frame-location (jdibug-active-frame jdibug-this))))
						  (equal (jdi-location-line-code-index (jdi-frame-location obj))
								 (jdi-location-line-code-index (jdi-frame-location (jdibug-active-frame jdibug-this))))))
																					  
				   (jdibug-suspended-frames jdibug-this)))
  (setf (jdibug-active-frame jdibug-this) (car (jdibug-suspended-frames jdibug-this)))

  (when (jdibug-active-frame jdibug-this)
	(if (jdibug-refresh-timer jdibug-this)
		(cancel-timer (jdibug-refresh-timer jdibug-this)))
	(setf (jdibug-refresh-timer jdibug-this)
		  (jdibug-run-with-timer jdibug-refresh-delay nil 
								 'jdibug-refresh-now 
								 (jdi-mirror-virtual-machine (jdibug-active-frame jdibug-this))
								 (jdi-thread-id (jdi-frame-thread (jdibug-active-frame jdibug-this)))
								 (jdi-location-class-id (jdi-frame-location (jdibug-active-frame jdibug-this)))
								 (jdi-location-method-id (jdi-frame-location (jdibug-active-frame jdibug-this)))
								 (jdi-location-line-code-index (jdi-frame-location (jdibug-active-frame jdibug-this))))))

  (message "JDIbug resumed"))

(defun jdibug-connected-p ()
  (interactive)
  (jdibug-debug "jdibug-connected-p")
  (let ((connected 0))
	(mapc (lambda (vm) 
			(let ((proc (jdwp-process (jdi-virtual-machine-jdwp vm))))
			  (if (and proc
					   (equal (process-status proc) 'open))
				  (incf connected))))
		  (jdibug-virtual-machines jdibug-this))
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
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer jdibug-breakpoints-buffer-name)
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
		(list jdibug-locals-buffer-name jdibug-frames-buffer-name jdibug-breakpoints-buffer-name)))

(add-hook 'jdibug-connected-hook 'jdibug-debug-view)
(add-hook 'jdibug-detached-hook 'jdibug-undebug-view)

;; Until when we have our own minor mode, we put it into jde-mode-map
(when (boundp 'jde-mode-map)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-c] 'jdibug-connect)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-d] 'jdibug-disconnect)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-b] 'jdibug-toggle-breakpoint)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-n] 'jdibug-step-over)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-i] 'jdibug-step-into)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-o] 'jdibug-step-out)
  (define-key jde-mode-map [?\C-c ?\C-c ?\C-r] 'jdibug-resume))

(defun jdibug-value-get-string (field-or-variable value)
  "[ASYNC] get a string to be displayed for a value"
  (jdibug-debug "jdibug-value-get-string:variable-name=%s:type=%s" 
				(jdi-field-or-variable-name field-or-variable)
				(jdi-value-type value))

  (cond ((or (equal (jdi-value-type value) jdwp-tag-int)
			 (equal (jdi-value-type value) jdwp-tag-byte)
			 (equal (jdi-value-type value) jdwp-tag-char)
			 (equal (jdi-value-type value) jdwp-tag-short))
		 (cont-values (format "%d" (jdi-value-value value))))

		((equal (jdi-value-type value) jdwp-tag-long)
		 (cont-values (format "%d" (jdwp-vec-to-int (jdi-value-value value)))))

		((equal (jdi-value-type value) jdwp-tag-float)
		 (cont-values (format "%f" (jdwp-vec-to-float (jdi-value-value value)))))

;; 		((and parent
;; 			  (equal (jdi-value-type parent) jdwp-tag-object)
;; 			  (equal (jdi-value-type value) jdwp-tag-object)
;; 			  (equal (jdi-value-value parent) (jdi-value-value value)))
;; 		 (cont-values "this"))

		((equal (jdi-value-type value) jdwp-tag-boolean)
		 (cont-values (if (= 0 (jdi-value-value value)) "false" "true")))

		((equal (jdi-value-type value) jdwp-tag-void)
		 (cont-values "void"))

		((equal (jdi-value-type value) jdwp-tag-class-object)
		 ;; TODO: put in the class name
		 (cont-values "class"))

		((equal (jdi-value-type value) jdwp-tag-class-loader)
		 (cont-values "class-loader"))

		((equal (jdi-value-type value) jdwp-tag-thread)
		 (cont-values "thread"))

		((equal (jdi-value-type value) jdwp-tag-thread-group)
		 (cont-values "thread-group"))

 		((equal (jdi-value-type value) jdwp-tag-object)
 		 (jdibug-value-get-string-object field-or-variable value))

		((equal (jdi-value-type value) jdwp-tag-string)
		 (jdibug-value-get-string-string value))

  		((equal (jdi-value-type value) jdwp-tag-array)
  		 (jdibug-value-get-string-array value))

		(t 
		 (jdi-error "fixme: do not know how to print value of type:%s" (jdi-value-type value))
		 (cont-values "..."))))

(defun jdibug-value-get-string-object (field-or-variable value)
  (jdibug-debug "jdibug-value-get-string-object:variable-name=%s:type=%s:value=%s" 
				(jdi-field-or-variable-name field-or-variable)
				(jdi-value-type value) (jdi-value-value value))
  (if (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	  (cont-values "null")
	(lexical-let ((field-or-variable field-or-variable)
				  (value value))
	  (cont-bind (class) (jdi-value-get-class value)
		(lexical-let ((class class))
		  (cont-bind (supers) (jdi-class-get-all-super class)
			(lexical-let ((supers supers))
			  (cont-bind (interfaces) (jdi-class-get-all-interfaces class)
				(cont-bind (signatures) (cont-mapcar 'jdi-class-get-signature (cons class (append supers interfaces)))
				  (jdibug-debug "jdibug-value-get-string-object: super signatures=%s" signatures)
				  (let* ((setter (find-if (lambda (custom)
											(member (jdi-class-name-to-class-signature (car custom)) signatures))
										  jdibug-value-custom-set-strings))
						 (setter2 (if setter (cadr setter))))
					(jdibug-debug "jdibug-value-get-string-object: found setter:%s" setter)
					(cond ((stringp setter2)
						   (jdibug-value-custom-set-string-with-method value (jdibug-active-thread jdibug-this) setter2))
						  ((functionp setter2)
						   (funcall setter2 field-or-variable value))
						  (t
						   (cont-bind (signature) (jdi-class-get-signature class)
							 (cont-values (format "%s (id=%s)" 
												  (jdi-class-name (jdi-field-or-variable-signature field-or-variable))
												  (jdwp-vec-to-int (jdi-value-value value)))))))))))))))))

(defun jdibug-value-get-string-array (value)
  (jdibug-debug "jdibug-value-get-string-array")
  (lexical-let ((value value))
	(jdibug-debug "get array-length")
	(cont-values "Array")))
;; 	(cont-bind (reply error jdwp id) (jdwp-send-command 
;; 									  (jdi-mirror-jdwp value) 
;; 									  "array-length" 
;; 									  `((:array-object . ,(jdi-value-value value))))
;; 	  (let ((size (bindat-get-field reply :array-length)))
;; 		(setf (jdi-value-array-length value) (bindat-get-field reply :array-length))
;; 		;;			(setf (jdi-value-has-children-p value) (> size 0))
;; 		(setf (jdi-value-string value) (jdi-value-array-display-string value size))
;; 		(jdi-debug "set jdi-value-string to %s" (jdi-value-string value))
;; 		(cont-values t))))))

(defun jdibug-value-get-string-string (value)
  (lexical-let ((value value))
	(jdibug-debug "jdibug-value-get-string-string")
	(cont-bind (reply error jdwp id)
	  (jdwp-send-command (jdi-mirror-jdwp value) "string-value" `((:object . ,(jdi-value-value value))))

	  (jdibug-debug "jdibug-value-get-string-string:%s:%s" (jdwp-get-string reply :value) (jdi-format-string (jdwp-get-string reply :value)))
	  (cont-values (jdi-format-string (jdwp-get-string reply :value))))))

;;; Customized display and expanders:
(defvar jdibug-value-custom-set-strings nil
  "a list of (class setter) where

class is a string which hold the class name of the object to be matched.
The matching will be done using something like instance of.

setter is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-string of the jdi-value. If setter
is a string, it will be the method that will be invoked on the java object
and the value that is returned is shown.")

(setq jdibug-value-custom-set-strings
      '(("java.lang.Boolean"      "toString")
		("java.lang.Number"       "toString")
		("java.lang.StringBuffer" "toString")
		("java.util.Date"         "toString")
		("java.util.Collection"   jdibug-value-custom-set-string-with-size)
		("java.util.Map"          jdibug-value-custom-set-string-with-size)))

(defvar jdibug-value-custom-expanders nil
  "a list of (instance expander-func) where

instance is a string that is matched with jdi-value-instance-of-p with the 
value

expander-func is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-values of the jdi-value.")

(setq jdibug-value-custom-expanders
      '(("java.util.Collection" jdibug-value-custom-expand-collection)
		("java.util.Map"        jdibug-value-custom-expand-map)))

(defun jdibug-value-custom-set-strings-find (value)
  (let ((element (find-if (lambda (custom)
							(jdi-value-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
						  jdibug-value-custom-set-strings)))
    (if element
		(cadr element))))

(defun jdibug-value-custom-expanders-find (value)
  (let ((element (find-if (lambda (custom)
							(jdi-value-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
						  jdibug-value-custom-expanders)))
    (if element
		(cadr element))))

(defun jdibug-value-custom-set-string-with-method (value thread method-name)
  (jdibug-debug "jdibug-value-custom-set-string-with-method")
  (lexical-let ((value value)
				(thread thread)
				(method-name method-name))
	(cont-bind (class) (jdi-value-get-class value)
	  (cont-bind (methods) (jdi-class-get-all-methods class)
		(let ((method (find-if (lambda (obj)
								 (and (string= (jdi-method-name obj) method-name)
									  (string= (jdi-method-signature obj) "()Ljava/lang/String;")))
							   methods)))
		  (if (null method)
			  (cont-values "setter %s not found" method-name)

			(cont-bind (result-value) (jdi-value-invoke-method value thread method nil nil)
			  (if (equal (jdi-value-value result-value) [0 0 0 0 0 0 0 0])
				  (cont-values "null")

				(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp value) "string-value" 
																	`((:object . ,(jdi-value-value result-value))))
				  (cont-values (jdi-format-string (jdwp-get-string reply :value))))))))))))

(defun jdibug-value-custom-set-string-with-size (field-or-variable value)
  (jdi-trace "jdibug-value-custom-set-string-with-size:%s" (jdi-field-or-variable-name field-or-variable))
  (lexical-let ((field-or-variable field-or-variable)
				(value value))
	(cont-bind (class) (jdi-value-get-class value)
	  (cont-bind (methods) (jdi-class-get-methods class)
		(let ((size-method (find-if (lambda (obj)
									  (equal (jdi-method-name obj)
											 "size"))
									methods)))
		  (if (null size-method)
			  (cont-values (format "%s[nosize]" (jdi-class-name (jdi-field-or-variable-signature field-or-variable))))

			(cont-bind (result-value) (jdi-value-invoke-method value (jdibug-active-thread jdibug-this) size-method nil nil)
			  (cont-values (format "%s[%s]" 
								   (jdi-class-name (jdi-field-or-variable-signature field-or-variable))
								   (bindat-get-field reply :return-value :u :value))))))))))

(defun jdibug-value-custom-expand-collection (value)
  (jdi-debug "jdibug-value-custom-expand-collection")
  (lexical-let ((value value))
	(cont-bind (reply error jdwp id)
	  (jdi-value-invoke-method value "toArray" nil)
	  (let ((array-value (bindat-get-field reply :return-value :u :value)))
		(setf (jdi-value-value value) array-value)
		(jdi-value-array-get-values value)))))

(defun jdibug-value-custom-expand-map (value)
  (jdi-debug "jdibug-value-custom-expand-collection")
  (lexical-let ((value value)
				(keyset-value (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)))
				(values-value (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value))))
	
	(cont-bind (reply error jdwp id)
	  (jdi-value-invoke-method value "keySet" nil)
	  
	  (setf (jdi-value-value keyset-value) (bindat-get-field reply :return-value :u :value))
	  (setf (jdi-value-class keyset-value) (car (gethash "Ljava/util/Set;" (jdi-virtual-machine-classes-by-signature (jdi-mirror-virtual-machine value)))))

	  (cont-bind (reply error jdwp id)
		(jdi-value-invoke-method keyset-value "toArray" nil)

		(let ((key-array (bindat-get-field reply :return-value :u :value)))
		  (setf (jdi-value-name keyset-value) "key  ")
		  (setf (jdi-value-array-length keyset-value) (jdi-value-array-length value))
		  (setf (jdi-value-value keyset-value) key-array)
		  (cont-bind (result) (jdi-value-array-get-values keyset-value)

			(cont-bind (reply error jdwp id) (jdi-value-invoke-method value "values" nil)

			  (setf (jdi-value-value values-value) (bindat-get-field reply :return-value :u :value))
			  (setf (jdi-value-class values-value) (car (gethash "Ljava/util/Collection;" (jdi-virtual-machine-classes-by-signature (jdi-mirror-virtual-machine value)))))

			  (cont-bind (reply error jdwp id) (jdi-value-invoke-method values-value "toArray" nil)
				(let ((values-array (bindat-get-field reply :return-value :u :value)))
				  (setf (jdi-value-name values-value) "value")
				  (setf (jdi-value-array-length values-value) (jdi-value-array-length value))
				  (setf (jdi-value-value values-value) values-array)
				  (cont-bind (result) (jdi-value-array-get-values values-value)

					(setf (jdi-value-values value)
						  (loop for key in (jdi-value-values keyset-value)
								for value in (jdi-value-values values-value)
								append (list key value)))
					(jdi-trace "keys=%s, values=%s" 
							   (length (jdi-value-values keyset-value))
							   (length (jdi-value-values values-value)))
					(cont-values t)))))))))))

(defun jdibug-value-object-get-fields (value)
  (jdibug-debug "jdibug-value-object-get-values")
  (lexical-let ((value value))
	(cont-bind (class) (jdi-value-get-class value)
	  (lexical-let ((class class))
		(cont-bind (supers) (jdi-class-get-all-super class)
		  (lexical-let ((supers supers))
			(cont-bind (interfaces) (jdi-class-get-all-interfaces class)
			  (cont-bind (signatures) (cont-mapcar 'jdi-class-get-signature (cons class (append supers interfaces)))
				(let* ((setter (find-if (lambda (custom)
										  (member (jdi-class-name-to-class-signature (car custom)) signatures))
										jdibug-value-custom-expanders))
					   (setter2 (if setter (cadr setter))))
				  (if setter2
					  (funcall setter2 value)

					(jdi-value-get-all-fields value)))))))))))

(provide 'jdibug)

(defun jdibug-signal-hook (error-symbol data)
  (jdibug-error "jdibug-signal-hook:%s:%s\n%s\n" error-symbol data
				(with-output-to-string (backtrace))))

(defun jdibug-run-with-timer (secs repeat function &rest args)
  (apply 'run-with-timer secs repeat (lambda (function &rest args)
									   (setq signal-hook-function 'jdibug-signal-hook)
									   (apply function args)
									   (setq signal-hook-function nil)) 
		 function args))