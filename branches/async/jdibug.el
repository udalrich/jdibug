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
  locals-tree-opened-path

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
  (jdibug-message "JDIbug connecting... ")
  (jdibug-time-start)
  (setf (jdibug-virtual-machines jdibug-this)
		(mapcar (lambda (connect-host-and-port)
				  (lexical-let* ((host (nth 0 (split-string connect-host-and-port ":")))
								 (port (string-to-number (nth 1 (split-string connect-host-and-port ":"))))
								 (jdwp (make-jdwp))
								 (vm (make-jdi-virtual-machine :host host :port port :jdwp jdwp)))
					(cont-bind (result) (jdi-virtual-machine-connect vm)
					  (jdibug-message (format "%s:%s" host port) t)
					  (if result
						  (progn
							(jdibug-time-end "connect")
							(jdibug-message "(connected)" t)
							(setf (jdibug-threads-buffer jdibug-this)     (get-buffer-create jdibug-threads-buffer-name)
								  (jdibug-locals-buffer jdibug-this)      (get-buffer-create jdibug-locals-buffer-name)
								  (jdibug-frames-buffer jdibug-this)      (get-buffer-create jdibug-frames-buffer-name)
								  (jdibug-breakpoints-buffer jdibug-this) (get-buffer-create jdibug-breakpoints-buffer-name))

;; 							(with-current-buffer (jdibug-frames-buffer jdibug-this)
;; 							  (toggle-read-only 1))
;; 							(dolist (buf (list (jdibug-frames-buffer jdibug-this)
;; 											   (jdibug-locals-buffer jdibug-this)
;; 											   (jdibug-breakpoints-buffer jdibug-this)))
;; 							  (with-current-buffer buf
;; 								(toggle-read-only 1)))

							(lexical-let* ((bps (jdibug-breakpoints jdibug-this))
										   (signatures (loop for bp in bps
															 collect (jdibug-source-file-to-class-signature
																	  (jdibug-breakpoint-source-file bp)))))
							  (setf (jdibug-breakpoints jdibug-this) nil)
							  (cont-bind () (jdi-classes-get-locations (apply 
																		'append 
																		(mapcar 
																		 (lambda (sig)
																		   (gethash sig (jdi-virtual-machine-classes-by-signature vm)))
																		 signatures)))
								(cont-wait (mapc 'jdibug-set-breakpoint bps)
								  (run-hooks 'jdibug-connected-hook)))))
						(jdibug-message "(failed)" t)))
					vm))
				jdibug-connect-hosts)))
  
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
		(jdibug-locals-tree-opened-path jdibug-this) nil

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
  (file-exists-p (jdibug-class-signature-to-source-file (jdi-class-signature class))))

(defun jdibug-find-buffer (file-name)
  "Return the buffer that is viewing this file."
  (catch 'found 
	(dolist (buf (buffer-list))
	  (if (equal file-name (buffer-file-name buf))
		  (throw 'found buf)))))

(defun jdibug-raise-window (window)
  "Raise the frame that is showing this window."
  (jdibug-info "jdibug-raise-window")
  (let ((frame (window-frame window)))
	(make-frame-visible frame)
	(raise-frame frame)
	(select-frame frame)
	(select-window window)))

(defun jdibug-show-file-and-line-number (jdibug file-name line-number &optional highlight)
  "Show the buffer containing the file, or open the file if there are no open buffer for this file.
And position the point at the line number."
  (jdibug-info "jdibug-show-file-and-line-number:%s:%d" file-name line-number)
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
  (jdibug-info "jdibug-goto-location:class=%s" (jdi-class-signature (jdi-location-class location)))
  (jdibug-show-file-and-line-number jdibug-this
									(jdibug-class-signature-to-source-file (jdi-class-signature (jdi-location-class location)))
									(jdi-location-line-number location)
									t))

(defun jdibug-handle-breakpoint (vm thread-id class-id method-id line-code-index)
  (jdibug-info "jdibug-handle-breakpoint")

  (let ((fake-frame (make-jdi-frame :virtual-machine vm
									:thread (make-jdi-thread :virtual-machine vm :id thread-id)
									:location (make-jdi-location :virtual-machine vm
																 :class-id class-id
																 :method-id method-id
																 :line-code-index line-code-index))))
	(unless (jdibug-active-frame jdibug-this)
	  (setf (jdibug-active-frame jdibug-this) fake-frame))
	(setf (jdibug-suspended-frames jdibug-this) (nreverse (cons fake-frame (jdibug-suspended-frames jdibug-this)))))

  (jdibug-info "number of suspended frames=%s" (length (jdibug-suspended-frames jdibug-this)))

  (if (jdibug-refresh-timer jdibug-this)
	  (cancel-timer (jdibug-refresh-timer jdibug-this)))
  (setf (jdibug-refresh-timer jdibug-this)
		(run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-now vm thread-id class-id method-id line-code-index))
  (run-hooks 'jdibug-breakpoint-hit-hook))

(defun jdibug-handle-step (vm thread-id class-id method-id line-code-index)
  (jdibug-info "jdibug-handle-step")

  (let ((fake-frame (make-jdi-frame :virtual-machine vm
									:thread (make-jdi-thread :virtual-machine vm :id thread-id)
									:location (make-jdi-location :virtual-machine vm
																 :class-id class-id
																 :method-id method-id
																 :line-code-index line-code-index))))
	(unless (jdibug-active-frame jdibug-this)
	  (setf (jdibug-active-frame jdibug-this) fake-frame))
	(setf (jdibug-suspended-frames jdibug-this) (nreverse (cons fake-frame (jdibug-suspended-frames jdibug-this)))))

  (let ((class (gethash class-id (jdi-virtual-machine-classes vm))))
	(jdibug-goto-location (jdi-class-find-location class method-id line-code-index)))

  (if (jdibug-refresh-timer jdibug-this)
	  (cancel-timer (jdibug-refresh-timer jdibug-this)))
  (setf (jdibug-refresh-timer jdibug-this)
		(run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-now vm thread-id class-id method-id line-code-index)))

(defun jdibug-handle-change-frame (frame)
  (jdibug-info "jdibug-handle-change-frame")

  (jdibug-goto-location (jdi-class-find-location (gethash (jdi-location-class-id (jdi-frame-location frame)) 
														  (jdi-virtual-machine-classes (jdi-mirror-virtual-machine frame)))
												 (jdi-location-method-id (jdi-frame-location frame))
												 (jdi-location-line-code-index (jdi-frame-location frame))))

  (if (jdibug-refresh-timer jdibug-this)
	  (cancel-timer (jdibug-refresh-timer jdibug-this)))
  (setf (jdibug-refresh-timer jdibug-this)
		(run-with-timer jdibug-refresh-delay nil 
						'jdibug-refresh-now 
						(jdi-mirror-virtual-machine frame)
						(jdi-thread-id (jdi-frame-thread frame))
						(jdi-class-id (jdi-location-class (jdi-frame-location frame)))
						(jdi-method-id (jdi-location-method (jdi-frame-location frame)))
						(jdi-location-line-code-index (jdi-frame-location frame)))))
  
(defun jdibug-refresh-now (vm thread-id class-id method-id line-code-index)
  (jdibug-info "jdibug-refresh-now")
  (lexical-let ((vm vm)
				(thread-id thread-id)
				(class-id class-id)
				(method-id method-id)
				(line-code-index line-code-index)
				(class (gethash class-id (jdi-virtual-machine-classes vm))))
	(cont-kill (jdibug-refresh-proc jdibug-this))
	(setf (jdibug-refresh-proc jdibug-this)
		  (cont-fork
		   (cont-bind () (jdi-class-get-all-line-locations class)
			 (lexical-let ((location (jdi-class-find-location class method-id line-code-index)))
			   (jdibug-goto-location (jdi-class-find-location class method-id line-code-index))
			   (cont-bind () (jdi-virtual-machine-get-all-frames vm)
				 (let ((thread (find-if (lambda (other)
										  (equal (jdi-thread-id other)
												 thread-id))
										(jdi-virtual-machine-suspended-threads vm))))
				   (when (null (jdi-frame-id (jdibug-active-frame jdibug-this)))
					 (setf (jdibug-active-frame jdibug-this) (car (jdi-thread-frames thread))))
				   (jdibug-refresh-locals-buffer-now (jdibug-active-frame jdibug-this) location)
				   (jdibug-refresh-frames-buffer-now)))))))))

(defun jdibug-handle-class-prepare (class)
  (jdibug-info "jdibug-handle-class-prepare")
  (lexical-let ((class class)
				(bps-matched-class (loop for bp in (jdibug-breakpoints jdibug-this)
										 if (equal (jdibug-source-file-to-class-signature (jdibug-breakpoint-source-file bp))
												   (jdi-class-signature class))
										 collect bp)))
	(jdibug-info "bps-matched-class:%s" (length bps-matched-class))
	(when bps-matched-class
	  (cont-bind () (jdi-class-get-all-line-locations class)
		(jdibug-info "jdibug-handle-class-prepare:after jdi-class-get-all-line-locations")
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

(defun jdibug-handle-resolved-breakpoint (jdi resolved-breakpoint)
  (jdibug-info "jdibug-handle-resolved-breakpoint")
  (let ((bp (find-if (lambda (bp)
					   (and (equal (jdibug-breakpoint-source-file bp) (jdi-breakpoint-request-source-file resolved-breakpoint))
							(equal (jdibug-breakpoint-line-number bp) (jdi-breakpoint-request-line-number resolved-breakpoint))))
					 (jdibug-breakpoints jdibug-this))))
    (setf (jdibug-breakpoint-status bp) 'enabled)
    (jdibug-breakpoint-update bp)
    (message "breakpoint-resolved:%s" resolved-breakpoint)))

(defun jdibug-highlight-current-line (line-number)
  (jdibug-info "jdibug-highlight-current-line:%d:current-buffer=%s" line-number (current-buffer))
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

(defun jdibug-make-thread-group-node (jdibug thread-group)
  (let ((child-group-nodes (mapcar (lambda (ctg) (jdibug-make-thread-group-node jdibug ctg))
								   (jdi-thread-group-thread-groups thread-group)))
		(child-thread-nodes (mapcar (lambda (thread) (jdibug-make-thread-node jdibug thread))
									(jdi-thread-group-threads thread-group))))
    `(tree-widget
      :node (push-button
			 :tag 
			 ,(format "%s (%s)" (jdi-thread-group-name thread-group) (jdi-thread-group-id thread-group))
			 :format "%[%t%]\n")
      :open t
      :args ,(append child-group-nodes child-thread-nodes))))

(defun jdibug-make-thread-node (jdibug thread)
  `(item 
    :value 
    ,(format "%s (%s) (%d) (%d)" (jdi-thread-name thread) (jdi-thread-id thread) (jdi-thread-status thread) (jdi-thread-suspend-status thread))))

(defun jdibug-make-threads-tree (jdibug)
  `(tree-widget
    :node (push-button
		   :tag "*"
		   :format "%[%t%]\n")
    :open t
    :args ,(mapcar (lambda (tg) (jdibug-make-thread-group-node jdibug tg)) (jdi-thread-groups (jdibug-jdi jdibug)))))

(defun jdibug-refresh-threads-buffer (jdibug)
  (with-current-buffer (jdibug-threads-buffer jdibug)
    (let ((inhibit-read-only t))
      (erase-buffer))
	(widget-create (jdibug-make-threads-tree jdibug))))

(defun jdibug-expand-method-node (tree)
  (jdibug-trace "jdibug-expand-method-node")
  (lexical-let* ((tree tree)
				 (value (widget-get tree :jdi-value))
				 (method (widget-get tree :jdi-method)))
	(let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
	  (cont-bind (reply error jdwp id)  (if (jdi-method-static-p method)
											(jdi-method-invoke method)
										  (jdi-value-invoke-method value method))
		(jdibug-info "type:%s,value:%s" 
					 (bindat-get-field reply :return-value :type)
					 (bindat-get-field reply :return-value :u :value))
		(lexical-let ((value (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
											 :name "returns"
											 :type (bindat-get-field reply :return-value :type) 
											 :value (bindat-get-field reply :return-value :u :value))))
		  (cont-bind (result) (jdi-value-get-string value)
			(jdibug-info "running method %s returned %s" (jdi-method-name method) (jdi-value-string value))
			(jdibug-locals-tree-set-and-refresh tree (list (jdibug-make-tree-from-value value))))))))
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

(defun jdibug-locals-tree-set-and-refresh (tree args)
  ;; have this run later, so we can call this function in expander, and do not have to worry
  ;; about this executing before we return the "loading..." sign!
  (run-with-timer 0 nil 'jdibug-locals-tree-set-and-refresh-now tree args))

(defun jdibug-locals-tree-set-and-refresh-now (tree args)
  (widget-put tree :args args)
  (widget-put tree :dynargs nil)
  (widget-put tree :expander nil)
  (with-current-buffer (jdibug-locals-buffer jdibug-this)
	(jdibug-tree-mode-reflesh-tree tree)))

(defun jdibug-expand-methods (tree)
  (lexical-let ((tree tree)
				(value (widget-get tree :jdi-value)))
	(let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
	  (cont-bind () (jdi-classes-get-super-r (list (jdi-value-class value)))
		(cont-bind () (jdi-classes-get-methods (jdi-class-all-super (jdi-value-class value)))
		  (jdibug-locals-tree-set-and-refresh tree (mapcar (lambda (method) 
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

(defun jdibug-make-locals-tree (locals)
  `(tree-widget
	:node (push-button
		   :tag "Locals"
		   :format "%[%t%]\n")
	:open t
	:args ,(mapcar 'jdibug-make-tree-from-value locals)))

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
  (let ((path (or (jdibug-tree-mode-find-child-path tree (jdibug-locals-tree-opened-path jdibug-this))
				  (tree-mode-opened-tree tree))))
	(jdibug-info "jdibug-tree-mode-reflesh-tree:tag=%s:path=%s" (widget-get (tree-widget-node tree) :tag) path)
	(jdibug-info "jdibug-tree-mode-reflesh-tree:opened-path=%s" (tree-mode-opened-tree tree))
    (if (widget-get tree :dynargs)
        (widget-put tree :args nil)
      (if (widget-get tree :old-args)
          (widget-put tree :args (widget-get tree :old-args))))
    (widget-value-set tree (widget-value tree))
    (tree-mode-open-tree tree path)))

(defun jdibug-value-expander (tree)
  (jdibug-info "jdibug-value-expander")
  (jdibug-trace "jdibug-expand-value-node")
  (lexical-let* ((tree tree)
				 (value (widget-get tree :jdi-value)))
	(jdibug-trace "Expanding value:%s" (jdi-value-name value))
	(jdibug-time-start)
	(cond ((= (jdi-value-type value) jdwp-tag-object)
		   (let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
			 (cont-bind () (jdi-value-object-get-values value)
			   (jdibug-info "jdibug-value-expander got %s values" (length (jdi-value-values value)))
			   (jdibug-time-end "expanded")
			   (jdibug-locals-tree-set-and-refresh tree (append (mapcar 'jdibug-make-tree-from-value (jdi-value-values value))
																(list (jdibug-make-methods-node value)))))))
		  ((= (jdi-value-type value) jdwp-tag-array)
		   (let ((cont-current-proc-id (jdibug-refresh-proc jdibug-this)))
			 (cont-bind (result) (jdi-value-array-get-values value)
			   (jdibug-time-end "expanded")
			   (jdibug-locals-tree-set-and-refresh tree (mapcar 'jdibug-make-tree-from-value (jdi-value-values value)))))))
	(jdibug-trace "setting into the tree"))

  (list 
   `(item 
	 :value "loading...")))

(defun jdibug-make-tree-from-value (value)
  (if (jdi-value-has-children-p value)
      `(tree-widget
		:node (push-button
			   :tag ,(format "%s: %s" (jdi-value-name value) (jdi-value-string value))
			   :format "%[%t%]\n")
		:open nil
		:args nil
		:jdi-value ,value
		:expander jdibug-value-expander
		:dynargs jdibug-value-expander)
    `(item 
      :value 
      ,(format "%s: %s" (jdi-value-name value) (jdi-value-string value)))))

(defun jdibug-refresh-locals-buffer-now (frame location)
  (condition-case err
	  (progn
		(jdibug-info "jdibug-refresh-locals-buffer-now:type-of frame:%s" (type-of frame))
		(with-current-buffer (jdibug-locals-buffer jdibug-this)
		  (let ((inhibit-read-only t))
			(erase-buffer))
		  (insert "Loading..."))

		(lexical-let ((frame frame)
					  (location location))
		  (jdibug-time-start)
		  (cont-bind (locals) (jdi-frame-get-locals frame location)
			(jdibug-time-end "jdibug-frame-get-locals")
			(jdibug-info "jdi-frame-get-locals returned %s locals" (length locals))
			(with-current-buffer (jdibug-locals-buffer jdibug-this)
			  (let ((inhibit-read-only t))
				(erase-buffer))
			  (if (null (jdibug-locals-tree jdibug-this))
				  (setf (jdibug-locals-tree jdibug-this)
						(tree-mode-insert (jdibug-make-locals-tree locals)))
				(setf (jdibug-locals-tree-opened-path jdibug-this)
					  (tree-mode-opened-tree (jdibug-locals-tree jdibug-this)))
				(jdi-info "current opened tree path:%s" (tree-mode-opened-tree (jdibug-locals-tree jdibug-this)))
				(widget-put (jdibug-locals-tree jdibug-this) 
							:args (mapcar 'jdibug-make-tree-from-value locals))
				(tree-mode-reflesh-tree (jdibug-locals-tree jdibug-this)))
			  (local-set-key "s" 'jdibug-node-tostring)
			  (local-set-key "c" 'jdibug-node-classname))
			(cont-values t))))
	(error (jdibug-error "jdibug-refresh-locals-buffer-now:%s" (error-message-string err)))))

(defun jdibug-frame-notify (button &rest ignore)
  (jdibug-info "jdibug-frame-notify")
  (lexical-let ((button button)
				(frame (widget-get button :jdi-frame)))
	(jdibug-info "going to class=%s method=%s line-number=%s"
				 (jdi-class-name (jdi-location-class (jdi-frame-location frame)))
				 (jdi-method-name (jdi-location-method (jdi-frame-location frame)))
				 (jdi-location-line-number (jdi-frame-location frame)))
	(lexical-let ((frame-index (position frame (jdi-thread-frames (jdi-frame-thread frame))))
				  (thread (jdi-frame-thread frame)))
	  (jdibug-info "looking at frame-id:%s frame-index:%s" (jdi-frame-id frame) frame-index)

;;	  (jdibug-refresh-locals-buffer (jdi-frame-thread frame) (jdi-frame-location frame)))
	  ;; we need to resolve again and get the frame id from the index
	  ;; if not, we will get into some invalid frame id errors
	  ;; when doing stack-get-values

	  (setf (jdi-thread-frames thread) nil)
	  (cont-bind () (jdi-thread-get-frames thread)
		(let ((frame (nth frame-index (jdi-thread-frames thread))))
		  (jdibug-info "after reload frame-id:%s" (jdi-frame-id frame))
		  (setf (jdibug-active-frame jdibug-this) frame)

		  (jdibug-handle-change-frame frame))))
	(jdibug-goto-location (jdi-frame-location frame))))

(defun jdibug-make-frame-node (frame)
  (jdibug-info "jdibug-make-frame-node")
  (if (jdibug-have-class-source-p (jdi-location-class (jdi-frame-location frame)))
	  `(push-button
		:tag ,(format "%s:%s:%s" 
					  (jdi-class-name (jdi-location-class (jdi-frame-location frame)))
					  (jdi-method-name (jdi-location-method (jdi-frame-location frame)))
					  (jdi-location-line-number (jdi-frame-location frame)))
		:jdi-frame ,frame
		:button-face ,(if (equal (jdi-frame-id frame) (jdi-frame-id (jdibug-active-frame jdibug-this))) 'jdibug-current-frame)
		:notify jdibug-frame-notify
		:format "%[%t%]\n")
	`(item 
	  :value 
	  ,(format "%s:%s:%s" 
			   (jdi-class-name (jdi-location-class (jdi-frame-location frame)))
			   (jdi-method-name (jdi-location-method (jdi-frame-location frame)))
			   (jdi-location-line-number (jdi-frame-location frame))))))
  
(defun jdibug-make-thread-tree (thread)
  `(tree-widget
    :node (push-button
		   :tag ,(format "%s" (jdi-thread-name thread))
		   :format "%[%t%]\n")
    :open t
    :args ,(mapcar 'jdibug-make-frame-node (jdi-thread-frames thread))))
  
(defun jdibug-make-virtual-machine-tree (vm)
  `(tree-widget
    :node (push-button
		   :tag ,(format "%s:%s" (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
		   :format "%[%t%]\n")
    :open t
    :args ,(mapcar 'jdibug-make-thread-tree (jdi-virtual-machine-suspended-threads vm))))

(defun jdibug-make-frames-tree ()
  `(tree-widget
	:node (push-button
		   :tag "*"
		   :format "%[%t%]\n")
	:open t
	:args ,(mapcar 'jdibug-make-virtual-machine-tree (jdibug-virtual-machines jdibug-this))))

(defun jdibug-refresh-frames-buffer-now ()
  (jdibug-info "jdibug-refresh-frames-buffer-now")
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

(defun jdibug-find-jdi-for-source-file (jdibug source-file)
  "Return the correct JDI that contains the source file. Currently do not handle same source file for multiple Debuggee!"
  (jdibug-trace "jdibug-find-jdi-for-source-file:%s" source-file)
  (let ((result (find-if (lambda (jdi) 
						   (jdibug-trace "looking for %s in jdi" source-file)
						   (jdi-file-in-source-paths-p jdi source-file))
						 (jdibug-jdi jdibug))))
	(jdibug-trace (if result "found" "not found"))
	result))
			 
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
  (jdibug-info "jdibug-set-breakpoint")
  (lexical-let ((bp bp)
				(source-file (jdibug-breakpoint-source-file bp))
				(line-number (jdibug-breakpoint-line-number bp)))
	(setf (jdibug-breakpoint-status bp) 'unresolved)
	(push bp (jdibug-breakpoints jdibug-this))
	(jdibug-breakpoint-update bp)
	(jdibug-message "JDIbug setting breakpoint...")
	(jdibug-time-start)
	(mapc (lambda (vm)
			(if (not (jdibug-file-in-source-paths-p source-file))
				(jdibug-message "file is not in source path" t)

			  (cont-bind (result) (jdi-virtual-machine-set-breakpoint 
								   vm 
								   (jdibug-source-file-to-class-signature source-file)
								   line-number)
				(unless (null result)
				  (jdibug-message "done" t)
				  (jdibug-time-end "set-breakpoint")
				  (setf (jdibug-breakpoint-status bp) 'enabled)
				  (push result (jdibug-breakpoint-event-requests bp))
				  (jdibug-breakpoint-update bp))
				(cont-values))))
		  (jdibug-virtual-machines jdibug-this))))

(defun jdibug-disable-breakpoint (bp)
  (mapc (lambda (er)
		  (jdi-event-request-disable er))
		(jdibug-breakpoint-event-requests bp))
  (setf (jdibug-breakpoint-status bp) 'disabled)
  (jdibug-breakpoint-update bp)
  (message "breakpoint disabled"))

(defun jdibug-enable-breakpoint (jdibug bp)
  (mapc (lambda (jdi)
		  (jdi-set-breakpoint jdi (jdibug-breakpoint-source-file bp) (jdibug-breakpoint-line-number bp))
		  (setf (jdibug-breakpoint-status bp) 'enabled)
		  (jdibug-breakpoint-update bp)
		  (message "breakpoint enabled"))
		(jdibug-jdi jdibug)))
    
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
	(jdibug-info "line-number:%s,current-line-text:%s" line-number current-line-text)
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
  (jdibug-info "jdibug-breakpoint-update")
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
		  (jdibug-disable-breakpoint jdibug-this bp))
	  (message ":action enable breakpoint")
	  (jdibug-enable-breakpoint jdibug-this bp))))

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
  (jdibug-info "jdibug-refresh-breakpoints-buffer:%s breakpoints" (length (jdibug-breakpoints jdibug)))
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
  (jdibug-info "jdibug-send-step")
  (mapc (lambda (buffer) 
		  (with-current-buffer buffer
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert "not suspended"))))
		(list (jdibug-frames-buffer jdibug-this)))
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (if (null (jdibug-active-frame jdibug-this))
      (message "JDIbug Can not step. Not suspended.")
    (jdi-thread-send-step (jdi-frame-thread (jdibug-active-frame jdibug-this)) depth)
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
		  (run-with-timer jdibug-refresh-delay nil 
						  'jdibug-refresh-now 
						  (jdi-mirror-virtual-machine (jdibug-active-frame jdibug-this))
						  (jdi-thread-id (jdi-frame-thread (jdibug-active-frame jdibug-this)))
						  (jdi-location-class-id (jdi-frame-location (jdibug-active-frame jdibug-this)))
						  (jdi-location-method-id (jdi-frame-location (jdibug-active-frame jdibug-this)))
						  (jdi-location-line-code-index (jdi-frame-location (jdibug-active-frame jdibug-this))))))

  (message "JDIbug resumed"))

(defun jdibug-connected-p ()
  (interactive)
  (jdibug-info "jdibug-connected-p")
  (let ((connected 0))
	(mapc (lambda (vm) 
			(jdibug-info "process-status=%s" (process-status (jdwp-process (jdi-virtual-machine-jdwp vm))))
			(if (equal (process-status (jdwp-process (jdi-virtual-machine-jdwp vm))) 'open)
				(incf connected)))
		  (jdibug-virtual-machines jdibug-this))
	(jdibug-info "connected = %s" connected)
	(if (equal connected 0)
		nil
	  connected)))

(defun jdibug-debug-view ()
  "Change into debug view."
  (interactive)
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

(provide 'jdibug)