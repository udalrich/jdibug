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

(defvar jdibug-current-line-overlay nil)

(defvar jdibug-threads-tree nil)

(defvar jdibug-threads-buffer nil)

(defvar jdibug-breakpoints nil
  "list of jdibug-breakpoint")

(defvar jdibug-breakpoints-buffer nil)

(defvar jdibug-locals-buffer nil
  "buffer the shows a tree of the local variables")

(defvar jdibug-locals-tree nil
  "the variable that hold the tree-widget of the locals buffer")

(defvar jdibug-frames-buffer nil)
(defvar jdibug-frames-tree nil)

(defvar jdibug-refresh-frames-buffer-timer nil)
(defvar jdibug-refresh-locals-buffer-timer nil)

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

(unless (fboundp 'mappend)
  (defun mappend (fn &rest lsts)
	(apply 'append (apply 'mapcar fn lsts))))

(defun jdibug-connect ()
  (interactive)

  (jdibug-debug "jdibug-connect")

  (jdibug-message "JDIbug connecting... ")

  (setq jdibug-threads-buffer     (get-buffer-create jdibug-threads-buffer-name)
		jdibug-locals-buffer      (get-buffer-create jdibug-locals-buffer-name)
		jdibug-frames-buffer      (get-buffer-create jdibug-frames-buffer-name)
		jdibug-breakpoints-buffer (get-buffer-create jdibug-breakpoints-buffer-name)

		jdibug-active-frame       nil
		jdibug-active-thread      nil)

  (with-current-buffer jdibug-breakpoints-buffer
	(use-local-map jdibug-breakpoints-mode-map)
	(toggle-read-only 1))

  (setq jdibug-virtual-machines 
		(mapcar (lambda (connect-host-and-port)
				  (let* ((host (nth 0 (split-string connect-host-and-port ":")))
						 (port (string-to-number (nth 1 (split-string connect-host-and-port ":"))))
						 (jdwp (make-jdwp))
						 (vm (make-jdi-virtual-machine :host host :port port :jdwp jdwp)))
					(let ((result (jdi-virtual-machine-connect vm)))
					  (jdibug-message (format "%s:%s(%s)" host port (if result "connected" "failed")) t)
					  vm)))
				jdibug-connect-hosts))

  (let* ((bps jdibug-breakpoints)
		 (signatures (loop for bp in bps
						   collect (jdibug-source-file-to-class-signature
									(jdibug-breakpoint-source-file bp)))))
	(setq jdibug-breakpoints nil)
	(mapcar 'jdibug-set-breakpoint bps)

	(run-hooks 'jdibug-connected-hook)
	(jdibug-refresh-frames-buffer)))
  
(defun jdibug-disconnect ()
  (interactive)
  (jdibug-trace "jdibug-disconnect")
  (if jdibug-current-line-overlay
	  (delete-overlay jdibug-current-line-overlay))
  (mapc (lambda (bp) 
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp))))
		jdibug-breakpoints)

  (and (timerp jdibug-locals-refresh-timer)
	   (cancel-timer jdibug-locals-refresh-timer))

  (and (timerp jdibug-frames-refresh-timer)
	   (cancel-timer jdibug-frames-refresh-timer))

  (kill-buffer jdibug-locals-buffer)
  (kill-buffer jdibug-frames-buffer)
  (setq jdibug-locals-tree    nil
		jdibug-locals-buffer  nil

		jdibug-frames-tree    nil
		jdibug-frames-buffer  nil

		jdibug-active-frame   nil)

  (jdibug-message "JDIbug disconnecting... ")
  (mapc (lambda (vm)
		  (jdibug-message (format "%s:%s" 
								  (jdi-virtual-machine-host vm) 
								  (jdi-virtual-machine-port vm)) t)
		  (jdi-virtual-machine-disconnect vm)
		  (jdibug-message "(disconnected) " t))
		jdibug-virtual-machines)
  (setq jdibug-virtual-machines nil)
  (run-hooks 'jdibug-detached-hook))

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
  (let ((signature (jdi-class-get-signature (jdi-location-class location))))
	(let ((line-number (jdi-location-get-line-number location)))
	  (jdibug-show-file-and-line-number (jdibug-class-signature-to-source-file signature)
										line-number
										t))))

(defun jdibug-handle-breakpoint (thread location)
  (jdibug-debug "jdibug-handle-breakpoint")

  (unless jdibug-active-frame
	(jdibug-goto-location location)
	(setq jdibug-active-frame (car (jdi-thread-get-frames thread)))
	(jdibug-refresh-locals-buffer)
	(setq jdibug-active-thread thread))

  (jdibug-refresh-frames-buffer)

  (run-hooks 'jdibug-breakpoint-hit-hook))

(defun jdibug-handle-step (thread location)
  (jdibug-debug "jdibug-handle-step")

  (setq jdibug-active-thread thread)

  (jdibug-goto-location location)

  (jdibug-refresh-frames-buffer)

  (unless jdibug-active-frame
	(setq jdibug-active-frame (car (jdi-thread-get-frames jdibug-active-thread)))
	(jdibug-refresh-locals-buffer)))

(defun jdibug-handle-change-frame (frame)
  (jdibug-debug "jdibug-handle-change-frame")

  (jdibug-goto-location (jdi-frame-location frame))
  (jdibug-refresh-frames-buffer)
  (jdibug-refresh-locals-buffer))
  
(defun jdibug-handle-class-prepare (class thread)
  (jdibug-debug "jdibug-handle-class-prepare")
  (dolist (bp jdibug-breakpoints)
	(when (equal (jdibug-breakpoint-source-file bp) (jdibug-class-signature-to-source-file (jdi-class-get-signature class)))
	  (dolist (location (jdi-class-get-locations-of-line class (jdibug-breakpoint-line-number bp)))
		(let ((er (jdi-event-request-manager-create-breakpoint (jdi-virtual-machine-event-request-manager (jdi-mirror-virtual-machine class)) location)))
		  (jdi-event-request-enable er)
		  (push er (jdibug-breakpoint-event-requests bp))))
	  (setf (jdibug-breakpoint-status bp) 'enabled)
	  (jdibug-breakpoint-update bp)
	  (jdibug-refresh-breakpoints-buffer)))
  (jdi-thread-resume thread))

(defun jdibug-handle-detach (vm)
  (interactive)
  (if jdibug-current-line-overlay
	  (delete-overlay jdibug-current-line-overlay))
  (mapc (lambda (bp) 
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp))))
		jdibug-breakpoints)
  (message "JDIbug %s:%s vm detached" (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
  (unless (jdibug-connected-p)
	(run-hooks 'jdibug-detached-hook)))

(defun jdibug-highlight-current-line (line-number)
  (jdibug-debug "jdibug-highlight-current-line:%d:current-buffer=%s" line-number (current-buffer))
  (goto-line line-number)
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
  (jdibug-debug "jdibug-expand-method-node")
  (let ((value (widget-get tree :jdi-value))
		(method (widget-get tree :jdi-method)))
	(let ((result-value (jdi-value-invoke-method value jdibug-active-thread method nil nil)))
	  (let ((result-string (jdibug-value-get-string result-value)))
		(jdibug-debug "running method %s returned %s" (jdi-method-name method) result-string)
		(list (jdibug-make-tree-from-value "result" result-value result-string))))))

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
  (let ((value (widget-get tree :jdi-value)))
	(let ((class (jdi-value-get-class value)))
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
								 (jdi-method-name obj2)))))))))

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
  (let ((value (widget-get tree :jdi-value)))
	(jdibug-debug "jdibug-value-expander:type=%s" (jdi-value-type value))

	;; call the custom expander here, so that the custom expander can 
	;; make a ArrayList look like an array by changing the value-type
	(let* ((expander (find-if (lambda (custom)
								(jdi-value-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
							  jdibug-value-custom-expanders))
		   (expander2 (if expander (cadr expander))))
	  (cond (expander2
			 (funcall expander2 value))
			((= (jdi-value-type value) jdwp-tag-object)
			 (jdibug-value-expander-object value))
			((= (jdi-value-type value) jdwp-tag-array)
			 (jdibug-value-expander-array value))))))

(defun jdibug-value-expander-object (value)
  (let* ((class (jdi-value-get-class value))
		 (fields (sort (copy-sequence (jdi-class-get-all-fields class)) 'jdibug-field-sorter))
		 (values (jdi-value-get-values value fields)))
	(jdibug-debug "jdibug-value-expander got %s values" (length values))
	(append (loop for v in values
				  for f in fields
				  collect (jdibug-make-tree-from-field-value f v))
			(list (jdibug-make-methods-node value)))))

(defun jdibug-value-expander-array (value)
  (let* ((values (jdi-value-array-get-values value))
		 (strings (mapcar 'jdibug-value-get-string values)))
	(loop for v in values
		  for s in strings
		  for i from 0 by 1
		  collect (jdibug-make-tree-from-value (format "[%s]" i) v s))))

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

(defun jdibug-refresh-locals-buffer ()
  (if (timerp jdibug-refresh-locals-buffer-timer)
	  (cancel-timer jdibug-refresh-locals-buffer-timer))
  (setq jdibug-refresh-locals-buffer-timer 
		(jdibug-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-locals-buffer-now)))

(defun jdibug-refresh-locals-buffer-now ()
  (when (null (catch 'jdwp-input-pending
				(with-current-buffer jdibug-locals-buffer
				  (let ((inhibit-read-only t))
					(erase-buffer))

				  (jdibug-debug "jdibug-refresh-locals-buffer-now:active-thread=%s active-frame=%s" 
								(if jdibug-active-thread "yes" "no")
								(if jdibug-active-frame "yes" "no"))
				  (if (null jdibug-active-thread)
					  (insert "Not suspended")

					(let* ((variables (sort (copy-sequence (jdi-frame-get-visible-variables jdibug-active-frame)) 'jdibug-variable-sorter))
						   (values (jdi-frame-get-values jdibug-active-frame variables)))
					  (with-current-buffer jdibug-locals-buffer
						(let ((inhibit-read-only t))
						  (erase-buffer))
						(setq jdibug-locals-tree
							  (tree-mode-insert (jdibug-make-locals-tree variables values)))
						(widget-put jdibug-locals-tree :jdibug-opened-path (tree-mode-opened-tree jdibug-locals-tree))
						(jdi-info "current opened tree path:%s" (tree-mode-opened-tree jdibug-locals-tree))
						(local-set-key "s" 'jdibug-node-tostring)
						(local-set-key "c" 'jdibug-node-classname))))
				  t)))
	(jdibug-debug "jdibug-refresh-frames-buffer-now:input-pending")
	(jdibug-refresh-locals-buffer)))

(defun jdibug-frame-notify (button &rest ignore)
  (jdibug-debug "jdibug-frame-notify")
  (let ((frame (widget-get button :jdi-frame)))
	(jdibug-debug "going to class=%s method=%s line-number=%s"
				 (jdi-class-name (jdi-location-class (jdi-frame-location frame)))
				 (jdi-method-name (jdi-location-method (jdi-frame-location frame)))
				 (jdi-location-line-number (jdi-frame-location frame)))
	(let ((frame-index (position frame (jdi-thread-frames (jdi-frame-thread frame))))
		  (thread (jdi-frame-thread frame)))
	  (jdibug-debug "looking at frame-id:%s frame-index:%s" (jdi-frame-id frame) frame-index)

;;	  (jdibug-refresh-locals-buffer (jdi-frame-thread frame) (jdi-frame-location frame)))
	  ;; we need to resolve again and get the frame id from the index
	  ;; if not, we will get into some invalid frame id errors
	  ;; when doing stack-get-values

	  (setf (jdi-thread-frames thread) nil)
	  (jdi-thread-get-frames thread)
	  (let ((frame (nth frame-index (jdi-thread-frames thread))))
		(jdibug-debug "after reload frame-id:%s" (jdi-frame-id frame))
		(setq jdibug-active-frame frame)

		(jdibug-handle-change-frame frame)))
	(jdibug-goto-location (jdi-frame-location frame))))

(defun jdibug-make-frame-node (frame)
  (jdibug-debug "jdibug-make-frame-node:frame-id=%s" (jdi-frame-id frame))
  (let* ((location (jdi-frame-location frame))
		 (value (format "%s.%s(%s) line: %s" 
						(jdi-class-name (jdi-location-class location))
						(jdi-method-get-name (jdi-location-method location))
						(mapconcat 'identity (cdr (jdi-jni-to-print (jdi-method-get-signature (jdi-location-method location)) t)) ", ")
						(jdi-location-get-line-number location))))
	(jdibug-info "jdibug-make-frame-node:frame-id=%s:active-frame-id=%s" (jdi-frame-id frame) (jdi-frame-id jdibug-active-frame))
	(if (jdibug-have-class-source-p (jdi-location-class (jdi-frame-location frame)))
		`(push-button
		  :value ,value
		  :jdi-frame ,frame
		  :button-face ,(if (equal (jdi-frame-id frame) (jdi-frame-id jdibug-active-frame)) 'jdibug-current-frame)
		  :notify jdibug-frame-notify
		  :format "%[%t%]\n")
	  `(item 
		:value ,value))))
  
(defun jdibug-make-thread-value (thread)
  (jdibug-debug "jdibug-make-thread-value")
  (let ((name (jdi-thread-get-name thread)))
	(multiple-value-bind (status suspend-status) (jdi-thread-get-status thread)
	  (format "%s (%s)" name (if (equal suspend-status jdwp-suspend-status-suspended)
								 "Suspended"
							   "Running")))))

(defun jdibug-make-frames-node (tree)
  (jdibug-debug "jdibug-make-frames-node")
  (let ((thread (widget-get tree :jdi-thread)))
	(let ((suspended (jdi-thread-get-suspended-p thread)))
	  (jdibug-debug "jdibug-make-frames-node:suspended=%s" suspended)
	  (if suspended
		  (let ((frames (jdi-thread-get-frames thread)))
			(jdibug-debug "jdibug-make-frames-node: number of frames = %s" (length frames))
			(mapcar 'jdibug-make-frame-node frames))))))

(defun jdibug-make-thread-tree (thread)
  (jdibug-debug "jdibug-make-thread-tree")
  (let ((value (jdibug-make-thread-value thread)))
	(jdibug-debug "jdibug-make-thread-tree:value=%s" value)
	`(tree-widget
	  :value ,value
	  :open t
	  :jdi-thread ,thread
	  :args nil
	  :expander jdibug-make-frames-node
	  :dynargs jdibug-make-frames-node)))

(defun jdibug-make-threads-tree (tree)
  (jdibug-debug "jdibug-make-threads-tree")
  (mapcar 'jdibug-make-thread-tree 
		  (loop for thread in (jdi-virtual-machine-get-threads (widget-get tree :jdi-virtual-machine))
				unless (jdi-thread-get-system-thread-p thread) collect thread)))

(defun jdibug-make-virtual-machine-tree (vm)
  `(tree-widget
    :value ,(format "%s [%s:%s]" (jdi-virtual-machine-name vm) (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
    :open t
	:args nil
	:jdi-virtual-machine ,vm
	:dynargs jdibug-make-threads-tree
	:expander jdibug-make-threads-tree))

(defun jdibug-make-frames-tree ()
  (jdibug-debug "jdibug-make-frames-tree")
  `(tree-widget
	:node (push-button
		   :tag "Debuggees"
		   :format "%[%t%]\n")
	:open t
	:args ,(mapcar 'jdibug-make-virtual-machine-tree jdibug-virtual-machines)))

(defun jdibug-refresh-frames-buffer ()
  (if (timerp jdibug-refresh-frames-buffer-timer)
	  (cancel-timer jdibug-refresh-frames-buffer-timer))
  (setq jdibug-refresh-frames-buffer-timer 
		(jdibug-run-with-timer jdibug-refresh-delay nil 'jdibug-refresh-frames-buffer-now)))

(defun jdibug-refresh-frames-buffer-now ()
  (jdibug-debug "jdibug-refresh-frames-buffer-now")
  (when (null (catch 'jdwp-input-pending
				(with-current-buffer jdibug-frames-buffer
				  ;; 	(if (jdibug-frames-tree jdibug-this)
				  ;; 		(tree-mode-reflesh-tree (jdibug-frames-tree jdibug-this))
				  (let ((inhibit-read-only t))
					(erase-buffer))
				  (setq jdibug-frames-tree
						(tree-mode-insert (jdibug-make-frames-tree)))
				  (tree-mode)
				  (let ((active-frame-point (jdibug-point-of-active-frame)))
					(when active-frame-point 
					  (goto-char active-frame-point)
					  (set-window-point (get-buffer-window jdibug-frames-buffer t) (point))
					  (forward-line -1)
					  (set-window-start (get-buffer-window jdibug-frames-buffer t) (point))))
				  t)))
	(jdibug-debug "jdibug-refresh-frames-buffer-now:input-pending")
	(jdibug-refresh-frames-buffer)))

(defun jdibug-node-tostring ()
  (interactive)
  (let* ((wid (widget-at))
		 (tree (widget-get wid :parent))
		 (value (widget-get tree :jdi-value)))
    (if (null value)
		(message "not an object")
	  (let ((class (jdi-value-get-class value)))
		(let ((methods (jdi-class-get-all-methods class)))
		  (let ((tostringmethod (find-if (lambda (obj)
										   (and (equal (jdi-method-name obj)
													   "toString")
												(equal (jdi-method-signature obj)
													   "()Ljava/lang/String;")))
										 methods)))
			(if tostringmethod
				(let ((result-value (jdi-value-invoke-method value jdibug-active-thread tostringmethod nil nil)))
				  (let ((result-string (jdibug-value-get-string-string result-value)))
					(message "%s" result-string)))
			  (message "no toString method"))))))))

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
  (let ((source-file (jdibug-breakpoint-source-file bp))
		(line-number (jdibug-breakpoint-line-number bp)))
	(setf (jdibug-breakpoint-status bp) 'unresolved)
	(push bp jdibug-breakpoints)
	(jdibug-breakpoint-update bp)
	(jdibug-message "JDIbug setting breakpoint...")
	(dolist (vm jdibug-virtual-machines)
	  (if (not (jdibug-file-in-source-paths-p source-file))
		  (progn
			(jdibug-message "file is not in source path" t)
			t)

		(let ((result (jdi-virtual-machine-set-breakpoint 
					   vm 
					   (jdibug-source-file-to-class-signature source-file)
					   line-number)))
		  (if (null result)
			  (jdibug-message "pending" t)

			(jdibug-message "done" t)
			(setf (jdibug-breakpoint-status bp) 'enabled)
			(setf (jdibug-breakpoint-event-requests bp)
				  (append result (jdibug-breakpoint-event-requests bp))))
		  (jdibug-breakpoint-update bp))
		t))))

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
  (setq jdibug-breakpoints (delete bp jdibug-breakpoints))
  (delete-overlay (jdibug-breakpoint-overlay bp))
  (jdibug-refresh-breakpoints-buffer)
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
					  jdibug-breakpoints)))
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
  (jdibug-refresh-breakpoints-buffer)
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
		(jdibug-show-file-and-line-number (jdibug-breakpoint-source-file bp)
										  (jdibug-breakpoint-line-number bp)))))

(defun jdibug-breakpoints-jde-mode-hook ()
  (mapc (lambda (bp)
		  (jdibug-breakpoint-update bp))
		jdibug-breakpoints))

(add-hook 'jde-mode-hook 'jdibug-breakpoints-jde-mode-hook)

(defun jdibug-refresh-breakpoints-buffer ()
  (jdibug-debug "jdibug-refresh-breakpoints-buffer:%s breakpoints" (length jdibug-breakpoints))
  (if (buffer-live-p jdibug-breakpoints-buffer)
	  (let ((orig-line (line-number-at-pos)))
		(with-current-buffer jdibug-breakpoints-buffer
		  (let* ((fmt "%s %-40s %-20s")
				 (header (format fmt "  S" "File" "Line"))
				 (inhibit-read-only t))
			(erase-buffer)
			(setq header-line-format header)
			;; the breakpoints are added in reverse order, so in order to display the first one added first, we reverse it here
			(dolist (bp (reverse jdibug-breakpoints))
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
  (if jdibug-current-line-overlay
      (delete-overlay jdibug-current-line-overlay))
  (if (null jdibug-active-thread)
      (message "JDIbug Can not step. Not suspended.")
	(let ((active-thread jdibug-active-thread))
	  ;; we can not clear the active-thread after calling send-step
	  ;; because the send-step command is going to trigger the 
	  ;; handle-step command first, and then return here
	  ;; and we will be clearing it again
	  (setq jdibug-active-thread nil)
	  (setq jdibug-active-frame nil)
	  (jdi-thread-send-step active-thread depth)
	  (jdibug-debug "jdibug-send-step:end"))))

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
  (if jdibug-current-line-overlay
      (delete-overlay jdibug-current-line-overlay))
  (let ((active-thread jdibug-active-thread))
	(setq jdibug-active-thread nil
		  jdibug-active-frame nil)
	(jdi-thread-resume active-thread)
	(run-hooks 'jdibug-resumed-hook)

	(jdibug-refresh-frames-buffer)
	(message "JDIbug resumed")))

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

(defun jdibug-value-get-string (value)
  "[ASYNC] get a string to be displayed for a value"
  (jdibug-debug "jdibug-value-get-string:type=%s" 
				(jdi-value-type value))

  (cond ((or (equal (jdi-value-type value) jdwp-tag-int)
			 (equal (jdi-value-type value) jdwp-tag-byte)
			 (equal (jdi-value-type value) jdwp-tag-char)
			 (equal (jdi-value-type value) jdwp-tag-short))
		 (format "%d" (jdi-value-value value)))

		((equal (jdi-value-type value) jdwp-tag-long)
		 (format "%d" (jdwp-vec-to-int (jdi-value-value value))))

		((equal (jdi-value-type value) jdwp-tag-float)
		 (format "%f" (jdwp-vec-to-float (jdi-value-value value))))

;; 		((and parent
;; 			  (equal (jdi-value-type parent) jdwp-tag-object)
;; 			  (equal (jdi-value-type value) jdwp-tag-object)
;; 			  (equal (jdi-value-value parent) (jdi-value-value value)))
;; 		 "this")

		((equal (jdi-value-type value) jdwp-tag-boolean)
		 (if (= 0 (jdi-value-value value)) "false" "true"))

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
 		 (jdibug-value-get-string-object value))

		((equal (jdi-value-type value) jdwp-tag-string)
		 (jdibug-value-get-string-string value))

  		((equal (jdi-value-type value) jdwp-tag-array)
  		 (jdibug-value-get-string-array value))

		(t 
		 (jdi-error "fixme: do not know how to print value of type:%s" (jdi-value-type value))
		 "...")))

(defun jdibug-value-get-string-object (value)
  (jdibug-debug "jdibug-value-get-string-object:type=%s:value=%s" 
				(jdi-value-type value) (jdi-value-value value))
  (if (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	  "null"

	(let* ((setter (find-if (lambda (custom)
							  (jdi-value-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
							jdibug-value-custom-set-strings))
		   (setter2 (if setter (cadr setter))))
	  (jdibug-debug "jdibug-value-get-string-object: value=%s found-setter:%s" (jdi-value-value value) setter)
	  (cond ((stringp setter2)
			 (jdibug-value-custom-set-string-with-method value jdibug-active-thread setter2))
			((functionp setter2)
			 (funcall setter2 value))
			(t
			 (format "%s (id=%s)" 
					 (jdi-class-name (jdi-value-get-class value))
					 (jdwp-vec-to-int (jdi-value-value value))))))))

(defun jdibug-value-get-string-array (value)
  (jdibug-debug "jdibug-value-get-string-array")
  (let ((length (jdi-value-get-array-length value)))
	(jdi-value-array-display-string value length)))

(defun jdibug-value-get-string-string (value)
  (jdibug-debug "jdibug-value-get-string-string")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp value) "string-value" `((:object . ,(jdi-value-value value))))))

	(jdibug-debug "jdibug-value-get-string-string:%s:%s" (jdwp-get-string reply :value) (jdi-format-string (jdwp-get-string reply :value)))
	(jdi-format-string (jdwp-get-string reply :value))))

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
  (let ((class (jdi-value-get-class value)))
	(let ((methods (jdi-class-get-all-methods class)))
	  (let ((method (find-if (lambda (obj)
							   (and (string= (jdi-method-name obj) method-name)
									(string= (jdi-method-signature obj) "()Ljava/lang/String;")))
							 methods)))
		(if (null method)
			(format "setter %s not found" method-name)

		  (let ((result-value (jdi-value-invoke-method value thread method nil nil)))
			(if (equal (jdi-value-value result-value) [0 0 0 0 0 0 0 0])
				"null"

			  (let ((reply (jdwp-send-command (jdi-mirror-jdwp value) "string-value" 
											  `((:object . ,(jdi-value-value result-value))))))
				(jdi-format-string (jdwp-get-string reply :value))))))))))

(defun jdibug-value-custom-set-string-with-size (value)
  (jdi-debug "jdibug-value-custom-set-string-with-size")
  (let ((result-value (jdi-value-invoke-method value jdibug-active-thread "size" nil nil)))
	(if result-value
		(format "%s[%s]" (jdi-class-name (jdi-value-get-class value)) (jdi-value-value result-value))
	  (format "%s[nosize]" (jdi-class-name (jdi-value-get-class value))))))

(defun jdibug-value-custom-expand-collection (value)
  (jdi-debug "jdibug-value-custom-expand-collection")
  (let ((result-value (jdi-value-invoke-method value jdibug-active-thread "toArray" nil nil)))
	(if result-value
		(jdibug-value-expander-array result-value))))

(defun jdibug-value-custom-expand-map (value)
  (jdi-debug "jdibug-value-custom-expand-collection")
  (let ((keyset-value (jdi-value-invoke-method value jdibug-active-thread "keySet" nil nil))
		(values-value (jdi-value-invoke-method value jdibug-active-thread "values" nil nil)))
	(when (and keyset-value values-value)
	  (let ((keyset-array (jdi-value-invoke-method keyset-value jdibug-active-thread "toArray" nil nil))
			(values-array (jdi-value-invoke-method values-value jdibug-active-thread "toArray" nil nil)))
		(when (and keyset-array values-array)
		  (loop for v in (loop for key in (jdi-value-array-get-values keyset-array)
							   for value in (jdi-value-array-get-values values-array)
							   append (list key value))
				for s = (jdibug-value-get-string v)
				for i from 0 by 1
				collect (jdibug-make-tree-from-value (format "[%s%s]" 
															 (if (= 0 (mod i 2)) "k" "v")
															 (/ i 2))
													 v s)))))))

(defun jdibug-value-object-get-fields (value)
  (jdibug-debug "jdibug-value-object-get-values")
  (let ((class (jdi-value-get-class value)))
	(let ((supers (jdi-class-get-all-super class)))
	  (let ((interfaces (jdi-class-get-all-interfaces class)))
		(let ((signatures (mapcar 'jdi-class-get-signature (cons class (append supers interfaces)))))
		  (let* ((setter (find-if (lambda (custom)
									(member (jdi-class-name-to-class-signature (car custom)) signatures))
								  jdibug-value-custom-expanders))
				 (setter2 (if setter (cadr setter))))
			(if setter2
				(funcall setter2 value)

			  (jdi-value-get-all-fields value))))))))

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

(provide 'jdibug)

(defun jdibug-signal-hook (error-symbol data)
  (jdibug-error "jdibug-signal-hook:%s:%s\n%s\n" error-symbol data
				(with-output-to-string (backtrace))))

(defun jdibug-run-with-timer (secs repeat function &rest args)
  (apply 'run-with-timer secs repeat (lambda (function &rest args)
									   (setq signal-hook-function 'jdibug-signal-hook)
									   (unwind-protect 
										   (apply function args)
										 (setq signal-hook-function nil)))
		 function args))