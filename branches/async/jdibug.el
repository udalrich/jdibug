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

(defstruct jdibug
  jdi   ;; this is now a list of jdi, one for each connection 
  current-jdi ;; this is the one that is suspended from a breakpoint
  current-line-overlay
  threads-tree 
  threads-buffer
  frames-buffer
  breakpoints ;; list of jdibug-breakpoint
  breakpoints-buffer

  ;; buffer the shows a tree of the local variables
  locals-buffer

  ;; the variable that hold the tree-widget of the locals buffer
  locals-tree
  )

(defstruct jdibug-breakpoint
  source-file
  line-number
  status ;; one of 'unresolved 'enabled 'disabled
  overlay)

(defun jdibug-breakpoint-short-source-file (bp)
  (let ((buf (jdibug-breakpoint-source-file bp)))
	(setf buf (replace-regexp-in-string ".*/" "" buf))
	buf))

(defvar jdibug-this (make-jdibug)
  "The current instance of jdibug, we can only have one jdibug running.")

(defvar jdibug-current-message "")

(defun jdibug-message (message &optional append)
  (if append
	  (setf jdibug-current-message (concat jdibug-current-message message))
	(setf jdibug-current-message message))
  (message jdibug-current-message))
  
(defun jdibug-connect ()
  (interactive)
  (jdibug-message "JDIbug connecting... ")
  (setf (jdibug-jdi jdibug-this) nil)
  (jdibug-disconnect)
  (let ((one-connected nil))
	(mapc 
	 (lambda (connect-host-and-port)
	   (jdibug-message connect-host-and-port t)
	   (let* ((host (nth 0 (split-string connect-host-and-port ":")))
			  (port (string-to-number (nth 1 (split-string connect-host-and-port ":"))))
			  (source-paths (if jdibug-use-jde-source-paths
								(mapcar 
								 (lambda (path)
								   (jde-normalize-path path 'jde-sourcepath))
								 jde-sourcepath)
							  jdibug-source-paths))
			  (start-time (current-time)))
				 (let ((invalid-source-paths (loop for sp in source-paths
										   when (not (file-exists-p sp)) 
										   collect sp)))
		   (if invalid-source-paths
			   (message "source paths invalid:%s" invalid-source-paths)
			 (let ((jdi (make-jdi)))
			   (setf (jdibug-jdi jdibug-this)
					 (cons jdi (jdibug-jdi jdibug-this)))
			   (jdi-connect jdi host port source-paths)
			   (if (equal (jdi-get-last-error jdi) 'failed-to-connect)
				   (jdibug-message "(failed) " t)
				 (setf (jdi-breakpoint-handler jdi) 'jdibug-handle-breakpoint)
				 (setf (jdi-step-handler jdi) 'jdibug-handle-step)
				 (setf (jdi-breakpoint-resolved-handler jdi) 'jdibug-handle-resolved-breakpoint)
				 (setf (jdi-detached-handler jdi) 'jdibug-handle-detach)
				 (setf (jdibug-threads-buffer jdibug-this) (get-buffer-create jdibug-threads-buffer-name))
				 (setf (jdibug-locals-buffer jdibug-this) (get-buffer-create jdibug-locals-buffer-name))
				 (setf (jdibug-frames-buffer jdibug-this) (get-buffer-create jdibug-frames-buffer-name))
				 (setf (jdibug-breakpoints-buffer jdibug-this) (get-buffer-create jdibug-breakpoints-buffer-name))
				 (with-current-buffer (jdibug-breakpoints-buffer jdibug-this)
				   (use-local-map jdibug-breakpoints-mode-map)
				   (toggle-read-only 1))
				 (mapc (lambda (buf)
						 (with-current-buffer buf
						   (toggle-read-only 1)))
					   (list jdibug-locals-buffer-name jdibug-threads-buffer-name))
				 ;; TODO			   (jdibug-refresh-threads-buffer jdibug-this)
				 (let ((bps (jdibug-breakpoints jdibug-this)))
				   (setf (jdibug-breakpoints jdibug-this) nil)
				   (mapc (lambda (bp) 
						   (jdibug-set-breakpoint jdibug-this bp))
						 bps))
				 (jdibug-message (format "(%s seconds) " (float-time (time-subtract (current-time) start-time))) t)
				 (setf one-connected t)))))))
	 jdibug-connect-hosts)
	(if one-connected
		(run-hooks 'jdibug-connected-hook))))

(defun jdibug-have-class-source-p (jdibug class)
  (find-if (lambda (jdi)
			 (let ((file-name (jdi-class-signature-to-source jdi (jdi-class-signature class))))
			   (file-exists-p file-name)))
		   (jdibug-jdi jdibug)))

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

(defun jdibug-goto-location (jdibug jdi class location)
  (jdibug-info "jdibug-goto-location:%s:%s" (jdi-class-name class) location)
  (jdibug-show-file-and-line-number jdibug-this
									(jdi-class-signature-to-source jdi (jdi-class-signature class))
									(jdi-location-line-number location)
									t))

(defun jdibug-handle-breakpoint (jdi class location)
  (jdibug-info "jdibug-handle-breakpoint")
  ;; for breakpoints, we do the long operation first
  ;; then only we bring jdibug up and show user the location
  ;; this will give the desired (but false) impression
  ;; that jdibug is fast
  (setf (jdibug-current-jdi jdibug-this) jdi)
  (jdi-resolve-frames jdi (jdi-suspended-thread-id jdi))
  (setf (jdi-current-frame jdi) (car (jdi-frames jdi)))
  (jdibug-refresh-locals-buffer jdibug-this jdi)
  (jdibug-refresh-frames-buffer jdibug-this jdi)
  (message "JDIbug: breakpoint hit:%s" location)
  (jdibug-goto-location jdibug-this jdi class location)
  (run-hooks 'jdibug-breakpoint-hit-hook))

(defun jdibug-handle-step (jdi class location)
  (jdibug-info "jdibug-handle-step")
  ;; but for stepping, the user is already in jdibug
  ;; and might not be interested in the data in the
  ;; locals browser or the frames buffer
  ;; hence we show it faster
  (setf (jdibug-current-jdi jdibug-this) jdi)
  (jdibug-goto-location jdibug-this jdi class location)
  (jdi-resolve-frames jdi (jdi-suspended-thread-id jdi))
  (setf (jdi-current-frame jdi) (car (jdi-frames jdi)))
  (jdibug-refresh-locals-buffer jdibug-this jdi)
  (jdibug-refresh-frames-buffer jdibug-this jdi))

(defun jdibug-handle-detach (jdi)
  (interactive)
  (if (jdibug-current-line-overlay jdibug-this)
	  (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (mapc (lambda (bp) 
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp))))
		(jdibug-breakpoints jdibug-this))
  (message "JDIbug %s:%s vm detached" (jdwp-server (jdi-jdwp jdi)) (jdwp-port (jdi-jdwp jdi)))
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
      :has-children t
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
    :has-children t
    :args ,(mapcar (lambda (tg) (jdibug-make-thread-group-node jdibug tg)) (jdi-thread-groups (jdibug-jdi jdibug)))))

(defun jdibug-refresh-threads-buffer (jdibug)
  (with-current-buffer (jdibug-threads-buffer jdibug)
    (let ((inhibit-read-only t))
      (erase-buffer))
	(widget-create (jdibug-make-threads-tree jdibug))))

(defun jdibug-expand-locals-node (tree)
  "Expander function on the root node in the locals tree."
  (jdibug-trace "jdibug-expand-locals-node:from=%s" (widget-get tree :from))
  (jdibug-trace "jdibug-expand-locals-node2:from=%s" (widget-get (jdibug-locals-tree jdibug-this) :from))
  (let* ((jdibug (widget-get tree :jdibug))
		 (jdi   (jdibug-current-jdi jdibug)))
	(when (jdi-suspended-p jdi)
	  (jdi-locals-refresh jdi)
	  (mapcar (lambda (value) (jdibug-make-value-node jdibug value)) 
			  (jdi-locals jdi)))))

(defun jdibug-expand-value-node (tree)
  "Expander function on any other expandable nodes in the locals tree."
  (jdibug-trace "jdibug-expand-value-node")
  (let* ((value (widget-get tree :jdi-value))
		 (jdibug (widget-get tree :jdibug))
		 (jdi   (jdibug-current-jdi jdibug))
		 (class (jdi-value-class value)))
	(jdibug-trace "Expanding value:%s" value)
	(cond ((= (jdi-value-type value) jdwp-tag-object)
		   (let ((start-time (current-time)))
			 (jdi-class-resolve-parent jdi class)
			 (jdibug-info "resolve parent took %s seconds" (float-time (time-subtract (current-time) start-time)))

			 (jdi-class-resolve-fields jdi class)
			 (jdibug-info "resolve fields took %s seconds" (float-time (time-subtract (current-time) start-time)))

			 (setf (jdi-value-values value) nil)
			 (jdi-value-get-nonstatic-values jdi value)
			 (jdibug-info "get nonstatic nonstatic took %s seconds" (float-time (time-subtract (current-time) start-time)))

			 (jdi-value-get-static-values jdi value)

			 (jdibug-info "get static took %s seconds" (float-time (time-subtract (current-time) start-time)))

			 (let ((expander-func (jdi-value-custom-expanders-find jdi value)))
			   (if expander-func
				   (funcall expander-func jdi value)))

			 (jdibug-info "expanders took %s seconds" (float-time (time-subtract (current-time) start-time)))

			 (jdi-values-resolve jdi (jdi-value-values value) value)
			 (jdibug-info "resolving took %s seconds" (float-time (time-subtract (current-time) start-time)))
			 (jdibug-info "done resolving values")))
		  ((= (jdi-value-type value) jdwp-tag-array)
		   (jdi-value-get-array-values jdi value)
		   (jdibug-trace "jdi-values:%s" (jdi-value-values value))
		   (jdi-values-resolve jdi (jdi-value-values value))))
	(jdibug-trace "setting into the tree")

	(append
	 (mapcar (lambda (value) (jdibug-make-value-node jdibug value)) (jdi-value-values value))
	 (list (jdibug-make-methods-node jdibug value class)))))

(defun jdibug-expand-method-node (tree)
  (jdibug-trace "jdibug-expand-method-node")
  (let* ((jdibug (widget-get tree :jdibug))
		 (jdi    (jdibug-current-jdi jdibug))
		 (value  (widget-get tree :jdi-value))
		 (method (widget-get tree :jdi-method))
		 (class (widget-get tree :jdi-class)))
	(multiple-value-bind (reply error jdwp id)
		(if (jdi-method-static-p method)
			(jdi-class-invoke-method jdi class (jdi-method-name method))
		  (jdi-value-invoke-method jdi value (jdi-method-name method)))
	  (jdibug-info "type:%s,value:%s" 
				   (bindat-get-field reply :return-value :type)
				   (bindat-get-field reply :return-value :u :value))
	  (let ((value (make-jdi-value :name "returns"
								   :type (bindat-get-field reply :return-value :type) 
								   :value (bindat-get-field reply :return-value :u :value))))
		(jdi-value-resolve jdi value)
		(jdibug-info "running method %s returned %s" (jdi-method-name method) (jdi-value-string value))
		(list (jdibug-make-value-node jdibug value))))))

(defun jdibug-make-method-node (jdibug value class method)
  `(tree-widget
	:node (push-button
		   :tag ,(format "%s: %s" (jdi-method-name method) (jdi-method-signature method))
		   :format "%[%t%]\n")
	:open nil
	:has-children t
	:jdi-value  ,value
	:jdi-method ,method
	:jdi-class ,class
	:jdibug ,jdibug
	:dynargs jdibug-expand-method-node
	:expander jdibug-expand-method-node))


(defun jdibug-expand-methods (tree)
  (let* ((jdibug (widget-get tree :jdibug))
		 (jdi   (jdibug-current-jdi jdibug))
		 (value (widget-get tree :jdi-value))
		 (class (widget-get tree :jdi-class)))
	(when (jdi-suspended-p jdi)
	  (jdi-class-resolve-parent jdi class)
	  (jdi-class-resolve-methods jdi class)
	  (mapcar (lambda (method) (jdibug-make-method-node jdibug value class method)) 
			  (jdi-class-all-methods class)))))

(defun jdibug-make-methods-node (jdibug value class)
  `(tree-widget
	:node (push-button
		   :tag "methods"
		   :format "%[%t%]\n")
	:open nil
	:has-children t
	:jdi-value ,value
	:jdi-class ,class
	:jdibug ,jdibug
	:dynargs jdibug-expand-methods
	:expander jdibug-expand-methods))

(defun jdibug-make-value-node (jdibug value)
  (if (jdi-value-has-children-p value)
      `(tree-widget
		:node (push-button
			   :tag ,(format "%s: %s" (jdi-value-name value) (jdi-value-string value))
			   :format "%[%t%]\n")
		:open nil
		:has-children t
		:jdi-value ,value
		:jdibug ,jdibug
		:dynargs jdibug-expand-value-node
		:expander jdibug-expand-value-node)
    `(item 
      :value 
      ,(format "%s: %s" (jdi-value-name value) (jdi-value-string value)))))

(defun jdibug-make-values-tree (jdibug jdi values)
  `(tree-widget
    :node (push-button
		   :tag "Locals"
		   :format "%[%t%]\n")
    :open nil
    :has-children t
	:jdibug ,jdibug
	:dynargs jdibug-expand-locals-node
	:expander jdibug-expand-locals-node))

(defun jdibug-refresh-locals-buffer (jdibug jdi)
  (jdibug-info "jdibug-refresh-locals-buffer")

  (with-current-buffer (jdibug-locals-buffer jdibug)
	(let ((tree (jdibug-locals-tree jdibug)))
	  (if tree
		  ;; after the first time, we do not need to recreate the tree
		  ;; just reopen it
		  (progn
			(jdibug-info "we already have the tree, just reopen it will do")
  			(let ((inhibit-read-only t))
  			  (erase-buffer))
			(tree-mode-reflesh-tree tree))

		;; first time, create the buffer
 		(let ((inhibit-read-only t))
 		  (erase-buffer))
		(local-set-key "s" 'jdibug-node-tostring)
		(local-set-key "c" 'jdibug-node-classname)
		(jdibug-info "making the locals tree")
		(setf (jdibug-locals-tree jdibug)
			  (tree-mode-insert (jdibug-make-values-tree jdibug jdi (jdi-locals jdi))))
		(jdibug-info "1:from=%s" (widget-get (jdibug-locals-tree jdibug) :from))
		;; default to open it the first time 
		(widget-apply-action (jdibug-locals-tree jdibug))))))

(defun jdibug-refresh-frames-buffer (jdibug jdi)
  (jdibug-info "jdibug-refresh-frames-buffer, frames=%d" (length (jdi-frames jdi)))

  (mapc (lambda (buffer) 
		  (with-current-buffer buffer
			(toggle-read-only 1)
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert "refreshing..."))))
		(list (jdibug-frames-buffer jdibug)))

  (jdibug-trace "associate the locations with their classes")
  (dolist (frame (jdi-frames jdi))
	(let* ((location (jdi-frame-location frame))
		   (class (jdi-classes-find-by-id jdi (jdi-location-class-id location))))
	  (setf (jdi-location-class location) class)))

  (jdibug-trace "resolve the methods for all the classes in the location")
  (dolist (frame (jdi-frames jdi))
	(jdi-class-resolve-methods jdi (jdi-location-class (jdi-frame-location frame))))

  (jdibug-trace "associate all the locations with their methods")
  (dolist (frame (jdi-frames jdi))
	(let* ((location (jdi-frame-location frame))
		   (class (jdi-location-class location))
		   (method (jdi-methods-find-by-id class
										   (jdi-location-method-id location))))
	  (setf (jdi-location-method location) method)))

  (jdibug-trace "resolve the locations for all the methods")
  (dolist (frame (jdi-frames jdi))
	(jdi-method-resolve-locations jdi (jdi-location-method (jdi-frame-location frame))))

  ;; display it
  (jdibug-trace "now we output the name of the location")

  (with-current-buffer (jdibug-frames-buffer jdibug)
	(let ((inhibit-read-only t))
	  (erase-buffer)))

  (loop for frame in (jdi-frames jdi)
		for i from 0
		do
		(with-current-buffer (jdibug-frames-buffer jdibug)
		  (let ((inhibit-read-only t))
			(let* ((location (jdi-frame-location frame))
				   (class (jdi-location-class location))
				   (method (jdi-location-method location))
				   (resolved-location (jdi-locations-find-by-line-code-index method
																			 (jdi-location-line-code-index location))))
			  (if (null class) (jdibug-error "failed to find class of location"))
			  (if (null method) (jdibug-error "failed to find method of location"))

			  (if (and resolved-location
					   (jdibug-have-class-source-p jdibug-this class))
				  (insert-button (format "%s.%s():%s"
										 (jdi-class-name class)
										 (jdi-method-name method)
										 (if resolved-location
											 (jdi-location-line-number resolved-location)))
								 'class class
								 'method method
								 'location resolved-location
								 'frame frame
								 'index i
								 'jdi jdi
								 'action (lambda (but) 
										   (let ((class (button-get but 'class))
												 (location (button-get but 'location))
												 (jdi (button-get but 'jdi))
												 (frame (button-get but 'frame))
												 (index (button-get but 'index)))
											 (jdibug-info "clicked on frame index:%d" index)
											 (setf (jdi-frames jdi) nil)
											 ;; we need to resolve again and get the frame id from the index
											 ;; if not, we will get into some invalid frame id errors
											 ;; when doing stack-get-values
											 (jdi-resolve-frames jdi (jdi-suspended-thread-id jdi))
											 (setf (jdi-current-frame jdi) (nth index (jdi-frames jdi)))
											 (jdibug-goto-location jdibug-this jdi class location)
											 (setf (jdi-locals jdi) nil)
											 (setf (jdi-current-location jdi) (jdi-frame-location frame))
											 (jdibug-refresh-locals-buffer jdibug-this jdi)))
								 'follow-link t)
				(insert (format "%s.%s()" (jdi-class-name class) (jdi-method-name method))))
			  (insert "\n"))))))

(defun jdibug-node-tostring ()
  (interactive)
  (let* ((wid (widget-at))
		 (tree (widget-get wid :parent))
		 (value (widget-get tree :jdi-value))
		 (jdibug (widget-get tree :jdibug))
		 (jdi (jdibug-current-jdi jdibug)))
    (if (null value)
		(message "not an object")
	  (multiple-value-bind (reply error jdwp id)
		  (jdi-value-invoke-method jdi value "toString")
		(let ((object-id (bindat-get-field reply :return-value :u :value)))
		  (multiple-value-bind (reply error jdwp id)
			  (jdwp-send-command (jdi-jdwp jdi) "string-value" 
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

(defun jdibug-disconnect ()
  (interactive)
  (jdibug-trace "jdibug-disconnect")
  (if (jdibug-current-line-overlay jdibug-this)
	  (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (mapc (lambda (bp) 
		  (if (jdibug-breakpoint-overlay bp)
			  (delete-overlay (jdibug-breakpoint-overlay bp))))
		(jdibug-breakpoints jdibug-this))
  (setf (jdibug-locals-tree jdibug-this) nil)
  (jdibug-trace "number of debuggee:%d" (length (jdibug-jdi jdibug-this)))
  (jdibug-message "JDIbug disconnecting... ")
  (mapc (lambda (jdi)
		  (let ((jdwp (jdi-jdwp jdi)))
			(jdibug-message (format "%s:%s" (jdwp-server jdwp) (jdwp-port jdwp)) t)
			(if (jdwp-process jdwp)
				(progn
				  (jdi-disconnect jdi)
				  (jdibug-message "(disconnected) " t))
			  (jdibug-message "(not connected) " t))))
		(jdibug-jdi jdibug-this))
  (run-hooks 'jdibug-detached-hook))

(defun jdibug-find-jdi-for-source-file (jdibug source-file)
  "Return the correct JDI that contains the source file. Currently do not handle same source file for multiple Debuggee!"
  (jdibug-trace "jdibug-find-jdi-for-source-file:%s" source-file)
  (let ((result (find-if (lambda (jdi) 
						   (jdibug-trace "looking for %s in jdi" source-file)
						   (jdi-file-in-source-paths-p jdi source-file))
						 (jdibug-jdi jdibug))))
	(jdibug-trace (if result "found" "not found"))
	result))
			 
(defun jdibug-set-breakpoint (jdibug bp)
  (let* ((source-file (jdibug-breakpoint-source-file bp))
		 (line-number (jdibug-breakpoint-line-number bp)))
	(message "JDIbug setting breakpoint...")
	(if (not (jdibug-connected-p))
		;; not connected yet
		(progn
		  (message "JDIbug setting breakpoint...breakpoint will be set when debuggee is connected")
		  (setf (jdibug-breakpoint-status bp) 'unresolved)
		  (push bp (jdibug-breakpoints jdibug))
		  (jdibug-breakpoint-update bp))
	  (let ((status))
		(mapc (lambda (jdi)
				(when (jdi-file-in-source-paths-p jdi source-file)
				  ;; we are connected, go ahead and install the breakpoint
				  (jdi-set-breakpoint jdi source-file line-number)
				  (let ((le (jdi-get-last-error jdi)))
					(cond ((equal le 'no-code-at-line)
						   (unless status 
							 (setf status 'no-code-at-line)
							 (message "JDIbug setting breakpoint...No code at line!")))
						  ((equal le 'not-loaded)
						   (unless status 
							 (setf status 'not-loaded)
							 (message "JDIbug setting breakpoint...breakpoint will be set when class is loaded")
							 (setf (jdibug-breakpoint-status bp) 'unresolved)))
						  (t 
						   (setf status t)
						   (message "JDIbug setting breakpoint...done")
						   (setf (jdibug-breakpoint-status bp) 'enabled)
						   (jdibug-breakpoint-update bp))))))
			  (jdibug-jdi jdibug))
		(when (or status (equal status 'not-loaded))
		  (push bp (jdibug-breakpoints jdibug))
		  (jdibug-breakpoint-update bp))))))

(defun jdibug-disable-breakpoint (jdibug bp)
  (mapc (lambda (jdi)
		  (jdi-clear-breakpoint jdi (jdibug-breakpoint-source-file bp) (jdibug-breakpoint-line-number bp))
		  (setf (jdibug-breakpoint-status bp) 'disabled)
		  (jdibug-breakpoint-update bp)
		  (message "breakpoint disabled"))
		(jdibug-jdi jdibug)))

(defun jdibug-enable-breakpoint (jdibug bp)
  (mapc (lambda (jdi)
		  (jdi-set-breakpoint jdi (jdibug-breakpoint-source-file bp) (jdibug-breakpoint-line-number bp))
		  (setf (jdibug-breakpoint-status bp) 'enabled)
		  (jdibug-breakpoint-update bp)
		  (message "breakpoint enabled"))
		(jdibug-jdi jdibug)))
    
(defun jdibug-remove-breakpoint (jdibug bp)
  (setf (jdibug-breakpoints jdibug) (delete bp (jdibug-breakpoints jdibug)))
  (delete-overlay (jdibug-breakpoint-overlay bp))
  (jdibug-refresh-breakpoints-buffer jdibug)
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
		   (jdibug-disable-breakpoint jdibug-this bp))
		  ((and bp (equal (jdibug-breakpoint-status bp) 'disabled))
		   (jdibug-remove-breakpoint jdibug-this bp))
		  (t 
		   (jdibug-set-breakpoint jdibug-this (make-jdibug-breakpoint :source-file source-file :line-number line-number))))))

(defun jdibug-get-class-line-number ()
  "Get the line number of the class declaration."
  (save-excursion
	(goto-char (point-min))
	(search-forward-regexp jdibug-break-on-class-regexp)
	(line-number-at-pos)))

(defun jdibug-breakpoint-update (bp)
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
  (mapc (lambda (buffer) 
		  (with-current-buffer buffer
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert "not suspended"))))
		(list (jdibug-frames-buffer jdibug-this)))
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (if (null (jdi-suspended-thread-id (jdibug-current-jdi jdibug-this)))
      (message "JDIbug Can not step. Not suspended.")
    (jdi-send-step (jdibug-current-jdi jdibug-this) depth)))

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
  (jdi-resume (jdibug-current-jdi jdibug-this))
  (run-hooks 'jdibug-resumed-hook)
  (message "JDIbug resumed"))

(defun jdibug-connected-p ()
  (interactive)
  (let ((connected 0))
	(mapc (lambda (jdi) 
			(if (jdwp-process (jdi-jdwp jdi))
				(incf connected)))
		  (jdibug-jdi jdibug-this))
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