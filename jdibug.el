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
  :prefix "jdibug")

(defcustom jdibug-connect-host ""
  "Host of the debuggee to connect to."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-connect-port 6001
  "Port of the debuggee to connect to."
  :group 'jdibug
  :type 'integer)

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

(defvar jdibug-connected-hook nil
  "Hook to run when we are connected to the debuggee.")

(defvar jdibug-breakpoint-hit-hook nil
  "Hook to run when a breakpoint is hit.")

(defvar jdibug-detached-hook nil
  "Hook to run when debuggee is detached, either from jdibug-disconnect or from vm-death event.")

(defvar jdibug-resumed-hook nil
  "Hook to run when debuggee is resumed.")

(require 'bindat)
(require 'ado)
(require 'elog)
(require 'jdi)
(require 'tree-mode)
(require 'tree-widget-dynamic)

(elog-make-logger jdibug)

(defstruct jdibug
  (jdi      (make-jdi))
  current-line-overlay
  threads-tree 
  threads-buffer
  locals-buffer
  locals-tree
  frames-buffer
  breakpoints ;; list of jdibug-breakpoint
  breakpoints-buffer

  ;; when the user steps through quickly, do not bother getting the locals from the debuggee
  locals-update-timer)

(defstruct jdibug-breakpoint
  source-file
  line-number
  status ;; one of 'unresolved 'enabled 'disabled
  overlay)

(defun jdibug-breakpoint-class-name (bp)
  (let ((buf (jdibug-breakpoint-source-file bp)))
	(setf buf (replace-regexp-in-string ".*/" "" buf))
	(setf buf (replace-regexp-in-string ".java$" "" buf))
	buf))

(defvar jdibug-this (make-jdibug)
  "The current instance of jdibug, we can only have one jdibug running.")

(defun jdibug-connect ()
  (interactive)
  (message "JDIbug connecting...")
  (lexical-let ((host jdibug-connect-host)
				(port jdibug-connect-port)
				(source-paths (if jdibug-use-jde-source-paths
								  jde-sourcepath
								jdibug-source-paths))
				(start-time (current-time)))
    (let ((invalid-source-paths (loop for sp in source-paths
				      when (not (file-exists-p sp)) 
				      collect sp)))
      (if invalid-source-paths
		  (message "source paths invalid:%s" invalid-source-paths)
		(lexical-let ((jdi (jdibug-jdi jdibug-this)))
		  (ado (start-time)
			(if (jdibug-connected-p)
				(condition-case err
					(jdibug-disconnect)))
			(jdi-connect (jdibug-jdi jdibug-this) host port source-paths)
			(if (equal (jdi-get-last-error jdi) 'failed-to-connect)
				(message "JDIbug connecting...failed to connect to %s:%d" host port)
			  (setf (jdi-breakpoint-handler jdi) 'jdibug-handle-breakpoint)
			  (setf (jdi-step-handler jdi) 'jdibug-handle-step)
			  (setf (jdi-breakpoint-resolved-handler jdi) 'jdibug-handle-resolved-breakpoint)
			  (setf (jdi-detached-handler jdi) 'jdibug-handle-detach)
			  (setf (jdibug-threads-buffer jdibug-this) (get-buffer-create jdibug-threads-buffer-name))
			  (setf (jdibug-locals-buffer jdibug-this) (get-buffer-create jdibug-locals-buffer-name))
			  (setf (jdibug-frames-buffer jdibug-this) (get-buffer-create jdibug-frames-buffer-name))
			  (setf (jdibug-breakpoints-buffer jdibug-this) (get-buffer-create jdibug-breakpoints-buffer-name))
			  (with-current-buffer (jdibug-breakpoints-buffer jdibug-this)
				(toggle-read-only 1))
			  (jdibug-refresh-threads-buffer jdibug-this)
  			  (message "JDIbug connecting...done in %s seconds"
  					   (float-time (time-subtract (current-time) start-time)))
			  (run-hooks 'jdibug-connected-hook))))))))

(defun jdibug-have-class-source-p (jdibug class)
  (let ((file-name (jdi-class-signature-to-source (jdibug-jdi jdibug-this) (jdi-class-signature class))))
    (file-exists-p file-name)))

(defun jdibug-find-buffer (file-name)
  "Return the buffer that is viewing this file."
  (catch 'found 
	(dolist (buf (buffer-list))
	  (if (equal file-name (buffer-file-name buf))
		  (throw 'found buf)))))

(defun jdibug-goto-location (jdibug class location)
  (jdibug-info "jdibug-goto-location:%s:%s" (jdi-class-name class) location)
  (let* ((jdi (jdibug-jdi jdibug-this))
	 (file-name (jdi-class-signature-to-source jdi (jdi-class-signature class)))
	 (line-number (jdi-location-line-number location))
	 (buffer-name (jdibug-find-buffer file-name))
	 (win (get-buffer-window buffer-name t)))
    (jdibug-info "going to file-name:%s buffer-name:%s" file-name buffer-name)
    (if win
	(let ((frame (window-frame win)))
	  (jdibug-info "raise-frame")
	  (make-frame-visible frame)
	  (raise-frame frame)
	  (select-frame frame)
	  (jdibug-info "select-window")
	  (select-window win)
	  (with-current-buffer buffer-name
	    (jdibug-highlight-current-line jdi line-number)))
      (if (file-exists-p file-name)
	  (if line-number
	      (progn
		(if (window-dedicated-p (get-buffer-window (current-buffer)))
		    (find-file-other-window file-name)
		  (find-file file-name))
		(with-current-buffer buffer-name
		  (jdibug-highlight-current-line jdi line-number)))
	    (message "JDIbug class %s does not have line number information" (jdi-class-name class)))
	(message "JDIbug file %s not in source path" file-name)))))

(defun jdibug-handle-breakpoint (jdi class location)
  (jdibug-info "jdibug-handle-breakpoint")
  (jdibug-refresh-locals-buffer jdibug-this)
  (message "JDIbug: breakpoint hit:%s" location)
  (jdibug-goto-location jdibug-this class location)
  (run-hooks 'jdibug-breakpoint-hit-hook))

(defun jdibug-handle-step (jdi class location)
  (jdibug-info "jdibug-handle-step")
  (jdibug-goto-location jdibug-this class location)
  (jdibug-refresh-locals-buffer jdibug-this))

(defun jdibug-handle-detach (jdi)
  (interactive)
  (ado ()
    (if (jdibug-current-line-overlay jdibug-this)
	(delete-overlay (jdibug-current-line-overlay jdibug-this)))
    (mapc (lambda (bp) 
	    (if (jdibug-breakpoint-overlay bp)
		(delete-overlay (jdibug-breakpoint-overlay bp))))
	  (jdibug-breakpoints jdibug-this))
    (setf (jdibug-breakpoints jdibug-this) nil)
    (message "JDIBUG vm detached")
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

(defun jdibug-highlight-current-line (jdi line-number)
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
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (tree-mode)
    (tree-mode-insert (jdibug-make-threads-tree jdibug))))

(defun jdibug-expand-locals-node (tree)
  "Expander function on the root node in the locals tree."
  (jdibug-trace "jdibug-expand-locals-node")
  (let* ((jdibug (widget-get tree :jdibug))
		 (jdi   (jdibug-jdi jdibug)))
	(ado (jdibug jdi tree)
	  (jdi-locals-refresh jdi)
	  (with-current-buffer (jdibug-locals-buffer jdibug)
		(widget-put tree :args nil)
		(widget-apply-action tree)

		(widget-put tree :args 
					(mapcar (lambda (value) (jdibug-make-value-node jdibug value)) 
							(jdi-locals jdi)))

		;; open the tree back again
		(widget-apply-action tree)
		(tree-widget-dynamic-reopen (jdibug-locals-tree jdibug)))))
  (list `(item :value "loading...")))

(defun jdibug-expand-value-node (tree)
  "Expander function on any other expandable nodes in the locals tree."
  (jdibug-trace "jdibug-expand-value-node")
  (let* ((value (widget-get tree :jdi-value))
	 (jdibug (widget-get tree :jdibug))
	 (jdi   (jdibug-jdi jdibug))
	 (class (jdi-value-class value)))
    (ado (jdibug jdi tree class value)
      (jdibug-trace "Expanding value:%s" value)
      (cond ((= (jdi-value-type value) jdwp-tag-object)
	     (ado (jdi value class)
	       (jdi-class-resolve-parent jdi class)
	       (jdi-class-resolve-fields jdi class)
	       (setf (jdi-value-values value) nil)
	       (jdi-value-get-nonstatic-values jdi value)
	       (jdi-value-get-static-values jdi value)

	       (let ((expander-func (jdi-value-custom-expanders-find jdi value)))
		 (if expander-func
		     (funcall expander-func jdi value)))

	       (jdi-values-resolve jdi (jdi-value-values value))
	       (jdibug-info "done resolving values")))
	    ((= (jdi-value-type value) jdwp-tag-array)
	     (ado (jdi value)
	       (jdi-value-get-array-values jdi value)
	       (jdibug-trace "jdi-values:%s" (jdi-value-values value))
	       (jdi-values-resolve jdi (jdi-value-values value)))))
      (jdibug-trace "setting into the tree")

      ;; clear the "loading" child and close it
	  (with-current-buffer (jdibug-locals-buffer jdibug)
		(widget-put tree :args nil)
		(widget-apply-action tree)

		(widget-put tree :args 
					(mapcar (lambda (value) (jdibug-make-value-node jdibug value)) (jdi-value-values value)))

		(jdibug-trace "jdi-value-values:%s" (jdi-value-values value))

		(unless (jdi-value-values value)
		  (widget-put tree :args
					  (list `(item :value "empty"))))

		;; open the tree back again
		(widget-apply-action tree)
		(tree-widget-dynamic-reopen (jdibug-locals-tree jdibug)))))
  ;; we will get the values later, now you have to live with the empty list
  (jdibug-trace "returning loading sign")
  (list `(item :value "loading...")))

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
	:expander jdibug-expand-value-node)
    `(item 
      :value 
      ,(format "%s: %s" (jdi-value-name value) (jdi-value-string value)))))

(defun jdibug-make-values-tree (jdibug values)
  `(tree-widget-dynamic
    :node (push-button
	   :tag "Locals"
	   :format "%[%t%]\n")
    :open nil
    :has-children t
	:jdibug ,jdibug
	:expander jdibug-expand-locals-node))

(defun jdibug-refresh-locals-buffer (jdibug)
  (jdibug-info "jdibug-refresh-locals-buffer")
  (if (jdibug-locals-update-timer jdibug)
      (cancel-timer (jdibug-locals-update-timer jdibug)))
  (mapc (lambda (buffer) 
 		  (with-current-buffer buffer
 			(toggle-read-only 1)
 			(let ((inhibit-read-only t))
 			  (erase-buffer)
 			  (insert "refreshing..."))))
 		(list (jdibug-frames-buffer jdibug)))
  (jdibug-refresh-locals-buffer-now jdibug))


;;   (setf (jdibug-locals-update-timer jdibug)
;; 		(run-with-idle-timer 1 nil 'jdibug-refresh-locals-buffer-now jdibug)))

(defun jdibug-refresh-locals-buffer-now (jdibug)
  (jdibug-info "jdibug-refresh-locals-buffer-now")
  (ado (jdibug)
    (with-current-buffer (jdibug-locals-buffer jdibug)
	  (let ((tree (jdibug-locals-tree jdibug)))
		(if tree
			(progn
			  (let ((inhibit-read-only t))
				(erase-buffer))
			  (tree-widget-dynamic-refresh tree))
		  (kill-all-local-variables)
		  (let ((inhibit-read-only t))
			(erase-buffer))
		  (tree-mode)
		  (local-set-key "s" 'jdibug-node-tostring)
		  (jdibug-info "making the locals tree")
		  (setf (jdibug-locals-tree jdibug)
				(tree-mode-insert (jdibug-make-values-tree jdibug (jdi-locals (jdibug-jdi jdibug)))))
		  ;; default to open it the first time 
		  (widget-apply-action (jdibug-locals-tree jdibug)))

		(jdibug-refresh-frames-buffer jdibug)
		(jdibug-info "jdibug-refresh-locals-buffer-now done")))))

(defun jdibug-refresh-frames-buffer (jdibug)
  (jdibug-info "jdibug-refresh-frames-buffer, frames=%d" (length (jdi-frames (jdibug-jdi jdibug-this))))
  (let ((jdi (jdibug-jdi jdibug-this)))
    (ado (jdibug jdi)
      (jdi-resolve-frames jdi (jdi-suspended-thread-id jdi))
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

      (dolist (frame (jdi-frames (jdibug-jdi jdibug-this)))
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
				 'action (lambda (but) 
					   (jdibug-goto-location jdibug-this 
								 (button-get but 'class)
								 (button-get but 'location)))
				 'follow-link t)
		(insert (format "%s.%s()" (jdi-class-name class) (jdi-method-name method))))
	      (insert "\n"))))))))

(defun jdibug-node-tostring ()
  (interactive)
  (let* ((wid (widget-at))
	 (tree (widget-get wid :parent))
	 (value (widget-get tree :jdi-value))
	 (jdi (jdibug-jdi jdibug-this)))
    (if (null value)
	(message "not an object")
      (ado (jdi value)
	(jdi-value-invoke-method jdi value "toString")
	(let ((object-id (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
	  (ado (jdi value object-id) 
	   (jdwp-send-command (jdi-jdwp jdi) "string-value" 
			      `((:object . ,object-id)))
	   (let ((reply (car ado-last-return-value)))
	     (jdibug-trace "tostring-reply:%s" (jdwp-get-string reply :value))
	     (message "%s" (jdwp-get-string reply :value)))))))))

(defun jdibug-disconnect ()
  (interactive)
  (ado ()
    (if (jdibug-current-line-overlay jdibug-this)
	(delete-overlay (jdibug-current-line-overlay jdibug-this)))
    (mapc (lambda (bp) 
	    (if (jdibug-breakpoint-overlay bp)
		(delete-overlay (jdibug-breakpoint-overlay bp))))
	  (jdibug-breakpoints jdibug-this))
    (setf (jdibug-breakpoints jdibug-this) nil)
	(setf (jdibug-locals-tree jdibug-this) nil)
    (if (not (jdibug-connected-p))
	(message "JDIbug already disconnected")
      (jdi-disconnect (jdibug-jdi jdibug-this))
      (message "JDIBUG disconnected")
      (run-hooks 'jdibug-detached-hook))))

(defun jdibug-set-breakpoint (jdibug bp)
  (let ((source-file (jdibug-breakpoint-source-file bp))
		(line-number (jdibug-breakpoint-line-number bp))
		(jdi         (jdibug-jdi jdibug)))
    (message "JDIbug setting breakpoint...")
    (ado (jdibug jdi source-file line-number bp)
      (if (not (jdi-file-in-source-paths-p jdi source-file))
		  (message "JDIbug setting breakpoint...file not in source path!")
		(jdi-set-breakpoint jdi source-file line-number)
		(let ((le (jdi-get-last-error jdi)))
		  (cond ((equal le 'no-code-at-line)
				 (message "JDIbug setting breakpoint...No code at line!"))
				((equal le 'not-loaded)
				 (message "JDIbug setting breakpoint...breakpoint will be set when class is loaded")
				 (setf (jdibug-breakpoint-status bp) 'unresolved)
				 (push bp (jdibug-breakpoints jdibug))
				 (jdibug-breakpoint-update bp))
				(t 
				 (message "JDIbug setting breakpoint...done")
				 (setf (jdibug-breakpoint-status bp) 'enabled)
				 (push bp (jdibug-breakpoints jdibug))
				 (jdibug-breakpoint-update bp))))))))


(defun jdibug-disable-breakpoint (jdibug bp)
  (ado (jdibug bp)
    (jdi-clear-breakpoint (jdibug-jdi jdibug) (jdibug-breakpoint-source-file bp) (jdibug-breakpoint-line-number bp))
    (setf (jdibug-breakpoint-status bp) 'disabled)
    (jdibug-breakpoint-update bp)
    (message "breakpoint disabled")))

(defun jdibug-enable-breakpoint (jdibug bp)
  (ado (jdibug bp)
    (jdi-set-breakpoint (jdibug-jdi jdibug) (jdibug-breakpoint-source-file bp) (jdibug-breakpoint-line-number bp))
    (setf (jdibug-breakpoint-status bp) 'enabled)
    (jdibug-breakpoint-update bp)
    (message "breakpoint enabled")))
  
(defun jdibug-remove-breakpoint (jdibug bp)
  (setf (jdibug-breakpoints jdibug) (delete bp (jdibug-breakpoints jdibug)))
  (delete-overlay (jdibug-breakpoint-overlay bp))
  (jdibug-refresh-breakpoints-buffer jdibug)
  (message "breakpoint removed"))

(defun jdibug-toggle-breakpoint ()
  (interactive)
  (let* ((source-file (buffer-file-name))
	 (line-number (line-number-at-pos))
	 (jdi (jdibug-jdi jdibug-this))
	 (bp (find-if (lambda (bp) 
			(and (equal (jdibug-breakpoint-source-file bp) source-file)
			     (equal (jdibug-breakpoint-line-number bp) line-number)))
		      (jdibug-breakpoints jdibug-this))))
    (cond ((and bp (member (jdibug-breakpoint-status bp) (list 'enabled 'unresolved)))
	   (jdibug-disable-breakpoint jdibug-this bp))
	  ((and bp (equal (jdibug-breakpoint-status bp) 'disabled))
	   (jdibug-remove-breakpoint jdibug-this bp))
	  (t 
	   (jdibug-set-breakpoint jdibug-this (make-jdibug-breakpoint :source-file source-file :line-number line-number))))))

(defun jdibug-breakpoint-update (bp)
  (jdibug-refresh-breakpoints-buffer jdibug-this)
  (let ((buffer (find-if (lambda (buf)
			   (string= (buffer-file-name buf) (jdibug-breakpoint-source-file bp)))
			 (buffer-list))))
    (with-current-buffer buffer
      (goto-line (jdibug-breakpoint-line-number bp))
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
			  'jdibug-breakpoint-disabled))))))

(defun jdibug-refresh-breakpoints-buffer (jdibug)
  (with-current-buffer (jdibug-breakpoints-buffer jdibug)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (let ((i 1))
		;; the breakpoints are added in reverse order, so in order to display the first one added first, we reverse it here
		(dolist (bp (reverse (jdibug-breakpoints jdibug)))
		  (let ((cb (widget-create 
					 'checkbox 
					 :jdibug-breakpoint bp
					 :jdibug jdibug)))
			(if (equal (jdibug-breakpoint-status bp) 'enabled)
				(widget-apply-action cb))
			(widget-put cb :action
						(lambda (widget &rest ignore)
						  (let ((bp (widget-get widget :jdibug-breakpoint)))
							(if (equal (jdibug-breakpoint-status bp) 'enabled)
								(progn
								  (message ":action disable breakpoint")
								  (jdibug-disable-breakpoint jdibug-this bp))
							  (message ":action enable breakpoint")
							  (jdibug-enable-breakpoint jdibug-this bp))))))
		  (insert (format " %2d. %s:%d\n" i (jdibug-breakpoint-class-name bp) (jdibug-breakpoint-line-number bp)))
		  (incf i)))
	  (use-local-map widget-keymap)
	  (widget-setup))))

(defun jdibug-send-step (depth)
  (mapc (lambda (buffer) 
		  (with-current-buffer buffer
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert "not suspended"))))
		(list (jdibug-frames-buffer jdibug-this) (jdibug-locals-buffer jdibug-this)))
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (if (null (jdi-suspended-thread-id (jdibug-jdi jdibug-this)))
      (message "JDIbug Can not step. Not suspended.")
    (jdi-send-step (jdibug-jdi jdibug-this) depth)))

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
		(list (jdibug-frames-buffer jdibug-this) (jdibug-locals-buffer jdibug-this)))
  (if (jdibug-current-line-overlay jdibug-this)
      (delete-overlay (jdibug-current-line-overlay jdibug-this)))
  (ado ()
	(jdi-resume (jdibug-jdi jdibug-this))
    (run-hooks 'jdibug-resumed-hook)
    (message "JDIbug resumed")))

(defun jdibug-connected-p ()
  (interactive)
  (if (jdwp-process (jdi-jdwp (jdibug-jdi jdibug-this)))
      t
    nil))

(defun jdibug-debug-view ()
  "Change into debug view."
  (interactive)
  (delete-other-windows)
  (split-window-vertically -20)
  (other-window 1)
  (switch-to-buffer jdibug-locals-buffer-name)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer jdibug-breakpoints-buffer-name)
  (set-window-dedicated-p (get-buffer-window (current-buffer)) t)
  (other-window 1))

(defun jdibug-undebug-view ()
  (interactive)
  (let ((localswin (get-buffer-window jdibug-locals-buffer-name t))
		(frameswin (get-buffer-window jdibug-frames-buffer-name t))
		(breakpointswin (get-buffer-window jdibug-breakpoints-buffer-name t)))
	(if localswin (delete-window localswin))
	(if frameswin (delete-window frameswin))
	(if breakpointswin (delete-window breakpointswin))))

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