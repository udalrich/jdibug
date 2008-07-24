;;; jdi.el --- library that provides the Java(tm) Debug Interface

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

;; This module try to implement everything documented here:
;; http://java.sun.com/j2se/1.4.2/docs/guide/jpda/jdi/

;; Look near the end of the file for customizing how special
;; objects (ArrayList, HashMap) are displayed in the locals buffer

;; This module requires ado.el, elog.el and jdwp.el

;;; Code:

(require 'bindat)
(require 'ado)
(require 'elog)
(require 'jdwp)

(elog-make-logger jdi)

(defstruct jdi
  ;; the jdwp
  (jdwp                 (make-jdwp))

  ;; list of paths that the source files are in
  source-paths

  ;; hash table with the class-id as the key and jdi-class as the value
  classes 

  ;; hash table with the object-id as the key and the class as the value
  objects

  ;; the current thread-id that have suspended, will be set after a breakpoint
  ;; or step event, this is used for the step events that we will send over
  suspended-thread-id

  ;; will have the current jdi-location if the debuggee is suspended, nil otherwise
  current-location

  thread-groups

  ;; list of jdi-frame, only valid if thread is suspended
  frames

  ;; list of jdi-value, only valid if thread is suspended
  locals

  ;; callback to be called when breakpoint is hit, called with (jdi jdi-class jdi-location)
  breakpoint-handler 

  ;; callback to be called when execution is stopped from stepping, called with (jdi jdi-class jdi-location)
  step-handler 

  ;; callback to be called when debuggee detached from us, called with (jdi)
  detached-handler 

  ;; handler to be called when the class is loaded for a breakpoint that wasn't resolved previously
  breakpoint-resolved-handler

  ;; list of jdi-breakpoint-request, storing breakpoint requests for classes that are not loaded yet
  breakpoint-requests

  ;; list of jdi-breakpoint, which stores the request id for each breakpoint
  breakpoints 

  last-error
  
  ;; you can store anything here
  plist)

(defstruct jdi-thread-group
  id
  name
  ;; list of jdi-thread-groups
  thread-groups
  ;; list of jdi-thread
  threads)

(defstruct jdi-thread
  id
  name
  status
  suspend-status)

(defstruct jdi-location
  ;; this might have to be resolved
  line-number 
  class-id
  method-id
  line-code-index
  type

  ;; jdi-class
  class	 
  ;; jdi-method
  method 

  ;; String to be displayed in the frames buffer
  string)

(defstruct jdi-breakpoint-request
  source-file
  line-number)

(defstruct jdi-breakpoint
  source-file
  line-number
  request-id)

(defstruct jdi-field
  id
  name
  signature
  mod-bits)

(defstruct jdi-method
  id
  name
  signature
  mod-bits
  ;; list of jdi-location
  locations    
  ;; t if we have got our locations resolved
  resolved-flag 

  ;; link back to our containing jdi-class
  class	
  )

;; as java returns interfaces using all-classes command, this might represent an interface
(defstruct jdi-class
  id
  ;; jni-style
  signature 
  ;; list of nonstatic jdi-field
  fields    
  ;; parent jdi-class
  super	    
  ;; list of interfaces implemented by this class
  interfaces
  ;; list of jdi-method
  methods)

(defstruct jdi-frame
  id     
  ;; jdi-location
  location)

(defstruct jdi-value
  name
  signature
  type
  value
  (string "null")
  (has-children-p nil)

  ;; this is only valid when this is top level (not contained in any other object)
  slot

  ;; for values which are of type object
  class	;; jdi-class

  ;; for values which are of type array
  array-length 

  ;; for object/array, this is the list that are displayed when the tree is expanded
  ;; list of jdi-values
  values)

;;; Constants:
(defconst jdi-access-public        #x0001)
(defconst jdi-access-private       #x0002)
(defconst jdi-access-protected     #x0004)
(defconst jdi-access-static        #x0008)
(defconst jdi-access-final         #x0010)
(defconst jdi-access-synchronized  #x0020)
(defconst jdi-access-volatile      #x0040)
(defconst jdi-access-transient     #x0080)
(defconst jdi-access-native        #x0100)
(defconst jdi-access-abstract      #x0400)

;;; And the functions:

(defun jdi-get-last-error (jdi)
  (let ((last-error (jdi-last-error jdi)))
    (setf (jdi-last-error jdi) nil)
    last-error))

(defun jdi-put (jdi key value)
  (setf (jdi-plist jdi)
		(plist-put (jdi-plist jdi) key value)))

(defun jdi-get (jdi key)
  (plist-get (jdi-plist jdi) key))

(defun jdi-connect (jdi host port source-paths)
  (lexical-let ((jdi jdi)
				(host host)
				(port port)
				(source-paths source-paths))
    (jdwp-put (jdi-jdwp jdi) 'jdi jdi)
    (setf (jdi-source-paths jdi) source-paths)
    (setf (jdi-suspended-thread-id jdi) nil)
    (setf (jdi-current-location jdi) nil)
	(setf (jdi-frames jdi) nil)
	(setf (jdi-locals jdi) nil)
    (condition-case err
		(ado ()
		  (jdi-info "connecting")
		  (jdwp-connect (jdi-jdwp jdi) host port)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-breakpoint 'jdi-handle-breakpoint-event)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-single-step 'jdi-handle-step-event)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-class-prepare 'jdi-handle-class-prepare)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-class-unload 'jdi-handle-class-unload)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-vm-start 'jdi-handle-vm-start)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-vm-death 'jdi-handle-vm-death)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-thread-start 'jdi-handle-thread-start)
		  (jdwp-set-event-handler (jdi-jdwp jdi) jdwp-event-thread-end 'jdi-handle-thread-end)
		  (jdi-get-idsizes jdi)
		  (progn
			(mapc (lambda (event)
					(jdwp-send-command (jdi-jdwp jdi) "set" 
									   `((:event-kind . ,event)
										 (:suspend-policy . ,jdwp-suspend-policy-none)
										 (:modifiers . 0))))
				  (list jdwp-event-class-prepare
						jdwp-event-class-unload
						jdwp-event-thread-start
						jdwp-event-thread-end
						jdwp-event-vm-death))

			(jdwp-send-command (jdi-jdwp jdi) "set" `((:event-kind . ,jdwp-event-exception) 
													  (:suspend-policy . ,jdwp-suspend-policy-none) 
													  (:modifiers . 1)
													  (:modifier
													   ((:mod-kind . 8)
														(:exception . [0 0 0 0 0 0 0 0])
														(:caught . 0)
														(:uncaught . 1)))))

			(jdi-get-version jdi)
			(jdwp-send-command (jdi-jdwp jdi) "capabilities-new" nil)
			(jdi-trace "capabilities:%s" (car ado-last-return-value))
			(jdi-info "getting classes")
			(jdi-get-classes jdi)
			(setf (jdi-objects jdi) (make-hash-table :test 'equal))
			;; 			(jdi-info "getting top level threads")
			;; 			(jdi-get-top-level-thread-groups jdi)
			(jdi-info "setting event handlers")))
      (file-error (setf (jdi-last-error jdi) 'failed-to-connect))))
  )

(defun jdi-resolve-frames (jdi thread-id)
  (jdi-info "jdi-resolve-frames")
  (unless (jdi-frames jdi)
	(ado (jdi thread-id) 
	  (jdwp-send-command (jdi-jdwp jdi) "frame-count" `((:thread . ,thread-id)))
	  (destructuring-bind (reply error id jdwp) ado-last-return-value
		(when (= error jdwp-error-none)
		  (ado (jdi) 
			(jdwp-send-command (jdi-jdwp jdi) "frames" `((:thread . ,thread-id)
														 (:start-frame . 0)
														 (:length . ,(bindat-get-field reply :frame-count))))
			(destructuring-bind (reply error id jdwp) ado-last-return-value
			  (unless (= error jdwp-error-thread-not-suspended)
				(jdi-info "suspended thread frame count:%d" (bindat-get-field reply :frames))
				(setf (jdi-frames jdi) 
					  (loop for frame in (bindat-get-field reply :frame)
							collect (make-jdi-frame :id (bindat-get-field frame :id) 
													:location (make-jdi-location :class-id (bindat-get-field frame :location :class-id)
																				 :method-id (bindat-get-field frame :location :method-id)
																				 :type (bindat-get-field frame :location :type)
																				 :line-code-index (bindat-get-field frame :location :index)))))))))))))

(defun jdi-resolve-locals (jdi class-id method-id line-code-index)
  "Make sure debuggee is suspended and jdi-frames is populated."
  (jdi-info "jdi-resolve-locals:class=%s:method=%s:line-code-index=%s:nframes:%d" class-id method-id line-code-index (length (jdi-frames jdi)))
  (unless (or (jdi-locals jdi)
			  (null (jdi-frames jdi)))
	(ado (jdi) 
	  (jdwp-send-command (jdi-jdwp jdi) "variable-table" `((:ref-type . ,class-id)
														   (:method-id . ,method-id)))
	  (destructuring-bind (reply error id jdwp) ado-last-return-value
		(unless (or (= error jdwp-error-absent-information)
					(null (jdi-frames jdi)))
		  (jdi-trace "variable-table arg-count:%d slots:%d" (bindat-get-field reply :arg-cnt) (bindat-get-field reply :slots))
		  (setf (jdi-locals jdi) nil)
		  (let* ((current-frame (car (jdi-frames jdi)))
				 (current-location (jdi-frame-location current-frame))
				 (current-frame-line-code (jdwp-vec-to-int (jdi-location-line-code-index current-location))))
			(jdi-trace "current-frame-line-code:%d" current-frame-line-code)
			(dolist (slot (bindat-get-field reply :slot))
			  (let ((start-valid-line-code (jdwp-get-int slot :code-index))
					(line-code-length      (bindat-get-field slot :length)))
				(jdi-trace "slot:%d code-index:%d length:%d name:%s signature:%s" 
						   (bindat-get-field slot :slot) 
						   start-valid-line-code
						   line-code-length
						   (jdwp-get-string slot :name) 
						   (jdwp-get-string slot :signature))
				(when (and (>= current-frame-line-code start-valid-line-code)
						   (<  current-frame-line-code (+ start-valid-line-code line-code-length)))
				  (push (make-jdi-value :slot (bindat-get-field slot :slot)
										:name (jdwp-get-string slot :name)
										:signature (jdwp-get-string slot :signature))
						(jdi-locals jdi)))))
			(ado (jdi)
			  (when (jdi-suspended-p jdi)
				(jdi-values-slot-resolve jdi (jdi-suspended-thread-id jdi) (jdi-frame-id current-frame) (jdi-locals jdi)))
			  (setf (jdi-locals jdi) (sort (jdi-locals jdi)
										   (lambda (a b) 
											 (string< (jdi-value-name a) (jdi-value-name b))))))))))))

(defun jdi-suspended-p (jdi)
  (and (jdi-suspended-thread-id jdi) (jdi-current-location jdi)))

(defun jdi-locals-refresh (jdi)
  (jdi-info "jdi-locals-refresh, suspended-thread-id:%s, current-location:%s" (jdi-suspended-thread-id jdi) (jdi-current-location jdi))
  (if (jdi-suspended-p jdi)
	  (ado (jdi)
		(jdi-resolve-locals jdi 
							(jdi-location-class-id (jdi-current-location jdi))
							(jdi-location-method-id (jdi-current-location jdi))
							(jdi-location-line-code-index (jdi-current-location jdi))))))

(defun jdi-handle-breakpoint-event (jdwp event)
  (jdi-trace "jdi-handle-breakpoint-event")
  (let* ((jdi (jdwp-get jdwp 'jdi))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))
		 (class (jdi-classes-find-by-id jdi class-id))
		 (location (jdi-class-find-location class method-id line-code-index)))
    (setf (jdi-suspended-thread-id jdi) (bindat-get-field event :u :thread))
    (setf (jdi-current-location jdi) location)
    (funcall (jdi-breakpoint-handler jdi) jdi class location)))

(defun jdi-class-find-location (class method-id line-code-index)
  (let* ((method (jdi-methods-find-by-id class method-id))
		 (location (jdi-locations-find-by-line-code-index method line-code-index)))
    (if location
		location
      (if (null (jdi-class-all-locations class))
		  (jdi-info "class do not have debug information")
		(jdi-error "failed to look line-code-index %s in class %s" line-code-index (jdi-class-name class))))))

(defun jdi-classes-find-by-id (jdi class-id)
  "Return the jdi-class with that class-id"
  (let ((result (gethash class-id (jdi-classes jdi))))
    (if (null result)
		(jdi-error "failed to find class with id:%s in %d classes" class-id (hash-table-count (jdi-classes jdi))))
    result))

(defun jdi-classes-find-by-signature (jdi signature)
  "Return the jdi-class with that class-id"
  (catch 'found
    (maphash (lambda (key value)
			   (if (equal (jdi-class-signature value) signature)
				   (throw 'found value)))
			 (jdi-classes jdi))
    nil))

(defun jdi-methods-find-by-id (class method-id)
  "Return the jdi-method with that method-id"
  (let ((result (find-if (lambda (method)
						   (equal (jdi-method-id method) method-id))
						 (jdi-class-methods class))))
    (if (null result)
		(jdi-error "failed to find method with id:%s, class %s have %d methods" method-id (jdi-class-name class) (length (jdi-class-methods class))))
    result))

(defun jdi-class-all-locations (class)
  (jdi-info "jdi-class-all-locations")
  (let ((result (loop for method in (jdi-class-methods class)
					  append (loop for location in (jdi-method-locations method)
								   collect location))))
    (jdi-info "class-all-locations:%d" (length result))
    result))

(defun jdi-handle-step-event (jdwp event)
  (jdi-info "jdi-handle-step-event")
  (let* ((jdi (jdwp-get jdwp 'jdi))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))
		 (class (jdi-classes-find-by-id jdi class-id)))
    (setf (jdi-suspended-thread-id jdi) (bindat-get-field event :u :thread))
    (ado (jdi class method-id line-code-index class-id)
      (jdi-class-resolve-all-locations jdi class)
      (let ((location (jdi-class-find-location class method-id line-code-index)))
		(unless location
		  ;; the class do not have debug information
		  (setq location (make-jdi-location :line-code-index line-code-index
											:class class
											:class-id class-id
											:method-id method-id)))
		(setf (jdi-current-location jdi) location)
		(funcall (jdi-step-handler jdi) jdi class location)))))

(defun jdi-handle-class-prepare (jdwp event)
  (jdi-trace "jdi-handle-class-prepare:%s" event)
  (let ((jdi (jdwp-get jdwp 'jdi))
		(type-id (bindat-get-field event :u :type-id))
		(signature (jdwp-get-string event :u :signature)))
    (jdi-info "class-loaded:%s" signature)
	(jdi-trace "breakpoint-requests:%s" (jdi-breakpoint-requests jdi))
	(if (gethash type-id (jdi-classes jdi))
		(jdi-info "class already in cache, not doing anything")
	  (puthash type-id (make-jdi-class :id type-id :signature signature) (jdi-classes jdi)))
    (let ((resolved-br nil))
      (mapc (lambda (br) 
			  (if (equal (jdi-breakpoint-request-source-file br) (jdi-class-signature-to-source jdi signature))
				  (push br resolved-br)))
			(jdi-breakpoint-requests jdi))
      (mapc (lambda (rbr)
			  (setf (jdi-breakpoint-requests jdi) 
					(delete rbr (jdi-breakpoint-requests jdi))))
			resolved-br)
      (ado (jdi resolved-br)
		(dolist (br resolved-br)
		  (ado (jdi br)
			(jdi-set-breakpoint jdi (jdi-breakpoint-request-source-file br) (jdi-breakpoint-request-line-number br))
			(jdi-resume jdi)
			(funcall (jdi-breakpoint-resolved-handler jdi) jdi br)))))))

(defun jdi-handle-class-unload (jdwp event)
  (let ((jdi (jdwp-get jdwp 'jdi))
		(signature (jdwp-get-string event :u :signature)))
    (jdi-info "jdi-handle-class-unload:%s" signature)))

(defun jdi-handle-vm-death (jdwp event)
  (let ((jdi (jdwp-get jdwp 'jdi)))
    (jdi-info "jdi-handle-vm-death")
    (setf (jdi-classes jdi) nil)
	(setf (jdi-objects jdi) nil)
    (setf (jdi-thread-groups jdi) nil)
    (setf (jdi-breakpoints jdi) nil)
    (setf (jdi-breakpoint-requests jdi) nil)

    (setf (process-sentinel (jdwp-process jdwp)) nil)
    (kill-buffer (process-buffer (jdwp-process jdwp)))
    (setf (jdwp-process jdwp) nil)
    (setf (jdwp-handshaked-p jdwp) nil)

    (funcall (jdi-detached-handler jdi) jdi)))

(defun jdi-handle-vm-start (jdwp event)
  (let ((jdi (jdwp-get jdwp 'jdi)))
    (jdi-info "jdi-handle-vm-start")))

(defun jdi-handle-thread-start (jdwp event)
  (jdi-info "jdi-handle-thread-start"))

(defun jdi-handle-thread-end (jdwp event)
  (jdi-info "jdi-handle-thread-end"))

(defun jdi-disconnect (jdi)
  (setf (jdi-classes jdi) nil)
  (setf (jdi-objects jdi) nil)
  (setf (jdi-thread-groups jdi) nil)
  (setf (jdi-breakpoints jdi) nil)
  (setf (jdi-breakpoint-requests jdi) nil)
  (jdwp-disconnect (jdi-jdwp jdi))
  (setf (jdi-jdwp jdi) (make-jdwp)))

(defun jdi-get-idsizes (jdi)
  (lexical-let ((reply))
    (ado (jdi)
      (jdwp-send-command (jdi-jdwp jdi) "id-sizes" nil)
      (setq reply (car ado-last-return-value))
      (jdi-trace "field-id-size         :%d" (bindat-get-field reply :field-id-size))
      (setf (jdwp-field-id-size (jdi-jdwp jdi)) (bindat-get-field reply :field-id-size))
      (jdi-trace "method-id-size        :%d" (bindat-get-field reply :method-id-size))
      (setf (jdwp-method-id-size (jdi-jdwp jdi)) (bindat-get-field reply :method-id-size))
      (jdi-trace "object-id-size        :%d" (bindat-get-field reply :object-id-size))
      (setf (jdwp-object-id-size (jdi-jdwp jdi)) (bindat-get-field reply :object-id-size))
      (jdi-trace "reference-type-id-size:%d" (bindat-get-field reply :reference-type-id-size))
      (setf (jdwp-reference-type-id-size (jdi-jdwp jdi)) (bindat-get-field reply :reference-type-id-size))
      (jdi-trace "frame-id-size         :%d" (bindat-get-field reply :frame-id-size))
      (setf (jdwp-frame-id-size (jdi-jdwp jdi)) (bindat-get-field reply :frame-id-size))
      (jdi-trace "get-idsizes done"))))

(defun jdi-get-all-threads (jdi)
  (=bind (jdwp id status reply) (jdi) (jdwp-send-command (jdi-jdwp jdi) "all-threads" nil)
		 (=dolist (thread (bindat-get-field reply :thread)) (jdi)
				  (=bind (jdwp id status reply) (jdi thread) (jdwp-send-command (jdi-jdwp jdi) "thread-name" `((:thread . ,(bindat-get-field thread :id))))
						 (jdi-trace "thread:%s:%s" (bindat-get-field thread :id) (jdwp-get-string reply :thread-name))))))

(defun jdi-get-classes (jdi)
  (lexical-let ((reply))
    (ado (jdi)
      (jdwp-send-command (jdi-jdwp jdi) "suspend" nil)
      (jdwp-send-command (jdi-jdwp jdi) "all-classes" nil)
      (setq reply (car ado-last-return-value))
      (jdi-info "number of classes loaded:%d" (bindat-get-field reply :classes))
      (setf (jdi-classes jdi) (make-hash-table :test 'equal))
      (loop for class in (bindat-get-field reply :class)
			do
			(puthash (bindat-get-field class :type-id)
					 (make-jdi-class :id (bindat-get-field class :type-id) :signature (jdwp-get-string class :signature))
					 (jdi-classes jdi)))
      (if jdi-trace-flag
		  (maphash (lambda (key value)
					 (jdi-trace "class key:%s" key))
				   (jdi-classes jdi)))
      (jdwp-send-command (jdi-jdwp jdi) "resume" nil) ())))

(defun jdi-get-version (jdi)
  (lexical-let ((reply))
    (ado ()
      (jdwp-send-command (jdi-jdwp jdi) "version" nil)
      (setq reply (car ado-last-return-value))
      (jdi-trace "description:%s" (jdwp-get-string reply :description))
      (jdi-trace "jdwp-major:%d" (bindat-get-field reply :jdwp-major))
      (jdi-trace "jdwp-minor:%d" (bindat-get-field reply :jdwp-minor))
      (jdi-trace "vm-version:%s" (jdwp-get-string reply :vm-version))
      (jdi-trace "vm-name:%s" (jdwp-get-string reply :vm-name)))))

(defun jdi-set-breakpoint (jdi source-file line-number)
  (jdi-info "jdi-set-breakpoint:%s:%d" source-file line-number)
  (let* ((sig (jdi-source-to-class-signature jdi source-file))
		 (class (jdi-classes-find-by-signature jdi sig)))
    (if (null class)
		(let* ((class-name (jdi-source-to-class-name jdi source-file)))
		  (ado (jdi sig class class-name source-file line-number)
			(jdi-info "could not find class %s in loaded class list" sig)
			(setf (jdi-last-error jdi) 'not-loaded)
			(push (make-jdi-breakpoint-request :source-file source-file :line-number line-number)
				  (jdi-breakpoint-requests jdi))
			(mapc (lambda (class-pattern)
					(jdwp-send-command (jdi-jdwp jdi) "set" 
									   `((:event-kind . ,jdwp-event-class-prepare)
										 (:suspend-policy . ,jdwp-suspend-policy-all)
										 (:modifiers . 1)
										 (:modifier
										  ((:mod-kind . 5)
										   (:class-pattern . ((:length . ,(length class-pattern))
															  (:string . ,class-pattern))))))))
				  (list class-name (concat class-name ".*") (concat class-name "$*")))))
      (ado (jdi class source-file line-number)
		(jdi-class-resolve-all-locations jdi class)
		(let ((location	(find-if (lambda (location)
								   (equal (jdi-location-line-number location) line-number))
								 (jdi-class-all-locations class))))
		  (if (null location)
			  (setf (jdi-last-error jdi) 'no-code-at-line)
			(jdi-trace "found location:%s" location)
			(let* ((location-data `((:type .   1)
									(:class-id . ,(jdi-location-class-id location))
									(:method-id . ,(jdi-location-method-id location))
									(:index     . ,(jdi-location-line-code-index location))))
				   (data `((:event-kind     . ,jdwp-event-breakpoint)
						   (:suspend-policy . ,jdwp-suspend-policy-all)
						   (:modifiers      . 1)
						   (:modifier       ((:mod-kind .  7)
											 (:location .  ,location-data))))))
			  (ado (jdi source-file line-number) 
				(jdwp-send-command (jdi-jdwp jdi) "set" data)
				(let ((reply (car ado-last-return-value)))
				  (push (make-jdi-breakpoint :source-file source-file :line-number line-number :request-id (bindat-get-field reply :request-id))
						(jdi-breakpoints jdi))
				  (jdi-trace "received requestid:%d" (bindat-get-field reply :request-id)))))))))))

(defun jdi-clear-breakpoint (jdi source-file line-number)
  (let* ((bp (find-if (lambda (bp)
						(and (equal (jdi-breakpoint-source-file bp) source-file)
							 (equal (jdi-breakpoint-line-number bp) line-number)))
					  (jdi-breakpoints jdi))))
    (if bp
		(jdwp-clear-breakpoint (jdi-jdwp jdi) (jdi-breakpoint-request-id bp))
      (jdwp-error "failed to find breakpoint %s:%s" source-file line-number))))

(defun jdi-source-to-class-signature (jdi source)
  "Converts a source file to a JNI class name."
  (let ((buf source))
    (mapc (lambda (sp)
			(setf buf (replace-in-string buf sp "")))
		  (jdi-source-paths jdi))
    (setf buf (replace-regexp-in-string "^/" "" buf))
    (setf buf (replace-regexp-in-string ".java$" "" buf))
    (setf buf (concat "L" buf ";"))
    (jdi-trace "jdi-source-to-class-signature : %s -> %s" source buf)
    buf))

(defun jdi-source-to-class-name (jdi source)
  "Converts a source file to a a.b.c class name."
  (let ((buf source))
    (mapc (lambda (sp)
			(setf buf (replace-in-string buf sp "")))
		  (jdi-source-paths jdi))
    (setf buf (replace-regexp-in-string "^/" "" buf))
    (setf buf (replace-regexp-in-string ".java$" "" buf))
    (setf buf (replace-regexp-in-string "/" "." buf))
    (jdi-trace "jdi-source-to-class-name : %s -> %s" source buf)
    buf))

(defun jdi-class-signature-to-source (jdi class-signature)
  "Converts a JNI class name to source file."
  (let ((buf class-signature))
    (setf buf (replace-regexp-in-string "^L" "" buf))
    (setf buf (replace-regexp-in-string ";$" "" buf))
    (setf buf (concat buf ".java"))
    (mapc (lambda (sp)
			(if (file-exists-p (concat sp "/" buf))
				(setf buf (concat sp "/" buf))))
		  (jdi-source-paths jdi))
    (jdi-trace "jdi-class-signature-to-source : %s -> %s" class-signature buf)
    buf))

(defun jdi-locations-find-by-line-code-index (method line-code-index)
  (jdi-trace "jdi-locations-find-by-line-code-index:%s:%s:%d" (jdi-method-name method) line-code-index (length (jdi-method-locations method)))
  (if jdi-trace-flag
      (loop for loc in (jdi-method-locations method)
			do
			(jdi-trace "line-code-index:%s line-number:%d" (jdi-location-line-code-index loc) (jdi-location-line-number loc))))
  (let ((result))
    (loop for loc in (jdi-method-locations method)
		  while (>= (jdwp-vec-to-int line-code-index)
					(jdwp-vec-to-int (jdi-location-line-code-index loc)))
		  do (setq result loc))
    (if (null result)
		(setq result (car (last (jdi-method-locations method)))))
    (if result
		(jdi-trace "found at line-number:%d" (jdi-location-line-number result)))
    result))

(defun jdi-access-string (bits)
  (let ((str)
		(map `((,jdi-access-public       . "[public]")
			   (,jdi-access-private      . "[private]")
			   (,jdi-access-protected    . "[protected]")
			   (,jdi-access-static       . "[static]")
			   (,jdi-access-final        . "[final]")
			   (,jdi-access-synchronized . "[final]")
			   (,jdi-access-volatile     . "[volatile]")
			   (,jdi-access-transient    . "[transient]")
			   (,jdi-access-native       . "[native]")
			   (,jdi-access-abstract     . "[abstract]"))))
    (mapc (lambda (m)
			(if (not (equal (logand bits (car m)) 0))
				(setq str (concat str (cdr m)))))
		  map)
    str))

(defun jdi-field-mod-bits-string (field)
  (jdi-access-string (jdi-field-mod-bits field)))

(defun jdi-field-static-p (field)
  (not (equal (logand (jdi-field-mod-bits field) jdi-access-static) 0)))

(defun jdi-method-mod-bits-string (method)
  (jdi-access-string (jdi-method-mod-bits method)))

(defun jdi-method-native-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-native) 0)))

(defun jdi-class-name (class)
  (jdi-trace "jdi-class-name:%s" (jdi-class-signature class))
  (let* ((class-name (jdi-class-signature class)))
    (setq class-name (replace-regexp-in-string "^\\[" "" class-name))
    (cond ((string= class-name "I")
		   (setq class-name "int"))
		  (t
		   (setq class-name (replace-regexp-in-string ".*/" "" class-name))
		   (setq class-name (replace-regexp-in-string ";" "" class-name))))
    class-name))

(defun jdi-class-all-fields (class)
  "Return a list of jdi-fields including those of parents."
  (let ((all-fields (append (jdi-class-fields class)
							(if (jdi-class-super class)
								(jdi-class-all-fields (jdi-class-super class))))))
    (sort all-fields (lambda (a b)
					   (string< (jdi-field-name a) (jdi-field-name b))))))

(defun jdi-class-all-methods (class)
  "Return a list of methods including those of parents."
  (let ((all-methods (append (jdi-class-methods class)
							 (if (jdi-class-super class)
								 (jdi-class-all-methods (jdi-class-super class))))))
    (sort all-methods (lambda (a b)
						(string< (jdi-method-name a) (jdi-method-name b))))))

(defun jdi-value-sigbyte (value)
  (string-to-char (jdi-value-signature value)))

(defun jdi-values-slot-resolve (jdi thread frame values)
  "Resolve a list of top level (not in objects) jdi-values."
  (jdi-info "jdi-values-slot-resolve thread=%s frame=%s" thread frame)
  (let ((data `((:thread . ,thread)
				(:frame  . ,frame)
				(:slots  . ,(length values))
				,(nconc (list :slot)
						(mapcar (lambda (value) 
								  `((:slot . ,(jdi-value-slot value))
									(:sigbyte . ,(jdi-value-sigbyte value))))
								values)))))
    (ado (jdi values) 
      (jdwp-send-command (jdi-jdwp jdi) "stack-get-values" data)
      (destructuring-bind (reply error id jdwp) ado-last-return-value
		(when (= error jdwp-error-none)
		  (let ((i 0))
			(dolist (value (bindat-get-field reply :value))
			  (setf (jdi-value-type (nth i values)) (bindat-get-field value :slot-value :type))
			  (setf (jdi-value-value (nth i values)) (bindat-get-field value :slot-value :u :value))
			  (incf i)))
		  (jdi-values-resolve jdi values))))))

(defun jdi-values-resolve (jdi values)
  "Resolve a list of jdi-values."
  (jdi-info "jdi-values-resolve %d values" (length values))
  (dolist (value values)
    (jdi-trace "resolving values:%s:%d:%s" (jdi-value-name value) (jdi-value-type value) (jdi-value-value value))
    (setf (jdi-value-has-children-p value) nil)
    (cond ((or (equal (jdi-value-type value) jdwp-tag-int)
			   (equal (jdi-value-type value) jdwp-tag-byte)
			   (equal (jdi-value-type value) jdwp-tag-char)
			   (equal (jdi-value-type value) jdwp-tag-short))
		   (setf (jdi-value-string value) (format "%d" (jdi-value-value value))))
		  ((equal (jdi-value-type value) jdwp-tag-long)
		   (setf (jdi-value-string value) (format "%d" (jdwp-vec-to-int (jdi-value-value value)))))
		  ((equal (jdi-value-type value) jdwp-tag-float)
		   (setf (jdi-value-string value) (format "%f" (jdwp-vec-to-float (jdi-value-value value)))))
		  ((equal (jdi-value-type value) jdwp-tag-object)
		   (jdi-value-resolve-ref-type jdi value))
		  ((equal (jdi-value-type value) jdwp-tag-array)
		   (jdi-value-resolve-array jdi value))
		  ((equal (jdi-value-type value) jdwp-tag-string)
		   (jdi-value-resolve-string jdi value))
		  ((equal (jdi-value-type value) jdwp-tag-boolean)
		   (setf (jdi-value-string value) (if (= 0 (jdi-value-value value)) "false" "true")))
		  ((equal (jdi-value-type value) jdwp-tag-class-object)
		   ;; TODO: put in the class name
		   (setf (jdi-value-string value) "class"))
		  (t 
		   (jdi-error "fixme: do not know how to print value of type:%s name:%s" (jdi-value-type value) (jdi-value-name value))
		   (setf (jdi-value-string value) "...")))))


(defun jdi-value-get-field-value (jdi value field-name func)
  "For object type jdi-value in value, get the value of the field
named field-name, and call func with (jdi value field-value) after that."
  (jdi-trace "jdi-value-get-field-value")
  (ado (jdi value field-name func)
    (jdi-class-resolve-parent jdi (jdi-value-class value))
    (jdi-class-resolve-fields jdi (jdi-value-class value))
    (let ((wanted-field-id (find-if (lambda (field) (equal (jdi-field-name field) field-name))
									(jdi-class-all-fields (jdi-value-class value)))))
      (ado (jdi value func wanted-field-id) 
		(jdwp-send-command (jdi-jdwp jdi) "object-get-values" 
						   `((:object . ,(jdi-value-value value))
							 (:fields . 1)
							 (:field  ((:id . ,(jdi-field-id wanted-field-id))))))
		(let ((reply (car ado-last-return-value)))
		  (funcall func jdi value (bindat-get-field (car (bindat-get-field reply :value)) :value :u :value)))))))

(defun jdi-value-invoke-method (jdi value method-name)
  "Invoke a simple method (do not require arguments) in the object in jdi-value."
  (jdi-info "jdi-value-invoke-method:%s" method-name)
  (let ((class (jdi-value-class value)))
    (ado (jdi value method-name class)
      (jdi-class-resolve-parent jdi class)
      (jdi-class-resolve-methods jdi class)
      (let ((method (find-if (lambda (method)
							   (equal (jdi-method-name method) method-name))
							 (jdi-class-all-methods class))))
		(jdwp-send-command (jdi-jdwp jdi) "object-invoke-method"
						   `((:object . ,(jdi-value-value value))
							 (:thread . ,(jdi-suspended-thread-id jdi))
							 (:class . ,(jdi-class-id class))
							 (:method-id . ,(jdi-method-id method))
							 (:arguments . 0)
							 (:options . ,jdwp-invoke-single-threaded)))))))

(defun jdi-objects-find-by-id (jdi object-id)
  "Return the jdi-class for this object-id"
  (gethash object-id (jdi-objects jdi)))

(defun jdi-value-resolve-ref-type (jdi value)
  (unless (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	(let ((class (jdi-objects-find-by-id jdi (jdi-value-value value))))
	  (if class
		  ;; we already have the class information in the cache for this object
		  ;; do not need to query again
		  (progn
			(setf (jdi-value-class value) class)
			(setf (jdi-value-has-children-p value) t)
			(let ((set-string-func (jdi-value-custom-set-strings-find jdi value)))
			  (if set-string-func
				  (funcall set-string-func jdi value)
				(setf (jdi-value-string value) (jdi-class-name class)))))

		;; we do not have the class for this value, query it and get all the parents
		;; of the class
		(jdi-info "jdi-value-resolve-ref-type:%s:%s" (jdi-value-name value) (jdi-value-value value))
		(ado (jdi value)
		  (setf (jdi-value-has-children-p value) t)
		  (jdwp-send-command (jdi-jdwp jdi) "reference-type" `((:object . ,(jdi-value-value value))))
		  (let* ((reply (car ado-last-return-value))
				 (class (jdi-classes-find-by-id jdi (bindat-get-field reply :type-id))))
			(ado (jdi value class)
			  (setf (jdi-value-class value) class)
			  (puthash (jdi-value-value value) class (jdi-objects jdi))
			  (jdi-class-resolve-parent jdi class)
			  (let ((set-string-func (jdi-value-custom-set-strings-find jdi value)))
				(if set-string-func
					(funcall set-string-func jdi value)
				  (setf (jdi-value-string value) (jdi-class-name class)))))))))))

(defun jdi-value-resolve-array (jdi value)
  (jdi-info "jdi-value-resolve-array:%s:%s" (jdi-value-name value) (jdi-value-value value))
  (ado (jdi value)
    (setf (jdi-value-has-children-p value) t)
    (jdwp-send-command (jdi-jdwp jdi) "reference-type" `((:object . ,(jdi-value-value value))))
    (let* ((reply (car ado-last-return-value))
		   (class (jdi-classes-find-by-id jdi (bindat-get-field reply :type-id))))
      (jdi-trace "found class %s for array" (jdi-class-name class))
      (setf (jdi-value-class value) class))
    (jdwp-send-command (jdi-jdwp jdi) "array-length" `((:array-object . ,(jdi-value-value value))))
    (let ((reply (car ado-last-return-value)))
      (setf (jdi-value-array-length value) (bindat-get-field reply :array-length))
      (setf (jdi-value-string value) (format "%s[%d]" (jdi-class-name (jdi-value-class value)) (bindat-get-field reply :array-length))))))

(defun jdi-value-resolve-string (jdi value)
  (ado (jdi value) 
    (jdwp-send-command (jdi-jdwp jdi) "string-value" `((:object . ,(jdi-value-value value))))
    (let ((reply (car ado-last-return-value)))
      (setf (jdi-value-string value) (format "\"%s\"" (jdwp-get-string reply :value))))))

(defun jdi-resolve-thread-group (jdi thread-group)
  (=progn (jdi thread-group)
		  (=bind (jdwp id status reply) (thread-group) (jdwp-send-command (jdi-jdwp jdi) "thread-group-name" `((:group . ,(jdi-thread-group-id thread-group))))
				 (jdi-trace "name of thread group:%s" (jdwp-get-string reply :group-name))
				 (setf (jdi-thread-group-name thread-group) (jdwp-get-string reply :group-name)))
		  (=bind (jdwp id status reply) (jdi thread-group) (jdwp-send-command (jdi-jdwp jdi) "thread-group-children" `((:group . ,(jdi-thread-group-id thread-group))))
				 (=progn (jdi thread-group reply)
						 (jdi-trace "number of child-group:%d:%s" (bindat-get-field reply :child-groups) (bindat-get-field reply :child-group))
						 (jdi-trace "number of child-thread:%d:%s" (bindat-get-field reply :child-threads) (bindat-get-field reply :child-thread))
						 (=dolist (child-thread (bindat-get-field reply :child-thread)) (jdi thread-group)
								  (lexical-let ((jdi-child-thread (make-jdi-thread :id (bindat-get-field child-thread :child-thread))))
									(push jdi-child-thread (jdi-thread-group-threads thread-group))
									(=progn (jdi jdi-child-thread child-thread)
											(=bind (jdwp id status reply) (jdi-child-thread) (jdwp-send-command (jdi-jdwp jdi) "thread-name" `((:thread . ,(bindat-get-field child-thread :child-thread))))
												   (setf (jdi-thread-name jdi-child-thread) (jdwp-get-string reply :thread-name)))
											(=bind (jdwp id status reply) (jdi-child-thread) (jdwp-send-command (jdi-jdwp jdi) "thread-status" `((:thread . ,(bindat-get-field child-thread :child-thread))))
												   (setf (jdi-thread-status jdi-child-thread) (bindat-get-field reply :thread-status))
												   (setf (jdi-thread-suspend-status jdi-child-thread) (bindat-get-field reply :suspend-status))))))
						 (=dolist (child-group (bindat-get-field reply :child-group)) (jdi thread-group)
								  (let ((new-thread-group (make-jdi-thread-group :id (bindat-get-field child-group :child-group))))	  
									(push new-thread-group (jdi-thread-group-thread-groups thread-group))								
									(jdi-resolve-thread-group jdi new-thread-group)))))))
        

(defun jdi-get-top-level-thread-groups (jdi)
  (=progn (jdi)
		  (setf (jdi-thread-groups jdi) nil)
		  (=bind (jdwp id status reply) (jdi) (jdwp-send-command (jdi-jdwp jdi) "top-level-thread-groups" nil)
				 (jdi-trace "number of top level groups:%d" (bindat-get-field reply :groups))
				 (=dolist (group (bindat-get-field reply :group)) (jdi)
						  (let ((top-level-thread-group (make-jdi-thread-group :id (bindat-get-field group :id))))
							(push top-level-thread-group (jdi-thread-groups jdi))
							(jdi-resolve-thread-group jdi top-level-thread-group))))))

(defun jdi-class-resolve-methods (jdi class)
  (unless (jdi-class-methods class)
    (jdi-info "jdi-class-resolve-methods:%s" (jdi-class-name class))
    (ado (jdi class) 
      (jdwp-send-command (jdi-jdwp jdi) "methods" `((:ref-type . ,(jdi-class-id class))))
      (let ((reply (car ado-last-return-value)))
		(jdi-info "number of methods:%d" (bindat-get-field reply :methods))
		(setf (jdi-class-methods class)
			  (loop for method in (bindat-get-field reply :method)
					collect (make-jdi-method :class class
											 :id (bindat-get-field method :method-id)
											 :name (jdwp-get-string method :name)
											 :signature (jdwp-get-string method :signature)
											 :mod-bits (bindat-get-field method :mod-bits))))
		(dolist (method (jdi-class-methods class))
		  (jdi-trace "method name:%s signature:%s id:%s" (jdi-method-name method) (jdi-method-signature method) (jdi-method-id method)))
		(if (jdi-class-super class)
			(jdi-class-resolve-methods jdi (jdi-class-super class)))))))

(defun jdi-method-resolve-locations (jdi method)
  (unless (or (jdi-method-resolved-flag method) (jdi-method-native-p method))
    (jdi-info "jdi-method-resolve-locations:%s:%s:%s" (jdi-class-name (jdi-method-class method)) (jdi-method-name method) (jdi-method-mod-bits-string method))
    (ado (jdi method) 
      (jdwp-send-command (jdi-jdwp jdi) "line-table" `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
													   (:method-id . ,(jdi-method-id method))))
      (let ((reply (car ado-last-return-value)))
		(jdi-trace "start=%s:end=%s:lines=%d" 
				   (jdwp-string-to-hex (bindat-get-field reply :start))
				   (jdwp-string-to-hex (bindat-get-field reply :end))
				   (bindat-get-field reply :lines))
		(setf (jdi-method-locations method)
			  (loop for line in (bindat-get-field reply :line)
					collect (make-jdi-location :class-id (jdi-class-id (jdi-method-class method))
											   :method-id (jdi-method-id method)
											   :line-code-index (bindat-get-field line :line-code-index)
											   :line-number (bindat-get-field line :line-number))))
		(jdi-info "number of locations:%d" (length (jdi-method-locations method)))
		(setf (jdi-method-resolved-flag method) t)))))

(defun jdi-class-resolve-all-locations (jdi class)
  (jdi-info "jdi-class-resolve-all-locations:%s" (jdi-class-name class))
  (ado (jdi class)
    (jdi-class-resolve-methods jdi class)
    (dolist (method (jdi-class-methods class))
      (jdi-method-resolve-locations jdi method))))

(defun jdi-class-resolve-parent (jdi class)
  "Get the whole parent hierarchy of this class, stop only after reaching the Object class."
  (unless (or (jdi-class-super class)
			  (string= (jdi-class-signature class) "Ljava/lang/Object;"))
    (jdi-info "jdi-class-resolve-parent for %s:%s" (jdi-class-signature class) (jdi-class-id class))
    (ado (jdi class)
      (jdwp-send-command (jdi-jdwp jdi) "interfaces" `((:ref-type . ,(jdi-class-id class))))
      (let ((reply (car ado-last-return-value)))
		(setf (jdi-class-interfaces class)
			  (loop for interface in (bindat-get-field reply :interface)
					collect (jdi-classes-find-by-id jdi (bindat-get-field interface :type))))
		(if jdi-info-flag
			(loop for interface in (jdi-class-interfaces class)
				  do (jdi-info "class:%s interface:%s"
							   (jdi-class-name class)
							   (jdi-class-name interface)))))
      (jdwp-send-command (jdi-jdwp jdi) "superclass" `((:class . ,(jdi-class-id class))))
      (let ((reply (car ado-last-return-value)))
		(unless (equal (bindat-get-field reply :superclass)
					   [0 0 0 0 0 0 0 0])
		  (let ((superclass (jdi-classes-find-by-id jdi (bindat-get-field reply :superclass))))
			(jdi-trace "class %s superclass:%s" (jdi-class-name class) (jdi-class-name superclass))
			(setf (jdi-class-super class) superclass)
			(jdi-class-resolve-parent jdi superclass)))))))

(defun jdi-value-instance-of-p (jdi value signature)
  (catch 'found
    (let ((class (jdi-value-class value)))
      (while class
		(if (string= (jdi-class-signature class) signature)
			(throw 'found t))
		(dolist (interface (jdi-class-interfaces class))
		  (if (string= (jdi-class-signature interface) signature)
			  (throw 'found t)))
		(setq class (jdi-class-super class)))
      ;; the return value if its not found
      nil)))

(defun jdi-class-resolve-fields (jdi class)
  "Get all the fields in this class, and also in the parent's class if this class have a parent."
  (unless (jdi-class-fields class)
    (jdi-info "jdi-class-resolve-fields for %s" (jdi-class-name class))
    (ado (jdi class) 
      (jdwp-send-command (jdi-jdwp jdi) "fields" `((:ref-type . ,(jdi-class-id class))))
      (let ((reply (car ado-last-return-value)))
		(jdi-trace "%s's fields:%d" (jdi-class-name class) (bindat-get-field reply :declared))
		(dolist (field (bindat-get-field reply :field))
		  (let ((new-field (make-jdi-field :id (bindat-get-field field :id)
										   :name (jdwp-get-string field :name)
										   :signature (jdwp-get-string field :signature)
										   :mod-bits (bindat-get-field field :mod-bits))))
			(jdi-trace "id:%s name:%s signature:%s modbits:%s" 
					   (bindat-get-field field :id) 
					   (jdwp-get-string field :name) 
					   (jdwp-get-string field :signature)
					   (jdi-field-mod-bits-string new-field))
			(push new-field (jdi-class-fields class))))
		(if (jdi-class-super class)
			(jdi-class-resolve-fields jdi (jdi-class-super class)))))))

(defun jdi-send-step (jdi depth)
  (let ((tid (jdi-suspended-thread-id jdi)))
    (setf (jdi-suspended-thread-id jdi) nil)
    (setf (jdi-current-location jdi) nil)
    (setf (jdi-frames jdi) nil)
    (setf (jdi-locals jdi) nil)
    (ado (jdi depth tid)
      (jdwp-send-step (jdi-jdwp jdi) depth tid)
      (jdwp-send-command (jdi-jdwp jdi) "resume" nil))))

(defun jdi-resume (jdi)
  (jdi-info "jdi-resume")
  (setf (jdi-suspended-thread-id jdi) nil)
  (setf (jdi-current-location jdi) nil)
  (setf (jdi-frames jdi) nil)
  (setf (jdi-locals jdi) nil)
  (ado (jdi)
	(jdwp-send-command (jdi-jdwp jdi) "resume" nil)))

(defun jdi-file-in-source-paths-p (jdi file)
  (find-if (lambda (sp) 
			 (string-match sp file))
		   (jdi-source-paths jdi)))

(defun jdi-value-get-nonstatic-values (jdi value)
  (jdi-info "jdi-value-get-nonstatic-values")
  (let* ((class (jdi-value-class value))
		 (fields (loop for field in (jdi-class-all-fields class)
					   if (not (jdi-field-static-p field)) collect field))
		 (values (mapcar (lambda (field) (make-jdi-value :name (jdi-field-name field)
														 :signature (jdi-field-signature field)))
						 fields)))
    (ado (jdi value class fields values)
      (jdwp-send-command (jdi-jdwp jdi) "object-get-values"
						 `((:object . ,(jdi-value-value value))
						   (:fields . ,(length fields))
						   ,(nconc (list :field)
								   (mapcar (lambda (field)
											 `((:id . ,(jdi-field-id field))))
										   fields))))
      (let ((reply (car ado-last-return-value)))
		(loop for jdi-value in values
			  for value in (bindat-get-field reply :value)
			  do
			  (setf (jdi-value-type jdi-value) (bindat-get-field value :value :type))
			  (setf (jdi-value-value jdi-value) (bindat-get-field value :value :u :value)))
		(setf (jdi-value-values value) (append (jdi-value-values value) values))))))

(defun jdi-value-get-static-values (jdi value)
  "Populate the values for the static fields in jdi-value"
  (jdi-info "jdi-value-get-static-values")
  (let* ((class (jdi-value-class value))
		 (fields (loop for field in (jdi-class-all-fields class)
					   if (jdi-field-static-p field) collect field))
		 (values (mapcar (lambda (field) (make-jdi-value :name (jdi-field-name field)
														 :signature (jdi-field-signature field)))
						 fields)))
    (ado (jdi value class fields values)
      (jdwp-send-command (jdi-jdwp jdi) "reference-get-values"
						 `((:ref-type . ,(jdi-class-id (jdi-value-class value)))
						   (:fields . ,(length fields))
						   ,(nconc (list :field)
								   (mapcar (lambda (field)
											 `((:id . ,(jdi-field-id field))))
										   fields))))
      (let ((reply (car ado-last-return-value)))
		(loop for jdi-value in values
			  for value in (bindat-get-field reply :value)
			  do
			  (setf (jdi-value-type jdi-value) (bindat-get-field value :value :type))
			  (setf (jdi-value-value jdi-value) (bindat-get-field value :value :u :value)))
		(setf (jdi-value-values value) (append (jdi-value-values value) values))))))

(defun jdi-value-get-array-values (jdi value)
  (jdi-info "jdi-value-get-array-values")
  (setf (jdi-value-values value)
		(loop for v from 0 to (- (jdi-value-array-length value) 1)
			  collect (make-jdi-value :name (format "%s[%d]" (jdi-value-name value) v))))
  (ado (jdi value) (jdwp-send-command (jdi-jdwp jdi) "array-get-values"
									  `((:array-object . ,(jdi-value-value value))
										(:first-index . 0)
										(:length . ,(jdi-value-array-length value))))
    (let* ((reply (car ado-last-return-value))
		   (array (jdwp-unpack-arrayregion reply)))
      (jdi-trace "got array-get-values:%s" array)
      (if (= (bindat-get-field array :type) jdwp-tag-object)
		  (loop for jdi-value in (jdi-value-values value)
				for value in (bindat-get-field array :value)
				do
				(setf (jdi-value-type jdi-value) (bindat-get-field value :value :type))
				(setf (jdi-value-value jdi-value) (bindat-get-field value :value :u :value)))
		(loop for jdi-value in (jdi-value-values value)
			  for value in (bindat-get-field array :value)
			  do
			  (setf (jdi-value-type jdi-value) (bindat-get-field array :type))
			  (setf (jdi-value-value jdi-value) (bindat-get-field value :value)))))))

;;; Customized display and expanders:
(defvar jdi-value-custom-set-strings nil
  "a list of (instance set-string-func) where

instance is a string that is matched with jdi-value-instance-of-p with the 
value

set-string-func is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-string of the jdi-value.")

(setq jdi-value-custom-set-strings
      '(("Ljava/lang/Boolean;"    jdi-value-custom-set-string-boolean)
		("Ljava/lang/Integer;"    jdi-value-custom-set-string-integer)
		("Ljava/util/Collection;" jdi-value-custom-set-string-with-size)
		("Ljava/util/Map;"        jdi-value-custom-set-string-with-size)))

(defvar jdi-value-custom-expanders nil
  "a list of (instance expander-func) where

instance is a string that is matched with jdi-value-instance-of-p with the 
value

expander-func is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-values of the jdi-value.")

(setq jdi-value-custom-expanders
      '(("Ljava/util/Collection;" jdi-value-custom-expand-collection)
		("Ljava/util/Map;"        jdi-value-custom-expand-map)))

(defun jdi-value-custom-set-strings-find (jdi value)
  (let ((element (find-if (lambda (custom)
							(jdi-value-instance-of-p jdi value (car custom)))
						  jdi-value-custom-set-strings)))
    (jdi-trace "jdi-value-custom-set-strings-find value:%s class:%s found:%s" value (jdi-value-class value) element)
    (if element
		(cadr element))))

(defun jdi-value-custom-expanders-find (jdi value)
  (let ((element (find-if (lambda (custom)
							(jdi-value-instance-of-p jdi value (car custom)))
						  jdi-value-custom-expanders)))
    (if element
		(cadr element))))

(defun jdi-value-custom-set-string-boolean (jdi value)
  (setf (jdi-value-has-children-p value) nil)
  (jdi-value-get-field-value 
   jdi value "value"
   (lambda (jdi value field-value)
     (if (equal 0 field-value)
		 (setf (jdi-value-string value) "Boolean.FALSE")
       (setf (jdi-value-string value) "Boolean.TRUE")))))

(defun jdi-value-custom-set-string-integer (jdi value)
  (setf (jdi-value-has-children-p value) nil)
  (jdi-value-get-field-value 
   jdi value "value"
   (lambda (jdi value field-value)
	 (setf (jdi-value-string value) (format "%d" field-value)))))

(defun jdi-value-custom-set-string-with-size (jdi value)
  (jdi-trace "jdi-value-custom-set-string-with-size:%s" (jdi-class-name (jdi-value-class value)))
  (ado (jdi value)
    (jdi-class-resolve-parent jdi (jdi-value-class value))
    (jdi-class-resolve-methods jdi (jdi-value-class value))
    (jdi-value-invoke-method jdi value "size")
    (let ((size (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
	  (setf (jdi-value-has-children-p value) 
			(> size 0))
      (setf (jdi-value-array-length value) size)
      (setf (jdi-value-string value) 
			(format "%s[%d]" (jdi-class-name (jdi-value-class value)) size)))))

(defun jdi-value-custom-expand-collection (jdi value)
  (jdi-info "jdi-value-custom-expand-collection")
  (ado (jdi value)
    (jdi-value-invoke-method jdi value "toArray")
    (let ((array-value (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
      (setf (jdi-value-value value) array-value)
      (jdi-value-get-array-values jdi value))))

(defun jdi-value-custom-expand-map (jdi value)
  (jdi-info "jdi-value-custom-expand-collection")
  (lexical-let (keyset-value
				values-value)
    (ado (jdi value)
      (jdi-value-invoke-method jdi value "keySet")
      (setf keyset-value (make-jdi-value :value 
										 (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
      (setf (jdi-value-class keyset-value) (jdi-classes-find-by-signature jdi "Ljava/util/Set;"))
      (jdi-trace "keyset-value:%s" keyset-value)
      (ado (jdi value keyset-value)
		(jdi-value-invoke-method jdi keyset-value "toArray")
		(let ((key-array (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
		  (ado (jdi value key-array)
			(setf (jdi-value-name keyset-value) "key  ")
			(setf (jdi-value-array-length keyset-value) (jdi-value-array-length value))
			(setf (jdi-value-value keyset-value) key-array)
			(jdi-value-get-array-values jdi keyset-value))))

      (jdi-value-invoke-method jdi value "values")
      ;;    (jdi-trace "values returned:%s" ado-last-return-value)
      (setf values-value (make-jdi-value :value 
										 (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
      (setf (jdi-value-class values-value) (jdi-classes-find-by-signature jdi "Ljava/util/Collection;"))
      (jdi-trace "values-value:%s" values-value)
      (ado (jdi value values-value)
		(jdi-value-invoke-method jdi values-value "toArray")
		(let ((values-array (bindat-get-field (car ado-last-return-value) :return-value :u :value)))
		  (ado (jdi value values-array)
			(setf (jdi-value-name values-value) "value")
			(setf (jdi-value-array-length values-value) (jdi-value-array-length value))
			(setf (jdi-value-value values-value) values-array)
			(jdi-value-get-array-values jdi values-value))))

      (setf (jdi-value-values value)
			(loop for key in (jdi-value-values keyset-value)
				  for value in (jdi-value-values values-value)
				  append (list key value)))
      (jdi-trace "keys=%d, values=%d" 
				 (length (jdi-value-values keyset-value))
				 (length (jdi-value-values values-value))))))

(provide 'jdi)

;;; jdi.el ends here