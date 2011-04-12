;;; jdi.el --- library that provides the Java(tm) Debug Interface

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

;; This module tries to implement everything documented here:
;; http://java.sun.com/j2se/1.4.2/docs/guide/jpda/jdi/

;; Look near the end of the file for customizing how special
;; objects (ArrayList, HashMap) are displayed in the locals buffer

;; This module requires bindat.el, elog.el and jdwp.el

;;; Code:

(require 'bindat)
(require 'elog)
(require 'jdwp)

(elog-make-logger jdi)


(defvar jdi-unit-testing nil)

(eval-when-compile
  (setq jdi-unit-testing t)
  (defvar jdibug-active-thread))

(defstruct jdi-mirror
  virtual-machine)

(defstruct jdi-virtual-machine
  jdwp

  description
  jdwp-major
  jdwp-minor
  version
  name

  (event-request-manager (make-jdi-event-request-manager))
  host
  port
  (classes (make-hash-table :test 'equal)) ;; hash table where key=class-id value=jdi-class
  classes-by-signature ;; hash table where key=signature value=jdi-class

  suspended-thread-id

  ;; list of jdi-frame that is suspended (by breakpoint/step events)
  suspended-frames

  (objects (make-hash-table :test 'equal)) ;; hash table where key=object-id value=jdi-object

  ;; Exception breakpoint workaround data
  (break-on-exception-data (make-hash-table))
  )

(defstruct (jdi-reference-type (:include jdi-mirror))
  id
  fields-cache)

(defstruct (jdi-class (:include jdi-reference-type))
  ;; jni-style
  signature
  generic-signature
  ref-type-tag
  status
  ;; list of jdi-field
  fields
  ;; parent jdi-class
  super
  ;; list of interfaces implemented by this class
  interfaces
  interfaces-count ;; we use this to see whether the interfaces field have been resolved or not
  ;; list of jdi-method
  methods
  ;; list of nested classes.  This is a cached value and might be nil.
  ;; Use jdi-class-get-nested-classes instead.
  nested-classes)

(defstruct (jdi-interface (:include jdi-reference-type)))

(defstruct (jdi-array-type (:include jdi-reference-type)))

(defstruct (jdi-method (:include jdi-mirror))
  id
  name
  signature
  mod-bits

  ;; list of jdi-location
  locations

  ;; list of jdi-variable
  variables

  ;; link back to our containing jdi-class
  class
  )

(defstruct (jdi-location (:include jdi-mirror))
  ;; this might have to be resolved
  line-number
  line-code-index
  type

  ;; jdi-class
  class
  ;; jdi-method
  method

  ;; String to be displayed in the frames buffer
  string)

(defstruct jdi-event-request-manager
  breakpoint-requests
  )

(defstruct (jdi-event-request (:include jdi-mirror))
  id
  data
  )

;; com.sun.jdi.Value
(defstruct (jdi-value (:include jdi-mirror))
  type)

(defstruct (jdi-primitive-value (:include jdi-value))
  value)

(defstruct (jdi-object (:include jdi-value)) ;; com.sun.jdi.ObjectReference
  id)

(defstruct (jdi-string (:include jdi-object)))

(defstruct (jdi-array (:include jdi-object)))

(defstruct (jdi-class-object (:include jdi-object)))

(defstruct (jdi-class-loader (:include jdi-object)))

(defstruct (jdi-thread (:include jdi-object))
  (running-p t)

  thread-group-cache
  daemon-p-cache
  system-thread-p-cache
  )

(defstruct (jdi-thread-group (:include jdi-object))
  name
  parent
  child-threads
  child-groups)

(defstruct (jdi-frame (:include jdi-mirror)) ;; this is actually StackFrame in JDI
  id
  values
  thread
  location)

(defstruct (jdi-variable (:include jdi-mirror)) ;; this is actually LocalVariable in JDI
  code-index
  name
  signature
  length
  slot)

(defstruct (jdi-field (:include jdi-mirror))
  id
  name
  signature
  generic-signature
  mod-bits)

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

(defun jdi-mirror-jdwp (mirror)
  (jdi-virtual-machine-jdwp (jdi-mirror-virtual-machine mirror)))

(defun jdi-event-request-manager-create-breakpoint (erm location)
  (let* ((location-data `((:type .   1)
						  (:class-id . ,(jdi-class-id (jdi-location-class location)))
						  (:method-id . ,(jdi-method-id (jdi-location-method location)))
						  (:index     . ,(jdi-location-line-code-index location))))
		 (data `((:event-kind     . ,jdwp-event-breakpoint)
				 (:suspend-policy . ,jdwp-suspend-policy-event-thread)
				 (:modifiers      . 1)
				 (:modifier       ((:mod-kind .  ,jdwp-mod-kind-location-only)
								   (:location .  ,location-data))))))
	(make-jdi-event-request :virtual-machine (jdi-mirror-virtual-machine location) :data data)))

(defun jdi-mappend (fn &rest lsts)
  (apply 'append (apply 'mapcar fn lsts)))

(defun jdi-event-request-manager-create-step (erm thread depth)
  (let ((data `((:event-kind     . ,jdwp-event-single-step)
				(:suspend-policy . ,jdwp-suspend-policy-event-thread)
				(:modifiers      . 2)
				(:modifier
				 ((:mod-kind . ,jdwp-mod-kind-case-step)
				  (:thread   . ,(jdi-thread-id thread))
				  (:size     . 1)
				  (:depth    . ,depth))
				 ((:mod-kind . ,jdwp-mod-kind-case-count)
				  (:count    . 1))))))
	(make-jdi-event-request :virtual-machine (jdi-mirror-virtual-machine thread) :data data)))

(defun jdi-event-request-manager-create-class-prepare (erm vm signature)
  (jdi-debug "jdi-event-request-manager-create-class-prepare %s" signature)
  (let ((data `((:event-kind     . ,jdwp-event-class-prepare)
				(:suspend-policy . ,jdwp-suspend-policy-event-thread)
				(:modifiers      . 1)
				(:modifier
				 ((:mod-kind . ,jdwp-mod-kind-class-match)
				  (:class-pattern . ((:length . ,(length signature))
									 (:string . ,signature))))))))
	(make-jdi-event-request :virtual-machine vm :data data)))

(defun jdi-event-request-manager-create-break-on-exception (erm vm class-id caught uncaught)
  (let ((data `((:event-kind     . ,jdwp-event-exception)
					 (:suspend-policy . ,jdwp-suspend-policy-event-thread)
					 (:modifiers      . 1)
					 (:modifier
					  ((:mod-kind . ,jdwp-mod-kind-exception-only)
					 	(:class-pattern . (
					 							 (:exception . ,class-id)
					 							 (:caught . ,caught)
					 							 (:uncaught . ,uncaught)))))

)))
	(make-jdi-event-request :virtual-machine vm :data data)))

(defun jdi-event-request-manager-create-break-on-exception-workaround (erm vm class-id caught uncaught)
  "It appears that the exception-only modifier does not work, as
the event is never sent.  This method generates an event request
that breaks for all exceptions.  The handling code must then
check the caught/uncaught properties and if the exception is the
correct class."
  (let ((data `((:event-kind     . ,jdwp-event-exception)
					 (:suspend-policy . ,jdwp-suspend-policy-event-thread)
					 (:modifiers      . 0)
					 )))
	 (make-jdi-event-request :virtual-machine vm :data data)))

(defstruct jdi-break-on-exception-workaround-data
  class-id caught uncaught)

(defun jdi-event-request-enable (er)
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp er) "set" (jdi-event-request-data er))))
	(jdi-trace "received requestid:%s" (bindat-get-field reply :request-id))
	(setf (jdi-event-request-id er) (bindat-get-field reply :request-id))))

(defun jdi-event-request-disable (er)
  (jdwp-send-command (jdi-mirror-jdwp er) "clear" `((:event . ,jdwp-event-breakpoint) (:request-id . ,(jdi-event-request-id er))))
  (jdi-trace "cleared event request"))

(defun jdi-virtual-machine-set-standard-events (vm)
  (mapcar (lambda (event)
			(jdwp-send-command (jdi-virtual-machine-jdwp vm) "set"
							   `((:event-kind . ,event)
								 (:suspend-policy . ,jdwp-suspend-policy-none)
								 (:modifiers . 0))))
		  (list jdwp-event-class-unload
				jdwp-event-thread-start
				jdwp-event-thread-end
				jdwp-event-vm-death)))

(defun jdi-virtual-machine-connect (vm)
  "[ASYNC] returns t if success, nil on failure"
  (when (jdwp-connect (jdi-virtual-machine-jdwp vm) (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
	(jdwp-put (jdi-virtual-machine-jdwp vm) 'jdi-virtual-machine vm)
	(let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "version" nil)))
	  (setf (jdi-virtual-machine-description vm) (jdwp-get-string reply :description)
			(jdi-virtual-machine-jdwp-major vm)  (bindat-get-field reply :jdwp-major)
			(jdi-virtual-machine-jdwp-minor vm)  (bindat-get-field reply :jdwp-minor)
			(jdi-virtual-machine-version vm)  (jdwp-get-string reply :vm-version)
			(jdi-virtual-machine-name vm)     (jdwp-get-string reply :vm-name))

	  (jdi-trace "description: \n%s" (jdwp-get-string reply :description))
	  (jdi-trace "major      : %s"   (bindat-get-field reply :jdwp-major))
	  (jdi-trace "minor      : %s"   (bindat-get-field reply :jdwp-minor))
	  (jdi-trace "version    : %s"   (jdwp-get-string reply :vm-version))
	  (jdi-trace "name       : %s"   (jdwp-get-string reply :vm-name))
	  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "capabilities-new" nil)))
		(jdi-trace "capabilities-new:%s" reply)
		(jdi-virtual-machine-set-standard-events vm)
		t))))

(defun jdi-virtual-machine-get-threads (vm)
  (jdi-debug "jdi-virtual-machine-get-threads")
  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "all-threads" nil)))
	(jdi-debug "number of threads:%s" (bindat-get-field reply :threads))
	(loop for thread in (bindat-get-field reply :thread)
		  collect (jdi-virtual-machine-get-object-create
				   vm
				   (make-jdi-thread :id (bindat-get-field thread :id))))))

(defun jdi-virtual-machine-get-top-level-thread-groups (vm)
  (jdi-debug "jdi-virtual-machine-get-top-level-thread-groups")
  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "top-level-thread-groups" nil)))
	(jdi-debug "jdi-virtual-machine-get-top-level-thread-groups:number = %s" (bindat-get-field reply :groups))
	(loop for group in (bindat-get-field reply :group)
		  collect (jdi-virtual-machine-get-object-create vm (make-jdi-thread-group :id (bindat-get-field group :id))))))

(defun jdi-virtual-machine-get-all-thread-groups (vm)
  (jdi-debug "jdi-virtual-machine-get-all-thread-groups")
  (jdi-virtual-machine-get-top-level-thread-groups vm))

(defun jdi-virtual-machine-get-classes-by-signature (vm signature)
  (jdi-debug "jdi-virtual-machine-get-classes-by-signature:%s" signature)
  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm)
								  "classes-by-signature"
								  `((:signature . ((:length . ,(length signature))
												   (:string . ,signature)))))))
	(jdi-debug "number of classes matched:%s" (bindat-get-field reply :classes))
	(loop for class        in (bindat-get-field reply :class)
		  for type-id      =  (bindat-get-field class :type-id)
		  for ref-type-tag =  (bindat-get-field class :ref-type-tag)
		  for status       =  (bindat-get-field class :status)
		  for newclass     =  (jdi-virtual-machine-get-class-create vm type-id
																	:signature signature
																	:ref-type-tag ref-type-tag
																	:status status)
		  collect newclass)))

(defun jdi-virtual-machine-get-suspended-threads (vm)
  (jdi-debug "jdi-virtual-machine-suspended-threads")
  (let* ((threads (jdi-virtual-machine-get-threads vm))
		 (suspendeds (mapcar 'jdi-thread-get-suspended-p threads)))
	(loop for suspended in suspendeds
		  for thread in threads
		  if suspended collect thread)))

(defun* jdi-virtual-machine-get-class-create (vm id &key signature ref-type-tag status)
  (let ((found (gethash id (jdi-virtual-machine-classes vm))))
	(jdi-debug "jdi-virtual-machine-get-class-create id=%s: %s" id (if found "found" "not found"))
	(if found
		(progn
		  (if signature (setf (jdi-class-signature found) signature))
		  (if ref-type-tag (setf (jdi-class-ref-type-tag found) ref-type-tag))
		  (if status (setf (jdi-class-status found)) status))
	  (puthash id (setq found (make-jdi-class :virtual-machine vm
											  :id id
											  :signature signature
											  :ref-type-tag ref-type-tag
											  :status status))
			   (jdi-virtual-machine-classes vm)))
	found))

(defun jdi-virtual-machine-get-object-create (vm newobj)
  (jdi-debug "jdi-virtual-machine-get-object-create:%s" (jdi-object-id newobj))
  (let ((found (gethash (jdi-object-id newobj) (jdi-virtual-machine-objects vm))))
	(jdi-debug "jdi-virtual-machine-get-object-create:%s" (if found "found" "not found"))
	(unless found
	  (setf (jdi-mirror-virtual-machine newobj) vm)
	  (puthash (jdi-object-id newobj) (setq found newobj) (jdi-virtual-machine-objects vm)))
	found))

(defun jdi-thread-get-name (thread)
  (jdi-debug "jdi-thread-get-name")
  (jdwp-get-string (jdwp-send-command (jdi-mirror-jdwp thread) "thread-name" `((:thread . ,(jdi-thread-id thread)))) :thread-name))

(defun jdi-thread-update-status (thread)
  "Query the virtual machine to get an up to date status on the thread since the cached value can get stale"
  (jdi-debug "jdi-thread-update-status")
  (multiple-value-bind (status suspend-status) (jdi-thread-get-status thread)
	(jdi-debug "thread status=%s suspend-status=%s jdwp-susp=%s " status suspend-status jdwp-suspend-status-suspended)
	(setf (jdi-thread-running-p thread) (not (= suspend-status jdwp-suspend-status-suspended)))))


(defun jdi-thread-get-status (thread)
  (jdi-debug "jdi-thread-get-status")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "thread-status" `((:thread . ,(jdi-thread-id thread))))))
	(jdi-trace "reply=%s" reply)
	(values (bindat-get-field reply :thread-status)
			(bindat-get-field reply :suspend-status))))

;; a symbol that we use to mean that we have resolved
;; the value and the result is nil
(defvar jdi-cache-nil nil)

(defmacro jdi-with-cache (acc obj form)
  `(cond ((null (,acc ,obj))
		  (setf (,acc ,obj)
				,form)
		  (if (,acc ,obj)
			  (,acc ,obj)
			(setf (,acc ,obj) 'jdi-cache-nil)
			nil))
		 ((equal (,acc ,obj) 'jdi-cache-nil)
		  nil)
		 (t (,acc ,obj))))

(defun jdi-thread-get-thread-group (thread)
  (jdi-debug "jdi-thread-get-thread-group:%s" (jdi-thread-id thread))
  (jdi-with-cache jdi-thread-thread-group-cache thread
				  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "thread-group" `((:thread . ,(jdi-thread-id thread))))))
					(jdi-debug "jdi-thread-get-thread-group:group=%s" (bindat-get-field reply :group))
					(jdi-virtual-machine-get-object-create
					 (jdi-mirror-virtual-machine thread)
					 (make-jdi-thread-group :id (bindat-get-field reply :group))))))

(defun jdi-thread-get-system-thread-p (thread)
  (jdi-debug "jdi-thread-get-system-thread-p:%s" (jdi-thread-id thread))
  (jdi-with-cache jdi-thread-system-thread-p-cache thread
				  (let ((group (jdi-thread-get-thread-group thread)))
					(jdi-thread-group-get-system-thread-p group))))

(defun jdi-thread-get-daemon-p (thread)
  (jdi-debug "jdi-thread-get-daemon-p:%s" (jdi-thread-id thread))
  (jdi-with-cache jdi-thread-daemon-p-cache thread
				  (let* ((reference-type (jdi-object-get-reference-type thread))
						 (field (jdi-reference-type-get-field-by-name reference-type "daemon")))
					(if (null field)
						(setq field (jdi-reference-type-get-field-by-name reference-type "isDaemon")))
					(if field
						(if (string= (car (jdi-jni-to-print (jdi-field-signature field))) "boolean")
							(not (= 0 (jdi-primitive-value-value (jdi-object-get-value thread field)))))))))

(defun jdi-thread-group-get-system-thread-p (thread-group)
  (jdi-debug "jdi-thread-group-get-system-thread-p:%s" (jdi-thread-group-id thread-group))
  (let ((name (jdi-thread-group-get-name thread-group)))
	(let ((parent (jdi-thread-group-get-parent thread-group)))
	  (if (string= name "main")
		  nil
		(if parent
			(jdi-thread-group-get-system-thread-p parent)
		  t)))))

(defun jdi-thread-group-get-name (thread-group)
  (jdi-debug "jdi-thread-group-get-name:%s" (jdi-thread-group-id thread-group))
  (if (jdi-thread-group-name thread-group)
	  (jdi-thread-group-name thread-group)

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread-group) "thread-group-name" `((:group . ,(jdi-thread-group-id thread-group))))))
	  (setf (jdi-thread-group-name thread-group) (jdwp-get-string reply :group-name)))))

(defun jdi-thread-group-get-parent (thread-group)
  (jdi-debug "jdi-thread-group-get-parent:%s" (jdi-thread-group-id thread-group))
  (if (jdi-thread-group-parent thread-group)
	  (if (equal (jdi-thread-group-parent thread-group) t)
		  nil
		(jdi-thread-group-parent thread-group))

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread-group) "thread-group-parent" `((:group . ,(jdi-thread-group-id thread-group))))))
	  (let ((group-id (bindat-get-field reply :parent-group)))
		(if (equal group-id [0 0 0 0 0 0 0 0])
			(progn
			  (setf (jdi-thread-group-parent thread-group) t)
			  nil)
		  (setf (jdi-thread-group-parent thread-group) (jdi-virtual-machine-get-object-create
														(jdi-mirror-virtual-machine thread-group)
														(make-jdi-thread-group :id (bindat-get-field reply :parent-group)))))))))

(defun jdi-thread-group-get-children (thread-group)
  (jdi-debug "jdi-thread-group-get-children:%s" (jdi-thread-group-id thread-group))
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread-group) "thread-group-children" `((:group . ,(jdi-thread-group-id thread-group))))))
	(setf (jdi-thread-group-child-threads thread-group)
		  (loop for thread in (bindat-get-field reply :child-thread)
				collect (jdi-virtual-machine-get-object-create (jdi-mirror-virtual-machine thread-group)
															   (make-jdi-thread :id (bindat-get-field thread :child-thread)))))
	(setf (jdi-thread-group-child-groups thread-group)
		  (loop for group in (bindat-get-field reply :child-group)
				collect (jdi-virtual-machine-get-object-create (jdi-mirror-virtual-machine thread-group)
															   (make-jdi-thread-group :id (bindat-get-field group :child-group)))))
	(values (jdi-thread-group-child-threads thread-group) (jdi-thread-group-child-groups thread-group))))

(defun jdi-thread-group-get-thread-groups (thread-group)
  (jdi-debug "jdi-thread-group-get-thread-groups:%s" (jdi-thread-group-id thread-group))
  (multiple-value-bind (child-threads child-groups) (jdi-thread-group-get-children thread-group)
	child-groups))

(defun jdi-thread-group-get-threads (thread-group)
  (jdi-debug "jdi-thread-group-get-threads:%s" (jdi-thread-group-id thread-group))
  (multiple-value-bind (child-threads child-groups) (jdi-thread-group-get-children thread-group)
	child-groups))

(defun jdi-virtual-machine-disconnect (vm)
  (jdwp-disconnect (jdi-virtual-machine-jdwp vm)))

(defun jdi-virtual-machine-exit (vm exit-code)
  (jdwp-exit (jdi-virtual-machine-jdwp vm) `((:exit-code . ,exit-code))))

(defun jdi-class-get-all-methods (class)
  (jdi-debug "jdi-class-get-all-methods")
  (let ((supers (jdi-class-get-all-super class)))
	(jdi-mappend 'jdi-class-get-methods (cons class supers))))

(defun jdi-class-get-methods (class)
  "returns a list of jdi-method in the class"
  (jdi-debug "jdi-class-get-methods:%s" (jdi-class-id class))
  (if (jdi-class-methods class)
	  (jdi-class-methods class)
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class)
											  (jdi-get-methods-command-name class)
											  `((:ref-type . ,(jdi-class-id class))))))
	  (jdi-debug "number of methods:%s" (bindat-get-field reply :methods))
	  (setf (jdi-class-methods class)
			(loop for method in (bindat-get-field reply :method)
				  collect (make-jdi-method :virtual-machine (jdi-mirror-virtual-machine class)
										   :class class
										   :id (bindat-get-field method :method-id)
										   :name (jdwp-get-string method :name)
										   :signature (jdwp-get-string method :signature)
										   :mod-bits (bindat-get-field method :mod-bits))
				  do (jdi-debug "jdi-class-get-methods:name=%s:signature=%s" (jdwp-get-string method :name) (jdwp-get-string method :signature))))
	  (jdi-class-methods class))))

(defun jdi-class-get-signature (class)
  (jdi-debug "jdi-class-get-signature")
  (if (jdi-class-signature class)
	  (jdi-class-signature class)
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class)
											  (if (jdi-virtual-machine-has-generic-p
													 (jdi-mirror-virtual-machine class))
													"signature-with-generic"
												 "signature")
											  `((:ref-type . ,(jdi-class-id class))))))
	  (setf (jdi-class-generic-signature class) (jdwp-get-string reply :generic-signature)
			  (jdi-class-signature class) (jdwp-get-string reply :signature)))))

(defun jdi-class-get-signature-by-id (vm class-id)
  (let ((class (jdi-virtual-machine-get-class-create vm class-id)))
	 (jdi-class-get-signature class)))

(defun jdi-class-get-generic-signature (class)
  (jdi-debug "jdi-class-get-generic-signature")
  (when (jdi-virtual-machine-has-generic-p (jdi-mirror-virtual-machine class))
	(if (jdi-class-generic-signature class)
		(jdi-class-generic-signature class)
	  (let ((reply (jdwp-send-command (jdi-mirror-jdwp class) "signature-with-generic" `((:ref-type . ,(jdi-class-id class))))))
		(setf (jdi-class-signature class) (jdwp-get-string reply :signature)
			  (jdi-class-generic-signature class) (jdwp-get-string reply :generic-signature))))))

(defcustom jdi-class-nested-classes-workaround t
  "The default 1.6 JVM will not return anonymous nested classes.
Among other things, this can break setting breakpoints in such
classes.  If this is true, an alternate implementation is used
that works around this problem.  It is possible for the
workaround to return incorrect values is classes are created that
have $ in the class name but are not inner classes."
  :type 'boolean
  :group 'jdibug)

(defun jdi-class-get-nested-classes (class)
  "Return the (possibly cached) list of nested classes for this class.  Note: this is not implemented in the default 1.6 JVM for anonymous classes.  As a workaround, if `jdi-class-nested-classes-workaround' is non-nil, we use a workaround to query for all classes and match against the names to determine the direct nested classes."
  (jdi-debug "jdi-class-get-nested-classes")
  (or (jdi-class-nested-classes class)
		(setf (jdi-class-nested-classes class)
				(if jdi-class-nested-classes-workaround
					 (jdi-fetch-nested-classes-workaround class)
				  (jdi-fetch-nested-classes class)))
			 ;; TODO: create a class-prepare event to update the list of
			 ;; nested classes if additional nested classes are loaded.
		))

(defun jdi-fetch-nested-classes-workaround (class)
  "Get the nested classes of CLASS by fetching all the classes,
then filter out any that do not match the signature of a nested
class of CLASS.  This works around the bug in JDWP that does not
return anonymous nested classes when asking for nested classes.
It is probably very slow."
  (let* ((vm (jdi-mirror-virtual-machine class))
			(candidate-classes (jdi-get-all-classes vm))
			(signature  (jdi-class-get-signature class))
			(sig-less-1 (substring signature 0 (1- (length signature))))
			(inner-sig-re (concat "^"
										 (regexp-quote sig-less-1)
										 "\\$[^$]+;$")))
			(jdi-debug "Accepting classes of the form %s" inner-sig-re)
	 (loop for nested in candidate-classes
			 for nested-sig = (jdi-class-get-signature nested)
			 if (string-match inner-sig-re nested-sig)
			 collect nested)))

(defun jdi-get-all-classes (vm)
  "Get all classes from a VM. This is probably very slow and should not be used."
  (jdi-debug "jdi-get-all-classes")
  (let* ((jdwp  (jdi-virtual-machine-jdwp vm))
			(reply (jdwp-send-command jdwp "all-classes" nil)))
	(jdi-debug "number of classes matched:%s" (bindat-get-field reply :classes))
	(loop for class       in (bindat-get-field reply :class)
		  for type-id      =  (bindat-get-field class :type-id)
		  for ref-type-tag =  (bindat-get-field class :ref-type-tag)
		  for status       =  (bindat-get-field class :status)
		  for signature    =  (jdwp-get-string class :signature)
		  for newclass     =  (jdi-virtual-machine-get-class-create vm type-id
																	:signature signature
																	:ref-type-tag ref-type-tag
																	:status status)
		  collect newclass)))


(defun jdi-fetch-nested-classes (class)
  "Get the nested classes of CLASS by fetching the classes using
the nested-classes command."
  (let* ((jdwp  (jdi-mirror-jdwp class))
			(reply (jdwp-send-command jdwp "nested-types"
											  `((:ref-type . ,(jdi-class-id class))))))
	 (loop with vm = (jdi-class-virtual-machine class)
			 for nested in (bindat-get-field reply :nested)
			 for id = (bindat-get-field nested :id)
			 for new-class = (jdi-virtual-machine-get-class-create vm id)
			 collect new-class
			 do (jdi-debug "jdi-class-get-nested-classes:id=%s" id))))


(defun jdi-method-get-signature (method)
  (jdi-debug "jdi-method-get-signature")
  (if (jdi-method-signature method)
	  (jdi-method-signature method)
	(setf (jdi-method-signature method)
		  (jdi-method-signature (find-if (lambda (obj)
										   (equal (jdi-method-id method) (jdi-method-id obj)))
										 (jdi-class-get-methods (jdi-method-class method)))))))

(defun jdi-method-get-locations (method)
  (jdi-debug "jdi-method-get-locations:class=%s method=%s" (jdi-class-signature (jdi-method-class method)) (jdi-method-name method))
  (if (or (jdi-method-locations method)
		  (jdi-method-native-p method))
	  (jdi-method-locations method)
	(let ((reply (jdwp-send-command
				  (jdi-mirror-jdwp method) "line-table" `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
														  (:method-id . ,(jdi-method-id method))))))
	  (jdi-trace "start=%s:end=%s:lines=%s"
				 (jdwp-string-to-hex (bindat-get-field reply :start))
				 (jdwp-string-to-hex (bindat-get-field reply :end))
				 (bindat-get-field reply :lines))
	  (setf (jdi-method-locations method)
			(loop for line in (bindat-get-field reply :line)
				  collect (make-jdi-location :virtual-machine (jdi-mirror-virtual-machine method)
											 :class (jdi-method-class method)
											 :method method
											 :line-code-index (bindat-get-field line :line-code-index)
											 :line-number (bindat-get-field line :line-number))))
	  (dolist (location (jdi-method-locations method))
		(jdi-trace "line-number:%s line-code-index:%s" (jdi-location-line-number location) (jdi-location-line-code-index location)))
	  (jdi-method-locations method))))

(defun jdi-method-get-name (method)
  (jdi-debug "jdi-method-get-signature")
  (let ((methods (jdi-class-get-methods (jdi-method-class method))))
	(jdi-method-name (find-if (lambda (obj)
								(equal (jdi-method-id obj)
									   (jdi-method-id method)))
							  methods))))

(defun jdi-virtual-machine-set-breakpoint (vm signature line)
  "Set breakpoint and return a list of jdi-event-request"

  (jdi-debug "jdi-virtual-machine-set-breakpoint:signature=%s:line=%s" signature line)
  (let* ((classes (jdi-virtual-machine-get-classes-by-signature vm signature))
			(result (jdi-virtual-machine-set-breakpoint-1 vm classes line))
			(request-manager (jdi-virtual-machine-event-request-manager vm))
			(class-name (car (jdi-jni-to-print signature)))
			;; the class might be loaded again by another class loader
			;; so we install the class prepare event anyway.
			(er (jdi-event-request-manager-create-class-prepare
				  request-manager vm class-name)))
	 (jdi-event-request-enable er)

	 ;; Install a handler for inner classes if we didn't set the
	 ;; breakpoint, since that might be what the problem was
	 (unless result
		(setq er (jdi-event-request-manager-create-class-prepare
					 request-manager vm (concat class-name "$*")))
		(jdi-event-request-enable er))

	 result))

(defun jdi-virtual-machine-set-breakpoint-1 (vm classes line)
  "Attempt to set a breakpoint in CLASSES and all inner classes at LINE"
	(if classes
		 (let (found result)
			(dolist (class classes)
			  ;; If we found the breakpoint in one class, we can skip the
			  ;; rest of the iterations (which should be over other inner
			  ;; classes)
			  (jdi-debug "jdi-virtual-machine-set-breakpoint-1 checking %s" (jdi-class-signature class))
			  (unless found
				 (dolist (location (jdi-class-get-locations-of-line class line))
					(setq found t)
					(let ((er (jdi-event-request-manager-create-breakpoint
								  (jdi-virtual-machine-event-request-manager vm)
								  location)))
					  (jdi-event-request-enable er)
					  (push er result)))
				 ;; If we did not find the breakpoint, look in inner classes.
				 (unless found
					(setq result (append
									  (jdi-virtual-machine-set-breakpoint-1
										vm
										(jdi-class-get-nested-classes class)
										line)
									  result)))))
			result)))


(defcustom jdi-break-on-exception-workaround t
  "Apply a workaround to an apparent bug which causes the JVM to
not report exceptions if they are restricted to a specific
class."
   :type 'boolean
  :group 'jdibug)

(defun jdi-virtual-machine-set-break-on-exception (vm signature caught uncaught)
  "Set break for when an exception is thrown and return a list of
jdi-event-request.  SIGNATURE should be a JNI format
signature (e.g., Ljava/lang/RuntimeException not
java.lang.RuntimeException.

Returns a list of event requests sent to the VM.  This may be nil
if no class matching SIGNATURE is found."

  (jdi-debug "jdi-virtual-machine-set-break-on-exception:signature=%s:caught=%s:uncaught=%s"
			 signature caught uncaught)
  (let (result
		(classes (jdi-virtual-machine-get-classes-by-signature vm signature)))
	(dolist (class classes)
	  (if (jdi-class-instance-of-p
			 class
			 (jdi-class-name-to-class-signature "java.lang.Throwable"))
		  (progn
			 ;; Create the event request and enable it.
			 (let* ((class-id  (jdi-class-id class))
					 (erm (jdi-virtual-machine-event-request-manager vm))
					 ;; Form of the event request depends on if the
					 ;; workaround is enabled.
					 (er (funcall (if jdi-break-on-exception-workaround
											'jdi-event-request-manager-create-break-on-exception-workaround
										 'jdi-event-request-manager-create-break-on-exception)
									  erm vm class-id caught uncaught))
					 (request-id (jdi-event-request-enable er)))
				;; Store the data for the exception workaround.  Always
				;; store it in case the user turns on the workaround
				;; later.
				(puthash request-id (make-jdi-break-on-exception-workaround-data
											:class-id class-id
											:caught caught
											:uncaught uncaught)
							(jdi-virtual-machine-break-on-exception-data vm))
				(push er result)))
		 (jdi-warn "%s is not a subclass of Throwable.  Not setting break when thrown."
					  (jdi-jni-to-print (jdi-class-get-signature class)))))

	;; the class might be loaded again by another class loader
	;; so we install the class prepare event anyway
	(let ((er (jdi-event-request-manager-create-class-prepare
			   (jdi-virtual-machine-event-request-manager vm)
			   vm
			   (car (jdi-jni-to-print signature)))))
	  (jdi-event-request-enable er))
	;; Return event requests.
	result))



(defun jdi-event-request-manager-create-inner-class-prepare (class)
  "Request prepare events for the inner classes of CLASS"
  (jdi-debug "jdi-event-request-manager-create-inner-class-prepare %s" (jdi-class-signature class))
  (let* ((vm (jdi-mirror-virtual-machine class))
			(signature (jdi-class-signature class))
			(er (jdi-event-request-manager-create-class-prepare
				  (jdi-virtual-machine-event-request-manager vm)
				  vm
				  (concat (car (jdi-jni-to-print signature)) "$*"))))
	 er))



(defun jdi-virtual-machine-has-generic-p (vm)
  (>= (jdi-virtual-machine-jdwp-minor vm) 5))

(defun jdi-method-location-by-line-code-index (method line-code-index)
  (jdi-trace "jdi-method-location-by-line-code-index:%s:%s:%s" (jdi-method-name method) line-code-index (length (jdi-method-locations method)))
  (if jdi-trace-flag
      (loop for loc in (jdi-method-locations method)
			do
			(jdi-trace "line-code-index:%s line-number:%s" (jdi-location-line-code-index loc) (jdi-location-line-number loc))))
  (let ((result))
    (loop for loc in (jdi-method-locations method)
		  while (>= (jdwp-vec-to-int line-code-index)
					(jdwp-vec-to-int (jdi-location-line-code-index loc)))
		  do (setq result loc))
    (if (null result)
		(setq result (car (last (jdi-method-locations method)))))
    (if result
		(jdi-trace "found at line-number:%s" (jdi-location-line-number result)))
    result))

(defun jdi-class-get-locations-of-line (class line-number)
  (jdi-debug "jdi-class-get-locations-of-line")
  (let ((result))
	(dolist (method (jdi-class-get-methods class))
	  (dolist (location (jdi-method-get-locations method))
		(if (equal line-number (jdi-location-line-number location))
			(push location result))))
	result))

(defun jdi-class-find-location (class method-id line-code-index)
  (jdi-debug "jdi-class-find-location:number of methods=%s" (length (jdi-class-methods class)))
  (let* ((method (find-if (lambda (method)
							(equal (jdi-method-id method) method-id))
						  (jdi-class-methods class)))
		 (location (if method (jdi-method-location-by-line-code-index method line-code-index))))
    (if location
		location
	  (jdi-error "failed to look line-code-index %s in class %s" line-code-index (jdi-class-name class)))))

(defun jdi-location-get-line-number (location)
  (jdi-debug "jdi-location-get-line-number:wanted-line-code-index=%s" (jdi-location-line-code-index location))
  (let ((methods (jdi-class-get-methods (jdi-location-class location))))
	(jdi-debug "jdi-location-get-line-number: methods=%s" (length methods))
	(setf (jdi-location-method location)
		  (find-if (lambda (obj)
					 (equal (jdi-method-id obj)
							(jdi-method-id (jdi-location-method location))))
				   methods))
	(let ((locations (jdi-method-get-locations (jdi-location-method location))))
	  (jdi-debug "jdi-location-get-line-number: locations=%s" (length locations))
	  (let ((found))
		(loop for loc in locations
			  while (>= (jdwp-vec-to-int (jdi-location-line-code-index location))
						(jdwp-vec-to-int (jdi-location-line-code-index loc)))
			  do (setq found loc))
		(jdi-debug "jdi-location-get-line-number:found=%s" (if found "yes" "no"))
		(if found (setf (jdi-location-line-number location) (jdi-location-line-number found)))
		(jdi-location-line-number location)))))

(defun jdi-thread-get-frames (thread)
  (jdi-debug "jdi-thread-get-frames")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "frame-count" `((:thread . ,(jdi-thread-id thread))))))
	(jdi-debug "number of frames:%s" (bindat-get-field reply :frame-count))
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "frames" `((:thread . ,(jdi-thread-id thread))
																		(:start-frame . 0)
																		(:length . ,(bindat-get-field reply :frame-count))))))
	  (loop for frame           in (bindat-get-field reply :frame)
			for class-id        = (bindat-get-field frame :location :class-id)
			for method-id       = (bindat-get-field frame :location :method-id)
			for type            = (bindat-get-field frame :location :type)
			for line-code-index = (bindat-get-field frame :location :index)

			for class           = (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine thread) class-id)
			for method          = (make-jdi-method :virtual-machine (jdi-mirror-virtual-machine thread)
												   :id method-id
												   :class class)

			for location        = (make-jdi-location :virtual-machine (jdi-mirror-virtual-machine thread)
													 :class class
													 :method method
													 :type type
													 :line-code-index line-code-index)
			collect (make-jdi-frame :virtual-machine (jdi-mirror-virtual-machine thread)
									:thread thread
									:location location
									:id (bindat-get-field frame :id))))))

(defun jdi-thread-get-suspended-p (thread)
  (jdi-debug "jdi-thread-get-suspended-p")
  (multiple-value-bind (status suspend-status) (jdi-thread-get-status thread)
	(= suspend-status jdwp-suspend-status-suspended)))

(defun jdi-thread-get-suspend-count (thread)
  (jdi-trace "jdi-thread-get-suspend-count")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread)
								  "suspend-count"
								  `((:thread . ,(jdi-thread-id thread))))))
	(bindat-get-field reply :suspend-count)))

(defun jdi-thread-resume (thread)
  (jdi-debug "jdi-thread-resume")
  (setf (jdi-thread-running-p thread) t)
  (jdwp-send-command (jdi-mirror-jdwp thread) "thread-resume" `((:thread . ,(jdi-thread-id thread)))))

(defun jdi-thread-suspend (thread)
  (jdi-debug "jdi-thread-suspend")
  (setf (jdi-thread-running-p thread) nil)
  (jdwp-send-command (jdi-mirror-jdwp thread) "thread-suspend" `((:thread . ,(jdi-thread-id thread)))))

(defun jdi-resume (vm)
  (jdi-debug "jdi-resume")
  ;; TODO: We should update jdi-thread-running-p, but I don't think we have
  ;; a good way of getting all thread threads in the vm
  (jdwp-send-command (jdi-virtual-machine-jdwp vm) "resume" nil))

(defun jdi-thread-send-step (thread depth)
  (jdi-debug "jdi-thread-send-step:depth=%s" depth)
  (setf (jdi-thread-running-p thread) t)
  (let* ((vm (jdi-mirror-virtual-machine thread))
		 (erm (jdi-virtual-machine-event-request-manager vm))
		 (er (jdi-event-request-manager-create-step erm thread depth)))
	(jdi-event-request-enable er)
	(jdi-thread-resume thread)))

(defun jdi-variable-sigbyte (variable)
  (string-to-char (jdi-variable-signature variable)))

(defun jdi-method-get-variables (method)
  (jdi-debug "jdi-method-get-variables")
  (if (jdi-method-variables method)
	  (jdi-method-variables method)

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp method)
											  (jdi-get-variable-table-command-name method)
									`((:ref-type . ,(jdi-class-id (jdi-method-class method)))
									  (:method-id . ,(jdi-method-id method))))))
	  (jdi-trace "variable-table arg-count:%s slots:%s" (bindat-get-field reply :arg-cnt) (bindat-get-field reply :slots))

	  (setf (jdi-method-variables method)
			(loop for slot in (bindat-get-field reply :slot)

				  for code-index = (jdwp-get-int slot :code-index)
				  for name       = (jdwp-get-string slot :name)
				  for signature  = (jdwp-get-string slot :signature)
				  for length     = (bindat-get-field slot :length)
				  for slot2      = (bindat-get-field slot :slot)
				  do (jdi-trace "slot:%s code-index:%s length:%s name:%s signature:%s generic-signature:%s"
								slot2 code-index length name signature nil)
				  collect (make-jdi-variable :virtual-machine (jdi-mirror-virtual-machine method)
											 :code-index code-index
											 :name name
											 :signature signature
											 :length length
											 :slot slot2)))
	  (jdi-method-variables method))))

(defun jdi-frame-get-visible-variables (frame)
  (jdi-debug "jdi-frame-get-visible-variables")
  ;; we don't cache the variables as the location within the same frame might change
  (let ((variables (jdi-method-get-variables (jdi-location-method (jdi-frame-location frame)))))
	(loop for variable in variables

		  for line-code = (jdwp-vec-to-int (jdi-location-line-code-index (jdi-frame-location frame)))
		  if (and (>= line-code (jdi-variable-code-index variable))
				  (<  line-code (+ (jdi-variable-code-index variable) (jdi-variable-length variable))))
		  collect variable)))

(defun jdi-frame-get-values (frame variables)
  (jdi-debug "jdi-frame-get-values")
  (let ((data `((:thread . ,(jdi-thread-id (jdi-frame-thread frame)))
				(:frame  . ,(jdi-frame-id frame))
				(:slots  . ,(length variables))
				,(nconc (list :slot)
						(mapcar (lambda (variable)
								  `((:slot . ,(jdi-variable-slot variable))
									(:sigbyte . ,(jdi-variable-sigbyte variable))))
								variables)))))
	(let ((reply (jdwp-send-command
				  (jdi-mirror-jdwp frame)
				  "stack-frame-get-values"
				  data)))
	  (loop for variable in variables
			for value in (bindat-get-field reply :value)
			collect (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine frame)
														  (bindat-get-field value :slot-value :type)
														  (bindat-get-field value :slot-value :u :value))))))

(defun jdi-frame-get-this-object (frame)
  (jdi-debug "jdi-frame-get-this-object")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp frame) "stack-frame-this-object" `((:thread . ,(jdi-thread-id (jdi-frame-thread frame)))
																					  (:frame  . ,(jdi-frame-id frame))))))
	(jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine frame)
										  (bindat-get-field reply :object-this :type)
										  (bindat-get-field reply :object-this :u :value))))

(defun jdi-virtual-machine-get-value-create (vm type value)
  (cond ((member type (list jdwp-tag-boolean
							jdwp-tag-byte
							jdwp-tag-char
							jdwp-tag-double
							jdwp-tag-float
							jdwp-tag-int
							jdwp-tag-long
							jdwp-tag-short
							jdwp-tag-void))
		 (make-jdi-primitive-value :virtual-machine vm
								   :type type
								   :value value))
		((= type jdwp-tag-object)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-object
													:type type
													:id value)))
		((= type jdwp-tag-array)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-array
													:type type
													:id value)))
		((= type jdwp-tag-string)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-string
													:type type
													:id value)))
		((= type jdwp-tag-thread)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-thread
													:type type
													:id value)))
		((= type jdwp-tag-thread-group)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-thread-group
													:type type
													:id value)))
		((= type jdwp-tag-class-object)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-class-object
													:type type
													:id value)))
		((= type jdwp-tag-class-loader)
		 (jdi-virtual-machine-get-object-create vm (make-jdi-class-loader
													:type type
													:id value)))
		(t (error "do not know how to create value of type:%s" type))))

(defun jdi-value-object-p (value)
  (equal (jdi-value-type value) jdwp-tag-object))

(defun jdi-value-array-p (value)
  (equal (jdi-value-type value) jdwp-tag-array))

(defun jdi-generic-signature-to-print (generic-signature)
  (setq generic-signature (replace-regexp-in-string "^<" "" generic-signature))
  (setq generic-signature (replace-regexp-in-string ">.*$" "" generic-signature))
  (setq generic-signature (replace-regexp-in-string "L[^;]*;" "" generic-signature))
  (setq generic-signature (replace-regexp-in-string ":$" "" generic-signature))
  (setq generic-signature (replace-regexp-in-string ":" "," generic-signature))
  generic-signature)

(when jdi-unit-testing
  (assert (equal (jdi-generic-signature-to-print "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/util/AbstractMap<TK;TV;>;Ljava/util/Map<TK;TV;>;Ljava/lang/Cloneable;Ljava/io/Serializable;") "K,V"))
  (assert (equal (jdi-generic-signature-to-print "<E:Ljava/lang/Object;>Ljava/util/AbstractList<TE;>;Ljava/util/List<TE;>;Ljava/util/RandomAccess;Ljava/lang/Cloneable;Ljava/io/Serializable;") "E")))

(defun jdi-class-name (class)
  (jdi-debug "jdi-class-name")
  (if (null class)
	  "null"
	(let ((signature (jdi-class-get-signature class))
		  (generic-signature (jdi-class-get-generic-signature class)))
	  (jdi-debug "jdi-class-name:signature=%s:generic-signature=%s" signature generic-signature)
	  (format "%s%s"
			  (car (jdi-jni-to-print signature t))
			  (if (> (length generic-signature) 0)
				  (format "<%s>" (jdi-generic-signature-to-print generic-signature))
				"")))))


(defun jdi-jni-to-print (sig &optional short)
  "Convert a string of JNI signature to list of printable characters.

If second parameter short is true, it will only return the class name instead of the
fully qualified class name.

For method-type, the return type is returned as the first element of the list.

Implemented according to
http://java.sun.com/j2se/1.5.0/docs/guide/jni/spec/types.html#wp428"
  (let ((array-count 0)
		result)
	(while (> (length sig) 0)
	  (let ((first (substring sig 0 1)))
		(cond ((string= first "Z") (push "boolean" result) (setq sig (substring sig 1)))
			  ((string= first "B") (push "byte"    result) (setq sig (substring sig 1)))
			  ((string= first "C") (push "char"    result) (setq sig (substring sig 1)))
			  ((string= first "S") (push "short"   result) (setq sig (substring sig 1)))
			  ((string= first "I") (push "int"     result) (setq sig (substring sig 1)))
			  ((string= first "J") (push "long"    result) (setq sig (substring sig 1)))
			  ((string= first "F") (push "float"   result) (setq sig (substring sig 1)))
			  ((string= first "D") (push "double"  result) (setq sig (substring sig 1)))
			  ((string= first "V") (push "void"    result) (setq sig (substring sig 1)))
			  ((string= first "L")
			   (let ((end (string-match ";" sig)))
				 (unless end
				   (error "jni class without ;"))
				 (push (if short
						   (replace-regexp-in-string ".*/" "" (substring sig 1 end))
						 (replace-regexp-in-string "/" "." (substring sig 1 end)))
					   result)
				 (setq sig (substring sig (1+ end)))))

			  ;; for array type, we will process it later, so handling of multiple dimension
			  ;; array will be easier
			  ((string= first "[") (incf array-count)      (setq sig (substring sig 1)))

			  ((string= first "(")
			   (let ((end (string-match ")" sig)))
				 (unless end
				   (error "jni method without )"))
				 ;; we will just move the return type to the front and remove the parenthesis
				 (let ((return-type (substring sig (1+ end)))
					   (without-paren (substring sig 1 end)))
				   (setq sig (concat return-type without-paren)))))

			  (t (error "invalid jni signature:%s" first)))

		;; append [] at the end of arrays
		(unless (or (string= first "[")
					(= array-count 0))
		  (let ((lasttype (car result)))
			(dotimes (i array-count)
			  (setq lasttype (concat lasttype "[]")))
			(setq array-count 0)
			(setq result (cons lasttype (cdr result)))))))
	(reverse result)))

(when jdi-unit-testing
  (assert (equal (jdi-jni-to-print "ZBCSIJFD") (list "boolean" "byte" "char" "short" "int" "long" "float" "double")))
  (assert (equal (jdi-jni-to-print "Ljava/lang/String;") (list "java.lang.String")))
  (assert (equal (jdi-jni-to-print "[I") (list "int[]")))
  (assert (equal (jdi-jni-to-print "[[I") (list "int[][]")))
  (assert (equal (jdi-jni-to-print "[[Ljava/lang/String;") (list "java.lang.String[][]")))
  (assert (equal (jdi-jni-to-print "[[Ljava/lang/String;" t) (list "String[][]")))
  (assert (equal (jdi-jni-to-print "()J" t) (list "long")))
  (assert (equal (jdi-jni-to-print "(I)J") (list "long" "int")))
  (assert (equal (jdi-jni-to-print "(ILjava/lang/String;[I)J" t) (list "long" "int" "String" "int[]"))))

(defun jdi-object-get-reference-type (object)
  (jdi-debug "jdi-object-get-reference-type")
  (if (equal (jdi-object-id object) [0 0 0 0 0 0 0 0])
	  nil
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp object)
									"object-reference-type"
									`((:object . ,(jdi-object-id object))))))
	  (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine object) (bindat-get-field reply :type-id)))))

(defalias 'jdi-value-get-reference-type 'jdi-object-get-reference-type)

(defun jdi-array-get-array-length (array)
  (jdi-debug "jdi-array-get-array-length:%s" (jdi-object-id array))
  (let* ((reply (jdwp-send-command
				(jdi-mirror-jdwp array)
				"array-length"
				`((:array-object . ,(jdi-object-id array)))))
		 (field (bindat-get-field reply :array-length)))
	(if (vectorp field)
		(jdwp-vec-to-int field)
	  field)))


(defun jdi-format-string (str)
  "Truncate and escape the string to be displayed."
  (with-output-to-string
	(let ((print-escape-newlines t)
		  (print-escape-nonascii t))
	  (prin1 str))))

(defun jdi-value-has-children-p (value)
  (jdi-debug "jdi-value-has-children-p")
  (and (not (jdi-primitive-value-p value))
	   (and (jdi-object-p value)
			(not (equal (jdi-object-id value) [0 0 0 0 0 0 0 0])))))

(defun jdi-field-static-p (field)
  (not (equal (logand (jdi-field-mod-bits field) jdi-access-static) 0)))

(defun jdi-field-final-p (field)
  (not (equal (logand (jdi-field-mod-bits field) jdi-access-final) 0)))

(defun jdi-remove-duplicated (fields)
  "Return a list of jdi-fields, without duplicated names, the first field is kept."
  (let (names filtered)
	(mapc (lambda (field)
			(when (not (member (jdi-field-name field) names))
			  (push (jdi-field-name field) names)
			  (push field filtered)))
		  fields)
	filtered))

(defun jdi-reference-type-get-all-fields (reference-type)
  (jdi-debug "jdi-reference-type-get-all-fields:%s" (jdi-reference-type-id reference-type))
  (cond ((jdi-class-p reference-type)
		 (let ((supers (jdi-class-get-all-super reference-type)))
		   (jdi-debug "jdi-reference-type-get-all-fields:%s:%s" (jdi-reference-type-id reference-type) (loop for super in supers
																											 collect (jdi-reference-type-id super)))
		   (jdi-mappend 'jdi-reference-type-get-fields (cons reference-type supers))))
		(t (error "do not know how to get all fields for type:%s" (type-of reference-type)))))

(defun jdi-reference-type-get-field-by-name (reference-type field)
  (jdi-debug "jdi-reference-type-get-field-by-name:%s:%s" (jdi-reference-type-id reference-type) field)
  (let ((all-fields (jdi-reference-type-get-all-fields reference-type)))
	(find-if (lambda (obj)
			   (string= (jdi-field-name obj) field))
			 all-fields)))

(defun jdi-object-get-value (object field)
  (jdi-debug "jdi-object-get-value")
  (car (jdi-object-get-values object (list field))))

(defun jdi-object-get-values (object fields)
  (jdi-debug "jdi-object-get-values")
  (let ((static-fields (loop for field in fields
							 if (jdi-field-static-p field) collect field))
		(nonstatic-fields (loop for field in fields
								unless (jdi-field-static-p field) collect field))
		(results (make-hash-table)))
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp object) "object-get-values"
									`((:object . ,(jdi-object-id object))
									  (:fields . ,(length nonstatic-fields))
									  ,(nconc (list :field)
											  (mapcar (lambda (field)
														`((:id . ,(jdi-field-id field))))
													  nonstatic-fields))))))
	  (loop for field in nonstatic-fields
			for value2 in (bindat-get-field reply :value)
			do (puthash field (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine object)
																	(bindat-get-field value2 :value :type)
																	(bindat-get-field value2 :value :u :value))
						results)
			do (jdi-debug "jdi-object-get-values: nonstatic type=%s value=%s" (bindat-get-field value2 :value :type)
						  (bindat-get-field value2 :value :u :type)))
	  (let ((class (jdi-object-get-reference-type object)))
		(let ((reply (jdwp-send-command (jdi-mirror-jdwp object) "reference-get-values"
										`((:ref-type . ,(jdi-class-id class))
										  (:fields . ,(length static-fields))
										  ,(nconc (list :field)
												  (mapcar (lambda (field)
															`((:id . ,(jdi-field-id field))))
														  static-fields))))))
		  (loop for field in static-fields
				for value2 in (bindat-get-field reply :value)
				do (puthash field (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine object)
																		(bindat-get-field value2 :value :type)
																		(bindat-get-field value2 :value :u :value))
							results)
				do (jdi-debug "jdi-object-get-values: static type=%s value=%s" (bindat-get-field value2 :value :type)
							  (bindat-get-field value2 :value :u :type)))
		  (loop for field in fields
				collect (gethash field results)))))))

(defalias 'jdi-value-get-values 'jdi-object-get-values)

(defun jdi-class-get-all-super (class &optional supers)
  (jdi-debug "jdi-class-get-all-super")
  (if (null class)
	  supers
	(let ((super (jdi-class-get-super class)))
	  (if super
		  (jdi-class-get-all-super super (cons super supers))
		supers))))

(defun jdi-class-get-super (class)
  "populate the jdi-class-super of this class"
  (jdi-debug "jdi-class-get-super:id=%s" (jdi-class-id class))
  (if (jdi-class-super class)
	  (if (equal (jdi-class-super class) t)
		  nil
		(jdi-class-super class))
	(let ((reply (jdwp-send-command
				  (jdi-mirror-jdwp class)
				  "superclass"
				  `((:class . ,(jdi-class-id class))))))
	  (if (equal (bindat-get-field reply :superclass)
				 [0 0 0 0 0 0 0 0])
		  (progn
			(setf (jdi-class-super class) t)
			nil)
		(setf (jdi-class-super class) (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine class) (bindat-get-field reply :superclass)))
		(jdi-class-super class)))))

(defun jdi-class-get-interfaces (class)
  "Gets the interfaces directly implemented by this class.
Only the interfaces that are declared with the 'implements' keyword in this class are included. "
  (jdi-debug "jdi-class-get-interfaces:id=%s" (jdi-class-id class))
  (if (or (jdi-class-interfaces-count class)
		  (string= (jdi-class-signature class) "Ljava/lang/Object;"))
	  (jdi-class-interfaces class)

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class)
									"interfaces"
									`((:ref-type . ,(jdi-class-id class))))))
	  (setf (jdi-class-interfaces class)
			(loop for interface in (bindat-get-field reply :interface)
				  collect (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine class) (bindat-get-field interface :type))))
	  (setf (jdi-class-interfaces-count class) (bindat-get-field reply :interfaces))
	  (jdi-class-interfaces class))))

(defun jdi-class-get-all-interfaces (class)
  "Gets the interfaces directly and indirectly implemented by this class.
Interfaces returned by interfaces()  are returned as well all superinterfaces."
  (jdi-debug "jdi-class-get-all-interfaces")
  (jdi-mappend 'jdi-class-get-interfaces (cons class (jdi-class-get-all-super class))))

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

(defun jdi-reference-type-get-fields (reference-type)
  "Get all the fields in this reference-type only."
  (jdi-with-cache jdi-reference-type-fields-cache reference-type
				  (let ((reply (jdwp-send-command (jdi-mirror-jdwp reference-type)
															 (jdi-get-fields-command-name reference-type)
															 `((:ref-type . ,(jdi-reference-type-id reference-type))))))
					(jdi-debug "jdi-reference-type-get-fields: %s's fields:%s" (jdi-reference-type-id reference-type) (bindat-get-field reply :declared))
					(loop for field in (bindat-get-field reply :field)
						  collect (make-jdi-field :virtual-machine (jdi-mirror-virtual-machine reference-type)
												  :id (bindat-get-field field :id)
												  :name (jdwp-get-string field :name)
												  :signature (jdwp-get-string field :signature)
												  :generic-signature (jdwp-get-string field :generic-signature)
												  :mod-bits (bindat-get-field field :mod-bits))
						  do (jdi-debug "jdi-reference-type-get-fields:%s id:%s name:%s signature:%s generic-signature:%s modbits:%s"
										(jdi-reference-type-id reference-type)
										(bindat-get-field field :id)
										(jdwp-get-string field :name)
										(jdwp-get-string field :signature)
										(jdwp-get-string field :generic-signature)
										(bindat-get-field field :mod-bits))))))

(defun jdi-array-get-values (array &optional first last)
  "Get the values of ARRAY from FIRST (inclusive) to
LAST (exclusive).  If FIRST or LAST are not supplied, they
default to 0 and the length of the array.

Returns nil if array is the null object.  Otherwise, it returns a
list whose nth element is the array element at index FIRST + n"
  (jdi-debug "jdi-array-get-values:object-id=%s" (jdi-object-id array))

  (unless (equal (jdi-object-id array) [0 0 0 0 0 0 0 0])
	(let (array-length)
	  (setq first (or first 0)
			last (or last (jdi-array-get-array-length array)))
	  (when (> last first)
		(let* ((reply (jdwp-send-command (jdi-mirror-jdwp array) "array-get-values"
										 `((:array-object . ,(jdi-object-id array))
										   (:first-index . ,first)
										   (:length . ,(- last first)))))
			   (reply-array (jdwp-unpack-arrayregion (jdi-mirror-jdwp array) reply)))
		  (jdi-trace "got array-get-values:%s" reply-array)
		  (if (or (= (bindat-get-field reply-array :type) jdwp-tag-object)
			  (= (bindat-get-field reply-array :type) jdwp-tag-array))
		      (loop for value-reply in (bindat-get-field reply-array :value)
			    for i from first to (1- last)
			    collect (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine array)
									  (bindat-get-field value-reply :value :type)
									  (bindat-get-field value-reply :value :u :value)))
		    (loop for value-reply in (bindat-get-field reply-array :value)
			  for i from first to (1- last)
			  collect (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine array)
									(bindat-get-field reply-array :type)
									(bindat-get-field value-reply :value)))))))))

(defun jdi-handle-breakpoint-event (jdwp event)
  (jdi-debug "jdi-handle-breakpoint-event")
  (jdi-handle-general-breakpoint-event jdwp event 'jdi-breakpoint-hooks))

(defun jdi-handle-exception-event (jdwp event)
  "Handle an exception EVENT from the JDWP. TODO: Handle multiple
events in one message."
  (jdi-debug "jdi-handle-exception-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
			(exception (jdi-virtual-machine-get-object-create
							vm (make-jdi-object
								 :id (bindat-get-field event :u :exception :object))))
			(exc-type (jdi-object-get-reference-type exception))
			)
	 (when (jdi-breakpoint-conditions-met vm
												 (bindat-get-field event :u :request-id)
												 (bindat-get-field event :u :catch-location)
												 exc-type)
		(jdi-debug "exception: %s %s"
						  (jdi-class-get-signature exc-type)
						  (jdi-object-get-values exception
														 (jdi-reference-type-get-all-fields exc-type)))
		(jdi-handle-general-breakpoint-event jdwp event 'jdi-exception-hooks))))

(defun jdi-breakpoint-conditions-met (vm request-id catch-location exc-type)
  (if jdi-break-on-exception-workaround
		(let* ((exc-data (gethash request-id
										  (jdi-virtual-machine-break-on-exception-data vm)))
				 (class-id (jdi-break-on-exception-workaround-data-class-id exc-data))
				 (caught (jdi-break-on-exception-workaround-data-caught exc-data))
				 (uncaught (jdi-break-on-exception-workaround-data-uncaught exc-data))
				 (caught-class-id (bindat-get-field catch-location :class-id))
				 is-caught)
		  ;; Uncaught exceptions will have all zeros in the class-id of the catch location.
		  (setq is-caught (notevery 'zerop caught-class-id))
		  (when is-caught
			 (jdi-debug "jdi-breakpoint-conditions-met: caught in %s"
							(jdi-class-get-signature-by-id vm caught-class-id)))

		  (and (or (eq caught is-caught)
					  (eq uncaught (not is-caught)))
				 (jdi-class-instance-of-by-id-p exc-type class-id)))
	 t))

(defun jdi-handle-general-breakpoint-event (jdwp event hooks)
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (thread-id (bindat-get-field event :u :thread))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))
		 (request-id (bindat-get-field event :u :request-id))

		 (class (jdi-virtual-machine-get-class-create vm class-id))
		 (method (make-jdi-method :virtual-machine vm
								  :id method-id
								  :class class))

		 (location (make-jdi-location :virtual-machine vm
									  :class class
									  :method method
									  :line-code-index line-code-index))
		 (thread (jdi-virtual-machine-get-object-create vm (make-jdi-thread :id thread-id)))
		 (frame (make-jdi-frame :virtual-machine vm
								:thread thread
								:location location)))

	(setf (jdi-thread-running-p thread) nil)
	(setf (jdi-virtual-machine-suspended-frames vm) (nreverse (cons frame (jdi-virtual-machine-suspended-frames vm))))

	(setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
	(run-hook-with-args hooks thread location request-id)))




(defun jdi-handle-step-event (jdwp event)
  (jdi-debug "jdi-handle-step-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (thread-id (bindat-get-field event :u :thread))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))

		 (class (jdi-virtual-machine-get-class-create vm class-id))
		 (method (make-jdi-method :virtual-machine vm
								  :id method-id
								  :class class))
		 (location (make-jdi-location :virtual-machine vm
									  :class class
									  :method method
									  :line-code-index line-code-index))
		 (thread (jdi-virtual-machine-get-object-create vm (make-jdi-thread :id thread-id)))
		 (frame (make-jdi-frame :virtual-machine vm
								:thread thread
								:location location)))

	(setf (jdi-thread-running-p thread) nil)

	(setf (jdi-virtual-machine-suspended-frames vm) (nreverse (cons frame (jdi-virtual-machine-suspended-frames vm))))

	(setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
	(run-hook-with-args 'jdi-step-hooks thread location)))

(defun jdi-handle-class-prepare-event (jdwp event)
  (jdi-debug "jdi-handle-class-prepare-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
			(type-id (bindat-get-field event :u :type-id))
			(signature (jdwp-get-string event :u :signature))
			(thread
			 (jdi-virtual-machine-get-object-create vm
																 (make-jdi-thread
																  :id (bindat-get-field event :u :thread))))
			(newclass (jdi-virtual-machine-get-class-create vm type-id
																			:signature signature)))
	(jdi-debug "thread status: %s %d" (jdi-thread-get-status thread)
			   (jdi-thread-get-suspend-count thread))
	(jdi-debug "class-loaded:%s" signature)
;; 	(puthash type-id newclass (jdi-virtual-machine-classes vm))
;; 	(puthash signature (cons newclass (gethash signature (jdi-virtual-machine-classes-by-signature vm)))
;; 			 (jdi-virtual-machine-classes-by-signature vm))
	(run-hook-with-args 'jdi-class-prepare-hooks newclass thread)))

(defun jdi-handle-class-unload-event (jdwp event)
  (jdi-debug "jdi-handle-class-unload-event sig=%s"  (jdwp-get-string event :u :signature))
  ())

(defun jdi-handle-thread-start (jdwp event)
  (jdi-debug "jdi-handle-thread-start %s" event)
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (thread (jdi-virtual-machine-get-object-create vm (make-jdi-thread
															:id (bindat-get-field event :u :thread)))))
	(condition-case nil
		(progn
		  (jdi-debug "jdi-handle-thread-start:%s" (jdi-thread-id thread))
		  (jdi-debug "thread status: %s %d" (jdi-thread-get-status thread)
					 (jdi-thread-get-suspend-count thread)))
	  (error nil))
	(run-hook-with-args 'jdi-thread-start-hooks thread)))

(defun jdi-handle-thread-end (jdwp event)
  (jdi-debug "jdi-handle-thread-end")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (thread (jdi-virtual-machine-get-object-create vm (make-jdi-thread
															:id (bindat-get-field event :u :thread)))))
	(jdi-debug "jdi-handle-thread-end:%s" (jdi-thread-id thread))
	(run-hook-with-args 'jdi-thread-end-hooks thread)))

(defun jdi-handle-vm-death (jdwp event)
  (jdi-debug "jdi-handle-vm-death")
  (let ((vm (jdwp-get jdwp 'jdi-virtual-machine)))
	(if vm (run-hook-with-args 'jdi-detached-hooks vm))))

(defun jdi-handle-vm-start (jdwp event)
  (jdi-trace "jdi-handle-vm-start %s" event)
  (let ((vm (jdwp-get jdwp 'jdi-virtual-machine)))
	(if vm
		(let ((thread
			  (jdi-virtual-machine-get-object-create vm
													 (make-jdi-thread :id (bindat-get-field event :u :thread)))))
		  (jdi-debug "thread %s" thread)
;; 		  (jdi-debug "setting jdibug-active-thread to %s in jdi-handle-vm-start"
;; 					 thread)
		  (setf (jdi-thread-running-p thread) nil)
		  (setq jdibug-active-thread thread))
	  ;; vm isn't initialized yet, so save this and run it later
	  (add-to-list 'jdi-deferred-vm-start-events (list jdwp event))
	  (add-hook 'jdibug-connected-hook 'jdi-deferred-vm-start)
	  )))

(defvar jdi-deferred-vm-start-events nil)
(defun jdi-deferred-vm-start nil
  "Sometime the vm-start event is received before we are fully connected.
This handles the event later, once we are connected."
  (jdi-trace "jdi-deferred-vm-start")
  (while jdi-deferred-vm-start-events
	(let ((args (car jdi-deferred-vm-start-events)))
	  (setq jdi-deferred-vm-start-events (cdr jdi-deferred-vm-start-events))
	  (apply 'jdi-handle-vm-start args))))

(defun jdi-class-name-to-class-signature (class-name)
  "Converts a.b.c CLASS-NAME to JNI class signature.  Inner
classes must be specified using $ notation (e.g.,
java.util.Map$Entry, not java.util.Map.Entry)."
  (let ((buf class-name))
	(setf buf (replace-regexp-in-string "\\." "/" buf))
	(setf buf (format "L%s;" buf))
	(jdi-debug "jdi-class-name-to-class-signature:%s:%s" class-name buf)
	buf))

(defun jdi-class-instance-of-p (class signature)
  (jdi-debug "jdi-class-instance-of-p:signature=%s" signature)
  (let* ((supers (jdi-class-get-all-super class))
		 (interfaces (jdi-class-get-all-interfaces class))
		 (signatures (mapcar 'jdi-class-get-signature (cons class (append supers interfaces)))))
	(jdi-debug "jdi-class-instance-of-p:all-signatures=%s" signatures)
	(member signature signatures)))

(defun jdi-class-instance-of-by-id-p (class id)
  "Test if CLASS has a super class with ID"
  (let* ((supers (jdi-class-get-all-super class))
		 (interfaces (jdi-class-get-all-interfaces class))
		 (all-ids (mapcar 'jdi-class-id (cons class (append supers interfaces)))))
	(jdi-debug "jdi-class-instance-of-by-id-p:all-ids=%s" all-ids)
	(member id all-ids)))


(defun jdi-object-instance-of-p (object signature)
  (jdi-debug "jdi-object-instance-of-p:signature=%s" signature)
  (jdi-class-instance-of-p (jdi-object-get-reference-type object) signature))

(defun jdi-value-extract-generic-class-name (generic-signature)
  (string-match "<L.*/\\(.*\\);>" generic-signature)
  (match-string 1 generic-signature))

(defun jdi-method-native-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-native) 0)))

(defun jdi-method-static-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-static) 0)))

(defun jdi-class-invoke-method (class thread method arguments options)
  "Invoke the static method on the class on the thread, if method is a string, it will pick the first method that matches the method-name."
  (jdi-debug "jdi-class-invoke-method")
  (when (stringp method)
	(setq method (find-if (lambda (obj)
							(equal (jdi-method-name obj) method))
						  (jdi-class-get-all-methods class))))
  (when method
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class) "class-invoke-method"
									`((:class . ,(jdi-class-id class))
									  (:thread . ,(jdi-thread-id thread))
									  (:method-id . ,(jdi-method-id method))
									  (:arguments . 0)
									  (:options . ,jdwp-invoke-single-threaded)))))
	  (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine class)
											(bindat-get-field reply :return-value :type)
											(bindat-get-field reply :return-value :u :value)))))

(defun jdi-object-invoke-method (object thread method arguments options)
  "Invoke the method on the value on the thread, if method is a string, it will pick the first method that matches the method-name.  OPTIONS is currently ignored."
  (jdi-debug "jdi-object-invoke-method")
  (when (stringp method)
	(setq method (find-if (lambda (obj)
							(equal (jdi-method-name obj) method))
						  (jdi-class-get-all-methods (jdi-object-get-reference-type object)))))
  (when method
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp object) "object-invoke-method"
									`((:object . ,(jdi-object-id object))
									  (:thread . ,(jdi-thread-id thread))
									  (:class . ,(jdi-class-id (jdi-object-get-reference-type object)))
									  (:method-id . ,(jdi-method-id method))
									  (:arguments . 0)
									  (:options . ,jdwp-invoke-single-threaded)))))
	  (jdi-virtual-machine-get-value-create (jdi-mirror-virtual-machine object)
											(bindat-get-field reply :return-value :type)
											(bindat-get-field reply :return-value :u :value)))))

(defalias 'jdi-value-invoke-method 'jdi-object-invoke-method)

(defvar jdi-breakpoint-hooks nil
  "callback to be called when breakpoint is hit, called with (jdi-thread jdi-location event-request)")

(defvar jdi-exception-hooks nil
  "callback to be called when exception is hit, called with (jdi-thread jdi-location event-request)")

(defvar jdi-step-hooks nil
  "callback to be called when execution is stopped from stepping, called with (jdi-virtual-machine thread-id class-id method-id line-code-index)")

(defvar jdi-detached-hooks nil
  "callback to be called when debuggee detached from us, called with (jdi-virtual-machine)")

(defvar jdi-class-prepare-hooks nil
  "handler to be called when a class is prepared, called with (jdi-class thread)")

(defvar jdi-class-unload-hooks nil
  "handler to be called when a class is unloaded, called with (jdi-class)")

(defvar jdi-thread-start-hooks nil
  "handler to be called when a thread is started, called with (jdi-thread)")

(defvar jdi-thread-end-hooks nil
  "handler to be called when a thread is ended, called with (jdi-thread)")

(defun jdi-handle-event (jdwp event)
  (jdi-debug "jdi-handle-event")
  ;; If we are currenting sending a command, any handler that tries to
  ;; send a command will cause an error since we don't support nested
  ;; commands
  (jdi-debug "sending-command=%s, event=%s" jdwp-sending-command event)
  (let* ((handlers (list
					`(,jdwp-event-breakpoint    . jdi-handle-breakpoint-event)
					`(,jdwp-event-single-step   . jdi-handle-step-event)
					`(,jdwp-event-class-prepare . jdi-handle-class-prepare-event)
					`(,jdwp-event-class-unload  . jdi-handle-class-unload-event)
					`(,jdwp-event-thread-start  . jdi-handle-thread-start)
					`(,jdwp-event-thread-end    . jdi-handle-thread-end)
					`(,jdwp-event-vm-death      . jdi-handle-vm-death)
					`(,jdwp-event-vm-start      . jdi-handle-vm-start)
					`(,jdwp-event-exception     . jdi-handle-exception-event)
					))
		 (event-kind (if (integerp event) event (bindat-get-field event :event-kind)))
		 (handler (find-if (lambda (pair)
							 (equal event-kind (car pair)))
						   handlers)))
	(if handler
		(funcall (cdr handler) jdwp event)
	  (jdi-error "do not know how to handle event:%s" event))))

(add-hook 'jdwp-event-hooks 'jdi-handle-event)

(defun jdi-primitive-emacs-value (jdi-value)
  "Convert a jdi-primitive JDI-VALUE into the corresponding emacs form."
  (let ((type (jdi-value-type jdi-value))
		(value (jdi-primitive-value-value jdi-value)))
	(cond
		((memq type (list jdwp-tag-short jdwp-tag-byte jdwp-tag-int jdwp-tag-long))
		 (jdwp-vec-to-int value))
		((eql type jdwp-tag-float)
		 (jdwp-vec-to-float value))
		((eql type jdwp-tag-double)
		 (jdwp-vec-to-double value))
		((eql type jdwp-tag-boolean)
		 (not (= 0 value)))
		(t
		 (jdi-error "Unknown type (%s) for value: %s" type jdi-value)
		 (error "%s is not a numerical type" (jdwp-type-name type))))))

(defun jdi-get-fields-command-name (reference-type)
  "Return command name for getting fields"
  (if (jdi-virtual-machine-has-generic-p
       (jdi-mirror-virtual-machine reference-type))
      (progn
        (jdi-debug "JVM has generic")
        "fields-with-generic")
    (progn
      (jdi-debug "JVM does not support generic")
      "fields")))

(defun jdi-get-methods-command-name (class)
  "Return command name for getting methods"
  (if (jdi-virtual-machine-has-generic-p
		 (jdi-mirror-virtual-machine class))
      (progn
        (jdi-debug "JVM has generic")
        "methods-with-generic")
    (progn
      (jdi-debug "JVM does not support generic")
      "methods")))

(defun jdi-get-variable-table-command-name (method)
  "Return command name for getting variable table"
  (if (jdi-virtual-machine-has-generic-p
		 (jdi-mirror-virtual-machine method))
		(progn
        (jdi-debug "JVM has generic")
        "variable-table-with-generic")
    (progn
      (jdi-debug "JVM does not support generic")
      "variable-table")))


(provide 'jdi)
