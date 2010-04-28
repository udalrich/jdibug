;;; jdwp.el --- library to communicate using Java(tm) Debug Wire Protocol

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

;; This module try to implement everything documented here:
;; http://java.sun.com/j2se/1.4.2/docs/guide/jpda/jdwp/jdwp-protocol.html
;; http://java.sun.com/j2se/1.4.2/docs/guide/jpda/jdwp-spec.html

;; This module requires elog.el

;;; Code:

(require 'bindat)
(require 'elog)
(eval-when-compile
  (load "cl-seq")
  (load "cl-extra"))

(defcustom jdwp-timeout 3
  "Number of seconds to timeout before replies arrive from the debuggee."
  :group 'jdibug
  :type 'integer)

(defcustom jdwp-block-seconds 0.1
  "The number of seconds we block before checking for user activity"
  :group 'jdibug
  :type 'float)

(elog-make-logger jdwp)
(elog-make-logger jdwp-traffic)

(defvar jdwp-ignore-error nil
  "If an error response to `jdwp-send-command' is contained in this list, nil is returned rather than throwing an error")

(defvar jdwp-uninterruptibly-running-p nil
  "Flag to indicate if an uninterruptible function is already running")
(defvar jdwp-uninterruptibly-waiting nil
  "List of objects to process uninterruptibly.")

(defstruct jdwp
  ;; the elisp process that connects to the debuggee
  process

  ;; the state of our process
  handshaked-p

  ;; the last used command id that we sent to the server
  (current-id            -1)

  ;; place where you can store anything
  plist

  current-reply

  ;; VM specific sizes that need to be set before any communication happens
  (field-id-size          4)
  (method-id-size         4)
  (object-id-size         4)
  (reference-type-id-size 4)
  (frame-id-size          4)

  server
  port)

(defstruct jdwp-packet
  length
  id
  flags

  data)

(defstruct (jdwp-packet-reply (:include jdwp-packet))
  error)

(defstruct (jdwp-packet-command (:include jdwp-packet))
  command-set
  command)

;;; Constants:
(defconst jdwp-event-single-step         1)
(defconst jdwp-event-breakpoint          2)
(defconst jdwp-event-frame-pop           3)
(defconst jdwp-event-exception           4)
(defconst jdwp-event-user-defined        5)
(defconst jdwp-event-thread-start        6)
(defconst jdwp-event-thread-end          7)
(defconst jdwp-event-class-prepare       8)
(defconst jdwp-event-class-unload        9)
(defconst jdwp-event-class-load         10)
(defconst jdwp-event-field-access       20)
(defconst jdwp-event-field-modification 21)
(defconst jdwp-event-exception-catch    30)
(defconst jdwp-event-method-entry       40)
(defconst jdwp-event-method-exit        41)
(defconst jdwp-event-vm-init            90)
(defconst jdwp-event-vm-start           90)
(defconst jdwp-event-vm-death           99)

(defconst jdwp-step-depth-into   0)
(defconst jdwp-step-depth-over   1)
(defconst jdwp-step-depth-out    2)

(defconst jdwp-tag-array        91)
(defconst jdwp-tag-byte         66)
(defconst jdwp-tag-char         67)
(defconst jdwp-tag-object       76)
(defconst jdwp-tag-float        70)
(defconst jdwp-tag-double       68)
(defconst jdwp-tag-int          73)
(defconst jdwp-tag-long         74)
(defconst jdwp-tag-short        83)
(defconst jdwp-tag-void         86)
(defconst jdwp-tag-boolean      90)
(defconst jdwp-tag-class-object 99)
(defconst jdwp-tag-string       115)
(defconst jdwp-tag-thread       116)
(defconst jdwp-tag-thread-group 103)
(defconst jdwp-tag-class-loader 108)

(defconst jdwp-type-tag-class     1)
(defconst jdwp-type-tag-interface 2)
(defconst jdwp-type-tag-array     3)

(defconst jdwp-suspend-policy-none 0)
(defconst jdwp-suspend-policy-event-thread 1)
(defconst jdwp-suspend-policy-all 2)

(defconst jdwp-invoke-single-threaded 1)
(defconst jdwp-invoke-nonvirtual 2)

(defconst jdwp-thread-status-zombie 0)
(defconst jdwp-thread-status-running 1)
(defconst jdwp-thread-status-sleeping 2)
(defconst jdwp-thread-status-monitor 3)
(defconst jdwp-thread-status-wait 4)

(defconst jdwp-thread-status-constants
  `( (,jdwp-thread-status-zombie . "Zombie")
	 (,jdwp-thread-status-running . "Running")
	 (,jdwp-thread-status-sleeping . "Sleeping")
	 (,jdwp-thread-status-monitor . "Monitor")
	 (,jdwp-thread-status-wait . "Waiting")))

(defconst jdwp-suspend-status-suspended 1)

(defconst jdwp-mod-kind-case-count       1)
(defconst jdwp-mod-kind-case-conditional 2)
(defconst jdwp-mod-kind-case-thread-only 3)
(defconst jdwp-mod-kind-class-only       4)
(defconst jdwp-mod-kind-class-match      5)
(defconst jdwp-mod-kind-class-exclude    6)
(defconst jdwp-mod-kind-location-only    7)
(defconst jdwp-mod-kind-exception-only   8)
(defconst jdwp-mod-kind-field-only       9)
(defconst jdwp-mod-kind-case-step       10)
(defconst jdwp-mod-kind-instance-only   11)

(defconst jdwp-error-constants
  `((0   none                 "No error has occured.")
	(10  invalid-thread       "Passed thread is null, is not a valid thread or has exited.")
	(11  invalid-thread-group "Thread group invalid.")
	(12  invalid-priority     "Invalid priority.")
	(13  thread-not-suspended "If the specified thread as not been suspended by and event.")
	(14  thread-suspended     "Thread already suspended.")
	(20  invalid-object       "If this reference type has been unloaded and garbage collected.")
	(21  invalid-class        "Invalid class.")
	(22  invalid-location     "Invalid location.")
	(23  invalid-methodid     "Invalid method.")
	(24  invalid-location     "Invalid location.")
	(25  invalid-fieldid      "Invalid field.")
	(30  invalid-frameid      "Invalid jframeID.")
	(31  no-more-frames       "There are no more Java or JNI frames on the call stack.")
	(32  opaque-frame         "Information about the frame is not available.")
	(33  not-current-frame    "Operation can only be performed on current frame.")
	(34  type-mismatch        "The variable is not appropriate type for the function used.")
	(35  invalid-slot         "Invalid slot")
	(40  duplicate            "Item already set.")
	(41  not-found            "Desired element not found.")
	(50  invalid-monitor      "Invalid monitor.")
	(51  not-monitor-owner    "This thread doesn't own the monitor.")
	(52  interrupt            "The call has been interrupted before completion.")
	(60  invalid-class-format "The virtual machine attempted to read a class file and determined that the file is malformed or otherwise cannot be interpreted as a class file")
	(61  circular-class-definition "A circularity has been detected while initializing a class.")
	(62  fails_verification   "The verifier detected that a class file, though well formed, contained some sort of internal inconsistency or security problem.")
	(63  add-method-not-implemented "Adding methods has not been implemented.")
	(64  schema-change-not-implemented "Schema change has not been implemented.")
	(65  invalid-typestate    "The state of the thread has been modified, and is not inconsistent.")
	(66  hierarchy-change-not-implemented "A direct superclass is different for the new class version, or the set of directly implemented interfaces is different and canUnrestrictedlyRedefineClasses is false.")
	(67  delete-method-not-implemented "The new class version does not declare a method declared in the old class version and canUnrestrictedlyRedefineClasses is false.")
	(68  unsupported-version  "A class file has a version number not supported by this VM.")
	(69  names-dont-match     "The class name defined in the new class file is different from the name in the old class object.")
	(70  class-modifiers-change-not-implemented "The new class version has different modifiers and canUnrestrictedlyRedefineClasses is false.")
	(71  method-modifiers-change-not-implemented "A method in the new class version has different modifiers than its counterpart in the old class version and canUnrestrictedlyRedefineClasses is false.")
	(99  not-implemented      "The functionality is not implemented in this virtual machine.")
	(100 null-pointer         "Invalid pointer.")
	(101 absent-information   "Desired information is not available.")
	(102 invalid-event-type   "The specified event type id is not recognized.")
	(103 illegal-argument     "Illegal argument.")
	(110 out-of-memory        "The function needed to allocate memory and no more memory was available for allocation.")
	(111 access-denied        "Debugging has not been enabled in this virtual machine. JVMDI cannot be used.")
	(112 vm-dead              "The virtual machine is not running.")
	(113 internal             "An unexpected internal error has occurred.")
	(500 invalid-tag          "object type id or class tag.")
	(502 already-invoking     "Previous invoke not complete.")
	(503 invalid-index        "Index is invalid.")
	(504 invalid-length       "The length is invalid.")
	(506 invalid-string       "The string is invalid.")
	(507 invalid-class-loader "The class loader is invalid.")
	(508 invalid-array        "The array is invalid.")
	(509 transport-load       "Unable to load the transport.")
	(510 transport-init       "Unable to initialize the transport.")
	(511 native-method        "")
	(512 invalid-count        "The count is invalid.")))

;; generate variables for each of the error codes for easier access
(dolist (element jdwp-error-constants)
  (let ((code (car element))
		(sym (intern (format "jdwp-error-%s" (nth 1 element)))))
    (set sym code)))

;;; The bindat specifications:
(defconst jdwp-packet-spec
  '((:length     u32)
	(:id         u32)
	(:flags      u8)))

(defconst jdwp-command-spec
  '((:length      u32)
	(:id          u32)
	(:flags       u8)
	(:command-set u8)
	(:command     u8)))

(defconst jdwp-reply-spec
  '((:length     u32)
	(:id         u32)
	(:flags      u8)
	(:error      u16)))

(defconst jdwp-string-spec
  '((:length     u32)
	(:string     vec (:length))))

(defconst jdwp-location-spec
  '((:type      u8)
	(:class-id  vec (eval jdwp-reference-type-id-size))
	(:method-id vec (eval jdwp-method-id-size))
	(:index     vec 8)))

(defconst jdwp-event-spec
  `((:suspend-policy         u8)
	(:events                 u32)
	(:event                  repeat (:events)
							 (:event-kind     u8)
							 (:u              union (:event-kind)
											  (,jdwp-event-vm-start       (:request-id    u32)
																		  (:thread        vec (eval jdwp-object-id-size)))
											  (,jdwp-event-single-step    (:request-id    u32)
																		  (:thread        vec (eval jdwp-object-id-size))
																		  (:location      struct jdwp-location-spec))
											  (,jdwp-event-thread-start   (:request-id    u32)
																		  (:thread        vec (eval jdwp-object-id-size)))
											  (,jdwp-event-breakpoint     (:request-id    u32)
																		  (:thread        vec (eval jdwp-object-id-size))
																		  (:location      struct jdwp-location-spec))
											  (,jdwp-event-class-prepare  (:request-id    u32)
																		  (:thread        vec (eval jdwp-object-id-size))
																		  (:ref-type-tag  u8)
																		  (:type-id       vec (eval jdwp-reference-type-id-size))
																		  (:signature     struct jdwp-string-spec)
																		  (:status        u32))
											  (,jdwp-event-class-unload   (:request-id    u32)
																		  (:signature     struct jdwp-string-spec))
											  (,jdwp-event-vm-death       (:request-id    u32))))))



(defconst jdwp-value-spec
  `((:type  u8)
	(:u     union (:type)
			(,jdwp-tag-array        (:value vec (eval jdwp-object-id-size)))
			(,jdwp-tag-byte         (:value u8))
			(,jdwp-tag-char         (:value u16))
			(,jdwp-tag-object       (:value vec (eval jdwp-object-id-size)))
			(,jdwp-tag-float        (:value vec 4))
			(,jdwp-tag-double       (:value vec 8))
			(,jdwp-tag-int          (:value vec 4))
			(,jdwp-tag-long         (:value vec 8))
			(,jdwp-tag-short        (:value u16))
			(,jdwp-tag-void)
			(,jdwp-tag-boolean      (:value u8))
			(,jdwp-tag-string       (:value vec (eval jdwp-object-id-size)))
			(,jdwp-tag-thread       (:value vec (eval jdwp-object-id-size)))
			(,jdwp-tag-thread-group (:value vec (eval jdwp-object-id-size)))
			(,jdwp-tag-class-loader (:value vec (eval jdwp-object-id-size)))
			(,jdwp-tag-class-object (:value vec (eval jdwp-object-id-size))))))

(defconst jdwp-arrayregion-header-spec
  '((:type    u8)
	(:length  u32)))

;; declare the dynamic variables for our unpacker
(defmacro jdwp-with-size (jdwp &rest body)
  (declare (indent defun))
  `(let ((jdwp-field-id-size          (jdwp-field-id-size          ,jdwp))
		 (jdwp-method-id-size         (jdwp-method-id-size         ,jdwp))
		 (jdwp-object-id-size         (jdwp-object-id-size         ,jdwp))
		 (jdwp-reference-type-id-size (jdwp-reference-type-id-size ,jdwp))
		 (jdwp-frame-id-size          (jdwp-frame-id-size          ,jdwp)))
     ,@body))

(defun jdwp-unpack-arrayregion (jdwp packet)
  (jdwp-trace "jdwp-unpack-arrayregion:%s" (jdwp-string-to-hex packet))
  (jdwp-with-size
    jdwp
	(let* ((header (bindat-unpack jdwp-arrayregion-header-spec packet))
		   (type   (bindat-get-field header :type))
		   (length (bindat-get-field header :length))
		   repeater spec unpacked)
	  (setq repeater
			(case type
			  ; object
			  (76 '(:value struct jdwp-value-spec))
			  ; array
			  (91 '(:value struct jdwp-value-spec))
			  ; byte
			  (66 '(:value u8))
			  ; char
			  (67 '(:value u16))
			  ; float
			  (70 '(:value vec 4))
			  ; double
			  (68 '(:value vec 8))
			  ; int.  u32 will overflow.  Can we use vec 4?
			  (73 '(:value vec 4))
			  ; long
			  (74 '(:value vec 8))
			  ; short
			  (83 '(:value u16))
			  ; boolean
			  (90 '(:value u8))))
	  (setq spec `((:type u8) (:length u32) (:value repeat (:length) ,repeater)))
	  (bindat-unpack spec packet))))

(defconst jdwp-protocol
  `((:name         "version"
				   :command-set  1
				   :command      1
				   :command-spec nil
				   :reply-spec   ((:description   struct jdwp-string-spec)
								  (:jdwp-major    u32)
								  (:jdwp-minor    u32)
								  (:vm-version    struct jdwp-string-spec)
								  (:vm-name       struct jdwp-string-spec)))
	(:name         "classes-by-signature"
				   :command-set  1
				   :command      2
				   :command-spec ((:signature     struct jdwp-string-spec))
				   :reply-spec   ((:classes       u32)
								  (:class         repeat (:classes)
												  (:ref-type-tag u8)
												  (:type-id      vec (eval jdwp-reference-type-id-size))
												  (:status       u32))))
	(:name         "all-classes"
				   :command-set  1
				   :command      3
				   :command-spec nil
				   :reply-spec   ((:classes       u32)
								  (:class         repeat (:classes)
												  (:ref-type-tag u8)
												  (:type-id      vec (eval jdwp-reference-type-id-size))
												  (:signature    struct jdwp-string-spec)
												  (:status       u32))))
	(:name         "all-threads"
				   :command-set  1
				   :command      4
				   :command-spec nil
				   :reply-spec   ((:threads       u32)
								  (:thread        repeat (:threads)
												  (:id   vec (eval jdwp-object-id-size)))))
	(:name         "top-level-thread-groups"
				   :command-set  1
				   :command      5
				   :command-spec nil
				   :reply-spec   ((:groups        u32)
								  (:group         repeat (:group)
												  (:id   vec (eval jdwp-object-id-size)))))
	(:name         "dispose"
				   :command-set  1
				   :command      6
				   :command-spec nil
				   :reply-spec   nil)
	(:name         "id-sizes"
				   :command-set  1
				   :command      7
				   :command-spec nil
				   :reply-spec   ((:field-id-size          u32)
								  (:method-id-size         u32)
								  (:object-id-size         u32)
								  (:reference-type-id-size u32)
								  (:frame-id-size          u32)))
	(:name         "suspend"
				   :command-set  1
				   :command      8
				   :command-spec nil
				   :reply-spec   nil)
	(:name         "resume"
				   :command-set  1
				   :command      9
				   :command-spec nil
				   :reply-spec   nil)
	(:name         "exit"
				   :command-set  1
				   :command      10
				   :command-spec ((:exit-code u32))
				   :reply-spec   nil)
	(:name         "capabilities-new"
				   :command-set  1
				   :command      17
				   :command-spec nil
				   :reply-spec   ((:can-watch-field-modification      u8)
								  (:can-watch-field-access            u8)
								  (:can-get-bytecodes                 u8)
								  (:can-get-synthetic-attribute       u8)
								  (:can-get-owned-monitor-info        u8)
								  (:can-get-current-contended-monitor u8)
								  (:can-get-monitor-info              u8)
								  (:can-redefine-class                u8)
								  (:can-add-method                    u8)
								  (:can-unrestrictedly-redefine-class u8)
								  (:can-pop-frames                    u8)
								  (:can-use-instance-filters          u8)
								  (:can-get-source-debug-extension    u8)
								  (:can-request-vm-death-event        u8)
								  (:can-set-default-stratum           u8)))
	(:name         "all-classes-with-generic"
				   :command-set  1
				   :command      20
				   :command-spec nil
				   :reply-spec   ((:classes       u32)
								  (:class         repeat (:classes)
												  (:ref-type-tag u8)
												  (:type-id      vec (eval jdwp-reference-type-id-size))
												  (:signature    struct jdwp-string-spec)
												  (:generic-signature    struct jdwp-string-spec)
												  (:status       u32))))
	(:name         "signature"
				   :command-set  2
				   :command      1
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:signature     struct jdwp-string-spec)))
	(:name         "class-loader"
				   :command-set  2
				   :command      2
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:class-loader  vec (eval jdwp-object-id-size))))
	(:name         "fields"
				   :command-set  2
				   :command      4
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:declared      u32)
								  (:field         repeat (:declared)
												  (:id        vec (eval jdwp-field-id-size))
												  (:name      struct jdwp-string-spec)
												  (:signature struct jdwp-string-spec)
												  (:mod-bits  u32))))
	(:name         "methods"
				   :command-set  2
				   :command      5
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:methods       u32)
								  (:method        repeat (:methods)
												  (:method-id      vec (eval jdwp-method-id-size))
												  (:name           struct jdwp-string-spec)
												  (:signature      struct jdwp-string-spec)
												  (:mod-bits       u32))))
	(:name         "reference-get-values"
				   :command-set  2
				   :command      6
				   :command-spec ((:ref-type     vec (eval jdwp-object-id-size))
								  (:fields        u32)
								  (:field         repeat (:fields)
												  (:id vec (eval jdwp-field-id-size))))
				   :reply-spec   ((:values        u32)
								  (:value         repeat (:values)
												  (:value struct jdwp-value-spec))))
	(:name         "source-file"
				   :command-set  2
				   :command      7
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:source-file   struct jdwp-string-spec)))
	(:name         "interfaces"
				   :command-set  2
				   :command      10
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:interfaces    u32)
								  (:interface     repeat (:interfaces)
												  (:type   vec (eval jdwp-reference-type-id-size)))))
	(:name         "class-object"
				   :command-set  2
				   :command      11
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:class-object  vec (eval jdwp-object-id-size))))
	(:name         "signature-with-generic"
				   :command-set  2
				   :command      13
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:signature     struct jdwp-string-spec)
								  (:generic-signature struct jdwp-string-spec)))
	(:name         "fields-with-generic"
				   :command-set  2
				   :command      14
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:declared      u32)
								  (:field         repeat (:declared)
												  (:id        vec (eval jdwp-field-id-size))
												  (:name      struct jdwp-string-spec)
												  (:signature struct jdwp-string-spec)
												  (:generic-signature struct jdwp-string-spec)
												  (:mod-bits  u32))))
	(:name         "superclass"
				   :command-set  3
				   :command      1
				   :command-spec ((:class         vec (eval jdwp-reference-type-id-size)))
				   :reply-spec   ((:superclass    vec (eval jdwp-reference-type-id-size))))
	(:name         "class-invoke-method"
				   :command-set  3
				   :command      3
				   :command-spec ((:class         vec (eval jdwp-reference-type-id-size))
								  (:thread        vec (eval jdwp-object-id-size))
								  (:method-id     vec (eval jdwp-method-id-size))
								  (:arguments     u32)
								  (:argument      repeat (:arguments)
												  (:value struct jdwp-value-spec))
								  (:options       u32))
				   :reply-spec   ((:return-value     struct jdwp-value-spec)
								  (:exception-type   u8)
								  (:exception-object vec (eval jdwp-object-id-size))))
	(:name         "line-table"
				   :command-set  6
				   :command      1
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size))
								  (:method-id     vec (eval jdwp-method-id-size)))
				   :reply-spec   ((:start         vec 8)
								  (:end           vec 8)
								  (:lines         u32)
								  (:line          repeat (:lines)
												  (:line-code-index vec 8)
												  (:line-number     u32))))
	(:name         "variable-table"
				   :command-set  6
				   :command      2
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size))
								  (:method-id     vec (eval jdwp-method-id-size)))
				   :reply-spec   ((:arg-cnt       u32)
								  (:slots         u32)
								  (:slot          repeat (:slots)
												  (:code-index    vec 8)
												  (:name          struct jdwp-string-spec)
												  (:signature     struct jdwp-string-spec)
												  (:length        u32)
												  (:slot          u32))))
	(:name         "variable-table-with-generic"
				   :command-set  6
				   :command      5
				   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size))
								  (:method-id     vec (eval jdwp-method-id-size)))
				   :reply-spec   ((:arg-cnt       u32)
								  (:slots         u32)
								  (:slot          repeat (:slots)
												  (:code-index    vec 8)
												  (:name          struct jdwp-string-spec)
												  (:signature     struct jdwp-string-spec)
												  (:generic-signature     struct jdwp-string-spec)
												  (:length        u32)
												  (:slot          u32))))
	(:name         "object-reference-type"
				   :command-set  9
				   :command      1
				   :command-spec ((:object        vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:ref-type-tag  u8)
								  (:type-id       vec (eval jdwp-object-id-size))))
	(:name         "object-get-values"
				   :command-set  9
				   :command      2
				   :command-spec ((:object        vec (eval jdwp-object-id-size))
								  (:fields        u32)
								  (:field         repeat (:fields)
												  (:id vec (eval jdwp-field-id-size))))
				   :reply-spec   ((:values        u32)
								  (:value         repeat (:values)
												  (:value struct jdwp-value-spec))))
	(:name         "object-invoke-method"
				   :command-set  9
				   :command      6
				   :command-spec ((:object        vec (eval jdwp-object-id-size))
								  (:thread        vec (eval jdwp-object-id-size))
								  (:class         vec (eval jdwp-reference-type-id-size))
								  (:method-id     vec (eval jdwp-method-id-size))
								  (:arguments     u32)
								  (:argument      repeat (:arguments)
												  (:value struct jdwp-value-spec))
								  (:options       u32))
				   :reply-spec   ((:return-value     struct jdwp-value-spec)
								  (:exception-type   u8)
								  (:exception-object vec (eval jdwp-object-id-size))))
	(:name         "string-value"
				   :command-set  10
				   :command      1
				   :command-spec ((:object        vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:value         struct jdwp-string-spec)))
	(:name         "thread-name"
				   :command-set  11
				   :command      1
				   :command-spec ((:thread        vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:thread-name   struct jdwp-string-spec)))
	(:name         "thread-suspend"
				   :command-set  11
				   :command      2
				   :command-spec ((:thread        vec (eval jdwp-object-id-size)))
				   :reply-spec   nil)
	(:name         "thread-resume"
				   :command-set  11
				   :command      3
				   :command-spec ((:thread        vec (eval jdwp-object-id-size)))
				   :reply-spec   nil)
	(:name         "thread-status"
				   :command-set  11
				   :command      4
				   :command-spec ((:thread         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:thread-status  u32)
								  (:suspend-status u32)))
	(:name         "thread-group"
				   :command-set  11
				   :command      5
				   :command-spec ((:thread         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:group          vec (eval jdwp-object-id-size))))
	(:name         "frames"
				   :command-set  11
				   :command      6
				   :command-spec ((:thread         vec (eval jdwp-object-id-size))
								  (:start-frame    u32)
								  (:length         u32))
				   :reply-spec   ((:frames         u32)
								  (:frame          repeat (:frames)
												   (:id       vec (eval jdwp-frame-id-size))
												   (:location struct jdwp-location-spec))))
	(:name         "frame-count"
				   :command-set  11
				   :command      7
				   :command-spec ((:thread         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:frame-count    u32)))
	(:name         "suspend-count"
				   :command-set  11
				   :command      12
				   :command-spec ((:thread         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:suspend-count    u32)))
	(:name         "thread-group-name"
				   :command-set  12
				   :command      1
				   :command-spec ((:group         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:group-name    struct jdwp-string-spec)))
	(:name         "thread-group-parent"
				   :command-set  12
				   :command      2
				   :command-spec ((:group         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:parent-group  vec (eval jdwp-object-id-size))))
	(:name         "thread-group-children"
				   :command-set  12
				   :command      3
				   :command-spec ((:group         vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:child-threads u32)
								  (:child-thread  repeat (:child-threads)
												  (:child-thread   vec (eval jdwp-object-id-size)))
								  (:child-groups  u32)
								  (:child-group   repeat (:child-groups)
												  (:child-group    vec (eval jdwp-object-id-size)))))
	(:name         "array-length"
				   :command-set  13
				   :command      1
				   :command-spec ((:array-object  vec (eval jdwp-object-id-size)))
				   :reply-spec   ((:array-length  u32)))
	(:name         "array-get-values"
				   :command-set  13
				   :command      2
				   :command-spec ((:array-object  vec (eval jdwp-object-id-size))
								  (:first-index   u32)
								  (:length        u32))
				   :reply-spec   nil)
	(:name         "set"
				   :command-set  15
				   :command      1
				   :command-spec ((:event-kind     u8)
								  (:suspend-policy u8)
								  (:modifiers      u32)
								  (:modifier       repeat (:modifiers)
												   (:mod-kind    u8)
												   (:u           union (:mod-kind)
																 (,jdwp-mod-kind-case-count     (:count    u32))
																 (,jdwp-mod-kind-class-match    (:class-pattern struct jdwp-string-spec))
																 (,jdwp-mod-kind-location-only  (:location struct jdwp-location-spec))
																 (,jdwp-mod-kind-exception-only (:exception vec (eval jdwp-reference-type-id-size))
																								(:caught    u8)
																								(:uncaught  u8))
																 (,jdwp-mod-kind-case-step      (:thread   vec (eval jdwp-object-id-size))
																								(:size     u32)
																								(:depth    u32)))))
				   :reply-spec   ((:request-id    u32)))
	(:name         "clear"
				   :command-set  15
				   :command      2
				   :command-spec ((:event         u8)
								  (:request-id    u32))
				   :reply-spec   nil)
	(:name         "stack-frame-get-values"
				   :command-set  16
				   :command      1
				   :command-spec ((:thread        vec (eval jdwp-object-id-size))
								  (:frame         vec (eval jdwp-frame-id-size))
								  (:slots         u32)
								  (:slot          repeat (:slots)
												  (:slot    u32)
												  (:sigbyte u8)))
				   :reply-spec   ((:values        u32)
								  (:value         repeat (:values)
												  (:slot-value struct jdwp-value-spec))))
	(:name         "stack-frame-this-object"
				   :command-set  16
				   :command      3
				   :command-spec ((:thread        vec (eval jdwp-object-id-size))
								  (:frame         vec (eval jdwp-frame-id-size)))
				   :reply-spec   ((:object-this   struct jdwp-value-spec)))))

(defconst jdwp-handshake "JDWP-Handshake")

;;; And the functions:

(defun jdwp-string-to-hex (s &optional max)
  (let ((hex))
	(loop for c in (string-to-list s)
		  while (or (null max) (< (length hex) max))
		  do
		  (setf hex (concat hex (format "%02x " c))))
    (if (= (length hex) 0)
		""
      (substring hex 0 -1))))

(defun jdwp-put (jdwp key value)
  (setf (jdwp-plist jdwp)
		(plist-put (jdwp-plist jdwp) key value)))

(defun jdwp-get (jdwp key)
  (plist-get (jdwp-plist jdwp) key))

(defun jdwp-process-filter (process string)
  (jdwp-debug "jdwp-process-filter")
  (condition-case err
	  (progn
		(jdwp-ordinary-insertion-filter process string)
		(let ((jdwp (process-get process 'jdwp))
			  packet)
		  (while (setq packet (jdwp-packet-unpack (jdwp-residual-output jdwp)))
			(jdwp-consume-output jdwp (jdwp-packet-length packet))
			(jdwp-debug "jdwp-process-filter received packet:type=%s" (type-of packet))
			(if (jdwp-packet-reply-p packet)
				;; reply packet
				(progn
				  (jdwp-trace "jdwp-process-filter:reply packet:%s" packet)
				  (jdwp-trace "jdwp-process-filter:reply packet data:%s" (jdwp-string-to-hex (jdwp-packet-data packet)))
				  (setf (jdwp-current-reply jdwp) packet))

			  (jdwp-debug "jdwp-process-filter:command-packet")
			  ;; command packet
			  (jdwp-run-with-timer 0.1 nil 'jdwp-process-command jdwp packet)
			  (jdwp-trace "received command packet")))))
	(error (jdwp-error "jdwp-process-filter:%s" err))))

(defun jdwp-process-id-sizes (jdwp reply)
  (setf (jdwp-field-id-size jdwp) (bindat-get-field reply :field-id-size))
  (jdwp-trace "field-id-size         :%d" (jdwp-field-id-size jdwp))

  (setf (jdwp-method-id-size jdwp) (bindat-get-field reply :method-id-size))
  (jdwp-trace "method-id-size        :%d"  (jdwp-method-id-size jdwp))

  (setf (jdwp-object-id-size jdwp) (bindat-get-field reply :object-id-size))
  (jdwp-trace "object-id-size        :%d" (jdwp-object-id-size jdwp))

  (setf (jdwp-reference-type-id-size jdwp) (bindat-get-field reply :reference-type-id-size))
  (jdwp-trace "reference-type-id-size:%d" (jdwp-reference-type-id-size jdwp))

  (setf (jdwp-frame-id-size jdwp) (bindat-get-field reply :frame-id-size))
  (jdwp-trace "frame-id-size         :%d" (jdwp-frame-id-size jdwp)))

(defun jdwp-connect (jdwp server port)
  "[ASYNC] returns t if connected and an (ERROR-SYMBOL . SIGNAL-DATA) if there are problems connecting"
  (jdwp-trace "jdwp-connect:%s:%s" server port)
  (let ((buffer-name (concat " jdwp-socket-buffer-" server "-" (number-to-string port))))
	(if (get-buffer buffer-name) (kill-buffer buffer-name))
	(setf (jdwp-server jdwp) server)
	(setf (jdwp-port jdwp) port)
	(setf (jdwp-process jdwp) (open-network-stream "jdwp" buffer-name server port))
	(when (jdwp-process jdwp)
	  (process-put               (jdwp-process jdwp) 'jdwp jdwp)
	  (set-process-sentinel      (jdwp-process jdwp) 'jdwp-process-sentinel)
	  (with-current-buffer (process-buffer (jdwp-process jdwp))
		(set-buffer-multibyte nil))
	  (set-process-coding-system (jdwp-process jdwp) 'no-conversion 'no-conversion)
	  (jdwp-process-send-string jdwp jdwp-handshake)
	  (let ((received (jdwp-receive-message (jdwp-process jdwp)
											(lambda ()
											  (if (>= (jdwp-output-length jdwp) (length jdwp-handshake))
												  (jdwp-residual-output jdwp))))))
		;; note that if the debuggee is started with suspend=y
		;; we will get a command packet straight away after the handshake packet
		;; so we will need to do substring for the comparison
		(unless (string= jdwp-handshake (substring received 0 (length jdwp-handshake)))
		  (error "Handshake error:%s" received)))
	  (jdwp-consume-output jdwp (length jdwp-handshake))
	  ;; only after the handshake we use the process filter
	  (set-process-filter        (jdwp-process jdwp) 'jdwp-process-filter)
	  (jdwp-process-id-sizes jdwp (jdwp-send-command jdwp "id-sizes" nil))
	  jdwp)))

(defun jdwp-disconnect (jdwp)
  (condition-case err
	  (jdwp-send-command jdwp "dispose" nil)
	(error nil))
  (when (jdwp-process jdwp)
	(setf (process-sentinel (jdwp-process jdwp)) nil)
	(kill-buffer (process-buffer (jdwp-process jdwp))))
  ;;(delete-process (jdwp-process jdwp))
  (setf (jdwp-process jdwp) nil))

(defun jdwp-exit (jdwp command)
  (condition-case err
	  (jdwp-send-command jdwp "exit" command)
	(error (jdwp-error "Error executing exit command: %s" err)))
  (when (jdwp-process jdwp)
	(setf (process-sentinel (jdwp-process jdwp)) nil)
	(kill-buffer (process-buffer (jdwp-process jdwp))))
  ;;(delete-process (jdwp-process jdwp))
  (setf (jdwp-process jdwp) nil))

(defun jdwp-process-sentinel (proc string)
  (let ((jdwp (process-get proc 'jdwp)))
    (jdwp-debug "jdwp-process-sentinel:%s" string)
	(eval
	 `(jdwp-uninterruptibly
		(run-hook-with-args 'jdwp-event-hooks ,jdwp jdwp-event-vm-death)))))

(defun jdwp-process-reply (jdwp packet command-data)
  (jdwp-debug "jdwp-process-reply")
  (jdwp-with-size
    jdwp
    (let* ((id           (jdwp-packet-id packet))
		   (error-code   (jdwp-packet-reply-error packet))
		   (protocol     (jdwp-get-protocol (cdr (assoc :name command-data))))
		   (reply-spec   (getf protocol :reply-spec)))
	  (jdwp-trace "jdwp-process-reply packet-header:%s" packet)
      (if (not (= error-code 0))
		  (progn
			(jdwp-error "jdwp-process-reply: received error:%d:%s for id:%d command:%s" error-code (jdwp-error-string error-code) id (getf protocol :name))
			(if (memq error-code jdwp-ignore-error) nil (error "%s" error-code)))
		(if reply-spec
			(let ((reply-data   (bindat-unpack reply-spec (jdwp-packet-data packet))))
			  (jdwp-traffic-info "reply: %s" reply-data)
			  (jdwp-info "jdwp-process-reply:id:%5s command:[%20s] time:%-6s len:%5s error:%1d"
						 id
						 (getf protocol :name)
						 (float-time (time-subtract (current-time) (cdr (assoc :sent-time command-data))))
						 (jdwp-packet-length packet)
						 error-code)
			  (jdwp-info "reply-data:%s" (elog-trim reply-data 100))
			  reply-data)
		  ;; special case for array, we return the string so the caller can call unpack-arrayregion
		  (jdwp-traffic-info "reply: [no reply-spec]")
		  (jdwp-packet-data packet))))))

(defvar jdwp-event-hooks nil)

(defvar jdwp-throw-on-input-pending nil
  "When set to non-nil, jdwp-send-command will throw an error on user input.")

(defvar jdwp-input-pending nil
  "The symbol that will be thrown when jdwp-throw-on-input-pending is non-nil and there's input pending")

(defun jdwp-process-command (jdwp packet)
  (jdwp-debug "jdwp-process-command")
  (jdwp-with-size
    jdwp
    (let ((command-set    (jdwp-packet-command-command-set packet))
		  (command       (jdwp-packet-command-command packet))
		  (id            (jdwp-packet-id packet))
		  (flags         (jdwp-packet-flags packet)))
      (if (and (= command-set 64) (= command 100))
		  (let* ((packet          (bindat-unpack jdwp-event-spec (jdwp-packet-data packet)))
				 (suspend-policy  (bindat-get-field packet :suspend-policy))
				 (events          (bindat-get-field packet :events)))
			(jdwp-info "event suspend-policy:%d events:%d" suspend-policy events)
			(jdwp-trace "event:%s" (bindat-get-field packet :event))
			(jdwp-traffic-info "event: %s" packet)
			(dolist (event (bindat-get-field packet :event))
			  (eval `(jdwp-uninterruptibly
					   (run-hook-with-args 'jdwp-event-hooks ,jdwp (quote ,event))))))
		(jdwp-error "do not know how to handle command-set %d command %d" command-set command)))))

(defun jdwp-reply-packet-p (str)
  (let* ((packet (bindat-unpack jdwp-packet-spec str))
		 (flags (bindat-get-field packet :flags)))
	(= flags #x80)))

(defun jdwp-ordinary-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
	(let ((moving (= (point) (process-mark proc))))
	  (save-excursion
		;; Insert the text, advancing the process marker.
		(goto-char (process-mark proc))
		(insert string)
		(set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))))

(defun jdwp-consume-output (jdwp len)
  (jdwp-debug "jdwp-consume-output:len=%s" len)
  (when (jdwp-process jdwp)
    (with-current-buffer (process-buffer (jdwp-process jdwp))
      (goto-char (point-min))
      (delete-char len))))

(defun jdwp-residual-output (jdwp)
  (when (jdwp-process jdwp)
    (let* ((proc (jdwp-process jdwp))
		   (buf  (process-buffer proc)))
      (with-current-buffer buf
		(buffer-string)))))

(defun jdwp-output-length (jdwp)
  "Returns the length of the total output we get from the socket."
  (if (jdwp-process jdwp)
      (let* ((proc (jdwp-process jdwp))
			 (buf  (process-buffer proc)))
		(buffer-size buf))
    0))

(defun jdwp-output-first-packet-length (jdwp)
  "Returns the size of the first packet from the debuggee."
  (when (jdwp-process jdwp)
    (let* ((proc (jdwp-process jdwp))
		   (buf  (process-buffer proc)))
	  (with-current-buffer buf
		(if (< (buffer-size) 5)
			0

		  (bindat-get-field
		   (bindat-unpack '((:length u32)) (buffer-substring-no-properties 1 5))
		   :length))))))

(defun jdwp-get-string (s &rest fields)
  (concat (bindat-get-field (apply 'bindat-get-field s fields) :string)))

(defun jdwp-thread-status-string (status)
  "Convert a status to a meaning string representation"
  (let ((match (find-if (lambda (pair) (= (car pair) status)) jdwp-thread-status-constants)))
	(if match (cdr match) "Unknown")))

(defun jdwp-get-int (s &rest fields)
  (jdwp-vec-to-int (apply 'bindat-get-field s fields)))

(defun jdwp-vec-to-int (vec)
  "Converts a vector presentation into an integer."
  (let ((result 0)
		(i 0))
    (while (< i (length vec))
      (setq result (* result 256))
      (setq result (+ result (elt vec i)))
      (incf i))
    result))

(defun jdwp-vec-to-float (vec)
  (let* ((int (jdwp-vec-to-int vec))
		 (high (+ (* (elt vec 0) 256)
				  (elt vec 1)))
		 (sign (if (= 1 (lsh high -15)) -1 1))
		 (exponent (logand (lsh high -7) #xff))
		 (mantissa (logand int #x7fffff))
		 result)
	(jdwp-info "jdwp-vec-to-float vec=%s exponent=%d mantissa=%d" vec exponent mantissa)
	(case exponent
	  (0
	   ;; Subnormal
	   (setq result (/ (float mantissa) #x7fffff)
			 exponent -126)
	   (jdwp-debug "Converting %s %f %f" sign result exponent)
	   ;; Need to use a floating point in expt to avoid integer overflow
	   (setq result (* sign result (expt 2.0 exponent))))

	  (#xff
	   ;; NaN or infinity
	   (if (= 0 mantissa)
		   (if (> sign 0) '+infinity '-infinity)
		 'NaN))

	  (otherwise
	   ;; Normalized number
	   (setq result (+ 1.0 (/ (float mantissa) #x7fffff))
			 exponent (- exponent 127))
	   (jdwp-debug "Converting %s %f %f" sign result exponent)
	   ;; Need to use a floating point in expt to avoid integer overflow
	   (setq result (* sign result (expt 2.0 exponent)))
	   (jdwp-debug " to %f" result)
	   result))))

(defun jdwp-vec-to-double (vec)
  (jdwp-debug "jdwp-vec-to-double %s" vec)
  (let* ((short-high (jdwp-vec-to-int (subseq vec 0 2)))
		 (sign (if (= 1 (lsh short-high -15)) -1 1))
		 (exponent (logand (lsh short-high -4) #x7ff))
		 ;; bits 52-33 (20 bits)
		 (mantissa-high (logand (jdwp-vec-to-int (subseq vec 1 4)) #xfffff))
		 ;; bits 32-17 (16 bits)
		 (mantissa-mid (jdwp-vec-to-int (subseq vec 4 6)))
		 ;; bits 16-1  (16 bits)
		 (mantissa-low (jdwp-vec-to-int (subseq vec 6)))
		 result)
	(case exponent
	  (0 ;; subnormal
	   (setq result ( + (* mantissa-high (expt 2.0 -20))
						(* mantissa-mid (expt 2.0 -36))
						(* mantissa-low (expt 2.0 -52))))
	   ;; Need to use a floating point in expt to avoid integer overflow
	   (* sign result (expt 2.0 -1022)))
	  (#x7ff ;; infinite or nan
	   (if (and (= 0 mantissa-low) (= 0 mantissa-mid) (= 0 mantissa-high))
		   (if (> sign 0) '+infinity '-infinity)
		 'NaN))
	  (otherwise
	   (setq result (+ 1.0 (* mantissa-high (expt 2.0 -20))
					   (* mantissa-mid (expt 2.0 -36))
					   (* mantissa-low (expt 2.0 -52))))
	   (jdwp-debug "Converting %s %f %f" sign result exponent)
										; Need to use a floating point in expt to avoid integer overflow
	   (setq result (* sign result (expt 2.0 (- exponent 1023))))
	   (jdwp-debug " to %f" result)
	   result))))

(eval-when (eval)
  (when (featurep 'elunit)
	(defsuite jdwp-suite nil
	  :teardown-hooks (lambda () (message "done testing")))

	(deftest jdwp-test-vec-to-float jdwp-suite
	  "Testing function jdwp-vec-to-float"
	  ;; these tests are from http://people.uncw.edu/tompkinsj/133/Numbers/Reals.htm
	  (assert-equal 1234.0000250339538 (jdwp-vec-to-float [#x44 #x9a #x40 #x00]))
	  (assert-equal 25431.124125376242 (jdwp-vec-to-float [#x46 #xc6 #xae #x3f]))
	  (assert-equal 25431.112406624845 (jdwp-vec-to-float [#x46 #xc6 #xae #x39]))
	  )

	;;  (elunit "jdwp-suite")
	))

(defun jdwp-get-protocol (name)
  (find-if (lambda (p)
			 (string= name (getf p :name)))
		   jdwp-protocol))

(defun jdwp-get-next-id (jdwp)
  (incf (jdwp-current-id jdwp)))

;; this is a modified version of the bindat-pack in emacs23
;; which returns multibyte string
;; it should be removed when bug#2878 is fixed
(defun jdwp-bindat-pack (spec struct &optional bindat-raw bindat-idx)
  (when (multibyte-string-p bindat-raw)
    (error "Pre-allocated string is multibyte"))
  (let ((no-return bindat-raw))
    (unless bindat-idx (setq bindat-idx 0))
    (unless bindat-raw
      (setq bindat-raw (make-string (+ bindat-idx (bindat-length spec struct)) 0)))
    (bindat--pack-group struct spec)
    (if no-return nil bindat-raw)))

(defun jdwp-process-send-string (jdwp string)
  (process-send-string (jdwp-process jdwp) string))

(defvar jdwp-sending-command nil)
(defvar jdwp-after-send-command-actions nil
  "Actions to perform after a send-command completes.  This
allows us to delay handling asynchronous events from the JVM
while we are sending a command.  The value should be a list of
conses.  The car should be the function.  The cdr is passed to
the function as arguments.")

(defun jdwp-send-command (jdwp name data)
  (when (not jdwp-uninterruptibly-running-p)
	(jdwp-error "(jdwp-send-command %s %s) must be called from within jdwp-uninterruptibly" name data)
	(error "(jdwp-send-command %s %s) must be called from within jdwp-uninterruptibly" name data))
  (if jdwp-sending-command
	  (error "jdwp-sending-command:%s:%s" name jdwp-sending-command))

  (prog1
	  (let ((jdwp-sending-command name))
		(jdwp-with-size
		  jdwp
		  (if (null (jdwp-process jdwp))
			  (jdwp-trace "jdwp is not connected")

			(let* ((protocol     (jdwp-get-protocol name))
				   (reply-spec   (getf protocol :reply-spec))
				   (command-spec (getf protocol :command-spec))
				   (command-set   (getf protocol :command-set))
				   (command      (getf protocol :command))
				   (outdata      (jdwp-bindat-pack command-spec data))
				   (id           (jdwp-get-next-id jdwp))
				   (command-data `((:name        . ,name)
								   (:length      . ,(+ 11 (length outdata)))
								   (:id          . ,id)
								   (:flags       . 0)
								   (:command-set  . ,command-set)
								   (:command     . ,command)
								   (:sent-time   . ,(if jdwp-info-flag (current-time) 0))))
				   (command-packed (concat (jdwp-bindat-pack jdwp-command-spec command-data) outdata)))
			  (jdwp-info "sending command [%-20s] id:%-4d len:%-4d data:%s"
						 name
						 id
						 (+ 11 (length outdata))
						 (let ((outstr (jdwp-string-to-hex outdata)))
						   (if (> (length outstr) 100)
							   (substring outstr 0 100)
							 outstr)))
			  (jdwp-traffic-info "sending command [%-20s] %s" name data)
			  (jdwp-debug "data:%s" data)
			  (let ((inhibit-eol-conversion t))
				(setf (jdwp-current-reply jdwp) nil)
				(jdwp-debug "command-packed:%s" (jdwp-string-to-hex command-packed))
				(jdwp-process-send-string jdwp command-packed)
				(jdwp-process-reply jdwp
									(jdwp-receive-message (jdwp-process jdwp)
														  (lambda ()
															(if (and (jdwp-current-reply jdwp)
																	 (= (jdwp-packet-id (jdwp-current-reply jdwp)) id))
																(jdwp-current-reply jdwp)
															  (setf (jdwp-current-reply jdwp) nil))))
									command-data))))))
	;; Apply any delayed calls
	(jdwp-run-after-send-command-actions)))

(defun jdwp-run-after-send-command-actions nil
  "Run the after-send actions.  Be careful that if an action adds
to the list we do not rerun the action."

  (when jdwp-after-send-command-actions
	  (let ((pair (car jdwp-after-send-command-actions)))
		(setq jdwp-after-send-command-actions
			  (cdr jdwp-after-send-command-actions))
		(jdwp-debug "executing delayed %s" (car pair))
		(funcall (car pair) (cdr pair)))
	  ;; recurse to handle the rest of the list, which could have
	  ;; grown while we were running
	  (jdwp-run-after-send-command-actions)))


(defvar jdwp-accepting-process-output nil)
(defun jdwp-accepting-process-output-p ()
  jdwp-accepting-process-output)

(defun jdwp-accept-process-output (proc)
  (jdwp-debug "jdwp-accept-process-output")
  (accept-process-output proc jdwp-block-seconds nil nil))

(defun jdwp-receive-message (proc func)
  "Wait for message from proc, returns when func returns non-nil or timed out"
  (catch 'done
	(let ((timeout (+ (float-time) jdwp-timeout)))
	  (while t
		(let ((jdwp-accepting-process-output t))
		  (let ((result (jdwp-accept-process-output proc)))
			(jdwp-debug "jdwp-receive-message:accept-process-output returned %s" result))
		  (let ((result (funcall func)))
			(if result
				(throw 'done result)
			  (jdwp-debug "no result")
			  (when (and jdwp-throw-on-input-pending (input-pending-p))
				(jdwp-info "jdwp-receive-message:input-pending-p")
				(throw 'jdwp-input-pending nil))
			  (when (> (float-time) timeout)
				(jdwp-error "timed out")
				(error "timed out")))))))))

(defun jdwp-class-status-string (status)
  (concat (if (zerop (logand status 1)) nil "[VERIFIED]")
		  (if (zerop (logand status 2)) nil "[PREPARED]")
		  (if (zerop (logand status 4)) nil "[INITIALIZED]")
		  (if (zerop (logand status 8)) nil "[ERROR]")))

(defun jdwp-type-tag-string (type-tag)
  (cond ((= type-tag jdwp-type-tag-class)
		 "CLASS")
		((= type-tag jdwp-type-tag-interface)
		 "INTERFACE")
		((= type-tag jdwp-type-tag-array)
		 "ARRAY")))

(defun jdwp-error-string (errcode)
  (let ((err (find-if (lambda (err) (= (car err) errcode)) jdwp-error-constants)))
    (if err
		(format "%s:%s" (symbol-name (nth 1 err)) (nth 2 err))
      (format "Unknown error:%d" errcode))))

(defun jdwp-send-step (jdwp depth thread)
  (jdwp-trace "jdwp-send-step")
  (let ((data `((:event-kind     . 1)
				(:suspend-policy . ,jdwp-suspend-policy-event-thread)
				(:modifiers      . 2)
				(:modifier
				 ((:mod-kind . 10)
				  (:thread   . ,thread)
				  (:size     . 1)
				  (:depth    . ,depth))
				 ((:mod-kind . 1)
				  (:count    . 1))))))
    (jdwp-send-command jdwp "set" data)))

(defconst jdwp-packet-header-size 11)
(defconst jdwp-packet-reply-flag #x80)

(defun jdwp-packet-unpack (string)
  "Returns a jdwp-packet if there's enough information, nil if short.

This method does not consume the packet, the caller can know by checking jdwp-packet-length of the returned packet for the size."
  (jdwp-debug "jdwp-packet-unpack")
  (let ((string-length (length string)))
	(when (>= string-length jdwp-packet-header-size)
	  (let ((packet-length (bindat-get-field (bindat-unpack '((:length u32)) string) :length)))
		(when (>= string-length packet-length)
		  (jdwp-trace "jdwp-packet-unpack: found full packet:%s" (jdwp-string-to-hex string))
		  (let ((packet-flags (string-to-char (substring string 8))))
			(jdwp-trace "jdwp-packet-unpack: packet-flags=%s" packet-flags)
			(if (= packet-flags jdwp-packet-reply-flag)
				(let ((unpacked (bindat-unpack jdwp-reply-spec (substring string 0 jdwp-packet-header-size))))
				  (jdwp-trace "jdwp-packet-unpack: returning jdwp-packet-reply")
				  (make-jdwp-packet-reply :length (bindat-get-field unpacked :length)
										  :id     (bindat-get-field unpacked :id)
										  :flags  (bindat-get-field unpacked :flags)
										  :error  (bindat-get-field unpacked :error)
										  :data   (substring string jdwp-packet-header-size)))
			  (let ((unpacked (bindat-unpack jdwp-command-spec (substring string 0 jdwp-packet-header-size))))
				(jdwp-trace "jdwp-packet-unpack: returning jdwp-packet-command")
				(make-jdwp-packet-command :length      (bindat-get-field unpacked :length)
										  :id          (bindat-get-field unpacked :id)
										  :flags       (bindat-get-field unpacked :flags)
										  :command     (bindat-get-field unpacked :command)
										  :command-set (bindat-get-field unpacked :command-set)
										  :data        (substring string jdwp-packet-header-size))))))))))

(defvar jdwp-signal-count 0)
(defun jdwp-signal-hook (error-symbol data)
  (jdwp-info "debug-on-error=%s jdwp-signal-count=%s" debug-on-error jdwp-signal-count)
  (setq jdwp-signal-count (1+ jdwp-signal-count))
  (if (< jdwp-signal-count 5)
	  (jdwp-error "jdwp-signal-hook:%s:%s\n%s\n" error-symbol data
				  (with-output-to-string (backtrace)))
	(if (< jdwp-signal-count 50)
		(jdwp-error "jdwp-signal-hook:%s:%s (backtrace suppressed)"
					error-symbol data)
	  (let ((signal-hook-function nil)) (error error-symbol data)))))


(defun jdwp-run-with-timer (secs repeat function &rest args)
  (apply 'run-with-timer secs repeat (lambda (function &rest args)
									   (setq signal-hook-function 'jdwp-signal-hook)
									   (unwind-protect
										   (apply function args)
										 (setq signal-hook-function nil)))
		 function args))


(defmacro jdwp-uninterruptibly (&rest body)
  "Execute BODY, not allowing interrupts also wrapped in this
macro to run until after BODY finishes.

If BODY is executed immediately, the return value is the return value of BODY.  If execution is deferred, the return value is 'jdwp-deferred.

BODY might also not be evaluated
until after this returns, if another uninterruptable call is in
progress.  That means that locally scoped variables (such as
those defined in an enclosling `let' may have gone out of scope.

A workaround is to use the following (somewhat ugly) pattern:
	 (let ((extra \"456\"))
	   (eval `(jdwp-uninterruptibly
				(setq string (concat string ,extra)))))

Variables that have complicated structure may require explicit quoting:
	 (let ((extra '((:u (:foo bar)))))
	   (eval `(jdwp-uninterruptibly
				(setq string (concat string (format \"%s\" (quote ,extra)))))))

It is best if this is placed at a high level where
"
  (declare (indent defun))
  `(if jdwp-uninterruptibly-running-p
	   (progn
		 (setq jdwp-uninterruptibly-waiting
			   (add-to-list 'jdwp-uninterruptibly-waiting
							(lambda () ,@body)))
		 'jdwp-deferred)
	 (prog1
		 (let ((jdwp-uninterruptibly-running-p t))
		   ,@body)
	   ;; If something is waiting, run it
	   (when jdwp-uninterruptibly-waiting
		 (let ((func (pop jdwp-uninterruptibly-waiting)))
		   (jdwp-uninterruptibly (apply func nil)))))))



(provide 'jdwp)

;;; jdwp.el ends here
