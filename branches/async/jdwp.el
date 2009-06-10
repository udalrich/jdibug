;;; jdwp.el --- library to communicate using Java(tm) Debug Wire Protocol

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
;; http://java.sun.com/j2se/1.4.2/docs/guide/jpda/jdwp/jdwp-protocol.html
;; http://java.sun.com/j2se/1.4.2/docs/guide/jpda/jdwp-spec.html

;; This module requires elog.el

;;; Code:

(require 'bindat)
(require 'elog)

(defcustom jdwp-timeout 3
  "Number of seconds to timeout before replies arrive from the debuggee."
  :group 'jdibug
  :type 'integer)

(elog-make-logger jdwp)

(defstruct jdwp
  ;; the elisp process that connects to the debuggee
  process		   

  ;; t when we have received handshake from the server
  handshaked-p	   

  ;; the last used command id that we sent to the server
  (current-id            0) 

  ;; an alist with the event type as key and the handler as value
  event-handlers-alist 

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

(setq jdwp-error-constants
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
(setq jdwp-packet-spec
      '((:length     u32)
		(:id         u32)
		(:flags      u8)))

(setq jdwp-command-spec
      '((:length     u32)
		(:id         u32)
		(:flags      u8)
		(:commandset u8)
		(:command    u8)
		(:outdata   vec (eval (- (bindat-get-field struct :length) 11)))))

(setq jdwp-reply-spec
      '((:length     u32)
		(:id         u32)
		(:flags      u8)
		(:error      u16)))

(setq jdwp-string-spec
      '((:length     u32)
		(:string     vec (:length))))

(setq jdwp-location-spec
      '((:type      u8)
		(:class-id  vec (eval jdwp-reference-type-id-size))
		(:method-id vec (eval jdwp-method-id-size))
		(:index     vec 8)))

(setq jdwp-event-spec
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



(setq jdwp-value-spec
      `((:type  u8)
		(:u     union (:type)
				(,jdwp-tag-array        (:value vec (eval jdwp-object-id-size)))
				(,jdwp-tag-byte         (:value u8))
				(,jdwp-tag-char         (:value u16))
				(,jdwp-tag-object       (:value vec (eval jdwp-object-id-size)))
				(,jdwp-tag-float        (:value vec 4))
				(,jdwp-tag-double       (:value vec 8))
				(,jdwp-tag-int          (:value u32))
				(,jdwp-tag-long         (:value vec 8))
				(,jdwp-tag-short        (:value u16))
				(,jdwp-tag-void)
				(,jdwp-tag-boolean      (:value u8))
				(,jdwp-tag-string       (:value vec (eval jdwp-object-id-size)))
				(,jdwp-tag-thread       (:value vec (eval jdwp-object-id-size)))
				(,jdwp-tag-thread-group (:value vec (eval jdwp-object-id-size)))
				(,jdwp-tag-class-loader (:value vec (eval jdwp-object-id-size)))
				(,jdwp-tag-class-object (:value vec (eval jdwp-object-id-size))))))

(setq jdwp-arrayregion-header-spec
      '((:type    u8)
		(:length  u32)))

(defun jdwp-unpack-arrayregion (jdwp packet)
  (jdwp-trace "unpacking array-region:%s" (jdwp-string-to-hex packet))
  (jdwp-with-size 
    jdwp
	(let* ((header (bindat-unpack jdwp-arrayregion-header-spec packet))
		   (type   (bindat-get-field header :type))
		   (length (bindat-get-field header :length))
		   repeater spec unpacked)
	  (setq repeater 
			(case type
			  (76 '(:value struct jdwp-value-spec))
			  (91 '(:value struct jdwp-value-spec))
			  (66 '(:value u8))
			  (67 '(:value u16))
			  (70 '(:value u32))
			  (68 '(:value vec 8))
			  (73 '(:value u32))
			  (74 '(:value vec 8))
			  (83 '(:value u16))
			  (90 '(:value u8))))
	  (setq spec `((:type u8) (:length u32) (:value repeat (:length) ,repeater)))
	  (bindat-unpack spec packet))))

(setq jdwp-protocol
      `((:name         "version"
					   :commandset   1 
					   :command      1
					   :command-spec nil
					   :reply-spec   ((:description   struct jdwp-string-spec)
									  (:jdwp-major    u32)
									  (:jdwp-minor    u32)
									  (:vm-version    struct jdwp-string-spec)
									  (:vm-name       struct jdwp-string-spec)))
		(:name         "all-classes"
					   :commandset   1
					   :command      3
					   :command-spec nil
					   :reply-spec   ((:classes       u32)
									  (:class         repeat (:classes)
													  (:ref-type-tag u8)
													  (:type-id      vec (eval jdwp-reference-type-id-size))
													  (:signature    struct jdwp-string-spec)
													  (:status       u32))))
		(:name         "all-threads"
					   :commandset   1
					   :command      4
					   :command-spec nil
					   :reply-spec   ((:threads       u32)
									  (:thread        repeat (:threads)
													  (:id   vec (eval jdwp-object-id-size)))))
		(:name         "top-level-thread-groups"
					   :commandset   1
					   :command      5
					   :command-spec nil
					   :reply-spec   ((:groups        u32)
									  (:group         repeat (:group)
													  (:id   vec (eval jdwp-object-id-size)))))
		(:name         "dispose"
					   :commandset   1
					   :command      6
					   :command-spec nil
					   :reply-spec   nil)
		(:name         "id-sizes"
					   :commandset   1
					   :command      7
					   :command-spec nil
					   :reply-spec   ((:field-id-size          u32)
									  (:method-id-size         u32)
									  (:object-id-size         u32)
									  (:reference-type-id-size u32)
									  (:frame-id-size          u32)))
		(:name         "suspend"
					   :commandset   1
					   :command      8
					   :command-spec nil
					   :reply-spec   nil)
		(:name         "resume"
					   :commandset   1
					   :command      9
					   :command-spec nil
					   :reply-spec   nil)
		(:name         "capabilities-new"
					   :commandset   1
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
					   :commandset   1
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
					   :commandset   2
					   :command      1
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:signature     struct jdwp-string-spec)))
		(:name         "class-loader"
					   :commandset   2
					   :command      2
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:class-loader  vec (eval jdwp-object-id-size))))
		(:name         "fields"
					   :commandset   2
					   :command      4
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:declared      u32)
									  (:field         repeat (:declared)
													  (:id        vec (eval jdwp-field-id-size))
													  (:name      struct jdwp-string-spec)
													  (:signature struct jdwp-string-spec)
													  (:mod-bits  u32))))
		(:name         "methods"
					   :commandset   2
					   :command      5
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:methods       u32)
									  (:method        repeat (:methods)
													  (:method-id      vec (eval jdwp-method-id-size))
													  (:name           struct jdwp-string-spec)
													  (:signature      struct jdwp-string-spec)
													  (:mod-bits       u32))))
		(:name         "reference-get-values"
					   :commandset   2
					   :command      6
					   :command-spec ((:ref-type     vec (eval jdwp-object-id-size))
									  (:fields        u32)
									  (:field         repeat (:fields)
													  (:id vec (eval jdwp-field-id-size))))
					   :reply-spec   ((:values        u32)
									  (:value         repeat (:values)
													  (:value struct jdwp-value-spec))))
		(:name         "source-file"
					   :commandset   2
					   :command      7
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:source-file   struct jdwp-string-spec)))
		(:name         "interfaces"
					   :commandset   2
					   :command      10
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:interfaces    u32)
									  (:interface     repeat (:interfaces)
													  (:type   vec (eval jdwp-reference-type-id-size)))))
		(:name         "class-object"
					   :commandset   2
					   :command      11
					   :command-spec ((:ref-type      vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:class-object  vec (eval jdwp-object-id-size))))
		(:name         "fields-with-generic"
					   :commandset   2
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
					   :commandset   3
					   :command      1
					   :command-spec ((:class         vec (eval jdwp-reference-type-id-size)))
					   :reply-spec   ((:superclass    vec (eval jdwp-reference-type-id-size))))
		(:name         "class-invoke-method"
					   :commandset   3
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
					   :commandset   6
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
					   :commandset   6
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
					   :commandset   6
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
		(:name         "reference-type"
					   :commandset   9
					   :command      1
					   :command-spec ((:object        vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:ref-type-tag  u8)
									  (:type-id       vec (eval jdwp-object-id-size))))
		(:name         "object-get-values"
					   :commandset   9
					   :command      2
					   :command-spec ((:object        vec (eval jdwp-object-id-size))
									  (:fields        u32)
									  (:field         repeat (:fields)
													  (:id vec (eval jdwp-field-id-size))))
					   :reply-spec   ((:values        u32)
									  (:value         repeat (:values)
													  (:value struct jdwp-value-spec))))
		(:name         "object-invoke-method"
					   :commandset   9
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
					   :commandset   10
					   :command      1
					   :command-spec ((:object        vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:value         struct jdwp-string-spec)))
		(:name         "thread-name"
					   :commandset   11
					   :command      1
					   :command-spec ((:thread        vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:thread-name   struct jdwp-string-spec)))
		(:name         "thread-status"
					   :commandset   11
					   :command      4
					   :command-spec ((:thread         vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:thread-status  u32)
									  (:suspend-status u32)))
		(:name         "frames"
					   :commandset   11
					   :command      6
					   :command-spec ((:thread         vec (eval jdwp-object-id-size))
									  (:start-frame    u32)
									  (:length         u32))
					   :reply-spec   ((:frames         u32)
									  (:frame          repeat (:frames)
													   (:id       vec (eval jdwp-frame-id-size))
													   (:location struct jdwp-location-spec))))
		(:name         "frame-count"
					   :commandset   11
					   :command      7
					   :command-spec ((:thread         vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:frame-count    u32)))
		(:name         "thread-group-name"
					   :commandset   12
					   :command      1
					   :command-spec ((:group         vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:group-name    struct jdwp-string-spec)))
		(:name         "thread-group-children"
					   :commandset   12
					   :command      3
					   :command-spec ((:group         vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:child-threads u32)
									  (:child-thread  repeat (:child-threads)
													  (:child-thread   vec (eval jdwp-object-id-size)))
									  (:child-groups  u32)
									  (:child-group   repeat (:child-groups)
													  (:child-group    vec (eval jdwp-object-id-size)))))
		(:name         "array-length"
					   :commandset   13
					   :command      1
					   :command-spec ((:array-object  vec (eval jdwp-object-id-size)))
					   :reply-spec   ((:array-length  u32)))
		(:name         "array-get-values"
					   :commandset   13
					   :command      2
					   :command-spec ((:array-object  vec (eval jdwp-object-id-size))
									  (:first-index   u32)
									  (:length        u32))
					   :reply-spec   nil)
		(:name         "set"
					   :commandset   15
					   :command      1
					   :command-spec ((:event-kind     u8)
									  (:suspend-policy u8)
									  (:modifiers      u32)
									  (:modifier       repeat (:modifiers)
													   (:mod-kind    u8)
													   (:u           union (:mod-kind)
																	 (1    (:count    u32))
																	 (5    (:class-pattern struct jdwp-string-spec))
																	 (7    (:location struct jdwp-location-spec))
																	 (8    (:exception vec (eval jdwp-reference-type-id-size))
																		   (:caught    u8)
																		   (:uncaught  u8))
																	 (10   (:thread   vec (eval jdwp-object-id-size))
																		   (:size     u32)
																		   (:depth    u32)))))
					   :reply-spec   ((:request-id    u32)))
		(:name         "clear"
					   :commandset   15
					   :command      2
					   :command-spec ((:event         u8)
									  (:request-id    u32))
					   :reply-spec   nil)
		(:name         "stack-get-values"
					   :commandset   16
					   :command      1
					   :command-spec ((:thread        vec (eval jdwp-object-id-size))
									  (:frame         vec (eval jdwp-frame-id-size))
									  (:slots         u32)
									  (:slot          repeat (:slots)
													  (:slot    u32)
													  (:sigbyte u8)))
					   :reply-spec   ((:values        u32)
									  (:value         repeat (:values)
													  (:slot-value struct jdwp-value-spec))))))

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

(defun jdwp-send-handshake (jdwp)
  (process-send-string (jdwp-process jdwp) jdwp-handshake)
  (catch 'done
	(while t
	  (accept-process-output (jdwp-process jdwp) 1 0 t)
	  (jdwp-info "jdwp-residual-output:%s" (jdwp-residual-output jdwp))
	  (if (and (>= (length (jdwp-residual-output jdwp)) (length jdwp-handshake))
			   (string= jdwp-handshake (substring (jdwp-residual-output jdwp) 0 (length jdwp-handshake))))
		  (throw 'done nil))))
  (jdwp-info "received handshake")
  (setf (jdwp-handshaked-p jdwp) t)
  (jdwp-consume-output jdwp (length jdwp-handshake)))

(defun jdwp-connect (jdwp server port)
  (let ((buf (get-buffer " jdwp-socket-buffer")))
	(if buf (kill-buffer buf)))
  (setf (jdwp-server jdwp) server)
  (setf (jdwp-port jdwp) port)
  (setf (jdwp-process jdwp) (open-network-stream "jdwp" (concat " jdwp-socket-buffer-" server "-" (number-to-string port))  server port))
  (when (jdwp-process jdwp)
    (process-put               (jdwp-process jdwp) 'jdwp jdwp)
    (set-process-filter        (jdwp-process jdwp) 'jdwp-process-filter)
    (set-process-sentinel      (jdwp-process jdwp) 'jdwp-process-sentinel)
    (with-current-buffer (process-buffer (jdwp-process jdwp))
      (set-buffer-multibyte nil))
    (set-process-coding-system (jdwp-process jdwp) 'no-conversion 'no-conversion)
    (jdwp-send-handshake jdwp)))

(defun jdwp-disconnect (jdwp)
  (condition-case err
	  (jdwp-send-command jdwp "dispose" nil)
	(error nil))
  (when (jdwp-process jdwp)
	(setf (process-sentinel (jdwp-process jdwp)) nil)
	(kill-buffer (process-buffer (jdwp-process jdwp))))
  ;;(delete-process (jdwp-process jdwp))
  (setf (jdwp-process jdwp) nil)
  (setf (jdwp-handshaked-p jdwp) nil))

(defun jdwp-process-sentinel (proc string)
  (let ((jdwp (process-get proc 'jdwp)))
    (jdwp-trace "jdwp-process-sentinel:%s" string)
    (let ((handler (cdr (assoc jdwp-event-vm-death (jdwp-event-handlers-alist jdwp)))))
      (if handler
		  (funcall handler jdwp nil)))))

;; declare the dynamic variables for our unpacker
(defmacro jdwp-with-size (jdwp &rest body)
  (declare (indent defun))
  `(let ((jdwp-field-id-size          (jdwp-field-id-size          ,jdwp))
		 (jdwp-method-id-size         (jdwp-method-id-size         ,jdwp))
		 (jdwp-object-id-size         (jdwp-object-id-size         ,jdwp))
		 (jdwp-reference-type-id-size (jdwp-reference-type-id-size ,jdwp))
		 (jdwp-frame-id-size          (jdwp-frame-id-size          ,jdwp)))
     ,@body))

(defun jdwp-process-reply (jdwp str command-data)
  (jdwp-with-size 
    jdwp
    (let* ((packet       (bindat-unpack jdwp-reply-spec (substring str 0 11)))
		   (id           (bindat-get-field packet :id))
		   (error        (bindat-get-field packet :error))
		   (protocol     (jdwp-get-protocol (cdr (assoc :name command-data))))
		   (reply-spec   (getf protocol :reply-spec)))
	  (jdwp-info "process-reply packet-header:%s" packet)
      (if (not (= error 0))
		  (progn
			(if (member error (list jdwp-error-absent-information 
									jdwp-error-thread-not-suspended))
				(jdwp-info "received error:%d:%s for id:%d command:%s" error (jdwp-error-string error) id (getf protocol :name))
			  (jdwp-error "received error:%d:%s for id:%d command:%s" error (jdwp-error-string error) id (getf protocol :name)))
			(list (substring str 11) error jdwp id))
		(if reply-spec
			(let ((reply-data   (bindat-unpack reply-spec str 11)))
			  (jdwp-info "reply id:%5s command:%20s time:%-6s len:%5s error:%1d" 
						 id 
						 (getf protocol :name)
						 (float-time (time-subtract (current-time) (cdr (assoc :sent-time command-data))))
						 (bindat-get-field packet :length) 
						 (bindat-get-field packet :error))
			  (jdwp-trace "reply-data:%s" reply-data)
			  (list reply-data error jdwp id))
		  (list (substring str 11) error jdwp id))))))

(defun jdwp-process-command (jdwp str)
  (jdwp-with-size 
    jdwp
    (let* ((packet        (bindat-unpack jdwp-command-spec str))
		   (commandset    (bindat-get-field packet :commandset))
		   (command       (bindat-get-field packet :command))
		   (id            (bindat-get-field packet :id))
		   (flags         (bindat-get-field packet :flags)))
      (if (and (= commandset 64) (= command 100))
		  (let* ((packet          (bindat-unpack jdwp-event-spec str 11))
				 (suspend-policy  (bindat-get-field packet :suspend-policy))
				 (events          (bindat-get-field packet :events)))
			(jdwp-info "event suspend-policy:%d events:%d" suspend-policy events)
			(jdwp-trace "event:%s" (bindat-get-field packet :event))
			(dolist (event (bindat-get-field packet :event))
			  (let ((handler (cdr (assoc (bindat-get-field event :event-kind) (jdwp-event-handlers-alist jdwp)))))
				(if handler
					(funcall handler jdwp event)
				  (jdwp-error "do not know how to handle event %d:%s" (bindat-get-field event :event-kind) event)))))
		(jdwp-error "do not know how to handle commandset %d command %d" commandset command)))))

(defun jdwp-set-event-handler (jdwp event-type handler)
  (push `(,event-type . ,handler) (jdwp-event-handlers-alist jdwp)))

(defun jdwp-reply-packet-p (str)
  (let* ((packet (bindat-unpack jdwp-packet-spec str))
		 (flags (bindat-get-field packet :flags)))
	(= flags #x80)))

(defun jdwp-get-packet (jdwp)
  (jdwp-with-size jdwp
    (let ((packet nil)
		  (total-length (jdwp-output-length jdwp))
		  (first-packet-length (jdwp-output-first-packet-length jdwp)))
	  (jdwp-info "jdwp-get-packet total-length=%s first-packet-length=%s" total-length first-packet-length)
	  (when (and (>= total-length 11)
				 (>= total-length first-packet-length)
		  (setq packet (substring (jdwp-residual-output jdwp) 0 first-packet-length))
		  (jdwp-consume-output jdwp first-packet-length)))
	  packet)))

(defvar jdwp-accepting-more-output nil
  "This will be true when we are 'in' jdwp-accept-more-output")

(defun jdwp-accept-more-output (jdwp)
  (jdwp-info "jdwp-accept-more-output")
  (let ((jdwp-accepting-more-output t))
	(while (accept-process-output (jdwp-process jdwp) 0.01)
	  (jdwp-info "accepted some output"))))

(defun jdwp-ordinary-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)))

(defun jdwp-append-output (jdwp process string)
  (jdwp-ordinary-insertion-filter process string))

;; (defun jdwp-append-output (jdwp process string)
;;   (setf (jdwp-residual-output jdwp) (concat (jdwp-residual-output jdwp) (string-as-unibyte string))))

(defun jdwp-consume-output (jdwp len)
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
	  (condition-case err
		  (with-current-buffer buf
			(bindat-get-field
			 (bindat-unpack '((:length u32)) (buffer-substring-no-properties 1 5))
			 :length))
		('args-out-of-range 0)))))

(defun jdwp-output-first-packet-header (jdwp)
  "Returns the size of the first packet from the debuggee."
  (when (jdwp-process jdwp)
    (let* ((proc (jdwp-process jdwp))
		   (buf  (process-buffer proc)))
      (with-current-buffer buf
		(buffer-substring-no-properties 1 12)))))

(defun jdwp-process-filter (process string)
  (jdwp-info "jdwp-process-filter")
  (jdwp-trace "string=%s" string)
  (let* ((jdwp (process-get process 'jdwp)))
    (jdwp-append-output jdwp process string)
	;;(jdi-trace "residual=%s" (jdwp-residual-output jdwp))
    (if (jdwp-handshaked-p jdwp)
		(let ((packet))
		  (while (setq packet (jdwp-get-packet jdwp))
			(if (jdwp-reply-packet-p packet)
				(progn
				  (setf (jdwp-current-reply jdwp) packet)
				  (jdwp-info "received reply packet"))
;; 			  (run-with-idle-timer 0 nil
;; 								   (lambda (jdwp packet)
;; 									 (jdwp-process-command jdwp packet))
;; 								   jdwp packet)
			  (jdwp-process-command jdwp packet)
			  (jdwp-info "received command packet")))))))

(defun jdwp-get-string (s &rest fields)
  (concat (bindat-get-field (apply 'bindat-get-field s fields) :string)))

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
		 (sign (lsh high -15))
		 (exponent (- (logand (lsh high -7)
							  #xff)
					  127))
		 (mantissa (logand int #x7fffff))
		 result)
    (setq result (+ 1.0 (/ (float mantissa) #x7fffff)))
    (setq result (* result (expt 2 exponent)))
    result))

(eval-when (eval)
  (when (featurep 'elunit)
	(defsuite jdwp-suite nil
	  :teardown-hook (lambda () (message "done testing")))

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
  (incf (jdwp-current-id jdwp))
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

(defun jdwp-send-command (jdwp name data)
  (jdwp-with-size 
    jdwp
    (if (null (jdwp-process jdwp))
		(jdwp-trace "jdwp is not connected")
      (let* ((protocol     (jdwp-get-protocol name))
			 (reply-spec   (getf protocol :reply-spec))
			 (command-spec (getf protocol :command-spec))
			 (commandset   (getf protocol :commandset))
			 (command      (getf protocol :command))
			 (outdata      (jdwp-bindat-pack command-spec data))
			 (id           (jdwp-get-next-id jdwp))
			 (command-data `((:name        . ,name)
							 (:length      . ,(+ 11 (length outdata)))
							 (:id          . ,id)
							 (:flags       . 0)
							 (:commandset  . ,commandset)
							 (:command     . ,command)
							 (:data        . ,data)
							 (:outdata     . ,outdata)
							 (:sent-time   . ,(if jdwp-info-flag (current-time) 0))))
			 (command-packed (jdwp-bindat-pack jdwp-command-spec command-data)))
		(jdwp-info "sending command [%-20s] id:%-4d len:%-4d data:%s" 
				   name 
				   id 
				   (+ 11 (length outdata)) 
				   (let ((outstr (jdwp-string-to-hex outdata)))
					 (if (> (length outstr) 100)
						 (substring outstr 0 100)
					   outstr)))
		(let ((inhibit-eol-conversion t))
		  (setf (jdwp-current-reply jdwp) nil)
		  (process-send-string (jdwp-process jdwp) command-packed)
		  (catch 'done
			(with-timeout 
				(jdwp-timeout
				 ;; the callers do not have ways to handle this, just error for the moment
				 (error "JDWP Timed Out"))
			  (while t
				(accept-process-output (jdwp-process jdwp) 1 0 t)
				(if (jdwp-current-reply jdwp)
					(progn
					  (jdwp-info "got reply:%s" (jdwp-string-to-hex (jdwp-current-reply jdwp) 100))
					  (throw 'done (jdwp-process-reply jdwp (jdwp-current-reply jdwp) command-data)))
				  (sit-for 0))))))))))

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

(defun jdwp-clear-breakpoint (jdwp request-id)
  (jdwp-send-command jdwp "clear" `((:event . ,jdwp-event-breakpoint) (:request-id . ,request-id))))

(defun jdwp-send-step (jdwp depth thread)
  (jdwp-trace "jdwp-send-step")
  (let ((data `((:event-kind     . 1)
				(:suspend-policy . ,jdwp-suspend-policy-all)
				(:modifiers      . 2)
				(:modifier       
				 ((:mod-kind . 10)
				  (:thread   . ,thread)
				  (:size     . 1)
				  (:depth    . ,depth))
				 ((:mod-kind . 1)
				  (:count    . 1))))))
    (jdwp-send-command jdwp "set" data)))

(provide 'jdwp)

;;; jdwp.el ends here