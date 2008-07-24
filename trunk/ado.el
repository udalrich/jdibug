;;; ado.el --- asynchronously do things in elisp

;; Copyright (C) 2008 Phuah Yee Keat

;; Author: Phuah Yee Keat <ykphuah@gmail.com>
;; Maintainer: Phuah Yee Keat <ykphuah@gmail.com>
;; Created: 20 May 2008
;; Version: 0.1
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

;; When emacs interacts with the outside world, most of the communications
;; are done using hooks and callbacks, which are only simple for simple
;; programs. To simplify coding for the jdibug.el module, which interacts
;; with the debuggee process over TCP/IP, working solely on hooks and 
;; tables of callbacks is a process I would not go into.

;; So I present you, the code to syncrhonously do asyncrhonous commands in elisp.

;; Refer to the unit tests near the end of the file for fake usage examples.
;; Refer to jdibug for real usage examples. ;^)

;; The module requires elog.el

;;; Code:

(require 'elog)
(elog-make-logger ado)

(defstruct ado
  action
  saved-args
  parent
  next
  children
  (yielded 0))

(defvar ado-cc nil)

(defun ado-get-cc ()
  (when ado-cc
	(incf (ado-yielded ado-cc))
	ado-cc))

(defun ado-get-root (ado)
  (if (ado-parent ado)
	  (ado-get-root (ado-parent ado))
	ado))

(defun ado-cc-add-child (parent child)
  (ado-trace "ado-cc-add-child")
  (ado-dump "  child" child)
  (ado-dump "  parent" parent)
  (setf (ado-parent child) parent)
  (let ((lastchild (car (last (ado-children parent)))))
    (when lastchild
      (setf (ado-next lastchild) child)
      (ado-dump "  lastchild" lastchild)))
  (setf (ado-children parent) (append (ado-children parent) (list child))))

(defun ado-pop (child)
  (ado-dump "ado-pop child" child)
  (let ((parent (ado-parent child)))
    (when parent
      (setf (ado-children parent) (delete child (ado-children parent)))
      (if (null (ado-children parent))
		  (ado-pop parent)
		(car (ado-children parent))))))

(defun ado-dump (variable ado)
  (ado-trace "%s=" variable)
  (ado-trace "    action    :%s" (ado-action ado))
  (ado-trace "    saved-args:%s" (ado-saved-args ado))
  (ado-trace "    next's action:%s" (if (ado-next ado)
										(ado-action (ado-next ado))
									  "no next"))
  (ado-trace "    no of children:%d" (length (ado-children ado)))
  (ado-trace "    yielded:%d" (ado-yielded ado)))

(defun ado-child-waiting-p (ado)
  (ado-dump "ado-child-waiting-p ado" ado)
  (let ((result nil))
    (mapc (lambda (child)
			(if (ado-action child)
				(setq result t)))
		  (ado-children ado))))

(defun ado-start (cc &rest args)
  (ado-info "ado-start")
  
  (let ((done nil))
	(while (and cc (not done))
	  (let ((ado-cc cc)
			(ado-last-return-value (if (cdr args)
									   args
									 ;; if we have only one value, flatten it, don't have a list
									 ;; of one value
									 (car args))))
		(ado-dump "  executing " cc)
		(when (functionp (ado-action cc)) 
		  (apply (ado-action cc) (ado-saved-args cc))
		  (setf (ado-action cc) nil))
		(ado-dump "  executed " cc)
		(unless (or (> (ado-yielded cc) 0)
					(ado-children ado-cc))
		  (setq cc (ado-pop cc)))
		;;(setq args nil)
		(if (or (null cc)
				(> (ado-yielded ado-cc) 0)
				(ado-children ado-cc))
			(setq done t))))))

(defun ado-continue (cc &rest args)
  (ado-info "ado-continue")

  (decf (ado-yielded cc))

  (ado-trace "decreased yielded to:%d" (ado-yielded cc))
  (when (= (ado-yielded cc) 0)
	(apply 'ado-start (ado-pop cc) args)))

(defmacro ado (vars &rest body)
  (declare (indent defun))
  (let ((the-blocks (mapcar (lambda (expr) 
							  `(make-ado
								:action (lambda ,vars ,expr) 
								:saved-args (mapcar 'identity (list ,@vars))))
							body)))
    `(let ((parent ado-cc)
		   (ado-cc (make-ado)))
       (if parent
		   (ado-cc-add-child parent ado-cc))
       (mapc (lambda (ado-child) (ado-cc-add-child ado-cc ado-child)) (list ,@the-blocks))
       (ado-start (car (ado-children ado-cc))))))


;;; Start of Unit Tests
(when (featurep 'elunit)
  (defvar ado-test-log nil)
  (defvar ado-test-alist nil)

  (defun ado-test-start ()
    (setq ado-test-log nil)
	(setq ado-test-alist nil))

  (defun ado-log-append (s)
    (setq ado-test-log (nconc ado-test-log (list s))))

  (defsuite ado-suite nil
    :teardown-hook (lambda () (message "done testing")))

  (deftest ado-test-simple ado-suite
    "Testing simple commands."
    (ado-test-start)
    (ado ()
      (ado-log-append '1)
      (ado-log-append '2)
      (ado-log-append '3))
    (assert-equal '(1 2 3) ado-test-log))

  (deftest ado-test-simple-nested ado-suite
    "Testing nested simple commands."
    (ado-test-start)
    (ado ()
      (ado-log-append '1)
      (ado ()
		(ado-log-append '2)
		(ado-log-append '22))
      (ado-log-append '3))
    (assert-equal '(1 2 22 3) ado-test-log))

  (defun ado-test-send (s)
    (ado-trace "test-send:%s" s)
    (ado-log-append (format "send:%s" s))
    (push `(,s . ,(ado-get-cc)) ado-test-alist))
  
  (defun ado-test-received (s &rest args)
    (ado-trace "test-received:%s:%s" s args)
    (let* ((element (assoc s ado-test-alist))
		   (key (car element))
		   (cont (cdr element)))
      (apply 'ado-continue cont args)
      (setq ado-test-alist (assq-delete-all key ado-test-alist))))

  (deftest ado-test-async-nobody-interested ado-suite
    "Functions which wait but nobody interested in the result"
    (ado-test-start)
    (ado ()
      (ado-log-append '1)
      (ado-test-send "current-year")
      (ado-log-append '2))
    (assert-equal '(1 "send:current-year") ado-test-log)
    (ado-test-received "current-year" 2008)
    (assert-equal '(1 "send:current-year" 2) ado-test-log))

  (deftest ado-test-async-wait ado-suite
    "Functions which suspend executing."
    (ado-test-start)
    (ado ()
      (ado-log-append '1)
      (ado-test-send "current-year")
      (ado-log-append (format "current-year:%d" ado-last-return-value))
      (ado-log-append '2))
    (assert-equal '(1 "send:current-year") ado-test-log)
    (ado-test-received "current-year" 2008)
    (assert-equal '(1 "send:current-year" "current-year:2008" 2) ado-test-log))

  (deftest ado-test-async-nested-wait ado-suite
    "Nested expressions that send and wait"
    (ado-test-start)
    (lexical-let ((year))
      (ado ()
		(ado-log-append '1)
		(ado-test-send "current-year")
		(setq year ado-last-return-value)
		(ado-log-append (format "current-year:%d" year))
		(ado-test-send (format "months-in-year:%s" year))
		(let ((months ado-last-return-value))
		  (dolist (month ado-last-return-value)
			(ado-log-append (format "months-in-year:%s:%s" year month))))
		(ado-log-append '3)))
    (assert-equal '(1 "send:current-year") ado-test-log)
    (ado-test-received "current-year" 2008)
    (assert-equal '(1 "send:current-year" "current-year:2008" "send:months-in-year:2008") ado-test-log)
    (ado-test-received "months-in-year:2008" (list "Jan" "Feb" "Dec"))
    (assert-equal '(1 
					"send:current-year"
					"current-year:2008" 
					"send:months-in-year:2008" 
					"months-in-year:2008:Jan" 
					"months-in-year:2008:Feb" 
					"months-in-year:2008:Dec"
					3) ado-test-log))

  (deftest ado-test-async-sync-results-ignored ado-suite
    "Running two async expression at the same time."
    (ado-test-start)
    (ado ()
      (ado-log-append '1)
      (progn
		(ado-test-send "current-year")
		(ado-test-send "next-year"))
	  (ado-log-append '3))
    (assert-equal '(1 "send:current-year" "send:next-year") ado-test-log)
    (ado-test-received "current-year" 2008)
    (assert-equal '(1 "send:current-year" "send:next-year") ado-test-log)
    (ado-test-received "next-year" 2008)
    (assert-equal '(1 "send:current-year" "send:next-year" 3) ado-test-log))

  (deftest ado-test-async-sync ado-suite
    "Running two async expression at the same time."
    (ado-test-start)
    (ado ()
      (ado-log-append '1)
      (progn
		(ado ()
		  (ado-test-send "current-year")
		  (ado-log-append (format "current-year:%d" ado-last-return-value)))
		(ado ()
		  (ado-test-send "next-year")
		  (ado-log-append (format "next-year:%d" ado-last-return-value))))
      (ado-log-append '3))
    (assert-equal '(1 "send:current-year" "send:next-year") ado-test-log)
    (ado-test-received "current-year" 2008)
    (assert-equal '(1 "send:current-year" "send:next-year" "current-year:2008") ado-test-log)
    (ado-test-received "next-year" 2008)
    (assert-equal '(1 "send:current-year" "send:next-year" "current-year:2008" "next-year:2008" 3) ado-test-log))

  (defun ado-test-get-parents (name)
	(ado (name)
	  (ado-test-send (format "parents-of-%s" name))
	  (ado-log-append (format "father-of-%s:%s" name (car ado-last-return-value)))
	  (ado-log-append (format "mother-of-%s:%s" name (cdr ado-last-return-value)))))

  (defun ado-test-get-family-tree (name)
	(ado-trace "ado-test-get-family-tree:%s" name)
	(when name
	  (ado (name)
		(ado-test-get-parents name)
		(let ((father (car ado-last-return-value))
			  (mother (cdr ado-last-return-value)))
		  (ado-test-get-family-tree father)
		  (ado-test-get-family-tree mother)))))

  (setq ado-test-root-cc (make-ado))

  ;;(pp (ado-get-root (cdr (assoc "parents-of-jane" ado-test-alist))))
  (deftest ado-test-recursive-build-tree ado-suite
    "Recursive functions that build a tree."
    (ado-test-start)
	(let ((ado-cc ado-test-root-cc))
	  (ado ()
		(ado-log-append '1)
		(ado-test-get-family-tree "john")
		(ado-log-append '3)))
    (assert-equal '(1 "send:parents-of-john") ado-test-log)
    (ado-test-received "parents-of-john" '("jack" . "jane"))
    (assert-equal '(1 
					"send:parents-of-john" 
					"father-of-john:jack"
					"mother-of-john:jane"
					"send:parents-of-jack" 
					"send:parents-of-jane") ado-test-log)
    (ado-test-received "parents-of-jack" '("hogan" . nil))
    (assert-equal '(1 
					"send:parents-of-john" 
					"father-of-john:jack"
					"mother-of-john:jane"
					"send:parents-of-jack" 
					"send:parents-of-jane" 
					"father-of-jack:hogan"
					"mother-of-jack:nil"
					"send:parents-of-hogan") ado-test-log)
    (ado-test-received "parents-of-jane" '(nil . nil))
    (assert-equal '(1 
					"send:parents-of-john" 
					"father-of-john:jack"
					"mother-of-john:jane"
					"send:parents-of-jack" 
					"send:parents-of-jane"
					"father-of-jack:hogan"
					"mother-of-jack:nil"
					"send:parents-of-hogan"
					"father-of-jane:nil"
					"mother-of-jane:nil"
					) ado-test-log)
    (ado-test-received "parents-of-hogan" '(nil . nil))
    (assert-equal '(1 
					"send:parents-of-john" 
					"father-of-john:jack"
					"mother-of-john:jane"
					"send:parents-of-jack" 
					"send:parents-of-jane"
					"father-of-jack:hogan"
					"mother-of-jack:nil"
					"send:parents-of-hogan"
					"father-of-jane:nil"
					"mother-of-jane:nil"
					"father-of-hogan:nil"
					"mother-of-hogan:nil"
					3
					) ado-test-log)
	)


  (set (make-local-variable 'elunit-default-suite) "ado-suite")
  ;;(elunit "ado-suite")
  )

(provide 'ado)

;;; ado.el ends here