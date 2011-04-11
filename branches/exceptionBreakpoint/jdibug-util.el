;;; jdibug-util.el --- library to communicate using Java(tm) Debug Wire Protocol

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

;; Utility routines for jdibug and jdwp.

;; This module requires elog.el

;;; Code:

(require 'elog)
(elog-make-logger jdibug-util)

;; Install a signal hook to log errors, but don't do it while inside
;; condition-case.
(defadvice condition-case (around jdibug-util-condition-case )
  "Remember when we are ina  condition-case so that we can ignore signals"
  (let ((jdibug-util-in-condition-case t))
	 ad-do-it))


(defmacro jdibug-util-with-signal-hook (body)
  "Execute BODY with the jdibug-util signal handler installed"
  (declare (indent 'defun))
  `(progn
	  (ad-activate 'condition-case)
	  (let ((signal-hook-function 'jdibug-util--signal-hook))
		 ,body)
	  (ad-deactivate 'condition-case)))


(defvar jdibug-util-signal-count 0)
(defun jdibug-util--signal-hook (error-symbol data)
  "Signal hook for jdibug-util calls.  Do not set directly.  Instead, use `jdibug-util-with-signal-hook'"

  ;; If the error is going to be caught, just rethrow it
  (if (and (boundp 'jdibug-util-in-condition-case)
			  jdibug-util-in-condition-case)
		(let ((signal-hook-function nil))
		  (signal error-symbol data)))

  ;; Enter the debugger if debug-on-error is set and the error won't
  ;; be caught
  (jdibug-util-info "debug-on-error=%s jdibug-util-signal-count=%s %s %s"
						  debug-on-error jdibug-util-signal-count error-symbol data)
  (if (and debug-on-error
			  (and (boundp 'jdibug-util-in-condition-case)
					 (not jdibug-util-in-condition-case)))
		(debug))

  ;; Log it, but not to often
  (setq jdibug-util-signal-count (1+ jdibug-util-signal-count))
  (if jdibug-util-error-flag
		(if (< jdibug-util-signal-count 5)
			 (jdibug-util-error "jdibug-util-signal-hook:%s:%s\n%s\n" error-symbol data
							 (with-output-to-string (backtrace)))
		  (if (< jdibug-util-signal-count 50)
				(jdibug-util-error "jdibug-util-signal-hook:%s:%s (backtrace suppressed)"
								error-symbol data)
			 (let ((signal-hook-function nil)) (error error-symbol data))))
	 (let ((signal-hook-function nil)) (error error-symbol data))))

(defun jdibug-util-run-with-timer (secs repeat function &rest args)
  (apply 'run-with-timer secs repeat (lambda (function &rest args)
													(jdibug-util-with-signal-hook
													 (apply function args)))
		 function args))

(provide 'jdibug-util)

;;; jdibug-util.el ends here

