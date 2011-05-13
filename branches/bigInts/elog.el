;;; elog.el --- logging for elisp

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

;; I can't code without a logging module. This code closely mimics log4j.

;; To use the logger inside your foo module, do this
;; (elog-make-logger foo)
;; you will then have access to the following functions:
;;
;;     foo-trace, foo-debug, foo-info, foo-warn, foo-error and foo-fatal
;;
;; its up to you to make sure they do not clash with any functions in
;; your module each of the above functions have the same signature as
;; the 'message' function.  Note that `message' is actually a bad
;; function to use here, since the first argument might have percent
;; signs in it.  elog-safe-message is a better alternative.

;; To configure what log goes where, create another file (I call this elog-conf.el)
;; and load it from your .emacs file after you require elog.el, this is totally
;; unneccessary if you do not wish to see any debugging output.
;; call the elog-set-appenders function, like this:
;;
;; (elog-set-appenders
;;  (list
;;   (make-elog-appender :category 'foo
;; 					     :priority 'trace
;; 					     :layout "%H:%M:%S [%p] %c : %m%n"
;; 					     :output " foo-log")
;;   (make-elog-appender :category 'bar
;; 					     :priority 'error
;; 					     :layout "%H:%M:%S [%p] %c : %m%n"
;; 					     :output " bar-log")
;;   (make-elog-appender :priority 'error
;; 					     :layout "%H:%M:%S [%p] %c : %m%n"
;; 					     :output 'elog-safe-message)))
;;
;; :category specifies the module that you pass to elog-make-logger
;; :priority specifies that anything above and including this priority
;;           will be logged
;; :layout   limited mimic of the Layout of log4j, current supported layout
;;               specifies:
;;                 %H = hour
;;                 %M = minute
;;                 %S = second
;;                 %c = category
;;                 %p = priority
;;                 %m = log message
;;                 %n = platform newline
;; :output   can either be a string or a function
;;           if its a string, its the name of the buffer that will be created
;;               and output to.
;;           if its a function, it must conform to the same signature as
;;               the message function.
;;
;; The above sample, will log everything from the foo module into the foo-log
;; buffer, all errors from bar module into bar-log buffer, and errors from
;; all modules will be message-d.

;; All the logging functions foo-* are macros, so they will not be evaluating
;; their arguments if logging is not enabled for that category and priority.
;; so it's safe to do something like this
;; (foo-debug "dump=%s" (convert-a-very-long-string-to-hex-format packet))

;; If anyhow you wish to check that whether logging is enabled for a certain
;; category and priority, you can use the following variables
;;
;; foo-trace-flag, foo-debug-flag, foo-info-flag, foo-warn-flag, foo-error-flag, and foo-fatal-flag
;;
;; they will have a value of t if logging is enabled for that priority.
;; This can be used for something like this
;; (if foo-trace-flag
;;     (dolist (packet packets)
;;        (foo-trace "packet:%s" packet)))

;; Good Luck Have Fun

;;; Code:

(defstruct elog-appender
  category
  priority
  layout
  output)

(eval-and-compile
  (defvar elog-appenders nil
	"A list of elog-appender.")

  (defvar elog-categories nil
	"List of categories that we have so far.")

  (defvar elog-priorities nil
	"A list of priorities for logging, higher number means more important.")

  (setq elog-priorities
		'((trace . 0)
		  (debug . 1)
		  (info  . 2)
		  (warn  . 3)
		  (error . 4)
		  (fatal . 5)))

  (defun elog-priority-num (pri)
	(cdr (assoc pri elog-priorities)))

  (defun elog-get-appenders (priority category)
	(loop for app in elog-appenders
		  when (and (or (null (elog-appender-category app)) (equal category (elog-appender-category app)))
					(<= (elog-priority-num (elog-appender-priority app)) (elog-priority-num priority)))
		  collect app))

  (defun elog-update-flags ()
	(loop for cat in elog-categories
		  do
		  (loop for priority in elog-priorities
				do
				(let ((flag (intern (format "%s-%s-flag" cat (car priority)))))
				  (if (elog-get-appenders (car priority) cat)
					  (set flag t)
					(set flag nil)))))))

(defmacro elog-make-logger (category)
  (let ((macros
		 (mapcar (lambda (pri)
				   (let* ((suffix (car pri))
						  (funcname (concat (symbol-name category) "-" (symbol-name suffix))))
					 `(defmacro ,(intern funcname) (fmt &rest objects)
						`(elog-log ',',suffix ',',category ,fmt ,@objects))))
				 elog-priorities)))
	(add-to-list 'elog-categories category)
	(elog-update-flags)
    `(progn ,@macros (add-to-list 'elog-categories ',category) (elog-update-flags))))

(defun elog-set-appenders (appenders)
  (setq elog-appenders appenders)
  (elog-update-flags))

(defun elog-appender-layout-apply (layout priority category fmt &rest objects)
  (let* ((msg layout)
		 (now (current-time))
		 (hour (format-time-string "%H" now))
		 (minute (format-time-string "%M" now))
		 (second (format-time-string "%S" now))
		 (case-fold-search nil))
    (setq msg (replace-regexp-in-string "%H" hour msg))
    (setq msg (replace-regexp-in-string "%M" minute msg))
    (setq msg (replace-regexp-in-string "%S" second msg))
    (setq msg (replace-regexp-in-string "%c" (symbol-name category) msg))
    (setq msg (replace-regexp-in-string "%p" (format "%5s" (symbol-name priority)) msg))
    (setq msg (replace-regexp-in-string "%m" (apply 'format fmt objects) msg t t))
    (setq msg (replace-regexp-in-string "%n" "\n" msg))
    msg))

(defun elog-appender-apply (appender priority category fmt &rest objects)
  (let ((output (apply 'elog-appender-layout-apply (elog-appender-layout appender) priority category fmt objects)))
	(cond ((stringp (elog-appender-output appender))
		   (with-current-buffer (get-buffer-create (elog-appender-output appender))
			 (goto-char (point-max))
			 (insert output)))
		  ((functionp (elog-appender-output appender))
		   (funcall (elog-appender-output appender) output)))))

(defmacro elog-log (priority category fmt &rest objects)
  `(if (symbol-value (intern (format "%s-%s-flag" ,category ,priority)))
	   (let ((appenders (elog-get-appenders ,priority ,category)))
		 (if appenders
			 (dolist (app appenders)
			   (elog-appender-apply app ,priority ,category ,fmt ,@objects))))))

(defun elog-trim (obj max)
  (let ((str (if (stringp obj)
				 obj
			   (format "%s" obj))))
	(if (> (length str) max)
		(substring str 0 (- max 3))
	  str)))

(defun elog-safe-message (string &rest args)
  "Send STRING to message without causing problems if it contains
percent signs.  The remainder of the args are ignored."
  (message "%s" string))

(provide 'elog)

;;; elog.el ends here