;;; jdibug-run.el --- Fully elisp java debugger

;; Copyright (C) 2010 Troy Daniels

;; Author: Troy Daniels <udalrich.schermer@gmail.com>
;; Maintainer: Troy Daniels <udalrich.schermer@gmail.com>
;; Created: 05 June 2010
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

;; This file provide an elementary way to start a Java process and
;; connect the debugger to it.

(require 'jdibug-ui)

(defcustom jdibug-run-jvm-args nil
  "The arguments that will be passed to the JVM by `jdibug-run'.  The value is
prepended with additional arguments that will cause the JVM to
wait for the debugger to connect."
  :group 'jdibug
  :type '(repeat (string :tag "JVM Argument")))

(defcustom jdibug-run-application-args nil
  "List of arguments passed to the java class by `jdibug-run'."
  :group 'jdibug
  :type '(repeat (string :tag "Argument")))

(defcustom jdibug-run-main-class
  (if (boundp 'jde-run-application-class) jde-run-application-class)
  "Name of the class launched by `jdibug-run'.  The default value
is `jde-run-application-class' if that symbol is bound."
  :group 'jdibug
  :type 'string)

(defcustom jdibug-run-timeout 10
  "Number of seconds to wait for Java process to be ready to accept connections."
  :group 'jdibug
  :type 'float)

(defcustom jdibug-run-jvm-ready-regexp
  "Listening for transport dt_socket at address: "
  "The JVM will print this string when it is ready for a debugger to connect."
  :group 'jdibug
  :type 'string)


(defun jdibug-run ()
  "Start a java process and connect to it with JDIbug.  The main
class is `jdibug-run-main-class', the JVM arguments are a default
set which enable to JDIbug to connect and `jdibug-run-jvm-args'
and the application arguments are `jdibug-run-application-args'"
  (interactive)

  (unless (stringp jdibug-run-main-class)
	(error "jdibug-run-main-class must be a string"))

  (let* ((name (concat "*" jdibug-run-main-class "*"))
		(buffer (get-buffer-create name))
		(args (append (jdibug-run-server-args)
					  jdibug-run-jvm-args
					  (list jdibug-run-main-class)
					  jdibug-run-application-args)))
	;; Remove any old output from the buffer
	(pop-to-buffer buffer)
	(erase-buffer)
	(insert "Running java ")
	(mapc (lambda (arg) (insert " " arg)) args)
	(insert "\n")

	;; Start the java process
	(apply 'start-process name buffer "java" args)

	;; Wait for the process to start
	(let* ((interval 0.1)
		   (max-count (/ jdibug-run-timeout interval))
		   (count 0)
		   jvm-ready)
	  (while (and (not jvm-ready) (< count max-count))
		(save-excursion
		  (set-buffer buffer)
		  (goto-char (point-min))
		  (if (search-forward-regexp jdibug-run-jvm-ready-regexp
									 (point-max) t)
			  (setq jvm-ready t)
			(redisplay t)
			(sleep-for interval)))))

	;; Connect the debugger
	(jdibug-connect)))


(defun jdibug-run-server-args nil
  "Get the arguments that configure a JVM to run as a debug
server.  It uses the port of the first entry in the
`jdibug-connect-host' list."

  (let ((port (nth 1 (split-string (car jdibug-connect-hosts)
								   ":"))))
	(list "-Xdebug"
		  (format "-Xrunjdwp:transport=dt_socket,address=%s,server=y,suspend=y"
				  port))))

(provide 'jdibug-run)
