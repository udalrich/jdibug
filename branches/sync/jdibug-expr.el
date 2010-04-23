;;; jdibug-expr.el --- JDIbug expression parsing

;; Copyright (C) 2010 Troy Daniels

;; Author: Troy Daniels <udalrich.schermer@gmail.com>
;; Maintainer: Troy Daniels <udalrich.schermer@gmail.com>
;; Created: 20 April 2010
;; Keywords: lisp tools parser

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

;; This file is used for parsing expressions.  It supports
;; (eventually) conditional breakpoints and evaluations of
;; expressions.  It uses semantic to parse the expression into an
;; abstract syntax tree (AST).  It also provides functions to evaluate
;; the expression.

(require 'jdibug-java-expr-wy)
(require 'eieio)
(require 'elog)

(elog-make-logger jdibug-expr)


(defclass jdibug-eval-rule ()
  ;; fields
  ((class :initarg :class
		  :type symbol)
   (name :initarg :name
		 :type (or symbol string)))
  :abstract
  "Abstract super class defining a rule for evaluating a parse tree")

(defmethod jdibug-eval-rule-match ((this jdibug-eval-rule) tree)
  "Test to see if TREE matches against THIS rule"
  (let ((class (semantic-tag-class tree))
		(name (semantic-tag-name tree)))
	(and (eq class (oref this class))
		 (eq name (oref this name)))))

(defclass jdibug-eval-rule-dot (jdibug-eval-rule)
  ;; fields
  ()
  "Evaluation of a dot construct (foo.bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-dot) &rest fields)
  "Constructor for jdibug-eval-rule-dot instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'dot))

(defclass jdibug-eval-rule-mult (jdibug-eval-rule)
  ;; fields
  ()
  "Evaluation of a multiplication (foo * bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-mult) &rest fields)
  "Constructor for jdibug-eval-rule-dot instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'mult))


(defun jdibug-expr-make-concrete-instances (class)
  "Return a list of an instance of each concrete descendent of CLASS."
  (if (class-abstract-p class)
	  (apply #'append (mapcar #'jdibug-expr-make-concrete-instances (class-children class)))
	(list (make-instance class))))


(defconst jdibug-expr-eval-rule-list (jdibug-expr-make-concrete-instances 'jdibug-eval-rule)
  "Rules for evaluating an expression")


(defun jdibug-expr-eval-expr (jdwp tree)
  "Using the JDWP connection to the target JVM, calculate the
value of the expression represented by TREE.  TREE should be the
return value from `jdibug-expr-parse-expr'"
  (let ((rule (find-if (lambda (rule)
						 (jdibug-eval-rule-match rule tree))
					   jdibug-expr-eval-rule-list)))
	(if rule
		(jdibug-eval-rule-accept rule jdwp tree)
	  (error "Unable to match to any rule: %s" tree))))

(defun jdibug-expr-parse-expr (expr)
  "Parse EXPR.  Returns either a tree of semantic parser tokens (if successful) or a string describing the problem (if unsuccessful).  The failure string may not be very descriptive."
  ;; Create a temporary buffer contining the expression to parse.  Set up the buffer for parsing.
  (jdibug-expr-trace "jdibug-expr-parse-expr %s" expr)
  (with-temp-buffer
	(java-mode)
	(insert expr)
	(jdibug-java-expr-wy--install-parser)
	(setq
	 ;; Lexical analysis
	 semantic-lex-number-expression semantic-java-number-regexp
	 semantic-lex-depth nil
	 semantic-lex-analyzer 'jdibug-java-expr-lexer

	 ;; svn after save hook won't work here
	 after-save-hook nil)
	(semantic-lex-init)

	;; Parse it
	(let ((result
		   (semantic-parse-region (point-min) (point-max) 'expression nil 'stop-on-error)))
	  (if result
		  ;; We parsed the buffer.  However, it is possible that we
		  ;; parsed multiple expressions or that there is unparsed
		  ;; garbage at the end
		  (progn
			(jdibug-expr-debug "Parsed %s to %S" expr result)
			(if (> (length result) 1)
				(format "Multiple expressions in %s" expr)
			  (let* ((tree (car result))
					 (bounds (semantic-tag-bounds tree)))
			  (if semantic-unmatched-syntax-cache
				  (format "Unable to parse %s at %s"
						  expr
						  (substring expr (semantic-tag-end tree)))
				tree))))
		(jdibug-expr-debug "Unable to parse %s")
		(format "Unable to parse %s" expr)))))

(provide 'jdibug-expr)