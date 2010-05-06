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
(require 'jdwp)
(require 'jdi)
(require 'eieio)
(require 'elog)
(require 'semantic-java)

(elog-make-logger jdibug-expr)


(defclass jdibug-eval-rule ()
  ;; fields
  ((class :initarg :class
		  :type symbol))
  :abstract
  "Abstract super class defining a rule for evaluating a parse tree")

(defclass jdibug-eval-rule-with-name (jdibug-eval-rule)
  ;; fields
  ((name :initarg :name
		 :type (or symbol string)))
  :abstract
  "Abstract super class defining a rule for evaluating a parse tree")

(defmethod jdibug-eval-rule-match ((this jdibug-eval-rule) tree)
  "Test to see if TREE matches against THIS rule"
  (let ((class (semantic-tag-class tree)))
	(eq class (oref this class))))

(defmethod jdibug-eval-rule-match ((this jdibug-eval-rule-with-name) tree)
  "Test to see if TREE matches against THIS rule"
  (and (call-next-method)
	   (let ((name (semantic-tag-name tree)))
		 (eq name (oref this name)))))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule) jdwp tree frame)
  "Use this rule to evaluate TREE in the context of the JDWP and the stack FRAME.  This should only be called if `jdibug-eval-rule-match' has passed with THIS and TREE.

Subclasses must override this method."
  (error "jdibug-eval-rule-accept was not overriden in %s"
		 (object-class-name this)))

;; Dot operations
(defclass jdibug-eval-rule-dot (jdibug-eval-rule-with-name)
  ;; fields
  ()
  "Evaluation of a dot construct (foo.bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-dot) &rest fields)
  "Constructor for jdibug-eval-rule-dot instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'dot))

;; Multiplication
(defclass jdibug-eval-rule-mult (jdibug-eval-rule-with-name)
  ;; fields
  ()
  "Evaluation of a multiplication (foo * bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-mult) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'mult))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-mult) jdwp tree frame)
  "Evalutate multiplication"
  (let* ((attrs (semantic-tag-attributes tree))
		 (args (plist-get attrs :arguments))
		 (first-arg (car args))
		 (second-arg (nth 1 args))
		 (first-value (jdibug-expr-eval-expr jdwp first-arg frame))
		 (second-value (jdibug-expr-eval-expr jdwp second-arg frame))
		 (first-type (jdi-value-type first-value))
		 (second-type (jdi-value-type second-value))
		 (result-type (jdibug-expr-merge-types first-type second-type))
		 product vector)

	;; Multiplication is only defined between numbers.
	(if (and (jdibug-expr-type-is-number-p first-type)
			 (jdibug-expr-type-is-number-p second-type))
		(progn
		  (setq product (* (jdi-primitive-emacs-value first-value)
						   (jdi-primitive-emacs-value second-value))
				vector (jdwp-number-to-vec product result-type))
		  (make-jdi-primitive-value :type result-type
									:value vector))
	  (throw 'jdibug-expr-bad-eval
			 (format "Unable to multiple a %s and a %s"
					 (jdwp-type-name first-type)
					 (jdwp-type-name second-type))))))



;; Variable reference
(defclass jdibug-eval-rule-identifier (jdibug-eval-rule)
  ;; fields
  ()
  "Evaluation of a identfier (varName)")

(defmethod initialize-instance ((this jdibug-eval-rule-identifier) &rest fields)
  "Constructor for jdibug-eval-rule-identifier instance"
  (call-next-method)
  (oset this :class 'identifier))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-identifier) jdwp tree frame)
  "Get the object represented by the variable name"
  (let* ((name (semantic-tag-name tree))
		(variables (jdi-frame-get-visible-variables frame))
		(var (find name variables :key 'jdi-variable-name :test 'string-equal)))
	;; Get the value of the variable if it is in scope.  Otherwise, return an error string.
	(if  var
		;; jdi-frame-get-values will return a list but we want the bare value.
		(car (jdi-frame-get-values frame (list var)))
	  (throw jdibug-expr-bad-eval (format "%s is not in scope" name)))))



;; 	(cond
;; 	 ;; "this" is special but won't match this rule
;; 	 ((string-equal name "this")
;; 	  (jdi-frame-get-this-object frame))




(defun jdibug-expr-make-concrete-instances (class)
  "Return a list of an instance of each concrete descendent of CLASS."
  (if (class-abstract-p class)
	  (apply #'append (mapcar #'jdibug-expr-make-concrete-instances (class-children class)))
	(list (make-instance class))))


(defconst jdibug-expr-eval-rule-list (jdibug-expr-make-concrete-instances 'jdibug-eval-rule)
  "Rules for evaluating an expression")


(defun jdibug-expr-eval-expr (jdwp tree frame)
  "Using the JDWP connection to the target JVM, calculate the
value of the expression represented by TREE.  TREE should be the
return value from `jdibug-expr-parse-expr'.  References are
resolved within the context of FRAME.  The returned value is a jdi-value."
  (let ((rule (find-if (lambda (rule)
						 (jdibug-eval-rule-match rule tree))
					   jdibug-expr-eval-rule-list)))
	(if rule
		(jdibug-eval-rule-accept rule jdwp tree frame)
	  (jdibug-error "Unable to match to any rule: %s" tree)
	  (error "Unable to match to any rule: %s" tree))))

(defun jdibug-expr-parse-expr (expr)
  "Parse EXPR.  Returns either a tree of semantic parser
tokens (if successful) or a string describing the problem (if
unsuccessful).  The failure string may not be very descriptive."
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

(defun jdibug-expr-type-is-number-p (type)
  "Test if the jdwp TYPE is a number.  Returns t if it is and nil if it is not."
  (memq type (list jdwp-tag-byte jdwp-tag-float jdwp-tag-double jdwp-tag-int jdwp-tag-long jdwp-tag-short)))

(defconst jdibug-expr-merge-types-order
  (list jdwp-tag-byte
		jdwp-tag-short
		jdwp-tag-int
		jdwp-tag-long
		jdwp-tag-float
		jdwp-tag-double)
  "Ordering of types for merging operations.  If a binary
operation involves two values of different types, the one later
in the list will be the result of the operation.")

(defun jdibug-expr-merge-types (first second)
  "Determine what the result of an operation of primitive types FIRST and SECOND would result in.  For example, if the arguments are `jdwp-tag-float' and `jdwp-tag-int', the returned value is `jdwp-tag-float'."
  (if (eq first second)
	  ;; Same type, so no change
	  first
	;; Different types
	(let ((first-index (position first jdibug-expr-merge-types-order))
		  (second-index (position second jdibug-expr-merge-types-order)))
	  (nth (max first-index second-index) jdibug-expr-merge-types-order))))

(provide 'jdibug-expr)