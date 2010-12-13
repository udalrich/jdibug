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
(elog-make-logger jdibug-expr-parse)

(defconst jdibug-expr-bad-eval 'jdibug-expr-bad-eval
  "Symbol thrown when an evaluation fails")

(defun jdibug-expr-bad-eval (string &rest args)
  "Throw the standard error when an eval fails.  The associated
data is STRING, which is passed to `format' and supplied with
ARGS"
  (throw jdibug-expr-bad-eval (apply #'format string args)))

(defun jdibug-expr-build-numerical-jdi-value (result result-type)
  "Convert emacs value RESULT into a JDI structure of type RESULT-TYPE"
  (let ((vector (jdwp-number-to-vec result result-type)))
	(make-jdi-primitive-value :type result-type
							  :value vector)))
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
  (jdibug-expr-abstract-method this))

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

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-dot) jdwp tree frame)
  ;; There are several possibilities: we could be looking at
  ;; var.field, package.class.staticMethod or (implicit this)
  ;; field.field.
  (or (jdibug-eval-rule-accept-var-dot-fields this jdwp tree frame)
	  (jdibug-eval-rule-accept-package this jdwp tree frame)
	  (jdibug-eval-rule-accept-implicit-this this jdwp tree frame)))

(defmethod jdibug-eval-rule-accept-implicit-this ((this jdibug-eval-rule-dot) jdwp tree frame)
  (jdibug-expr-bad-eval "jdibug-eval-rule-accept-implicit-this not yet implemented"))

(defmethod jdibug-eval-rule-accept-package ((this jdibug-eval-rule-dot) jdwp tree frame)
  (jdibug-expr-bad-eval "jdibug-eval-rule-accept-package not yet implemented"))

(defmethod jdibug-eval-rule-accept-var-dot-fields ((this jdibug-eval-rule-dot) jdwp tree frame)
  (let* ((attrs (semantic-tag-attributes tree))
		 (args (plist-get attrs :arguments))
		 (first-arg (car args))
		 (second-arg (nth 1 args)))
	;; If this is a variable, we can evaluate it and get a value.  If
	;; it isn't, we'll probably throw, in which case we just want to
	;; return nil
	(condition-case nil
		(let* ((first-value (jdibug-expr-eval-expr jdwp first-arg frame))
			  (second-name (semantic-tag-name second-arg))
			  (second-type (semantic-tag-class second-arg))
			  class field)
		  (when (and first-value (jdi-value-object-p first-value)
					 (eql second-type 'identifier))
			;; TODO: also handle array.length.  Do we need to handle class and thread also?
			(setq class (jdi-object-get-reference-type first-value)
				  field (jdi-reference-type-get-field-by-name class second-name))
			(when field
			  (jdi-object-get-value first-value field))))
	  (error nil))))

;; Array access
(defclass jdibug-eval-rule-array-element (jdibug-eval-rule-with-name)
  ;; fields
  ()
  "Evaluation of a array-element construct (foo[bar])")

(defmethod initialize-instance ((this jdibug-eval-rule-array-element) &rest fields)
  "Constructor for jdibug-eval-rule-array-element instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'array))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-array-element) jdwp tree frame)
  "Evaluate a rule for array[index]"
  (let* ((attrs (semantic-tag-attributes tree))
		 (args (plist-get attrs :arguments))
		 (first-arg (car args))
		 (second-arg (nth 1 args))
		 (first-value (jdibug-expr-eval-expr jdwp first-arg frame))
		 (second-value (jdibug-expr-eval-expr jdwp second-arg frame)))
	(jdibug-expr-debug "jdibug-expr-eval-expr: first value is a %s" (jdi-value-type first-value))
	(if (and first-value (jdi-value-array-p first-value))
		(if (and second-value (equal (jdi-value-type second-value) jdwp-tag-int))
			(let ((emacs-index (jdi-primitive-emacs-value second-value)))
			  (if (or (< emacs-index 0) (>= emacs-index (jdi-array-get-array-length first-value)))
				  (jdibug-expr-bad-eval "index out of bounds: %s" emacs-index)
				(car (jdi-array-get-values first-value emacs-index (1+ emacs-index)))))
		  (jdibug-expr-bad-eval "array index must evaluate to an int: %s" (jdwp-type-name (jdi-value-type second-value))))
	  (jdibug-expr-bad-eval "not an array: %s" (jdwp-type-name (jdi-value-type first-value))))))


;; Method invocation
(defclass jdibug-eval-rule-method-invocation (jdibug-eval-rule)
  ;; fields
  ()
  "Evaluation of a method-invocation construct (foo[bar])")

(defmethod initialize-instance ((this jdibug-eval-rule-method-invocation) &rest fields)
  "Constructor for jdibug-eval-rule-method-invocation instance"
  (call-next-method)
  (oset this :class 'function))

(defmethod jdibug-eval-rule-match ((this jdibug-eval-rule-method-invocation) tree)
  "Test to see if TREE matches against THIS rule.  It will match
if the super method matches and the name in tree is a string."
  (and (call-next-method)
	   (stringp (semantic-tag-name tree))))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-method-invocation) jdwp tree frame)
  "Evaluate a rule for object.call(...)"
  (let* ((attrs (semantic-tag-attributes tree))
		 (args (plist-get attrs :arguments))
		 (object (plist-get attrs :this))
		 (thread (jdi-frame-thread frame))
		 (object-value (jdibug-expr-eval-expr jdwp object frame))
		 (method-name (semantic-tag-name tree))
		 arg-values method)
	(jdibug-expr-debug "method-invocation: this is a %s" (jdi-value-type object-value))
	(if (and object-value (jdi-value-object-p object-value))
		(progn
		  ;; Evaluate each argument
		  (setq arg-values
				(mapcar (lambda (arg)
						  (jdibug-expr-eval-expr jdwp arg frame))
						args))
		  ;; TODO: better method matching when the method is overloaded
		  (setq method method-name)
		  (jdi-object-invoke-method object-value thread method arg-values nil))
	  (jdibug-expr-bad-eval "not an object: %s" (jdwp-type-name (jdi-value-type object-value))))))


;; Constants
(defclass jdibug-eval-rule-constant (jdibug-eval-rule)
  ()
  "Rules representing constant values")

(defmethod initialize-instance ((this jdibug-eval-rule-constant) &rest fields)
  "Constructor for jdibug-eval-rule-constant instance"
  (call-next-method)
  (oset this :class 'constant))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-constant) jdwp tree frame)
  "Evalutate a constant."
  (jdibug-expr-debug "jdibug-eval-rule-accept: %S" tree)
  (let* ((attrs (semantic-tag-attributes tree))
		 (type (plist-get attrs :type))
		 (value (semantic-tag-name tree)))
	(case type
	  ;; TODO: handle hex and float notation
	  (number
	   (let* ((jdwp-type (jdibug-expr-guess-type value))
			  (value (string-to-number value))
			  (jdwp-value (jdwp-number-to-vec value jdwp-type)))
		 (make-jdi-primitive-value :type jdwp-type
					   :value jdwp-value)))
	  (otherwise
	   (jdibug-expr-bad-eval "Uncertain how to evaluate %s" value)))))

;; Unary minus
(defclass jdibug-eval-rule-unary-minus (jdibug-eval-rule-with-name)
  ;; fields
  ()
  "Numerical negation: - 3"
  )

(defmethod initialize-instance ((this jdibug-eval-rule-unary-minus) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'unary-minus))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-unary-minus)
									jdwp tree frame)
  "Evalutate a binary rule."
  (let* ((attrs (semantic-tag-attributes tree))
		 (args (plist-get attrs :arguments))
		 (first-arg (car args))
		 (first-value (jdibug-expr-eval-expr jdwp first-arg frame))
		 (first-type (jdi-value-type first-value))
		 result)

	;; Check that we have valid types for the operation
	(jdibug-expr-debug "%s: checking if valid types %s" (object-class-name this) (jdwp-type-name first-type))
	(if (jdibug-expr-type-is-number-p first-type)
		(progn
		  (setq result (- (jdi-primitive-emacs-value first-value)))
		  (jdibug-expr-build-numerical-jdi-value result first-type))
	  (jdibug-expr-bad-eval "Unable to negate a %s"
							(jdwp-type-name first-type)))))

(defmethod valid-types ((this jdibug-eval-rule-unary-minus)
						first-type)
  "Numerical operations are valid if on numbers"
  (jdibug-expr-type-is-number-p first-type))


;; Binary operations
(defclass jdibug-eval-rule-binary (jdibug-eval-rule-with-name)
  ((operation-name :initarg :operation-name
				   :type string))
  :abstract
  "Abstract class for rules representing binary numerical operations")

(defmethod eval-binary-expression ((this jdibug-eval-rule-binary) first second)
  "Perform the actual evaluation of FIRST and SECOND, which are emacs values."
  (jdibug-expr-abstract-method this))

(defmethod jdibug-eval-rule-accept ((this jdibug-eval-rule-binary)
									jdwp tree frame)
  "Evalutate a binary rule."
  (let* ((attrs (semantic-tag-attributes tree))
		 (args (plist-get attrs :arguments))
		 (first-arg (car args))
		 (second-arg (nth 1 args))
		 (first-value (jdibug-expr-eval-expr jdwp first-arg frame))
		 (second-value (jdibug-expr-eval-expr jdwp second-arg frame))
		 (first-type (jdi-value-type first-value))
		 (second-type (jdi-value-type second-value))
		 (result-type (determine-result-type this first-type second-type))
		 result)

	;; Check that we have valid types for the operation
	(jdibug-expr-debug "%s: checking if valid types %s %s" (object-class-name this) (jdwp-type-name first-type) (jdwp-type-name second-type))
	(if (valid-types this first-type second-type)
		(progn
		  (setq result (eval-binary-expression this
											   (jdi-primitive-emacs-value first-value)
											   (jdi-primitive-emacs-value second-value)))
		  (build-jdi-value this result result-type))
	  (jdibug-expr-bad-eval "Unable to %s a %s and a %s"
							(oref this operation-name)
							(jdwp-type-name first-type)
							(jdwp-type-name second-type)))))

(defmethod valid-types ((this jdibug-eval-rule-binary) first-type second-type)
  "Determine if the types are valid for this operation"
  (jdibug-expr-abstract-method this))

(defmethod determine-result-type ((this jdibug-eval-rule-binary) first-type second-type)
  "Determine what type the result is when operatining on FIRST-TYPE and SECOND-TYPE"
  (jdibug-expr-abstract-method this))

(defmethod build-jdi-value ((this jdibug-eval-rule-binary) result result-type)
  "Build a JDI value of type RESULT-TYPE from the RESULT value"
  (jdibug-expr-abstract-method this))

;; Binary numerical operations
(defclass jdibug-eval-rule-binary-numerical (jdibug-eval-rule-binary)
  nil ; fields
  :abstract
  "Abstract class for rules representing binary numerical operations")

(defmethod determine-result-type ((this jdibug-eval-rule-binary-numerical)
								  first-type second-type)
  "Determine the resulting type of a numerical binary operation
on FIRST-TYPE and SECOND-TYPE"
  (jdibug-expr-merge-types first-type second-type))

(defmethod valid-types ((this jdibug-eval-rule-binary-numerical)
						first-type second-type)
  "Numerical operations are valid if both types are numbers"
  (and (jdibug-expr-type-is-number-p first-type)
	   (jdibug-expr-type-is-number-p second-type)))

(defmethod build-jdi-value ((this jdibug-eval-rule-binary-numerical)
							result result-type)
  "Build the jdi value from the RESULT returned from
`eval-binary-expression' as a RESULT-TYPE"
  (jdibug-expr-build-numerical-jdi-value result result-type))

;; Binary numerical operations that yield a boolean
(defclass jdibug-eval-rule-binary-numerical-to-boolean
  (jdibug-eval-rule-binary-numerical) ; super class
  nil ; fields
  :abstract
  "Abstract class for logical operations on number (<, ==, etc)")

(defmethod determine-result-type ((this jdibug-eval-rule-binary-numerical-to-boolean)
								  first-type second-type)
  "Determine the resulting type of a numerical binary operation
on FIRST-TYPE and SECOND-TYPE"
  jdwp-tag-boolean)

(defmethod build-jdi-value ((this jdibug-eval-rule-binary-numerical-to-boolean)
							result result-type)
  "Build the jdi value from the RESULT returned from
`eval-binary-expression' as a RESULT-TYPE"
  (make-jdi-primitive-value :type result-type
							  :value (if result 1 0)))

;; Less than
(defclass jdibug-eval-rule-less-than (jdibug-eval-rule-binary-numerical-to-boolean)
  ;; fields
  ()
  "Evaluation of a less than (foo < bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-less-than) &rest fields)
  "Constructor for jdibug-eval-rule-less-than instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'less-than)
  (oset this :operation-name "less than"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-less-than) first second)
  (< first second))



;; Less than or equal
(defclass jdibug-eval-rule-less-equal (jdibug-eval-rule-binary-numerical-to-boolean)
  ;; fields
  ()
  "Evaluation of a less than or equal (foo <= bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-less-equal) &rest fields)
  "Constructor for jdibug-eval-rule-less-equal instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'less-equal)
  (oset this :operation-name "less than"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-less-equal) first second)
  (<= first second))

;; Greater than
(defclass jdibug-eval-rule-greater-than (jdibug-eval-rule-binary-numerical-to-boolean)
  ;; fields
  ()
  "Evaluation of a greater than (foo > bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-greater-than) &rest fields)
  "Constructor for jdibug-eval-rule-greater-than instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'greater-than)
  (oset this :operation-name "greater than"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-greater-than) first second)
  (> first second))



;; Greater than or equal
(defclass jdibug-eval-rule-greater-equal (jdibug-eval-rule-binary-numerical-to-boolean)
  ;; fields
  ()
  "Evaluation of a greater than or equal (foo >= bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-greater-equal) &rest fields)
  "Constructor for jdibug-eval-rule-greater-equal instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'greater-equal)
  (oset this :operation-name "greater than"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-greater-equal) first second)
  (>= first second))


;; not equal to
(defclass jdibug-eval-rule-not-equal-to
  (jdibug-eval-rule-binary-numerical-to-boolean)
  ;; fields
  ()
  "Evaluation of a not equal to (foo != bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-not-equal-to) &rest fields)
  "Constructor for jdibug-eval-rule-not-equal-to instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'not-equal)
  (oset this :operation-name "not equal"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-not-equal-to) first second)
  (not (= first second)))

;; equal to
(defclass jdibug-eval-rule-equal-to (jdibug-eval-rule-binary-numerical-to-boolean)
  ;; fields
  ()
  "Evaluation of a equal to (foo == bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-equal-to) &rest fields)
  "Constructor for jdibug-eval-rule-equal-to instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'equal)
  (oset this :operation-name "equal to"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-equal-to) first second)
  (= first second))



;; Multiplication
(defclass jdibug-eval-rule-mult (jdibug-eval-rule-binary-numerical)
  ;; fields
  ()
  "Evaluation of a multiplication (foo * bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-mult) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'mult)
  (oset this :operation-name "multiply"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-mult) first second)
  (* first second))

;; Division
(defclass jdibug-eval-rule-div (jdibug-eval-rule-binary-numerical)
  ;; fields
  ()
  "Evaluation of a division (foo / bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-div) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'div)
  (oset this :operation-name "divide"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-div) first second)
  (jdibug-expr-debug "eval-binary-expression(div): %s %s" first second)
  (/ first second))

;; Addition
(defclass jdibug-eval-rule-plus (jdibug-eval-rule-binary-numerical)
  ;; fields
  ()
  "Evaluation of an addition (foo + bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-plus) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'plus)
  (oset this :operation-name "add"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-plus) first second)
  (+ first second))



;; subtraction
(defclass jdibug-eval-rule-minus (jdibug-eval-rule-binary-numerical)
  ;; fields
  ()
  "Evaluation of a subtraction (foo - bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-minus) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'minus)
  (oset this :operation-name "subtract"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-minus) first second)
  (- first second))

;; Boolean binary operation (&&, ||, etc)

(defclass jdibug-eval-rule-binary-boolean (jdibug-eval-rule-binary)
  nil ; fields
  :abstract
  "Abstract class for rules representing binary boolean operations")

(defmethod determine-result-type ((this jdibug-eval-rule-binary-boolean)
								  first-type second-type)
  "Determine the resulting type of a boolean binary operation
on FIRST-TYPE and SECOND-TYPE.  Returns `jdwp-tag-boolean'"
  jdwp-tag-boolean)

(defmethod valid-types ((this jdibug-eval-rule-binary-boolean)
						first-type second-type)
  "Boolean operations are valid if both types are booleans"
  (and (equal jdwp-tag-boolean first-type)
	   (equal jdwp-tag-boolean second-type)))

(defmethod build-jdi-value ((this jdibug-eval-rule-binary-boolean)
							result result-type)
  "Build the jdi value from the RESULT returned from
`eval-binary-expression' as a RESULT-TYPE"
  (make-jdi-primitive-value :type result-type
							:value (if result 1 0)))

;; Logical and
(defclass jdibug-eval-rule-logand (jdibug-eval-rule-binary-boolean)
  ;; fields
  ()
  "Evaluation of a logical and (foo || bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-logand) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'logand)
  (oset this :operation-name "logical and"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-logand) first second)
  (and first second))



;; Logical or
(defclass jdibug-eval-rule-logor (jdibug-eval-rule-binary-boolean)
  ;; fields
  ()
  "Evaluation of a logical or (foo && bar)")

(defmethod initialize-instance ((this jdibug-eval-rule-logor) &rest fields)
  "Constructor for jdibug-eval-rule-mult instance"
  (call-next-method)
  (oset this :class 'function)
  (oset this :name 'logor)
  (oset this :operation-name "logical or"))


(defmethod eval-binary-expression ((this jdibug-eval-rule-logor) first second)
  (or first second))


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
  (jdibug-expr-debug "jdibug-eval-rule-accept %s" tree)
  (let* ((name (semantic-tag-name tree))
		(variables (jdi-frame-get-visible-variables frame))
		(var (find name variables :key 'jdi-variable-name :test 'string-equal)))
	;; Get the value of the variable if it is in scope.  Otherwise, return an error string.
	(if  var
		;; jdi-frame-get-values will return a list but we want the bare value.
		(car (jdi-frame-get-values frame (list var)))
	  (jdibug-expr-bad-eval "%s is not in scope" name))))



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
	  (jdibug-expr-error "Unable to match to any rule: %s" tree)
	  (jdibug-expr-bad-eval "Unable to match to any rule: %s" tree))))

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
	 ;; Turn of the built in java tag expander, since our tags have an
	 ;; inconsistent format.
	 semantic-tag-expand-function nil
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
			(jdibug-expr-parse-debug "Parsed %s to %S" expr result)
			(if (> (length result) 1)
				(format "Multiple expressions in %s" expr)
			  (let* ((tree (car result))
					 (bounds (semantic-tag-bounds tree)))
			  (if semantic-unmatched-syntax-cache
				  (format "Unable to parse %s at %s"
						  expr
						  (substring expr (1- (semantic-tag-end tree))))
				tree))))
		(jdibug-expr-parse-debug "Unable to parse %s" expr)
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


(defun jdibug-expr-guess-type (value)
  "Guess which jdwp type the string VALUE represents.  For example, '3' will return `jdwp-tag-int' while '3.' will return `jdwp-tag-double'"
  (jdibug-expr-trace "jdibug-expr-guess-type: %S" value)
  (cond
   ;; Double
   ((or (string-match "^[-+]?[0-9]+\\.[0-9]*$" value) ; explicit decimal point
		(string-match "^[-+]?[0-9]*\\.[0-9]+$" value) ; explicit decimal point
		(string-match "%[-+]?[0-9.]+[eE][-+]?[0-9]+$" value) ; scientific notation
		)
	jdwp-tag-double)
   ;; Float
   ((or (string-match "^[-+]?[0-9]+\\.[0-9]*[Ff]$" value) ; explicit decimal point
		(string-match "^[-+]?[0-9]*\\.[0-9]+[Ff]$" value) ; explicit decimal point
		(string-match "%[-+]?[0-9.]+[eE][-+]?[0-9]+[Ff]$" value) ; scientific notation
		)
	jdwp-tag-float)
   ;; Long
   ((string-match "^[-+]?[0-9]+[lL]$" value) jdwp-tag-long)

   ;; Int
   ((string-match "^[-+]?[0-9]+$" value) jdwp-tag-int)

   ;; No way to specify a byte or short constant

   (t (jdibug-expr-bad-eval "Unable to determine type of %s" value))))

(defun jdibug-expr-abstract-method (this)
  "'Implementation' for abstract methods.  Throws an error if actually called."
    (error "Subclasses must override %s: %s" eieio-generic-call-methodname (object-class-name this)))


(provide 'jdibug-expr)