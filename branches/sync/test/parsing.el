(add-hook 'after-save-hook
		  (lambda ()
			(eval-current-buffer)
			(elunit "parsing-suite"))
		  nil 'local)

(require 'elunit)
(require 'jdibug-expr)

(defsuite parsing-suite smoke-test-suite
  ;; :setup-hook (lambda () )
  ;; :teardown-hook (lambda () )
)

(deftest parse-this-super parsing-suite
  "Test parsing of this and super and other strange stuff"
  (let ((pairs '(("this.x" . (dot function (:arguments ((this identifier)
													   ("x" identifier)))))
				 ("java.lang.String.class" . (class function (:arguments
															  ((dot function
																	(:arguments
																	 ((dot function
																		   (:arguments
																			(("java" identifier)
																			 ("lang" identifier))))
																	  ("String" identifier))))))))
				 ("void.class" . (void class))
				 ("int[].class" . ((dims function (:arguments (("int" type) "[]")))  class))))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse expr))
		  pairs)))

(deftest parse-complicated parsing-suite
  "Parse complicated expressions"
  (let ((pairs '(("foo.bar(3 + x[this.size()]) / ( 2 - -3 >>> (a ^ b))" .
				  (div function (:arguments
								 (("bar" function (:this ("foo" identifier)
														 :arguments ((plus function
																		   (:arguments (("3" constant)
																						(array function
																							   (:arguments (("x" identifier)
																											("size" function
																											 (:this (this identifier)))))))))
														 )))
								  (unsigned-right-shift
								   function
								   (:arguments ((minus function (:arguments (("2" constant)
																			 (unary-minus function
																						  (:arguments (("3" constant)))))))
												(bitxor function (:arguments (("a" identifier)
																			  ("b" identifier)))))))))))
				 ("java.lang.String.class.getName()" .
				  ("getName" function (:this (class function
													(:arguments ((dot function
																	 (:arguments ((dot function
																					   (:arguments (("java" identifier)
																									("lang" identifier))))
																				  ("String" identifier))))))))))))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse expr))
		  pairs)))


(deftest parse-question parsing-suite
  "Test parsing of ?:"
  (let ((pairs '(("a?b:c" . (question function (:arguments (("a" identifier)
															("b" identifier)
															("c" identifier)))))))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse "bit operation"))
		  pairs)))

(deftest parse-shift parsing-suite
  "Test parsing of shift operators"
  (let ((pairs '(("a << b" . (left-shift function (:arguments (("a" identifier)
														  ("b" identifier)))))
				 ("a >> b" . (right-shift function (:arguments (("a" identifier)
														  ("b" identifier)))))
				 ("a >>> b" . (unsigned-right-shift function
													(:arguments (("a" identifier)
																 ("b" identifier)))))))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse "bit operation"))
		  pairs)))

(deftest parse-bitops parsing-suite
  "Test parsing of bit operators"
  (let ((pairs '(("a & b" . (bitand function (:arguments (("a" identifier)
														  ("b" identifier)))))
				 ("a | b" . (bitor function (:arguments (("a" identifier)
														  ("b" identifier)))))
				 ("a ^ b" . (bitxor function (:arguments (("a" identifier)
														  ("b" identifier)))))))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse "bit operation"))
		  pairs)))

(deftest parse-logicalop parsing-suite
  "Test parsing of bit operators"
  (let ((pairs '(("a && b" . (logand function (:arguments (("a" identifier)
														  ("b" identifier)))))
				 ("a || b" . (logor function (:arguments (("a" identifier)
														  ("b" identifier)))))
				 ;; No logical exclusive or
				 ))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse "bit operation"))
		  pairs)))

(deftest parse-constant parsing-suite
  "Test parsing an expressions with numerical constants"
  (let ((parse-int (jdibug-expr-parse-expr "3"))
		(parse-float (jdibug-expr-parse-expr "3.2f"))
		(parse-double (jdibug-expr-parse-expr "4.3"))
		(parse-exp (jdibug-expr-parse-expr "12.34e56"))
		(parse-boolean-1 (jdibug-expr-parse-expr "true"))
		(parse-boolean-2 (jdibug-expr-parse-expr "false"))
		(parse-string (jdibug-expr-parse-expr "\"a string\""))
		(parse-char (jdibug-expr-parse-expr "'c'"))
		(parse-special (jdibug-expr-parse-expr "'\\n'"))
		(parse-embedded-string (jdibug-expr-parse-expr "\"embedded \\\" string\"")))

	(assert-same-structure-prefix
	 '("3" constant (:type number))
	 parse-int)
	(assert-same-structure-prefix
	 '("3.2f" constant (:type number))
	 parse-float)
	(assert-same-structure-prefix
	 '("4.3" constant (:type number))
	 parse-double)
	(assert-same-structure-prefix
	 '("12.34e56" constant (:type number))
	 parse-exp)
	(assert-same-structure-prefix
	 '("true" constant (:type boolean))
	 parse-boolean-1)
	(assert-same-structure-prefix
	 '("false" constant (:type boolean))
	 parse-boolean-2)
	(assert-same-structure-prefix
	 '("a string" constant (:type string))
	 parse-string)
	(assert-same-structure-prefix
	 '("c" constant (:type char))
	 parse-char)
	(assert-same-structure-prefix
	 '("\\n" constant (:type char))
	 parse-special)
	(assert-same-structure-prefix
	 '("embedded \\\" string" constant (:type string))
	 parse-embedded-string)))

(deftest parse-bad-input parsing-suite
  "Test that parsing of bad input fails"
  (let ((parse-1 (jdibug-expr-parse-expr "foo.bar bad"))
		(parse-2 (jdibug-expr-parse-expr "foo.bar `")))
	(assert-that (stringp parse-1))
	(assert-that (stringp parse-2))))

(deftest parse-dot parsing-suite
  "Test parsing an expression with dots"
  (let ((parse (jdibug-expr-parse-expr "foo.bar.baz ")))
	(jdibug-expr-info "result is %s" parse)
	(assert-same-structure-prefix '(dot function
										(:arguments ((dot function
														  (:arguments (("foo")
																	   ("bar"))
																	  :type "dot"))
													 ("baz"))
													:type "dot"))
								  parse)))

(deftest parse-array-reference parsing-suite
  "Test that parsing a[b] expressions works"
  (let ((parse-constant (jdibug-expr-parse-expr "foo[3]"))
		(parse-variable (jdibug-expr-parse-expr "foo[bar]"))
		(parse-dot-expr (jdibug-expr-parse-expr "foo.quz[bar]")))
	(assert-same-structure-prefix '(array function
										  (:arguments (("foo" identifier)
													   ("3" constant))))
								  parse-constant)
	(assert-same-structure-prefix '(array function
										  (:arguments (("foo" identifier)
													   ("bar" identifier))))
								  parse-variable)
	(assert-same-structure-prefix '(array function
										  (:arguments  ((dot function
															 (:arguments (("foo" identifier)
																		  ("quz" identifier))))
														("bar" identifier))))
								  parse-dot-expr)))


(deftest parse-plus parsing-suite
  "Test parsing an expression with addition"
  (let ((parse (jdibug-expr-parse-expr "foo + bar.baz")))
	(jdibug-expr-info "result is %s" parse)
	(assert-same-structure-prefix '(plus function
										 (:arguments (("foo")
													  (dot function
														   (:arguments (("bar")
																		("baz"))
																	   :type "dot")))
													 :type "additive_expression type"))
								  parse)))

(deftest parse-times parsing-suite
  "Test parsing an expression with multiplication"
  (let ((parse (jdibug-expr-parse-expr " foo * bar.baz")))
	(jdibug-expr-info "result is %s" parse)
	(assert-same-structure-prefix '(mult function
										 (:arguments (("foo" identifier)
													  (dot function
														   (:arguments (("bar" identifier)
																		("baz" identifier))
																	   :type "dot")))
													 :type "multiplicative_expression type"))
								  parse)))

(deftest parse-method-call parsing-suite
  "Test passing of method calls; foo.bar(), a.b.c(d,e.f,g), etc"


  (let ((pairs '(("foo.bar()"  . ("bar" function (:this ("foo" identifier)
														;; No :arguments, since nil values are skipped
														:type)))
				 ("foo.bar(x)"  . ("bar" function (:this ("foo" identifier)
														 :arguments (("x" identifier))
														:type)))
				 ("foo.bar(x, y)"  . ("bar" function (:this ("foo" identifier)
															:arguments (("x" identifier)
																		("y" identifier))
															:type)))))
		expr parse expected)
	(mapc (lambda (pair)
			(setq expr (car pair)
				  expected (cdr pair)
				  parse (jdibug-expr-parse-expr expr))
			(assert-same-structure-prefix expected parse "method call"))
		  pairs)))

(defun assert-same-structure-prefix (expected actual &optional message)
  "Assert that ACTUAL has the same structure as EXPECTED.  Lists
in ACTUAL may have additional members that are ignored."
  (if (not (and expected actual))
	  ;; At least one is nil, so both must be nil
	  (assert-equal expected actual message)
	(if (listp expected)
		(progn
		  (assert-that (listp actual) (format "%s: %s is a list" message actual))
		  (loop for index from 0 below (length expected)
				do (assert-that (> (length actual) index) (format "%s: %s is long enough (%d)" message actual index))
				(assert-same-structure-prefix (nth index expected)
											  (nth index actual)
											  message)))
	  (assert-equal expected actual))))

(message "Loaded parsing.el")