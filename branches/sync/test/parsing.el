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
		(parse-variable (jdibug-expr-parse-expr "foo[bar]")))
	(assert-same-structure-prefix '(array function
										  (:arguments (("foo" identifier)
													   ("3" constant))))
								  parse-constant)
	(assert-same-structure-prefix '(array function
										  (:arguments (("foo" identifier)
													   ("bar" identifier))))
								  parse-variable)))


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
										 (:arguments (("foo")
													  (dot function
														   (:arguments (("bar")
																		("baz"))
																	   :type "dot")))
													 :type "multiplicative_expression type"))
								  parse)))


(defun assert-same-structure-prefix (expected actual)
  "Assert that ACTUAL has the same structure as EXPECTED.  Lists
in ACTUAL may have additional members that are ignored."
  (if (not (and expected actual))
	  ;; At least one is nil, so both must be nil
	  (assert-equal expected actual)
	(if (listp expected)
		(progn
		  (assert-that (listp actual))
		  (loop for index from 0 below (length expected)
				do (assert-that (> (length actual) index))
				(assert-same-structure-prefix (nth index expected)
											  (nth index actual))))
	  (assert-equal expected actual))))

(message "Loaded parsing.el")