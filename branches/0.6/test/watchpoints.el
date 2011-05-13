(add-hook 'after-save-hook
		  (lambda ()
			(unless (and (boundp 'emacs-is-exiting)
				     emacs-is-exiting)
			  (eval-buffer)
			  (elunit "jde-test-suite")))
		  nil 'local)

(require 'elunit)
(require 'jdibug)

(defsuite watchpoints-suite jde-test-suite
  :setup-hooks (list (lambda ()
					   ;; Remove any prexisting breakpoints
					   (jdwp-uninterruptibly
						 (mapc #'jdibug-remove-breakpoint (jdibug-all-breakpoints)))
					   (jdibug-remove-all-watchpoints)))
  ;; :teardown-hooks (lambda () )
)

(deftest add-watchpoint-array watchpoints-suite
  "Test that a watch point on a an array reference variable works"

  (let* ((array-name  "floatArray")
		(expr-name (concat array-name "[1]")))
	(watch-expression-and-run-to-first-reference array-name)
	(jdibug-add-watchpoint expr-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)
	(jdibug-test-step-over-and-wait)

	;; TODO: add an epsilon
	(assert-watchpoint-display-value expr-name (regexp-quote "-3.4"))))

(deftest add-watchpoint-var watchpoints-suite
  "Test that a watch point on a local variable works"

  (let ((var-name  "dblVar"))
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does not yet exist
	(assert-watchpoint-display-unknown var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	(jdibug-test-wait-for-refresh-timers)
	;; TODO: add an epsilon
	(assert-watchpoint-display-value var-name (regexp-quote "3.39999"))))

(deftest add-watchpoint-mult watchpoints-suite
  "Test that a watch point on a multiplication works"

  (let ((var-name  "dblVar")
		(expr-name  "dblVar * intVar"))
	(jdibug-add-watchpoint expr-name)
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does not yet exist
	(assert-watchpoint-display-unknown var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	;; TODO: add an epsilon
	(assert-watchpoint-display-value expr-name "10.19999")))

(deftest add-watchpoint-plus watchpoints-suite
  "Test that a watch point on a multiplication works"

  (let ((var-name  "dblVar")
		(expr-name  "dblVar + intVar"))
	(jdibug-add-watchpoint expr-name)
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does not yet exist
	(assert-watchpoint-display-unknown var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	;; TODO: add an epsilon
	(assert-watchpoint-display-value expr-name "6\\.4")))

(deftest add-watchpoint-minus watchpoints-suite
  "Test that a watch point on a multiplication works"

  (let ((var-name  "dblVar")
		(expr-name  "dblVar - intVar"))
	(jdibug-add-watchpoint expr-name)
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does not yet exist
	(assert-watchpoint-display-unknown var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	;; TODO: add an epsilon
	(assert-watchpoint-display-value expr-name "0\\.399999")))

(deftest add-watchpoint-divide watchpoints-suite
  "Test that a watch point on division works"

  (let* ((var-name  "dblVar")
		 (dbl-div-int  '("dblVar / intVar" . "1\\.13333"))
		 (dbl-div-dbl  '("dblVar / f" . "2\\.83333"))
		 (int-div-dbl  '("intVar / f" . "2\\.5\\|2.49999"))
		 (int-div-int  '("intVar / twoAsInt". "1$"))
		 (expr-list (list dbl-div-dbl dbl-div-int int-div-int int-div-dbl)))
	(mapc (lambda (expr-cons)
			(jdibug-add-watchpoint (car expr-cons)))
		  expr-list)
	(watch-expression-and-run-to-first-reference var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	;; TODO: add an epsilon
	(mapc (lambda (pair)
			(let ((expr-name (car pair))
				  (expected-value (cdr pair)))
			  (assert-watchpoint-display-value expr-name expected-value)))
		  expr-list)))

(deftest add-watchpoint-compare watchpoints-suite
  "Test that a watch point on numerical comparisons works"

  (let* ((var-name  "intVar")
		 (less-than  '("intVar < twoAsInt" . "false"))
		 (less-equal-1  '("twoAsInt <= intVar" . "true"))
		 (less-equal-2  '("twoAsInt <= 2" . "true"))
		 (greater-than  '("intVar > twoAsInt" . "true"))
		 (greater-equal-1  '("intVar >= twoAsInt". "true"))
		 (greater-equal-2  '("intVar >= 3". "true"))
		 (equal-to  '("intVar == twoAsInt". "false"))
		 (not-equal-to  '("intVar != f". "true"))
		 (expr-list (list less-than less-equal-1 less-equal-2
						  greater-than greater-equal-1 greater-equal-2
						  equal-to not-equal-to)))
	(mapc (lambda (expr-cons)
			(jdibug-add-watchpoint (car expr-cons)))
		  expr-list)
	(watch-expression-and-run-to-first-reference var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	(mapc (lambda (pair)
			(let ((expr-name (car pair))
				  (expected-value (cdr pair)))
			  (assert-watchpoint-display-value expr-name expected-value)))
		  expr-list)))

(deftest add-watchpoint-dot watchpoints-suite
  "Test that a watch point on a dot expression works"
  (let ((var-name  "stuff.x"))
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does exist.  Value has not yet been set.
	(assert-watchpoint-display-value var-name "0")

	;; Step over, which should set the value
	(jdibug-test-step-over-and-wait)
	(jdibug-test-wait-for-refresh-timers)
	(assert-watchpoint-display-value var-name "7")))

(deftest add-watchpoint-method watchpoints-suite
  "Test that a watch point on a dot expression works"
  (let ((var-name  "stuff.x")
		(expr "stuff.toString()"))
	(jdibug-add-watchpoint "stuff.toString()")
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows the string value
	(assert-watchpoint-display-value expr "\"com.jdibug.Main$Stuff@")))


(defun watch-expression-and-run-to-first-reference (var-name)
  (jdibug-test-connect-to-jvm)

  (jdibug-add-watchpoint var-name)

  ;; Set a breakpoint on the line where we define the variable
  (goto-char (point-min))
  (search-forward var-name)
  (jdibug-toggle-breakpoint)

  ;; Start running
  (jdibug-test-resume-and-wait-for-breakpoint)
  (jdibug-test-wait-for-refresh-timers)

  (jdibug-expr-debug "watch-expression-and-run-to-first-reference %s finished" var-name))


(defun assert-watchpoint-display-unknown (var-name)
  "Assert that VAR-NAME is listed in the watchpoint display window, but that it's value is unknown."

  (save-excursion
	(set-buffer jdibug-watchpoints-buffer)
	(goto-char (point-min))
	(assert-that
	 (search-forward-regexp (concat "^" (regexp-quote var-name)) nil t)
	 (format "Found %s in watchpoint buffer (%S)" var-name (buffer-string)))
	(let* ((eol (save-excursion (end-of-line) (point)))
		   (rest-of-line (buffer-substring-no-properties (point) eol)))
	  (assert-match "is not in scope" rest-of-line))))

(defun assert-watchpoint-display-value (var-name value)
  "Assert that VAR-NAME is listed in the watchpoint display window with a value of VALUE."

  (save-excursion
	(set-buffer jdibug-watchpoints-buffer)
	(goto-char (point-min))
	(assert-that
	 (search-forward-regexp (concat "^" (regexp-quote var-name)) nil t)
	 (format "Found %s in watchpoint buffer (%S)" var-name (buffer-string)))
	(let* ((eol (save-excursion (end-of-line) (point)))
		   (rest-of-line (buffer-substring-no-properties (point) eol)))
	  (assert-match (concat ":.*\\(" value "\\)") rest-of-line
					(format "Correct value in buffer(%S)" (buffer-string))))))

