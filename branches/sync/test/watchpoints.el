(add-hook 'after-save-hook
		  (lambda ()
			(eval-current-buffer)
         (elunit "jde-test-suite"))
		  nil 'local)

(require 'elunit)
(require 'jdibug)

(defsuite watchpoints-suite jde-test-suite
  :setup-hooks (list (lambda ()
					   ;; Remove any prexisting breakpoints
					   (jdwp-uninterruptibly
						 (mapc #'jdibug-remove-breakpoint jdibug-breakpoints))
					   (jdibug-remove-all-watchpoints)))
  ;; :teardown-hooks (lambda () )
)

(deftest add-watchpoint-var watchpoints-suite
  "Test that a watch point on a local variable works"

  (let ((var-name  "dblVar"))
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does not yet exist
	(assert-watchpoint-display-unknown var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	(assert-watchpoint-display-value var-name "3.4")))

(deftest add-watchpoint-dot watchpoints-suite
  "Test that a watch point on a dot expression works"
  (let ((var-name  "stuff.x"))
	(watch-expression-and-run-to-first-reference var-name)

	;; Check that the watchpoint buffer shows that the variable does exist.  Value has not yet been set.
	(assert-watchpoint-display-value var-name "0")

	;; Step over, which should set the value
	(jdibug-test-step-over-and-wait)
	(assert-watchpoint-display-value var-name "7")))

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
	  (assert-match (concat ":.*" value) rest-of-line (format "Correct value in buffer(%S)" (buffer-string))))))

