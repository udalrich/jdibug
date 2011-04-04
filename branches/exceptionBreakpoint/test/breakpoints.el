(add-hook 'after-save-hook
		  (lambda ()
			(unless (and (boundp 'emacs-is-exiting) emacs-is-exiting)
			  (eval-buffer)
			  (elunit "jde-test-suite")))
		  nil 'local)

(require 'elunit)
(require 'jdibug)

(defsuite breakpoints-suite jde-test-suite
  :setup-hooks (list (lambda ()
					   ;; Remove any prexisting breakpoints
					   (jdwp-uninterruptibly
						  (mapc #'jdibug-remove-breakpoint (jdibug-all-breakpoints)))))
  ;; :teardown-hooks (lambda () )
)

(deftest caught-exception breakpoints-suite
  "Test that an caught exception breakpoint works."

  (let ((jde-run-option-application-args (list "exceptions")))
	 (jdibug-break-on-exception "com.jdibug.JdibugException" t nil)

	 ;; Caught exception should cause us to hit a breakpoint
	 (jdibug-test-connect-to-jvm)
	 (jdibug-test-resume-and-wait-for-breakpoint)
	 (jdibug-test-wait-for-refresh-timers)

	 (assert-frames-display-value "throwAndCatch")))

(deftest exception-by-type breakpoints-suite
  "Test that catching exception by type works"

  (let ((jde-run-option-application-args (list "exceptions")))
	 (jdibug-break-on-exception "com.jdibug.JdibugFooException" t t)

	 ;;  Exception should cause us to hit a breakpoint
	 (jdibug-test-connect-to-jvm)
	 (jdibug-test-resume-and-wait-for-breakpoint)
	 (jdibug-test-wait-for-refresh-timers)

	 (assert-frames-display-value "throwWithoutCatch")))



(deftest uncaught-exception breakpoints-suite
  "Test that an uncaught exception breakpoint works."

  (jdibug-test-info "Running uncaught-exception")

  ;; As warned in the documentation, the compiler appears to insert a
  ;; catch block here, so this test fails because the exception
  ;; appears to be caught.  Nothing we can do about that.
  (jdibug-test-warn "Not running uncaught-exception test because compiler inserts a catch block")
  (when nil
  (let ((jde-run-option-application-args (list "exceptions")))
	 (jdibug-break-on-exception "com.jdibug.JdibugException" nil t)

	 ;; Caught exception should cause us to hit a breakpoint
	 (jdibug-test-connect-to-jvm)
	 (jdibug-test-resume-and-wait-for-breakpoint)
	 (jdibug-test-wait-for-refresh-timers)

	 (assert-frames-display-value "throwAndCatch")

	 ;; Uncaught exception should not trigger breakpoint.  There's no
	 ;; obvious event to wait for, so we'll just pause a short time
	 (jdibug-resume-all)
	 (sleep-for 2)
	 (with-current-buffer (jdibug-test-main-buffer-name)
		(goto-char (point-min))
		(let* ((expr (concat "Exception in thread \"main\" "
									"com.jdibug.JdibugFooException: uncaught"))
				 (found (search-forward expr nil 'no-error)))
		  (jdibug-test-info "Run buffer contents: %s"
								  (buffer-substring-no-properties (point-min)
																			 (point-max)))
		  (with-current-buffer jdibug-frames-buffer
			 (jdibug-test-info "Frames buffer contents: %s"
									 (buffer-substring-no-properties (point-min)
																				(point-max))))
		  (assert-that found "Uncaught exception message found"))))))



(deftest conditional-breakpoint breakpoints-suite
  "Test that a conditional breakpoint eventually suspends the program and does so on the correct iteration."

;;(debug)
  (jdibug-test-set-breakpoint-and-run "tasks.add" "index > 3")

  (assert-local-display-value "index" "4"))

(deftest nested-class-breakpoint breakpoints-suite
  "Test that breakpoints in nested classes work when set before
inner or outer class is loaded."
  (jdibug-test-info "Running nested-class-breakpoint");

  (jdibug-test-set-breakpoint-and-run "doStuff();")

  ;; Should be at an anonymous inner class
  (assert-frames-display-value "Main\\$[0-9]+\\.run"))

(deftest nested-class-breakpoint-after-load breakpoints-suite
  "Test that breakpoints in nested classes work when the breakpoint is set
after the class is loaded."

  (jdibug-test-set-breakpoint-and-run "service.submit")

  (jdibug-test-set-breakpoint-and-run "doStuff();" nil 'no-connect)

  ;; Should be at an anonymous inner class
  (assert-frames-display-value "Main\\$[0-9]+\\.run"))

(deftest nested-class-breakpoint-after-load-of-outer breakpoints-suite
  "Test that breakpoints in nested classes work when the outer
class is loaded but not the inner class"

  (jdibug-test-set-breakpoint-and-run "twoAsInt")

  (jdibug-test-set-breakpoint-and-run "doStuff();" nil 'no-connect)

  ;; Should be at an anonymous inner class
  (assert-frames-display-value "Main\\$[0-9]+\\.run"))

(defun jdibug-test-set-breakpoint-and-run (expr &optional cond no-connect)
  "Set a breakpoint at the first location of EXPR.  Make it conditional on COND.
Run until a breakpoint is hit. Do not connect to jvm if NO-CONNECT."

  (unless no-connect (jdibug-test-connect-to-jvm))

  ;; Set a breakpoint on the line where we define the variable
  (goto-char (point-min))
  (assert-that (search-forward expr)
			   (format "Found '%s' to set breakpoint" expr))
  (jdibug-toggle-breakpoint)
  (when cond
	(let* ((line-number (line-number-at-pos))
		   (regexp (concat "\\b" (number-to-string line-number) "\\b")))
	  (save-excursion
		(set-buffer jdibug-breakpoints-buffer)
		(goto-char (point-min))
		(search-forward-regexp regexp)
		(jdibug-breakpoints-add-condition cond))))

  ;; Start running
  (jdibug-test-resume-and-wait-for-breakpoint)
  (jdibug-test-wait-for-refresh-timers))



(defun assert-local-display-value (var-name value)
  "Assert that VAR-NAME is listed in the local variables window with a value of VALUE."

  (save-excursion
	(set-buffer jdibug-locals-buffer)
	(goto-char (point-min))
	(let ((regexp (concat (regexp-quote "|-  ") (regexp-quote var-name))))
	  (assert-that
	   (search-forward-regexp regexp nil t)
	   (format "Found %s(%s) in locals buffer (%S)"
				  var-name regexp
				  (buffer-substring-no-properties (point-min) (point-max)))))
	(let* ((eol (save-excursion (end-of-line) (point)))
		   (rest-of-line (buffer-substring-no-properties (point) eol)))
	  (assert-match (concat ":.*\\(" value "\\)") rest-of-line
					(format "Correct value in buffer(%S)" (buffer-substring-no-properties (point-min) (point-max)))))))

(defun assert-frames-display-value (regexp)
  "Assert that REGEXP is displayed in the frames window."

  (save-excursion
	(set-buffer jdibug-frames-buffer)
	(goto-char (point-min))
	(assert-that
	 (search-forward-regexp regexp nil t)
	   (format "Found %s in frames buffer (%S)" regexp
				  (buffer-substring-no-properties (point-min) (point-max))))))

