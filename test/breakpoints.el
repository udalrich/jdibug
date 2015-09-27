(add-hook 'after-save-hook
	  (lambda ()
	    (unless (and (boundp 'emacs-is-exiting) emacs-is-exiting)
	      (load (buffer-file-name))
	      (ert '(tag breakpoints))))
	  nil 'local)

(require 'ert)
(require 'jdibug)

(defmacro with-breakpoints-test (&rest body)
  (declare (debug 1) (indent 0))
  ;; Remove any prexisting breakpoints
  `(with-jde-test
    (jdwp-uninterruptibly
      (mapc #'jdibug-remove-breakpoint (jdibug-all-breakpoints)))
    ,@body))


(ert-deftest caught-exception ()
  "Test that an caught exception breakpoint works."
  :tags '(breakpoints jde)

(with-breakpoints-test
  (let ((jdee-run-option-application-args (list "exceptions")))
    (jdibug-break-on-exception "com.jdibug.JdibugException" t nil)

    ;; Caught exception should cause us to hit a breakpoint
    (jdibug-test-connect-to-jvm)
    (jdibug-test-resume-and-wait-for-breakpoint)
    (jdibug-test-wait-for-refresh-timers)

    (assert-frames-display-value "throwAndCatch"))))


(ert-deftest exception-by-type ()
  "Test that catching exception by type works"
  :tags '(breakpoints jde)

(with-breakpoints-test
  (let ((jdee-run-option-application-args (list "exceptions")))
    (jdibug-break-on-exception "com.jdibug.JdibugFooException" t t)

    ;;  Exception should cause us to hit a breakpoint
    (jdibug-test-connect-to-jvm)
    (jdibug-test-resume-and-wait-for-breakpoint)
    (jdibug-test-wait-for-refresh-timers)

    (assert-frames-display-value "throwWithoutCatch"))))



(ert-deftest uncaught-exception ()
  "Test that an uncaught exception breakpoint works."
  :tags '(breakpoints jde)

  (with-breakpoints-test
  (jdibug-test-info "Running uncaught-exception")

  ;; As warned in the documentation, the compiler appears to insert a
  ;; catch block here, so this test fails because the exception
  ;; appears to be caught.  Nothing we can do about that.
  (jdibug-test-warn "Not running uncaught-exception test because compiler inserts a catch block")
  (when nil
    (let ((jdee-run-option-application-args (list "exceptions")))
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
	  (should found)))))))



(ert-deftest conditional-breakpoint ()
  "Test that a conditional breakpoint eventually suspends the program and does so on the correct iteration."
  :tags '(breakpoints jde)

  (with-breakpoints-test
   ;;(debug)
   (jdibug-test-set-breakpoint-and-run "tasks.add" "index > 3")

   (assert-local-display-value "index" "4")))

(ert-deftest nested-class-breakpoint ()
  "Test that breakpoints in nested classes work when set before
inner or outer class is loaded."
  :tags '(breakpoints jde)

(with-breakpoints-test  (jdibug-test-info "Running nested-class-breakpoint");

  (jdibug-test-set-breakpoint-and-run "doStuff();")

  ;; Should be at an anonymous inner class
  (assert-frames-display-value "Main\\$[0-9]+\\.run")))

(ert-deftest nested-class-breakpoint-after-load ()
  "Test that breakpoints in nested classes work when the breakpoint is set
after the class is loaded."
  :tags '(breakpoints jde)

  (with-breakpoints-test
  (jdibug-test-set-breakpoint-and-run "service.submit")

  (jdibug-test-set-breakpoint-and-run "doStuff();" nil 'no-connect)

  ;; Should be at an anonymous inner class
  (assert-frames-display-value "Main\\$[0-9]+\\.run")))

(ert-deftest nested-class-breakpoint-after-load-of-outer ()
  "Test that breakpoints in nested classes work when the outer
class is loaded but not the inner class"
  :tags '(breakpoints jde)

  (with-breakpoints-test
  (jdibug-test-info "Running nested-class-breakpoint-after-load-of-outer")

  (jdibug-test-set-breakpoint-and-run "twoAsInt")

  (jdibug-test-set-breakpoint-and-run "doStuff();" nil 'no-connect)

  ;; Should be at an anonymous inner class
  (assert-frames-display-value "Main\\$[0-9]+\\.run")))

(ert-deftest step-into-java ()
    "Test that stepping into a java.* class automatically steps out"
  :tags '(breakpoints jde)
  (with-breakpoints-test
    (jdibug-test-set-breakpoint-and-run "Created stuff")
    (jdibug-test-step-and-wait 'into)
    (assert-frames-display-value "println" 'missing)))


(defun jdibug-test-set-breakpoint-and-run (expr &optional cond no-connect)
  "Set a breakpoint at the first location of EXPR.  Make it conditional on COND.
Run until a breakpoint is hit. Do not connect to jvm if NO-CONNECT."

  (unless no-connect (jdibug-test-connect-to-jvm))

  ;; Set a breakpoint on the line where we define the variable
  (goto-char (point-min))
  (should (search-forward expr))
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
      (should
       (search-forward-regexp regexp nil t)))
    (let* ((eol (save-excursion (end-of-line) (point)))
	   (rest-of-line (buffer-substring-no-properties (point) eol)))
      (should (string-match (concat ":.*\\(" value "\\)") rest-of-line)))))

(defun assert-frames-display-value (regexp &optional missing)
  "Assert that REGEXP is displayed in the frames window."

  (save-excursion
    (set-buffer jdibug-frames-buffer)
    (goto-char (point-min))
    (if missing
        (should-not (search-forward-regexp regexp nil t))
      (should (search-forward-regexp regexp nil t)))))


