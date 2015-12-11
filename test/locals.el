(add-hook 'after-save-hook
	  (lambda ()
	    (unless (and (boundp 'emacs-is-exiting) emacs-is-exiting)
	      (load (buffer-file-name)
		    (ert '(tag locals)))))
	  nil 'local)

(require 'ert)
(require 'jdibug)

(defmacro with-locals-test (&rest inner-body)
  (declare (debug t))
  `(with-jde-test
	   ;; Remove any prexisting breakpoints
	   (jdwp-uninterruptibly
	     (mapc #'jdibug-remove-breakpoint (jdibug-all-breakpoints)))
	   ,@inner-body))

(ert-deftest local-function-call ()
  "Test that calling the no-arg methods on a local variable works"
  :tags '(jde locals)

  (with-locals-test
   ;; Connect and set breakpoint after we initialize stuff
   (jdibug-test-connect-to-jvm)

   (goto-char (point-min))
   (search-forward "twoAsInt")
   (jdibug-toggle-breakpoint)

   ;; Start running
   (setq jdwp-signal-count 0)
   ;;   (debug-on-entry 'jdwp-signal-hook)
   ;;   (debug-on-error t)

   (jdibug-test-resume-and-wait-for-breakpoint)
   (jdibug-test-wait-for-refresh-timers)

   ;; Emulate the appropiate clicks
   (save-excursion
     (set-buffer jdibug-locals-buffer)
     ;; Click to expand stuff
     (goto-char (point-min))
     (jdibug-test-info "About to search for stuff in '%s'"
		       (buffer-substring-no-properties (point-min) (point-max)))
     (search-forward "stuff:")
     (search-backward "[+]")
     (widget-button-press (point))

     ;;Click to expand methods
     (search-forward "methods")
     (search-backward "[+]")
     (widget-button-press (point))

     ;;Click to expand toString
     (search-forward "toString:")
     (search-backward "[+]")
     (widget-button-press (point))

     ;; Check that we are looking at the correct value
     (forward-line 1)
     (assert-rest-of-line-matches
      "result: \"com.jdibug.Main$Stuff@[0-9a-f]+\"$"))))



(ert-deftest local-max-min-int ()
  "Test that min/max int/long displays correctly"
  :tags '(jde locals)

  (with-locals-test
   ;; Connect and set breakpoint after we initialize stuff
   (jdibug-test-connect-to-jvm)

   (goto-char (point-min))
   (search-forward "intVar")
   (jdibug-toggle-breakpoint)

   ;; Start running
   (setq jdwp-signal-count 0)

   (jdibug-test-resume-and-wait-for-breakpoint)
   (jdibug-test-wait-for-refresh-timers)

   ;; Emulate the appropriate clicks
   (save-excursion
     (set-buffer jdibug-locals-buffer)

     (goto-char (point-min))
     (search-forward "maxInt")
     (assert-rest-of-line-matches ": 2147483647$")

     (goto-char (point-min))
     (search-forward "minInt")
     (assert-rest-of-line-matches ": -2147483648$")

     (goto-char (point-min))
     (search-forward "maxLong")
     (assert-rest-of-line-matches ": 9223372036854775807$")

     (goto-char (point-min))
     (search-forward "minLong")
     (assert-rest-of-line-matches ": -9223372036854775808$"))))


(ert-deftest local-filter ()
  "Test filtering of variables in the locals buffer"
  :tags '(jde locals)
  (with-locals-test
   (jdibug-test-connect-to-jvm)

   (goto-char (point-min))
   (search-forward "main.submitJobs")
   (jdibug-toggle-breakpoint)

   ;; Run it
   (setq jdwp-signal-count 0)
   (jdibug-test-resume-and-wait-for-breakpoint)
   (jdibug-test-wait-for-refresh-timers)

   ;; Expand the Main object.  All the fields should be displayed
   (jdibug-test-check-filtered-fields
    nil
    '("value" "staticValue" "finalValue" "staticFinalValue")
    nil)

   (jdibug-test-check-filtered-fields
    '(jdi-field-static-p)
    '("value" "finalValue")
    '("staticValue" "staticFinalValue")
    nil)

   (jdibug-test-check-filtered-fields
    '(jdi-field-final-p)
    '("value" "staticValue")
    '("finalValue" "staticFinalValue")

   (jdibug-test-check-filtered-fields
    '(jdi-field-final-p jdi-field-static-p)
    '("value")
    '("staticValue" "finalValue" "staticFinalValue")
    )

   )))


(defun jdibug-test-check-filtered-fields (filters variables-present variables-missing)
  (let ((jdibug-locals-buffer-field-filters filters))
    ;; Step once, so the buffer is redrawn
    (jdibug-test-step-and-wait);

    (with-current-buffer jdibug-locals-buffer
       (goto-char (point-min))
       (search-forward "main: Main")
       (search-backward "[+]")
       (widget-button-press (point))

       (let ((limit (save-excursion (search-forward "maxInt"))))
         (should limit)
         (dolist (var variables-present)
           (save-excursion
             (should (search-forward-regexp (concat "\\b" var "\\b") limit))))
         (dolist (var variables-missing)
           (save-excursion
             (should-not (search-forward-regexp (concat "\\b" var "\\b") limit))))))))




(defun assert-rest-of-line-matches (regexp)
  "Assert that the line, starting at point, matches REGEXP"
  (let* ((eol (save-excursion (end-of-line) (point)))
	 (rest-of-line (buffer-substring-no-properties (point) eol)))
    (should (string-match regexp rest-of-line))))
