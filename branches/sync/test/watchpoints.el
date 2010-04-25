;; (add-hook 'after-save-hook
;; 		  (lambda ()
;; 			(eval-current-buffer)
;; 			(elunit "watchpoints-suite"))
;; 		  nil 'local)

(require 'elunit)
(require 'jdibug)

(defsuite watchpoints-suite jde-test-suite
  :setup-hooks (list (lambda ()
					   ;; Remove any prexisting breakpoints
					   (mapc #'jdibug-remove-breakpoint jdibug-breakpoints)))
  ;; :teardown-hooks (lambda () )
)

(deftest add-watchpoint-var watchpoints-suite
  "Test that a watch point on a local variable works"
  (jdibug-test-connect-to-jvm)

  (let ((var-name  "dblVar"))
	(jdibug-add-watchpoint var-name)

	;; Set a breakpoint on the line where we define the variable
	(goto-char (point-min))
	(search-forward var-name)
	(jdibug-toggle-breakpoint)

	;; Start running
	(jdibug-test-resume-and-wait-for-breakpoint)

	;; Check that the watchpoint buffer shows that the variable does not yet exist
	(assert-watchpoint-display-unknown var-name)

	;; Step to the next line, which should cause the display to show the value
	(jdibug-test-step-over-and-wait)

	(assert-watchpoint-display-value value "3.4")))