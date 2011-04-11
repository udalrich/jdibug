(add-hook 'after-save-hook
		  (lambda ()
			(unless (and (boundp 'emacs-is-exiting) emacs-is-exiting)
			  (eval-buffer)
			  (elunit "jde-test-suite")))
		  nil 'local)

(require 'elunit)
(require 'jdibug)

(defsuite locals-suite jde-test-suite
  :setup-hooks (list (lambda ()
					   ;; Remove any prexisting breakpoints
					   (jdwp-uninterruptibly
						 (mapc #'jdibug-remove-breakpoint (jdibug-all-breakpoints)))))

  ;; :teardown-hooks (lambda () )
)


(deftest local-function-call locals-suite
  "Test that calling the no-arg methods on a local variable works"

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
	(jdibug-test-info "About to search for stuff in '%s'b"
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
	(let* ((eol (save-excursion (end-of-line) (point)))
		   (rest-of-line (buffer-substring-no-properties (point) eol)))
	  (assert-match (concat "result: \"com.jdibug.Main$Stuff@[0-9a-f]+\"$") rest-of-line
					(format "Correct value in buffer(%S)" (buffer-string))))))


