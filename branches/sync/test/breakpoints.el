(add-hook 'after-save-hook
		  (lambda ()
			(unless (and (boundp emacs-is-exiting) emacs-is-exiting)
			  (eval-buffer)
			  (elunit "jde-test-suite")))
		  nil 'local)

(require 'elunit)
(require 'jdibug)

(defsuite breakpoints-suite jde-test-suite
  :setup-hooks (list (lambda ()
					   ;; Remove any prexisting breakpoints
					   (jdwp-uninterruptibly
						 (mapc #'jdibug-remove-breakpoint jdibug-breakpoints))))
  ;; :teardown-hooks (lambda () )
)

(deftest conditional-breakpoint breakpoints-suite
  "Test that a conditional breakpoint eventually suspends the program and does so on the correct iteration."

  (jdibug-test-set-breakpoint-and-run "results.add" "index > 3")

  (assert-local-display-value "index" "4"))


(defun jdibug-test-set-breakpoint-and-run (expr &optional cond)
  "Set a breakpoint at the first location of EXPR.  Make it conditionalon COND.
Run until a breakpoint is hit."

  (jdibug-test-connect-to-jvm)

  ;; Set a breakpoint on the line where we define the variable
  (goto-char (point-min))
  (search-forward expr)
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
	   (format "Found %s(%s) in locals buffer (%S)" var-name regexp (buffer-string))))
	(let* ((eol (save-excursion (end-of-line) (point)))
		   (rest-of-line (buffer-substring-no-properties (point) eol)))
	  (assert-match (concat ":.*\\(" value "\\)") rest-of-line
					(format "Correct value in buffer(%S)" (buffer-string))))))

