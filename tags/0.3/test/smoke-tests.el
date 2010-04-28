
(require 'elunit)

(defsuite smoke-test-suite nil)

(load "conversions.el")
(load "uninterrupt.el")

(defun elunit-report (test-count failure-count)
  (switch-to-buffer "*elunit report*")
  (goto-char (point-max))
  (insert (format "Suite: %s\n" elunit-default-suite))
  (insert (format "Total tests run: %d   Total failures: %d"
				  test-count failure-count))
  (newline)
  (mapc (lambda (test)
		  (insert (format "  Test %20s %20s:%-5d \n"
						  (test-name test)
						  (test-file test)
						  (test-line test)))
		  (insert (format "     %s\n" (test-message test)))
		  (insert (format "     %s\n" (test-problem test))))
		  elunit-failures))
(when (interactive-p)
  (add-hook 'elunit-done-running-hook 'elunit-report))

(elunit "smoke-test-suite")
