(load "conversions.el")
(add-hook 'elunit-done-running-hook 'elunit-report)

(defun elunit-report (test-count failure-count)
  (switch-to-buffer "*elunit report*")
  (erase-buffer)
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

(elunit "conversions-suite")