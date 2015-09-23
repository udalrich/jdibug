;; Tests that the jdwp-uninterruptibly functionality works

(add-hook 'after-save-hook
	  (lambda ()
	    (load (buffer-file-name))
	    (ert '(tag uninterrupt)))
	  nil 'local)
(require 'ert)
(require 'jdwp)


(ert-deftest interrupt-return-values ()
  "Test the the return values are as expected"
  :tags '(uninterrupt smoke)
  (let (result-1 result-2)
    (setq result-1
	  (jdwp-uninterruptibly
	    (setq result-2 (jdwp-uninterruptibly 'foo))
	    (should (equal 'jdwp-deferred result-2))
	    'bar))
    (should (equal 'bar result-1))))




(ert-deftest interrupt-scope-complicated-args ()
  "Try using a complicated argument in backquotes"
  :tags '(uninterrupt smoke)
  (let ((string ""))
    (jdwp-uninterruptibly
      (setq string (concat string "123"))
      (let ((extra '((:u (:foo bar)))))
	(eval `(jdwp-uninterruptibly
		 (setq string (concat string (format "%s" (quote ,extra)))))))
      (setq string (concat string "789")))
    
    (should (equal "123789((:u (:foo bar)))" string))))

(ert-deftest no-interrupt-scope-complicated-args ()
  "Try using a complicated argument in backquotes when execution will not be delayed."
  :tags '(uninterrupt smoke)
  (let ((string ""))
    (setq string (concat string "123"))
    (let ((extra '((:u (:foo bar)))))
      (eval `(jdwp-uninterruptibly
	       (setq string (concat string (format "%s" (quote ,extra)))))))
    (setq string (concat string "789"))
    
    (should (equal "123((:u (:foo bar)))789" string))))

(ert-deftest interrupt-scope-backquote ()
  "See what happens when variables go out of scope.
Extra (sadly) becomes undefined and causes an error with normal
invocations.  Backquoting should let us avoid that."
  :tags '(uninterrupt smoke)
  (let ((string ""))
    (jdwp-uninterruptibly
      (setq string (concat string "123"))
      (let ((extra "456"))
	(eval `(jdwp-uninterruptibly
		 (setq string (concat string ,extra)))))
      (setq string (concat string "789")))
    
    (should (equal "123789456" string))))

(ert-deftest interrupt-test ()
  "Test trying to interrupt uninterruptable code"
  :tags '(uninterrupt smoke)
  (let ((string ""))
    (jdwp-uninterruptibly
      (setq string (concat string "123"))
      (jdwp-uninterruptibly
	(setq string (concat string "456")))
      (setq string (concat string "789")))

    (should (equal "123789456" string))))

(ert-deftest interrupt-scope ()
  "See what happens when variables go out of scope.
Extra (sadly) becomes undefined and causes an error.  It would be
nice to change that, but I'm not sure how."
  :tags '(uninterrupt smoke)
  (let ((string ""))
    (should-error
     (jdwp-uninterruptibly
       (setq string (concat string "123"))
       (let ((extra "456"))
	 (jdwp-uninterruptibly
	   (setq string (concat string extra))))
       (setq string (concat string "789"))))))

(eval-when (eval) (ert '(tag uninterrupt)))





