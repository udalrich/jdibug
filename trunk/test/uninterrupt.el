;; Tests that the jdwp-uninterruptibly functionality works

;; (add-hook 'after-save-hook
;; 		  (lambda ()
;; 			(eval-current-buffer)
;; 			(elunit "uninterrupt-suite"))
;; 		  nil 'local)
(require 'elunit)
(require 'jdwp)

(defsuite uninterrupt-suite smoke-test-suite
  ;; :setup-hook (lambda () )
  ;; :teardown-hook (lambda () )
)

(deftest interrupt-return-values uninterrupt-suite
  "Test the the return values are as expected"
  (let (result-1 result-2)
	(setq result-1
		  (jdwp-uninterruptibly
			(setq result-2 (jdwp-uninterruptibly 'foo))
			(assert-equal 'jdwp-deferred result-2)
			'bar))
	(assert-equal 'bar result-1)))




(deftest interrupt-scope-complicated-args uninterrupt-suite
  "Try using a complicated argument in backquotes"
  (let ((string ""))
	(jdwp-uninterruptibly
	 (setq string (concat string "123"))
	 (let ((extra '((:u (:foo bar)))))
	   (eval `(jdwp-uninterruptibly
				(setq string (concat string (format "%s" (quote ,extra)))))))
	 (setq string (concat string "789")))

	(assert-equal "123789((:u (:foo bar)))" string)))

(deftest no-interrupt-scope-complicated-args uninterrupt-suite
  "Try using a complicated argument in backquotes when execution will not be delayed."
  (let ((string ""))
	 (setq string (concat string "123"))
	 (let ((extra '((:u (:foo bar)))))
	   (eval `(jdwp-uninterruptibly
				(setq string (concat string (format "%s" (quote ,extra)))))))
	 (setq string (concat string "789"))

	(assert-equal "123((:u (:foo bar)))789" string)))

(deftest interrupt-scope-backquote uninterrupt-suite
  "See what happens when variables go out of scope.
Extra (sadly) becomes undefined and causes an error with normal
invocations.  Backquoting should let us avoid that."
  (let ((string ""))
	(jdwp-uninterruptibly
	 (setq string (concat string "123"))
	 (let ((extra "456"))
	   (eval `(jdwp-uninterruptibly
				(setq string (concat string ,extra)))))
	 (setq string (concat string "789")))

	(assert-equal "123789456" string)))

(deftest interrupt-test uninterrupt-suite
  "Test trying to interrupt uninterruptable code"
  (let ((string ""))
	(jdwp-uninterruptibly
	 (setq string (concat string "123"))
	 (jdwp-uninterruptibly
	  (setq string (concat string "456")))
	 (setq string (concat string "789")))

	(assert-equal "123789456" string)))

(deftest interrupt-scope uninterrupt-suite
  "See what happens when variables go out of scope.
Extra (sadly) becomes undefined and causes an error.  It would be
nice to change that, but I'm not sure how."
  (let ((string ""))
	(assert-error
	(jdwp-uninterruptibly
	 (setq string (concat string "123"))
	 (let ((extra "456"))
	   (jdwp-uninterruptibly
		 (setq string (concat string extra))))
	 (setq string (concat string "789"))))))

(eval-when (eval) (elunit "uninterrupt-suite"))





