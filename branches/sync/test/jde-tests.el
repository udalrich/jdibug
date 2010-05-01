;;
;; These tests run JDE and JDI together, for a system level integration test suite.  These tests assume that the code in test/java has been built.
;;
(require 'elunit)
(require 'jde)
(require 'jdibug)

(defvar jdibug-jde-test-Main-buffer nil
  "Buffer with the source for the Main.java for testing")

(defsuite jde-test-suite nil
  :setup-hooks (list (lambda ()
				;; TODO figure out if there is already a process running and kill it
				(setq jdibug-jde-test-Main-buffer
					  (find-file (expand-file-name "java/src/com/jdibug/Main.java")))))
  :teardown-hooks (list (function jdibug-exit-jvm))
  )

(defvar jdibug-test-breakpoint-hit nil)
(defun jdibug-test-resume-and-wait-for-breakpoint ()
  "Resume all threads, and then wait until a breakpoint is hit."
  ;; Add a hook to remember when we hit a breakpoint
  (add-hook jdibug-breakpoint-hit-hook #'jdibug-test-breakpoint-hit-hook)
  (setq jdibug-test-breakpoint-hit nil)

  ;; Resume everything
  (jdibug-resume-all)

  ;; Wait for the flag to be set
  (let* ((interval 0.1) (max-count (/ 30 interval)))
	(while (and (not jdibug-test-breakpoint-hit) (< count max-count))
	  (setq count (1+ count))
	  (sleep interval)))

  (assert-that jdibug-test-breakpoint-hit))

(defun jdibug-test-breakpoint-hit-hook (&rest ignore)
  (setq jdibug-test-breakpoint-hit t)
  (remove-hook jdibug-breakpoint-hit-hook #'jdibug-test-breakpoint-hit-hook))

(defvar jdibug-test-step-hit nil)
(defun jdibug-test-step-over-and-wait ()
  "Step over the current thread, and then wait until the step is finished."
  ;; Add a hook to remember when we hit a breakpoint
  (add-hook jdi-step-hooks #'jdibug-test-step-hit-hook)
  (setq jdibug-test-step-hit nil)

  ;; Step
  (jdibug-step-over)

  ;; Wait for the flag to be set
  (let* ((interval 0.1) (max-count (/ 30 interval)))
	(while (and (not jdibug-test-step-hit) (< count max-count))
	  (setq count (1+ count))
	  (sleep interval)))

  (assert-that jdibug-test-step-hit))

(defun jdibug-test-step-hit-hook (&rest ignore)
  (setq jdibug-test-step-hit t)
  (remove-hook jdi-step-hooks #'jdibug-test-step-hit-hook))

(defun jdibug-test-connect-to-jvm (&optional buffer)
  "Start a JVM and attach to it with JDIbug from within BUFFER.
This assumes that the buffer has JDE variables set so that
`jde-run' will start the JVM correctly and allow us to attach to
it.  If BUFFER is not specified, `jdibug-jde-test-Main-buffer' is
used"
  (interactive)

  ;; Evaluate optional arguements
  (or buffer (setq buffer jdibug-jde-test-Main-buffer))

  (switch-to-buffer buffer)

  ;; TODO: there is probably a more general way to do this
  (let ((main-class (or (and (not (eq "" jde-run-application-class))
							 jde-run-application-class)
						(jde-run-get-main-class)))
		(count 0)
		buffer started)
	(save-excursion
	  ;; Start the JVM
	  (jde-run 1)
	  ;; Wait for it to start
	  (save-excursion
		(while (and (< count 10) (not started))
		  (sleep-for 1)
		  (setq buffer (get-buffer (concat "*" main-class "*"))
				count (1+ count))
		  (when buffer
			(set-buffer buffer)
			(goto-char point-min)
			(setq started (search-forward "Listening for transport dt_socket at address: "))))
		(assert-that started))
	  ;; Start jdibug
	  (jdibug-connect)
	  ;; Check that we actually connected
	  (assert-equal (length jdibug-virtual-machines)
					(length jdibug-connect-host))
	  )))


(load "watchpoints.el")

(elunit "jde-test-suite")
