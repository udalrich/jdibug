;
;; These tests run JDE and JDI together, for a system level
;; integration test suite.  These tests assume that the code in
;; test/java has been built.
;;
(require 'elunit)
(require 'jde)
(require 'jdibug)
(require 'elog)

(elog-make-logger jdibug-test)

(defvar jdibug-jde-test-Main-buffer nil
  "Buffer with the source for the Main.java for testing")

(defconst jdibug-test-root-dir
  (expand-file-name (concat
					 (or (symbol-file 'jdibug-jde-test-Main-buffer)
						 load-file-name
						 (buffer-file-name))
					 "/..")))
(message "jdibug-test-root-dir: %s" jdibug-test-root-dir)

(defvar jdibug-test-old-refresh-delay nil
  "Saved valud of the refresh delay")

(defsuite jde-test-suite nil
  :setup-hooks
  (list (lambda ()
			 ;; Shorten the refresh delay so tests can run faster
			 (unless jdibug-test-old-refresh-delay
				(setq jdibug-test-old-refresh-delay jdibug-refresh-delay
						jdibug-refresh-delay 0.1))
			 (let ((process (get-buffer-process
								  (jdibug-test-main-buffer-name))))
				(when process
				  (kill-process process)))
			 (setq jdibug-jde-test-Main-buffer
					 (find-file (expand-file-name
									 "java/src/com/jdibug/Main.java"
									 jdibug-test-root-dir)))))
  :teardown-hooks (list (lambda()
						  (jdibug-test-info "jde-test-suite teardown")
						  (setq jdibug-refresh-delay jdibug-test-old-refresh-delay
								jdibug-test-old-refresh-delay nil)
						  (jdibug-exit-jvm)
						  (jdwp-traffic-info "Test Finished")
						  ))
  )

(defvar jdibug-test-breakpoint-hit nil)
(defun jdibug-test-resume-and-wait-for-breakpoint ()
  "Resume all threads, and then wait until a breakpoint is hit."
  ;; Add a hook to remember when we hit a breakpoint
  (add-hook 'jdibug-breakpoint-hit-hook #'jdibug-test-breakpoint-hit-hook)
  (setq jdibug-test-breakpoint-hit nil)

  ;; Resume everything
  (jdibug-resume-all)

  ;; Wait for the flag to be set
  (let* ((interval 0.1)
		 (max-count (/ 10 interval))
		 (count 0))
	(while (and (not jdibug-test-breakpoint-hit) (< count max-count))
	  (setq count (1+ count))
	  (sleep-for interval)))

  (assert-that jdibug-test-breakpoint-hit "breakpoint hit"))

(defun jdibug-test-breakpoint-hit-hook (&rest ignore)
  (setq jdibug-test-breakpoint-hit t)
  (remove-hook 'jdibug-breakpoint-hit-hook #'jdibug-test-breakpoint-hit-hook))

(defvar jdibug-test-step-hit nil)
(defun jdibug-test-step-over-and-wait ()
  "Step over the current thread, and then wait until the step is
finished.  Also wait for all of the buffers to finish updating."
  ;; Add a hook to remember when we hit a breakpoint
  (add-hook 'jdi-step-hooks #'jdibug-test-step-hit-hook)
  (setq jdibug-test-step-hit nil)

  ;; Step
  (jdibug-step-over)

  ;; Wait for the flag to be set
  (jdibug-test-info "Waiting for step to finish")
  (let* ((interval 0.1) (max-count (/ 30 interval)) (count 0))
	(while (and (not jdibug-test-step-hit) (< count max-count))
	  (setq count (1+ count))
	  (sleep-for interval)))

  (jdibug-test-info "Step finished")
  (assert-that jdibug-test-step-hit "step hit")

  (jdibug-test-wait-for-refresh-timers))

(defun jdibug-test-step-hit-hook (&rest ignore)
  (setq jdibug-test-step-hit t)
  (remove-hook 'jdi-step-hooks #'jdibug-test-step-hit-hook))

(defvar jdibug-test-connected nil)
(defun jdibug-test-connected-hook (&rest ignore)
  (jdibug-test-info "connected: %s" ignore)
  (setq jdibug-test-connected t)
  (remove-hook 'jdibug-connected-hook #'jdibug-test-connected-hook))

(defun jdibug-test-main-buffer-name nil
  "Get the name of the buffer where the main process is running"
  (let ((main-class (or (and (not (string-equal "" jde-run-application-class))
									  jde-run-application-class)
								(jde-run-get-main-class))))
	 (concat "*" main-class "*")))

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
  (let* ((count 0)
			(run-buffer (jdibug-test-main-buffer-name))
			buffer started)
	(save-excursion
	  ;; Kill any currently running process
	  (let* ((buffer (get-buffer run-buffer))
			 (process (get-buffer-process buffer)))
		(when buffer
		  (when process
			(set-process-query-on-exit-flag process nil))
		  (flet ((yes-or-no-p (prompt) t))
			 (kill-buffer buffer))))

	  ;; Start the JVM
	  (jde-run 1)
	  ;; Wait for it to start
	  (save-excursion
		(while (and (< count 100) (not started))
		  (jdibug-test-info "Waiting for JVM to start %d" count)
		  (sleep-for 1)
		  (setq buffer (get-buffer run-buffer)
				count (1+ count))
		  (when buffer
			(jdibug-test-info "checking if running: %d %s" count buffer)
			(set-buffer buffer)
			(goto-char (point-min))
			(setq started (search-forward "Listening for transport dt_socket at address: " nil t)))
		  (jdibug-test-info "checked if running: %s" started))
		(assert-that started "jvm started"))
	  ;; Start jdibug
	  (setq jdibug-test-connected nil)
	  (add-hook 'jdibug-connected-hook 'jdibug-test-connected-hook)
	  (jdibug-connect)
	  (jdibug-test-wait-until jdibug-test-connected "Connected to JVM")

	  ;; Check that we actually connected
	  (assert-equal (length jdibug-virtual-machines)
					(length jdibug-connect-hosts)
					"Connected to all virtual machines")
	  )))

(defun jdibug-test-wait-for-refresh-timers nil
  "Wait until all of the buffer refresh timers have finished, or
an unreasonable amount of time has passed."
  (jdibug-test-debug "Waiting for timers: %s" timer-list)
  (mapc (lambda (timer-symbol)
		  (let* ((count 0)
				 (interval 0.1)
				 (max-count (/ 10 interval))
				 done)
			(while (and (< count max-count)
						(not done))
			  ;; Need to get the value every iteration,
			  ;; since it can change if the timer runs
			  ;; while we are sleeping and then needs to
			  ;; restart the timer.
			  (let ((timer (symbol-value timer-symbol)))
			    (if (memq timer timer-list)
					(sleep-for interval)
			      (jdibug-test-debug "%s no longer running" timer-symbol)
			      (setq done t)))
			  (setq count (1+ count)))
			  (assert-that done (format "Refresh timer never ran: %S" timer-symbol))))
		  '(jdibug-refresh-threads-buffer-timer
		    jdibug-refresh-locals-buffer-timer
		    jdibug-refresh-watchpoints-buffer-timer
		    jdibug-refresh-frames-buffer-timer))
  (jdibug-test-debug "Waiting for timers finished"))

(defmacro jdibug-test-wait-until (var message)
  "Wait until VAR becomes true."
  (declare (indent 2))
  `(let* ((count 0) (interval 0.1) (max-count (/ 10 interval)))
	 (while (and (< count max-count) (not ,var))
	   (sleep-for interval)
	   (jdibug-test-info "Waiting %d for %s: %s" count var ,var)
	   (setq	count (1+ count)))
	 (assert-that ,var ,message)))

(load "locals.el")
(load "watchpoints.el")
(load "breakpoints.el")

;; (debug-on-entry 'jdibug-test-connect-to-jvm)
;; (debug-on-entry 'jdibug-test-wait-for-refresh-timers)
;; (debug-on-entry 'jdibug-test-wait-until)
;; (debug-on-entry 'watch-expression-and-run-to-first-reference)

;;(debug-on-entry 'jdi-class-get-locations-of-line)

;; (debug-on-entry 'fail)
;; (debug-on-entry 'jdibug-handle-breakpoint)
;; (debug-on-entry 'jdibug-handle-class-prepare)

(let ((then (float-time))
	  now delta)
  ;; (debug-on-entry 'jdibug-handle-breakpoint)
  ;; (debug-on-entry 'jdibug-test-set-breakpoint-and-run)
  ;;  (debug)
  (elunit "jde-test-suite")
  (setq now (float-time)
		delta (- now then))
  (raise-frame)
  (message "Running jde-test-suite took %f seconds" delta))
