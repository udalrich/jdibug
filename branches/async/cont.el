(require 'elog)
(elog-make-logger cont)

(defstruct cont
  id
  parent-id	
  proc-id
  func
  )

(defun cont-new (parent-id proc-id &optional func)
  "Make a new continuation and add it to the current running continuations list. Return the new id."
  (cont-info "cont-new parent-id:%s proc-id:%s" parent-id proc-id)
  (let* ((id (cont-generate-id))
		 (new-cont (make-cont :id id :parent-id parent-id :proc-id proc-id :func func)))
	(cont-trace "cont-new:id=%s:parent-id=%s:func=\n    %s" 
				(cont-id new-cont) 
				(cont-parent-id new-cont)
				(elog-trim func 100))
	(push `(,id . ,new-cont) cont-alist)
	id))

(defvar cont-current-id 'nil
  "contains the id of the current continuation, can be saved to be invoked later with cont-values")

(defun cont-get-current-id ()
  (setq cont-waiting-id-list (cons cont-current-id cont-waiting-id-list))
  cont-current-id)

(defvar cont-waiting-id-list nil)

(defcustom cont-maximum 10000
  "Number of total continuations that we support."
  :group 'cont
  :type 'integer)

(defcustom cont-proc-maximum 10000
  "Number of total processes that we support."
  :group 'cont
  :type 'integer)

(defun cont-generate-id ()
  "Generate a unique id, throw error if too many continuations are running."
  (let ((num 0))
	(catch 'done
	  (while (< num cont-maximum)
		(if (null (assoc num cont-alist))
			(throw 'done num)
		  (setq num (1+ num))))
	  (error "too many continuations running"))))

(defun cont-have-child-p (id)
  "whether this id have any child continuations."
  (and id
	   (find-if (lambda (pair)
				  (equal (cont-parent-id (cdr pair)) id))
				cont-alist)))

;; cont-get and cont-delete should be the only access to cont-alist
;; it makes sure that we have a cont that points back to identity without
;; having a real cont in the cont-alist

(defvar cont-alist nil
  "A alist of cont with key=id that is current running or suspended.")

(defun cont-get (id)
  "Return a struct-cont from this id."
  (cont-info "cont-get:%s" id)
  (if (null id)
	  (make-cont :id nil :parent-id nil :func 'cont-identity)
	(or (cdr (assoc id cont-alist))
		(make-cont :id nil :parent-id nil :func 'cont-identity))))

		  
(defun cont-identity (&rest args)
  (if args
	  (if (cdr args)
		  (apply 'identity (list args))
		(apply 'identity args))))

(defun cont-delete (id)
  (cont-info "cont-delete:%s:func=%s" id (if (assoc id cont-alist) (elog-trim (cont-func (cdr (assoc id cont-alist))) 500)
										   nil))
  (if id
	  (setq cont-alist (assq-delete-all id cont-alist))))

(defvar cont-proc-list nil
  "A list of process ids that are currently running.")

(defvar cont-current-proc-id nil)

(defun cont-new-proc-id ()
  (let ((num 0))
	(catch 'done
	  (while (< num cont-proc-maximum)
		(if (not (member num cont-proc-list))
			(throw 'done num)
		  (setq num (1+ num))))
	  (error "too many process running"))))

(defmacro cont-fork (&rest body)
  `(let ((cont-current-proc-id ,(cont-new-proc-id)))
	 (setq cont-proc-list (cons cont-current-proc-id cont-proc-list))
	 ,@body
	 cont-current-proc-id))

(defun cont-values (&rest retvals)
  (cont-info "cont-values:current-id=%s:retvals=%s" cont-current-id (elog-trim retvals 100))
  (if (cont-have-child-p cont-current-id)
	  (cont-info "have child, not deleting/applying")
	(let* ((cont-previous-id cont-current-id)
		   (cont-current (cont-get cont-current-id))
		   ;; current-id = next-id that we are looking at
		   (cont-current-id (cont-id (cont-get (cont-parent-id cont-current))))
		   (cont-current-proc-id (cont-proc-id cont-current)))
	  (cont-delete cont-previous-id)
	  (cont-info "cont-values:deleted")
	  (let ((result (apply (cont-func cont-current) retvals)))
		(cont-gc)
		(cont-info "applied")
		result))))

(defun cont-values-this (id &rest retvals)
  (setq cont-waiting-id-list (delete id cont-waiting-id-list))
  (let ((cont-current (cont-get id))
		(cont-current-id id))
	(when cont-current
	  (apply 'cont-values retvals))))

(defmacro cont-bind (parms expr &rest body)
  (declare (indent defun))
  `(let* ((cont-current-id (cont-new cont-current-id
									 cont-current-proc-id
									 (lambda ,parms ,@body)))
		  (result (progn ,expr)))
	 (if (assoc cont-current-id cont-alist)
		 cont-current-id
	   result)))
	 
(defmacro cont-wait (expr &rest body)
  (declare (indent defun))
  `(let* ((cont-current-id (cont-new cont-current-id
									 cont-current-proc-id
									(lambda (&rest args) ,@body)))
		  (cont-last-child (cont-new cont-current-id cont-current-proc-id 'cont-values)))
	 ,expr
	 (cont-values-this cont-last-child)))
  
(defun cont-init ()
  (interactive)
  (setq cont-alist nil)
  (setq cont-proc-list nil)
  (setq cont-waiting-id-list nil))

(defun cont-clear-p ()
  (interactive)
  (and (null cont-alist)
	   (null cont-proc-list)
	   (null cont-waiting-id-list)))

(defun cont-list ()
  (interactive)
  (concat "cont-list:"
		  (apply 'concat (loop for cont-pair in cont-alist
							   for cont = (cdr cont-pair)
							   collect (format "\n    id=%s:parent-id=%s" (cont-id cont) (cont-parent-id cont))))))

(defun cont-kill (proc-id)
  "Stop all the continuations for the process id, and its child continuations."
  (cont-info "cont-kill:%s" proc-id)
  (dolist (pair cont-alist)
	(let ((cont (cdr pair)))
	  (cont-info "cont-kill:checking id:%s proc-id:%s" (cont-id cont) (cont-proc-id cont))
	  (when (equal (cont-proc-id cont) proc-id)
		(cont-info "cont-kill:setting to identity:%s" (cont-id cont))
		(setf (cont-func cont) 'cont-identity))
	  (when (and (equal (cont-proc-id cont) proc-id)
				 (not (member (cont-id cont) cont-waiting-id-list)))
		(cont-info "cont-kill:deleting because nobody's waiting:%s" (cont-id cont))
		(cont-delete (cont-id cont))))))

(defun cont-gc ()
  "Garbage Collect. :P"
  (let ((all-proc-id (loop for pair in cont-alist
						   for cont = (cdr pair)
						   collect (cont-proc-id cont))))
	(setq cont-proc-list
		  (loop for proc-id in cont-proc-list
				if (member proc-id all-proc-id)
				collect proc-id))))

(defun cont-mapcar (function sequence)
  (lexical-let ((sequence sequence)
				(cont-results (make-vector (length sequence) nil))
				(cont-remaining (length sequence)))
	(let ((index 0)) ;; this cannot be lexical
	  (dolist (item sequence)
		(lexical-let ((index index))
		  (cont-bind (cont-result) (funcall function item)
			(aset cont-results index cont-result)
			(decf cont-remaining)
			(if (= 0 cont-remaining)
				(cont-values (append cont-results nil)))))
		(incf index)))))

(defun cont-mappend (function sequence)
  (cont-bind (values) (cont-mapcar function sequence)
	(cont-debug "cont-mappend:values=%s" values)
	(cont-values (apply 'append values))))

(eval-when-compile
  (defun assert-equal (expected value)
	(unless (equal expected value)
	  (error "expected %s but got %s" expected value)))

  (cont-init)

  ;;;;
  ;; Testing one argument.
  ;;;;
  (defun cont-test-add1 (x) (cont-values (1+ x)))
  (assert-equal 3 (cont-test-add1 2))
  (assert (cont-clear-p))

  ;;;;
  ;; Testing two argument.
  ;;;;
  (defun cont-test-add2 (x y) (cont-values (list (1+ x) (1+ y))))
  (assert-equal (list 3 5) (cont-test-add2 2 4))
  (assert (cont-clear-p))

  ;;;;
  ;; Testing no argument.
  ;;;;
  (defun cont-test-message2 ()
	(cont-values 'hello))
  (assert-equal 'hello (cont-test-message2))
  (assert (cont-clear-p))

  ;;;;
  ;; Testing no return values
  ;;;;
  (defun cont-test-message3 ()
	(cont-values))
  (assert-equal nil (cont-test-message3))
  (assert (cont-clear-p))

  ;;;;
  ;; Test case from on lisp.
  ;;;;
  (defun cont-test-message4 ()
	(cont-values 'hello 'there))

  (defun cont-test-baz ()
	(cont-bind (m n) (cont-test-message4)
	  (cont-values (list m n))))

  (assert-equal (list 'hello 'there) (cont-test-baz))
  (assert (cont-clear-p))

  ;;;;
  ;; save the continuation somewhere and call it later
  ;;;;
  (defvar cont-test-saved-cont nil)
  (setq cont-test-saved-cont nil)
  (defvar cont-test-saved-reply nil)
  (setq cont-test-saved-reply nil)

  (defun cont-test-send-message ()
	(setq cont-test-saved-cont (cont-get-current-id)))
  
  (cont-bind (reply) (cont-test-send-message)
	(setq cont-test-saved-reply reply))

  (assert (null cont-test-saved-reply))
  (cont-values-this cont-test-saved-cont "aloha")
  (assert-equal "aloha" cont-test-saved-reply)
  (assert (cont-clear-p))

  ;;;;
  ;; Testing concurrent operations that does not save continuations
  ;;;;
  (cont-trace "Testing concurrent operations that does not save continuations")
  (setq cont-test-saved-reply nil)

  (cont-wait (mapc (lambda (reply)
					 (push reply cont-test-saved-reply)
					 (cont-values t))
				   (list "start" "running"))
	(push "end" cont-test-saved-reply))

  (assert-equal '("end" "running" "start") cont-test-saved-reply)


  ;;;;
  ;; Perform concurrent operations, calling a continuation when all of them is done
  ;;;;
  (defvar cont-test-saved-cont-1 nil)
  (setq cont-test-saved-cont-1 nil)
  (defvar cont-test-saved-cont-2 nil)
  (setq cont-test-saved-cont-2 nil)
  (defvar cont-test-saved-cont-3 nil)
  (setq cont-test-saved-cont-3 nil)
  (defvar cont-test-saved-reply nil)
  (setq cont-test-saved-reply nil)

  (defun cont-test-send-message-1 ()
	(setq cont-test-saved-cont-1 (cont-get-current-id))
	t)

  (defun cont-test-send-message-2 ()
	(setq cont-test-saved-cont-2 (cont-get-current-id))
	t)

  (defun cont-test-send-message-3 ()
	(setq cont-test-saved-cont-3 (cont-get-current-id))
	t)
    
  (cont-wait (progn
			   ;; this simulate the value might already be cached
			   ;; and calls cont-values straight away
			   (cont-bind (reply) (cont-values t)
				 (push "start" cont-test-saved-reply)
				 (cont-values t))
			   ;; while these two saved it first, then continue 
			   ;; it later
			   (cont-bind (reply) (cont-test-send-message-1)
				 (push reply cont-test-saved-reply)
				 (cont-values t))
			   (cont-bind (reply) (cont-test-send-message-2)
				 (push reply cont-test-saved-reply)
				 (cont-values t)))
	(push "end" cont-test-saved-reply))

  (assert-equal '("start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-1 "aloha")
  (assert-equal '("aloha" "start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-2 "there")
  (assert-equal '("end" "there" "aloha" "start") cont-test-saved-reply)
  (assert (cont-clear-p))

  ;;;;
  ;; Cancel a continuation
  ;;;;
  (setq cont-test-saved-cont-1 nil)
  (setq cont-test-saved-cont-2 nil)
  (setq cont-test-saved-reply nil)

  (defvar cont-test-proc nil)
  (setq cont-test-proc nil)

  (defun cont-test-refresh ()
	(cont-kill cont-test-proc)
	(setq cont-test-saved-reply nil)
	(setq cont-test-proc (cont-fork
						  (cont-bind (reply) (cont-test-send-message-1)
							(push reply cont-test-saved-reply)

							(cont-bind (reply) (cont-test-send-message-2)
							  (push reply cont-test-saved-reply)
							  (push "end" cont-test-saved-reply))))))

  (cont-test-refresh)
  (assert-equal nil cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-1 "aloha")
  (assert-equal '("aloha") cont-test-saved-reply)

  (cont-test-refresh)
  (cont-values-this cont-test-saved-cont-2 "there")
  (cont-values-this cont-test-saved-cont-1 "aloha again")
  (assert-equal '("aloha again") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-2 "there")
  (assert-equal '("end" "there" "aloha again") cont-test-saved-reply)
  (assert (cont-clear-p))

  ;;;;
  ;; Cancel a concurrent continuation
  ;;;;
  (setq cont-test-saved-cont-1 nil)
  (setq cont-test-saved-cont-2 nil)
  (setq cont-test-saved-cont-3 nil)
  (setq cont-test-saved-reply nil)
  (setq cont-test-proc nil)

  (defun cont-test-refresh ()
	(cont-kill cont-test-proc)
	(setq cont-test-saved-reply nil)
	(setq cont-test-proc (cont-fork
						  (cont-wait (progn
									   (cont-bind (reply) (cont-values t)
										 (push "start" cont-test-saved-reply)
										 (cont-values t))
									   (cont-bind (reply) (cont-test-send-message-1)
										 (push reply cont-test-saved-reply)
										 (cont-bind (reply) (cont-test-send-message-2)
										   (push reply cont-test-saved-reply)
										   (cont-values t)))
									   (cont-bind (reply) (cont-test-send-message-3)
										 (push reply cont-test-saved-reply)
										 (cont-values t)))
							(push "end" cont-test-saved-reply)))))

  (cont-test-refresh)
  (assert-equal '("start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-1 "aloha")
  (assert-equal '("aloha" "start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-3 "there")
  (assert-equal '("there" "aloha" "start") cont-test-saved-reply)

  (cont-test-refresh)
  (assert-equal '("start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-2 "there")

  (cont-values-this cont-test-saved-cont-1 "aloha again")
  (assert-equal '("aloha again" "start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-2 "there")
  (assert-equal '("there" "aloha again" "start") cont-test-saved-reply)
  (cont-values-this cont-test-saved-cont-3 "there again")
  (assert-equal '("end" "there again" "there" "aloha again" "start") cont-test-saved-reply)

  ;;;;
  ;; Test cont-mapcar
  ;;;;
  (cont-init)
  (setq cont-test-saved-reply nil)
  (setq cont-test-saved-cont nil)

  (defun cont-test-send-message (arg)
	(push `(,arg . ,(cont-get-current-id)) cont-test-saved-cont))

  (cont-bind (replies) (cont-mapcar 'cont-test-send-message '("1" "2" "3"))
	(setq cont-test-saved-reply replies))

  (assert-equal nil cont-test-saved-reply)

  (cont-values-this (aget cont-test-saved-cont "1") "2")
  (assert-equal nil cont-test-saved-reply)

  (cont-values-this (aget cont-test-saved-cont "2") "4")
  (assert-equal nil cont-test-saved-reply)

  (cont-values-this (aget cont-test-saved-cont "3") "6")
  (assert-equal '("2" "4" "6") cont-test-saved-reply)


  ;;;;
  ;; Test cont-mapcan
  ;;;;
  (cont-init)
  (setq cont-test-saved-reply nil)
  (setq cont-test-saved-cont nil)

  (cont-bind (replies) (cont-mappend 'cont-test-send-message '("1" "2" "3"))
	(setq cont-test-saved-reply replies))

  (assert-equal nil cont-test-saved-reply)

  (cont-values-this (aget cont-test-saved-cont "1") (list "2" "3"))
  (assert-equal nil cont-test-saved-reply)

  ;; the result should be in the order of the sequence
  ;; not in the order of result obtained, so we try with the 3rd item returning
  ;; first
  (cont-values-this (aget cont-test-saved-cont "3") (list "6" "9"))
  (assert-equal nil cont-test-saved-reply)

  (cont-values-this (aget cont-test-saved-cont "2") (list "4" "6"))
  (assert-equal '("2" "3" "4" "6" "6" "9") cont-test-saved-reply)

  (assert (cont-clear-p))

  (run-with-timer 0 nil (lambda () (message "cont.el unit test success")))
  )


(provide 'cont)