(require 'elog)
(elog-make-logger cont)

(defstruct cont
  id
  pid									; parent id
  func
  )

(defun cont-new (pid &optional func)
  "Make a new continuation and add it to the current running continuations list. Return the new id."
  (let* ((id (cont-generate-id))
		 (new-cont (make-cont :id id :pid pid :func func)))
	(cont-trace "cont-new:id=%s:pid=%s:func=\n    %s" 
				(cont-id new-cont) 
				(cont-pid new-cont)
				(elog-trim func 100))
	(push `(,id . ,new-cont) cont-alist)
	id))

(defvar cont-current-id 'nil
  "contains the id of the current continuation, can be saved to be invoked later with cont-values")

(defun cont-get-current-id ()
  cont-current-id)

(defcustom cont-maximum 10000
  "Number of total continuations that we support."
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
				  (equal (cont-pid (cdr pair)) id))
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
	  (make-cont :id nil :pid nil :func 'cont-identity)
	(let ((result (cdr (assoc id cont-alist))))
	  (if result result (error "can't find cont-id %s" id)))))
		  
(defun cont-identity (&rest args)
  (if args
	  (apply 'identity args)))

(defun cont-delete (id)
  (cont-info "cont-delete:%s:func=%s" id (if (assoc id cont-alist) (elog-trim (cont-func (cdr (assoc id cont-alist))) 100)
										   nil))
  (if id
	  (setq cont-alist (assq-delete-all id cont-alist))))
	  
(defun cont-values (&rest retvals)
  (cont-info "cont-values:current-id=%s:retvals=%s" cont-current-id (elog-trim retvals 100))
  (if (cont-have-child-p cont-current-id)
	  (cont-info "have child, not deleting/applying")
	(let* ((cont-previous-id cont-current-id)
		   (cont-current (cont-get cont-current-id))
		   ;; current-id = next-id that we are looking at
		   (cont-current-id (cont-id (cont-get (cont-pid cont-current)))))
	  (cont-delete cont-previous-id)
	  (apply (cont-func cont-current) retvals))))

;; (defmacro cont-defun (name parms &rest body)
;;   `(defun ,name ,parms
;; 	 ,@body))
;; 	 (let ((cont-current-id (cont-new (cont-id (cont-get cont-current-id))
;; 									  'cont-values)))
;; 	   ,@body)))

;; (defmacro cont-defun (name parms &rest body)
;;   `(defun ,name ,parms
;; 	 (cont-trace "calling %s %s cont-current" (symbol-name ',name) (if cont-current "with" "without"))
;; 	 (if cont-current
;; 		 ,@body
;; 	   (flet ((cont-values (&rest retvals) (apply 'identity retvals)))
;; 		 ,@body))))

;; (defun cont-values (&rest retvals)
;;   (cont-trace "cont-values:current-id=%s:retvals=%s:continuation=\n%s"
;; 			  (if cont-current (cont-id cont-current) "nil")
;; 			  (elog-trim retvals 100)
;; 			  (if (null cont-current)
;; 				  "identity"
;; 				(elog-trim (cont-inuation cont-current) 200)))
;;   (cont-trace "%s" (cont-list))
;;   (if (null cont-current)
;; 	  (and retvals (apply 'identity retvals))
;; 	(and (> (cont-referred cont-current) 0) (decf (cont-referred cont-current)))
;; 	(when (equal (cont-referred cont-current) 0)
;; 	  (let ((cont-previous cont-current)
;; 			(cont-current (cdr (assoc (cont-pid cont-current) cont-alist))))
;; 		(cont-trace "removing cont id:%s" (cont-id cont-previous))
;; 		(setq cont-alist (assq-delete-all (cont-id cont-previous) cont-alist))
;; 		(apply (cont-inuation cont-previous) retvals))))

;; 		(cont-trace "parent-id=%s:parent-have-child=%s" (cont-pid cont-current) (cont-have-child-p (cont-pid cont-current)))
;; 		(cont-trace "current-have-child=%s" (cont-have-child-p (cont-id cont-current)))
;; 		(let ((cont-current (if (cont-have-child-p (cont-pid cont-current))
;; 								nil
;; 							  (cdr (assoc (cont-pid cont-current) cont-alist)))))
;; 		  (apply (cont-inuation cont-previous) retvals))))))

(defun cont-values-this (id &rest retvals)
  (let ((cont-current (cont-get id))
		(cont-current-id id))
	(apply 'cont-values retvals)))

(defmacro cont-bind (parms expr &rest body)
  (declare (indent defun))
  `(let ((cont-current-id (cont-new cont-current-id
									(lambda ,parms ,@body))))
	 ,expr))

(defun cont-apply-when-last (&rest args)
  (cont-trace "cont-apply-when-last:id=%s:pid=%s" (cont-id cont-current) (cont-pid cont-current))
  (unless (cont-have-child-p (cont-id cont-current))
	(cont-values)))

(defmacro cont-wait (expr &rest body)
  (declare (indent defun))
  `(let* ((cont-current-id (cont-new cont-current-id
									(lambda (&rest args) ,@body)))
		  (cont-last-child (cont-new cont-current-id 'cont-values)))
	 ,expr
	 (cont-values-this cont-last-child)))
  
;; (defmacro cont-wait (expr &rest body)
;;   (declare (indent defun))
;;   `(let* ((cont-current (cont-new (if cont-current 
;; 									  (cont-current-id)
;; 									nil)
;; 								  (lambda (&rest args) ,@body)))
;; 		  ;; this is to solve the problem that when the first operation in expr
;; 		  ;; returns straight away, the rest of the expressions will not be waited on
;; 		  (cont-child (cont-new (cont-current-id) (lambda () (cont-values)))))
;; 	 (cont-trace "created the temp child id=%s:pid=%s" (cont-id cont-child) (cont-pid cont-child))
;; 	 ,expr
;; 	 (cont-values-this (cont-id cont-child))))

;; (defmacro cont-funcall (fn &rest args)
;;   `(funcall ,fn cont-cont ,@args))

;; (defmacro cont-apply (fn &rest args)
;;   `(apply ,fn cont-cont ,@args))

;; (defmacro cont-fork (&rest body)
;;   `(let ((cont-current (cont-new nil 'identity)))
;; 	 ,@body))

(defun cont-init ()
  (interactive)
  (setq cont-alist nil))

(defun cont-clear-p ()
  (interactive)
  (null cont-alist))
;;   (and cont-alist
;; 	   (car cont-alist)
;; 	   (null (caar cont-alist))
;; 	   (null (cont-pid (cdar cont-alist)))
;; 	   (eq (cont-func (cdar cont-alist)) 'identity)))

(defun cont-list ()
  (interactive)
  (concat "cont-list:"
		  (apply 'concat (loop for cont-pair in cont-alist
							   for cont = (cdr cont-pair)
							   collect (format "\n    id=%s:pid=%s" (cont-id cont) (cont-pid cont))))))

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
  (setq cont-test-saved-cont-1 nil)
  (setq cont-test-saved-cont-2 nil)
  (setq cont-test-saved-reply nil)

  (defun cont-test-send-message-1 ()
	(setq cont-test-saved-cont-1 (cont-get-current-id)))

  (defun cont-test-send-message-2 ()
	(setq cont-test-saved-cont-2 (cont-get-current-id)))
    
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

  (run-with-timer 0 nil (lambda () (message "cont.el unit test success")))
  )


(provide 'cont)