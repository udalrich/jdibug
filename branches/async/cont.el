(require 'elog)
(elog-make-logger cont)

(defstruct cont
  id
  pid									; parent id
  inuation
  )

(defun cont-new (pid &optional func)
  "Make a new continuation and add it to the current running continuations list."
  (let* ((id (cont-generate-id))
		 (new-cont (make-cont :id id :pid pid :inuation func)))
	(cont-trace "cont-new:id=%s:pid=%s:func=\n%s" 
				(cont-id new-cont) 
				(cont-pid new-cont)
				(elog-trim func 100))
	(push `(,id . ,new-cont) cont-alist)
	new-cont))

(defvar cont-alist nil
  "A alist of cont with key=id that is current running or suspended.")

(defvar cont-current 'nil)

(defcustom cont-maximum 10000
  "Number of total continuations that we support."
  :group 'cont
  :type 'integer)

(defun cont-current-id ()
  "return a unique id that you can save and then invoke it later using cont-values-this"
  (cont-info "cont-current-id:%s" (cont-id cont-current))
  (cont-id cont-current))

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

(defun cont-values (&rest retvals)
  (cont-trace "cont-values:current-id=%s:retvals=%s:continuation=\n%s"
			  (if cont-current (cont-id cont-current) "nil")
			  (elog-trim retvals 100)
			  (if (null cont-current)
				  "identity"
				(elog-trim (cont-inuation cont-current) 200)))
  (cont-trace "%s" (cont-list))
  (if (null cont-current)
	  (and retvals (apply 'identity retvals))
	(unless (cont-have-child-p (cont-id cont-current))
	  (let ((cont-previous cont-current))
		(setq cont-alist (assq-delete-all (cont-id cont-previous) cont-alist))
		(cont-trace "parent-id=%s:parent-have-child=%s" (cont-pid cont-current) (cont-have-child-p (cont-pid cont-current)))
		(cont-trace "current-have-child=%s" (cont-have-child-p (cont-id cont-current)))
		(let ((cont-current (if (cont-have-child-p (cont-pid cont-current))
								nil
							  (cdr (assoc (cont-pid cont-current) cont-alist)))))
		  (apply (cont-inuation cont-previous) retvals))))))

(defun cont-values-this (id &rest retvals)
  (cont-trace "cont-values-this:id=%s:retvals=%s" id (elog-trim retvals 100))
  (let ((cont-current (cdr (assoc id cont-alist))))
	(if (null cont-current) (error "canot find cont:%s" id))
	(apply 'cont-values retvals)))

(defmacro cont-bind (parms expr &rest body)
  (declare (indent defun))
  `(let ((cont-current (cont-new (if cont-current 
									 (cont-current-id)
								   nil)
								 (lambda ,parms ,@body))))
	 ,expr))

(defmacro cont-wait (expr &rest body)
  (declare (indent defun))
  `(let* ((cont-current (cont-new (if cont-current 
									  (cont-current-id)
									nil)
								  (lambda (&rest args) ,@body)))
		  ;; this is to solve the problem that when the first operation in expr
		  ;; returns straight away, the rest of the expressions will not be waited on
		  (cont-child (cont-new (cont-current-id) (lambda () (cont-values)))))
	 (cont-trace "created the temp child id=%s:pid=%s" (cont-id cont-child) (cont-pid cont-child))
	 ,expr
	 (cont-values-this (cont-id cont-child))))

;; (defmacro cont-funcall (fn &rest args)
;;   `(funcall ,fn cont-cont ,@args))

;; (defmacro cont-apply (fn &rest args)
;;   `(apply ,fn cont-cont ,@args))

;; (defmacro cont-fork (&rest body)
;;   `(let ((cont-current (cont-new nil 'identity)))
;; 	 ,@body))

(defun cont-clear ()
  (interactive)
  (setq cont-alist nil))
;;   (cont-new nil 'identity))

(defun cont-clear-p ()
  (interactive)
  (null cont-alist))
;;   (and cont-alist
;; 	   (car cont-alist)
;; 	   (equal (caar cont-alist) 0)
;; 	   (null (cont-pid (cdar cont-alist)))
;; 	   (eq (cont-inuation (cdar cont-alist)) 'identity)))

(defun cont-list ()
  (interactive)
  (concat "cont-list:"
		  (apply 'concat (loop for cont-pair in cont-alist
							   for cont = (cdr cont-pair)
							   collect (format "\n    id=%s:pid=%s" (cont-id cont) (cont-pid cont))))))

(eval-when-compile
  (cont-clear)

  ;;;;
  ;; Testing one argument.
  ;;;;
  (defun cont-test-add1 (x) (cont-values (1+ x)))
  (assert (equal 3 (cont-test-add1 2)))
  (assert (cont-clear-p))

  ;;;;
  ;; Testing two argument.
  ;;;;
  (defun cont-test-add2 (x y) (cont-values (list (1+ x) (1+ y))))
  (assert (equal (list 3 5) (cont-test-add2 2 4)))
  (assert (cont-clear-p))

  ;;;;
  ;; Testing no argument.
  ;;;;
  (defun cont-test-message2 ()
	(cont-values 'hello))
  (assert (equal 'hello (cont-test-message2)))
  (assert (cont-clear-p))

  ;;;;
  ;; Test case from on lisp.
  ;;;;
  (defun cont-test-message3 ()
	(cont-values 'hello 'there))

  (defun cont-test-baz ()
	(cont-bind (m n) (cont-test-message3)
	  (cont-values (list m n))))

  (assert (equal (list 'hello 'there) (cont-test-baz)))
  (assert (cont-clear-p))

  ;;;;
  ;; save the continuation somewhere and call it later
  ;;;;
  (defvar cont-test-saved-cont nil)
  (setq cont-test-saved-cont nil)
  (defvar cont-test-saved-reply nil)
  (setq cont-test-saved-reply nil)

  (defun cont-test-send-message ()
	(setq cont-test-saved-cont (cont-current-id)))
  
  (cont-bind (reply) (cont-test-send-message)
	(setq cont-test-saved-reply reply))

  (assert (null cont-test-saved-reply))
  (cont-values-this cont-test-saved-cont "aloha")
  (assert (equal "aloha" cont-test-saved-reply))
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

  (assert (equal '("end" "running" "start") cont-test-saved-reply))


  ;;;;
  ;; Perform concurrent operations, calling a continuation when all of them is done
  ;;;;
  (setq cont-test-saved-cont-1 nil)
  (setq cont-test-saved-cont-2 nil)
  (setq cont-test-saved-reply nil)

  (defun cont-test-send-message-1 ()
	(setq cont-test-saved-cont-1 (cont-current-id)))

  (defun cont-test-send-message-2 ()
	(setq cont-test-saved-cont-2 (cont-current-id)))
    
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

  (assert (equal '("start") cont-test-saved-reply))
  (cont-values-this cont-test-saved-cont-1 "aloha")
  (assert (equal '("aloha" "start") cont-test-saved-reply))
  (cont-values-this cont-test-saved-cont-2 "there")
  (assert (equal '("end" "there" "aloha" "start") cont-test-saved-reply))
  (assert (cont-clear-p))

  (run-with-timer 0 nil (lambda () (message "cont.el unit test success")))
  )


(provide 'cont)