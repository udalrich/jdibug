;; (defmacro cont-defun (name parms &rest body)
;;   (declare (indent defun))
;;   (let ((f (intern (concatenate 'string
;; 								"=" (symbol-name name)))))
;; 	`(progn
;; 	   (defmacro ,name ,parms
;; 		 `(,',f cont ,,@parms))
;; 	   (defun ,f (cont ,@parms) ,@body))))

(require 'elog)
(elog-make-logger cont)

(defstruct cont
  id
  pid ; parent id
  inuation
)

(defun cont-new (pid &optional func)
  "Make a new continuation and add it to the current running continuations list."
  (let* ((id (cont-generate-id))
		 (new-cont (make-cont :id id :pid pid :inuation func)))
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

;; (defmacro cont-defun (name parms &rest body)
;;   (declare (indent defun))
;;   (let ((f (intern (concatenate 'string
;; 								"=" (symbol-name name)))))
;; 	`(progn
;; 	   (defmacro ,name ,parms
;; 		 (cons ',f (cons 'cont-current (list ,@parms))))
;; 	   (defun ,f (cont-current ,@parms)
;; 		 ,@body))))

(defun cont-values (&rest retvals)
  (cont-trace "cont-values:current-id=%s:retvals=%s" (if cont-current (cont-id cont-current) "nil") retvals)
  (if (null cont-current)
	  (apply 'identity retvals)
	(let ((cont-previous cont-current))
	  (setq cont-alist (assq-delete-all (cont-id cont-previous) cont-alist))
	  (let ((cont-current (if (cont-have-child-p (cont-pid cont-current))
							 nil
						   (cdr (assoc (cont-pid cont-current) cont-alist)))))
		(apply (cont-inuation cont-previous) retvals)))))

(defun cont-values-this (id &rest retvals)
  (let ((cont-current (cdr (assoc id cont-alist))))
	(if (null cont-current) (error "canot find cont:%s" id))
	(apply 'cont-values retvals)))

(defmacro cont-bind (parms expr &rest body)
  (declare (indent defun))
  `(let ((cont-current (cont-new (if cont-current 
									 (cont-id cont-current)
								   nil)
								 (lambda ,parms ,@body))))
	 ,expr))

(defmacro cont-wait (expr &rest body)
  (declare (indent defun))
  `(let ((cont-current (cont-new (if cont-current 
									 (cont-id cont-current)
								   nil)
								 (lambda (&rest args) ,@body))))
	 ,expr))

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

(eval-when-compile
  (when (featurep 'elunit)
	(cont-clear)

	(defsuite cont-suite nil
	  :teardown-hook (lambda () (message "done testing")))

	(deftest cont-test-one-argument cont-suite
	  "Testing one argument."
	  (defun add1 (x) (cont-values (1+ x)))
	  (assert-equal 3 (add1 2))
	  (assert-that (cont-clear-p)))

	(deftest cont-test-two-argument cont-suite
	  "Testing two argument."
	  (defun add2 (x y) (cont-values (list (1+ x) (1+ y))))
	  (assert-equal (list 3 5) (add2 2 4))
	  (assert-that (cont-clear-p)))

	(deftest cont-test-no-argument cont-suite
	  "Testing no argument."
	  (defun message2 ()
		(cont-values 'hello))
	  (assert-equal 'hello (message2))
	  (assert-that (cont-clear-p)))

	(deftest cont-test-onlisp cont-suite
	  "Test case from on lisp."
	  (defun message3 ()
		(cont-values 'hello 'there))

	  (defun baz ()
		(cont-bind (m n) (message3)
		  (cont-values (list m n))))

	  (assert-equal (list 'hello 'there) (baz))
	  (assert-that (cont-clear-p)))

	(deftest cont-test-save cont-suite
	  "save the continuation somewhere and call it later"
	  (defvar saved-cont nil)
	  (setq saved-cont nil)
	  (defvar saved-reply nil)
	  (setq saved-reply nil)

	  (defun send-message ()
		(setq saved-cont (cont-current-id)))
  
	  (cont-bind (reply) (send-message)
		(setq saved-reply reply))

	  (assert-nil saved-reply)
	  (cont-values-this saved-cont "aloha")
	  (assert-equal "aloha" saved-reply)
	  (assert-that (cont-clear-p)))

	(deftest cont-test-concurrent cont-suite
	  "Perform concurrent operations, calling a continuation when all of them is done"
	  (setq saved-cont-1 nil)
	  (setq saved-cont-2 nil)
	  (setq saved-reply nil)

	  (defun send-message-1 ()
		(setq saved-cont-1 (cont-current-id)))

	  (defun send-message-2 ()
		(setq saved-cont-2 (cont-current-id)))
    
	  (cont-wait (progn
				   (cont-bind (reply) (send-message-1)
					 (push reply saved-reply)
					 (cont-values t))
				   (cont-bind (reply) (send-message-2)
					 (push reply saved-reply)
					 (cont-values t)))
		(push "end" saved-reply))

	  (assert-nil saved-reply)
	  (cont-values-this saved-cont-1 "aloha")
	  (assert-equal '("aloha") saved-reply)
	  (cont-values-this saved-cont-2 "there")
	  (assert-equal '("end" "there" "aloha") saved-reply)
	  (assert-that (cont-clear-p)))

	;; SUCCESS!
	(set (make-local-variable 'elunit-default-suite) "cont-suite")
	(elunit "cont-suite")
	)
)


(provide 'cont)