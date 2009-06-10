(defvar cont-cont 'identity)

;; (defmacro cont-defun (name parms &rest body)
;;   (declare (indent defun))
;;   (let ((f (intern (concatenate 'string
;; 								"=" (symbol-name name)))))
;; 	`(progn
;; 	   (defmacro ,name ,parms
;; 		 `(,',f cont ,,@parms))
;; 	   (defun ,f (cont ,@parms) ,@body))))

(defun cont-get ()
  cont-cont)

(defmacro cont-defun (name parms &rest body)
  (declare (indent defun))
  (let ((f (intern (concatenate 'string
								"=" (symbol-name name)))))
	`(progn
	   (defmacro ,name ,parms
		 (cons ',f (cons 'cont-cont (list ,@parms))))
	   (defun ,f (cont-cont ,@parms) 
		 (lexical-let ((cont-cont cont-cont))
		   ,@body)))))

(defmacro cont-values (&rest retvals)
  `(funcall cont-cont ,@retvals))

(defmacro cont-bind (parms expr &rest body)
  (declare (indent defun))
  `(lexical-let ((cont-cont (lambda ,parms ,@body))) ,expr))

(defmacro cont-funcall (fn &rest args)
  `(funcall ,fn cont-cont ,@args))

(defmacro cont-apply (fn &rest args)
  `(apply ,fn cont-cont ,@args))

(defmacro cont-fork (&rest body)
  `(lexical-let ((cont-cont 'identity))
	 ,@body))

(eval-when-compile
  ;; one argument
  (cont-defun add1 (x) (cont-values (1+ x)))
  (assert (equal (add1 2) 3))

  ;; two argument
  (cont-defun add2 (x y) (cont-values (list (1+ x) (1+ y))))
  (assert (equal (add2 2 4) (list 3 5)))

  ;; no argument
  (cont-defun message2 ()
	(cont-values 'hello))
  (assert (equal (message2) 'hello))

  ;; test case from onlisp
  (cont-defun message3 ()
	(cont-values 'hello 'there))

  (cont-defun baz ()
	(cont-bind (m n) (message3)
	  (cont-values (list m n))))

  (assert (equal (baz) (list 'hello 'there)))

  ;; save the continuation somewhere and call it later
  (setq saved-cont nil)
  (setq saved-reply nil)

  (cont-defun send-message ()
	(setq saved-cont (cont-get)))
  
  (cont-bind (reply) (send-message)
	(setq saved-reply reply))

  (assert (equal saved-reply nil))
  (funcall saved-cont "aloha")
  (assert (equal saved-reply "aloha"))

  ;; SUCCESS!
  (message "Unit test success")
)


(provide 'cont)