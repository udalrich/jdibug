(defvar cont-cont 'identity)
;(setq cont 'identity)

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
  (let ((f (intern (concatenate 'string
								"=" (symbol-value fn)))))
	`(funcall ,f cont-cont ,@args)))

(defmacro cont-funcall (fn &rest args)
  `(funcall ,fn cont-cont ,@args))

(defmacro cont-apply (fn &rest args)
  `(apply ,fn cont-cont ,@args))

(defmacro cont-fork (&rest body)
  `(lexical-let ((cont-cont 'identity))
	 ,@body))

;; one argument
;; (cont-defun add1 (x) (cont-values (1+ x)))
;; (add1 2)

;; two argument
;; (cont-defun add2 (x y) (cont-values (list (1+ x) (1+ y))))
;; (add2 2 4)

;; no argument
;; (cont-defun message2 ()
;;   (cont-values 'hello))
;; (message2)

;; third test
;; (cont-defun message3 ()
;;   (message "message3:cont=%s" cont)
;;   (cont-values 'hello 'there))

;; (cont-defun baz ()
;;   (message "baz:cont=%s" cont)
;;   (cont-bind (m n) (message3)
;; 	(message "cont-bind:cont=%s" cont)
;; 	(cont-values (list m n))))

;; (baz)

(provide 'cont)