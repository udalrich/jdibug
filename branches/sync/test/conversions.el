;; (add-hook 'after-save-hook
;; 		  (lambda ()
;; 			(eval-current-buffer)
;; 			(elunit "conversions-suite"))
;; 		  nil 'local)

(require 'elunit)

(defsuite conversions-suite smoke-test-suite
  ;; :setup-hook (lambda () )
  ;; :teardown-hook (lambda () )
)

(deftest vec-to-double conversions-suite
  "Test converting vectors to doubles"
  (let ((pairs '(([#x3f #xf0 0 0 0 0 0 0] . 1.0)
				 ([#x40 #x00 0 0 0 0 0 0] . 2.0)
				 ([#xc0 #x00 0 0 0 0 0 0] . -2.0)
				 ([#x40 #x28 #xAE #x14 #x7A #xE1 #x47 #xAE] . 12.34)
				 ([#xC0 #xFE #x24 #x0C #x9F #xC8 #xF3 #x23 ] . -123456.78901)
				 ([#xFF #xDD #xAA #x88 #x66 #x44 #x22 #x00] .
				  -8.33290984853887157956504130131e307)
				 ([0 0 0 0 0 0 0 0] . 0.0)
				 ([#x7f #xf0 0 0  0 0 0 1] . NaN)
				 ([#x7f #xf0 0 0  0 0 0 0] . +infinity)
				 ([#xff #xf0 0 0  0 0 0 0] . -infinity)))
		vec actual expected)
	(mapc (lambda (pair)
			(setq vec (car pair) expected (cdr pair))
			(assert-equal expected (jdwp-vec-to-double vec)))
		  pairs)))



