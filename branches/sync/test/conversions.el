;; (add-hook 'after-save-hook
;; 		  (lambda ()
;; 			(eval-current-buffer)
;; 			(elunit "conversions-suite"))
;; 		  nil 'local)

(require 'elunit)
(require 'jdwp)

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


(deftest int-to-vec-to-int conversions-suite
  "Test that converting int to vectors and back does not change the value."
  (mapc (lambda (int)
		  (let* ((vec (jdwp-integer-to-vec int 4))
				 (back-to-int (jdwp-vec-to-int vec)))
			(assert-equal int back-to-int
						  (format "converted %d to %s correctly"
								  int vec))))
		'(0 1 -1 256 -257 66666 -77777)))

(deftest int-to-vec conversions-suite
  "Test that negative values are converted with 32 bits, not 28."
  (assert-equal '[#xff #xff #xff #xff] (jdwp-integer-to-vec -1 4))
  (assert-equal '[#xff #xff #xff #xff #xff #xff #xff #xff]
				(jdwp-integer-to-vec -1 8)))
