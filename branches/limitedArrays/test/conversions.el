(add-hook 'after-save-hook
		  (lambda ()
			(eval-buffer)
			(elunit "conversions-suite"))
		  nil 'local)

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
		vec actual expected back-to-vec)
	(mapc (lambda (pair)
			(setq vec (car pair) expected (cdr pair)
				  actual (jdwp-vec-to-double vec))
			(assert-equal expected actual "conversion to double")
			(setq back-to-vec (jdwp-double-to-vec actual))
			(assert-equal vec back-to-vec (format "conversion back to vector of %s" actual)))
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

(deftest float-to-vec-to-float conversions-suite
  "Test that the float/vec conversion functions are inverses"
  (let ((pairs '(([#x3f #x80 0 0 ] . 1.0)
				 ([#x40 #x00 0 0 ] . 2.0)
				 ([#xc0 #x00 0 0 ] . -2.0)
				 ([#x40 #x28 #xAE #x14] . 2.635624961331482)
				 ([0 0 0 0] . 0.0)
				 ([#x7f #x80 0 1] . NaN)
				 ([#x7f #x80 0 0] . +infinity)
				 ([#xff #x80 0 0] . -infinity)))
		vec actual expected back-to-vec)
	(mapc (lambda (pair)
			(setq vec (car pair) expected (cdr pair)
				  actual (jdwp-vec-to-float vec))
			(assert-equal expected actual "conversion to float")
			(setq back-to-vec (jdwp-float-to-vec actual))
			(assert-equal vec back-to-vec (format "conversion back to vector of %s" actual)))
		  pairs)))

(deftest sub-array-size conversions-suite
  "Check that we scale the size of subarrays in a nice manner.
Not really a conversion like the rest of the file, but sort of."
  (let ((data '(; num-display min max expected
				(23            10  20  10)
				(30             3   7  5)
				(300            3   7  50)
				(230           10  20  20)
				(72832         14  35  3000)
)))
	(mapc (lambda (input)
			(let* ((num-display (nth 0 input))
				   (jdibug-locals-min-sub-array-size (nth 1 input))
				   (jdibug-locals-max-array-size (nth 2 input))
				   (expected (nth 3 input))
				   (step (jdibug-locals-step-size num-display)))
			  (assert-equal expected step (format "Correct step size: %s" input))
			  (assert-that (>= step jdibug-locals-min-sub-array-size)
						   (format "min size respected %d: %s" step input))
			  (assert-that (<= (ceiling num-display step)
							   jdibug-locals-max-array-size)
						   (format "max size respected %d: %s" step input))))
		  data)))

