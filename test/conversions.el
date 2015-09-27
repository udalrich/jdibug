(add-hook 'after-save-hook
	  (lambda ()
            (load (buffer-file-name))
	    (ert '(tag conversions)))
	  nil 'local)

(require 'ert)
(require 'jdwp)
(require 'jdibug-ui)
(require 'jdi)


(ert-deftest jdwp-string-plus ()
  "Test that jdwp-string-plus correctly adds numbers"
  :tags '(conversions smoke)
  ;; Basic addition
  (should (string-equal "3" (jdwp-string-plus "1" "2")))
  ;; Unequal lengths
  (should (string-equal "25" (jdwp-string-plus "1" "24")))
  ;; Unequal lengths (other order)
  (should (string-equal "25" (jdwp-string-plus "2" "23")))
  ;; Huge numbers
  (should (string-equal "11111111101111111110"
					 (jdwp-string-plus "1234567890987654321"
											 "9876543210123456789")))
  ;; Carry to get extra digits
  (should (string-equal "15" (jdwp-string-plus "7" "8")))
  ;; TODO: do we need to support negative numbers?
  )

(ert-deftest jdwp-string-mult ()
  "Test that jdwp-string-mult correctly multiplies"
  :tags '(conversions smoke)

  ;; Basic multiplication
  (should (string-equal (number-to-string (* 12 34))
                        (jdwp-string-mult "12" 34))))

(ert-deftest name-to-signature-matching ()
  "Test that we identify when a signature matches a wildcarded pattern"
  :tags '(conversions smoke)
  (let ((data '(("java.lang.RuntimeException" "Ljava/lang/RuntimeException;" t)
					 ;; Mismatch
					 ("java.lang.Object" "Ljava/lang/RuntimeException;" nil)
					 ;; Trailing wildcard
					 ("java.lang.*" "Ljava/lang/RuntimeException;" t)
					 ;; Trailing wildcard but no match
					 ("java.lang.*" "Ljava/util/Map;" nil)
					 ;; Leading wildcard
					 ("*.RuntimeException" "Ljava/lang/RuntimeException;" t)
					 ;; Leading wildcard but no match
					 ("*.Object" "Ljava/util/Map;" nil)
					 ;; Leading and trailing wildcard
					 ("*.lang.*" "Ljava/lang/Object;" t)
					 ;; Leading and trailing wildcard but no match
					 ("*.lang.*" "Ljava/util/Map;" nil))))
	 (mapc (lambda (item)
				(let* ((name     (first item))
						 (sig      (second item))
						 (expected (third item))
						 (actual   (jdibug-signature-matches-name-with-wildcards
										sig name))
                         )
				  (if expected
						(should actual)
					 (should-not actual))))
				data)))


(ert-deftest name-to-signature ()
  "Test that we correctly convert names to JNI signatures"
  :tags '(conversions smoke)
  (let ((pairs '(("java.lang.RuntimeException" . "Ljava/lang/RuntimeException;")
					  ("java.util.Map$Entry" . "Ljava/util/Map$Entry;")))
		  java-name jni-name)
	 (mapc (lambda (pair)
				(setq java-name (car pair)
						jni-name (cdr pair))
				(should (string-equal jni-name (jdi-class-name-to-class-signature java-name)))
				(should (equal (list java-name) (jdi-jni-to-print jni-name))))
			 pairs)))

(ert-deftest vec-to-double ()
  "Test converting vectors to doubles"
  :tags '(conversions smoke)
 (let ((pairs '(([#x3f #xf0 0 0 0 0 0 0] . 1.0)
				 ([#x40 #x00 0 0 0 0 0 0] . 2.0)
				 ([#xc0 #x00 0 0 0 0 0 0] . -2.0)
				 ([#x40 #x28 #xAE #x14 #x7A #xE1 #x47 #xAE] . 12.34)
				 ([#xC0 #xFE #x24 #x0C #x9F #xC8 #xF3 #x23 ] -123456.78901 . 1e-5)
				 ([#xFF #xDD #xAA #x88 #x66 #x44 #x22 #x00] .
				  -8.33290984853887157956504130131e307)
				 ([0 0 0 0 0 0 0 0] . 0.0)
				 ([#x7f #xf0 0 0  0 0 0 1] . NaN)
				 ([#x7f #xf0 0 0  0 0 0 0] . +infinity)
				 ([#xff #xf0 0 0  0 0 0 0] . -infinity)))
		vec actual expected back-to-vec epsilon)
	(mapc (lambda (pair)
			(setq vec (car pair) expected (cdr pair)
				  actual (jdwp-vec-to-double vec))
            (if (consp expected)
                (setq epsilon (cdr expected)
                      expected (car expected))
              (setq epsilon 0))
            ;; = does not work on NaN
            (if (numberp expected)
                (should (<= (abs (- expected actual)) epsilon))
              (should (equal expected actual)))
			(setq back-to-vec (jdwp-double-to-vec actual))
			(should (equal vec back-to-vec)))
		  pairs)))


(ert-deftest int-to-vec-to-int ()
  "Test that converting int to vectors and back does not change the value."
  :tags '(conversions smoke)
  (mapc (lambda (int)
		  (let* ((vec (jdwp-integer-to-vec int 4))
				 (back-to-int (jdwp-vec-to-int vec)))
			(should (= int back-to-int))))
		'(0 1 -1 256 -257 66666 -77777)))

(ert-deftest int-to-vec ()
  "Test that negative values are converted with 32 bits, not 28."
  :tags '(conversions smoke)
  (should (equal '[#xff #xff #xff #xff] (jdwp-integer-to-vec -1 4)))
  (should (equal '[#xff #xff #xff #xff #xff #xff #xff #xff]
				(jdwp-integer-to-vec -1 8))))

(ert-deftest vec-to-large-int ()
  "Test that numbers larger than emacs internal representation are converted properly"
  :tags '(conversions smoke)
  (let ((pairs (case jdwp--num-full-bytes-in-int
                 (3 (list (cons [#x40 0 0 0] (intern "1073741824"))
                          (cons [#x10 0 0] 1048576)
                          (cons [#xc0 0 0 0] (intern "-1073741824"))
                          ))
                 (7 (list (cons [#x40 0 0 0 0 0 0 0] (intern "4611686018427387904"))
                          (cons [#x10 0 0 0 0 0 0 0] (intern "1152921504606846976"))
                          (cons [#xc0 0 0 0 0 0 0 0] (intern "-4611686018427387904"))))
                 (t (error "Expected results not calculated for %d bytes in int" jdwp--num-full-bytes-in-int))))
        vec expected)
	(mapc (lambda (pair)
			(setq vec (car pair) expected (cdr pair)
				  actual (jdwp-vec-to-int vec))
			(should (equal expected actual )))
		  pairs)))


(ert-deftest float-to-vec-to-float ()
  "Test that the float/vec conversion functions are inverses"
  :tags '(conversions smoke)
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
			  (message "Testing %s" pair)
			(setq vec (car pair) expected (cdr pair)
				  actual (jdwp-vec-to-float vec))
            ;; = does not work on nan (and probably infinity)
			(should (equal expected actual))
			(setq back-to-vec (jdwp-float-to-vec actual))
			(should (equal vec back-to-vec)))
		  pairs)))

(ert-deftest sub-array-size ()
  "Check that we scale the size of subarrays in a nice manner.
Not really a conversion like the rest of the file, but sort of."
  :tags '(conversions smoke)
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
			  (should (= expected step))
			  (should (>= step jdibug-locals-min-sub-array-size))
			  (should (<= (ceiling num-display step)
                          jdibug-locals-max-array-size))))
		  data)))
