
(require 'elunit)

(defsuite smoke-test-suite nil)

(load "conversions.el")
(load "uninterrupt.el")
(load "parsing.el")

(elunit "smoke-test-suite")
