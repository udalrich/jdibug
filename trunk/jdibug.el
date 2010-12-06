;; Wrapper that requires all the relevant components
(require 'elog)
(require 'tree-mode)
(require 'jdwp)
(require 'jdi)
(require 'jdibug-expr)
(require 'jdibug-ui)
(require 'jdibug-run)
(require 'jdibug-menu)

(defconst jdibug-major-version "@major-version@"
  "Major version of this release")

(defconst jdibug-minor-version "@minor-version@"
  "Minor version of this release")

(defconst jdibug-version (concat jdibug-major-version "." jdibug-minor-version)
  "Version of this release")


(provide 'jdibug)
