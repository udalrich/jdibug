;; Wrapper that requires all the relevant components
(require 'elog)
(require 'tree-mode)
(require 'jdwp)
(require 'jdi)
(require 'jdibug-ui)
(require 'jdibug-menu)

(defvar jdibug-major-version "@major-version@"
  "Major version of this release")

(defvar jdibug-minor-version "@minor-version@"
  "Minor version of this release")

(defvar jdibug-version (concat jdibug-major-version "." jdibug-minor-version)
  "Version of this release")


(provide 'jdibug)
