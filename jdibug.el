;;; jdibug.el --- Elisp-based Java debugger

;; Author: Troy Daniels <udalrich.schermer@gmail.com>
;; Maintainer: Troy Daniels <udalrich.schermer@gmail.com>

;;; Commentary:

;; https://github.com/udalrich/jdibug/branches

;; If you are using JDEE, you only need to customize two variables
;;     jdibug-connect-hosts
;; which is a list of  hostname:port that you want to connect to.
;; Also ensure the
;;      jdibug-use-jdee-source-paths
;; is set to t.  This causes jdibug to use the jde sourcepath.
;;
;;
;; After that, make sure you have loaded the JDEE project with the
;; property variables (namely jdee-sourcepath)
;; and execute C-c C-c C-c in a JDEE buffer, wait for a while
;; until you see "Done" in the echo area.
;;
;; Then just go into any line in a java source code and do
;; C-c C-c C-b to break at that line.
;; Just run your application until you hit the breakpoint
;; after that stuffs are pretty much self explanatory
;;
;; The rest of the functions are almost similiar to that of jdb and jdebug
;; Wrapper that requires all the relevant components

;;; Code:

(require 'elog)
(require 'tree-mode)
(require 'jdwp)
(require 'jdi)
(require 'jdibug-expr)
(require 'jdibug-ui)
(require 'jdibug-run)
(require 'jdibug-menu)

(defconst jdibug-major-version "@major-version@"
  "Major version of this release.")

(defconst jdibug-minor-version "@minor-version@"
  "Minor version of this release.")

(defconst jdibug-version (concat jdibug-major-version "." jdibug-minor-version)
  "Version of this release.")


(provide 'jdibug)
;;; jdibug.el ends here
