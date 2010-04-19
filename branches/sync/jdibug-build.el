(defconst jdibug-build-files
  '("tree-mode"
	"elog"
	"jdwp"
    "jdi"
    "jdibug-ui"
	"jdibug-menu"))


(defun jdibug-build (&optional interactive)
  (interactive "p")
  (jdibug-byte-compile-files))

(defun jdibug-byte-compile-files ()
  (interactive)

  (add-to-list 'load-path (expand-file-name jdibug-build-directory))
  (if (get-buffer "*Compile-Log*")
	  (kill-buffer "*Compile-Log*"))
  (mapc (lambda (file)
		  (let* ((el-file (concat file ".el"))
				 (out-file (concat jdibug-build-directory "/" el-file)))
			(copy-file el-file jdibug-build-directory 'overwrite)
			(byte-compile-file out-file)))
		jdibug-build-files)
  (pop-to-buffer "*Compile-Log*")
  (message "finished compilation"))

