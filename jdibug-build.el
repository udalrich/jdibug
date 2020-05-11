(defconst jdibug-build-files
  '("tree-mode"
    "elog"
    "jdibug-util"
    "jdwp"
    "jdi"
    "jdibug-expr"
    "jdibug-ui"
    "jdibug-run"
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
                 (out-file (expand-file-name el-file jdibug-build-directory))
                 (max-specpdl-size 20000))
            (copy-file el-file (file-name-as-directory jdibug-build-directory) 'overwrite)
            (byte-compile-file out-file)))
        jdibug-build-files)

  ;; Update version numbers
  (mapc (lambda (in-file)
          (find-file in-file)
          (when (and (boundp 'jdibug-release-major-version)
                     (stringp jdibug-release-major-version)
                     (boundp 'jdibug-release-minor-version)
                     (stringp jdibug-release-minor-version))
            (goto-char (point-min))
            (while (search-forward "@major-version@" nil t)
              (replace-match jdibug-release-major-version))
            (goto-char (point-min))
            (while (search-forward "@minor-version@" nil t)
              (replace-match jdibug-release-minor-version)))
          (let ((out-file (expand-file-name in-file jdibug-build-directory)))
            (write-file out-file)
            (byte-compile-file out-file)))
        '("jdibug.el" "jdibug-pkg.el"))

  (pop-to-buffer "*Compile-Log*")
  (message "finished compilation"))

