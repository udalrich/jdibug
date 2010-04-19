(defconst jdibug-build-files
  '("tree-mode"
	"elog"
	"jdwp"
    "jdi"
    "jdibug-ui"
	"jdibug-menu"))

(defconst jdibug-build-directory
  "./")

(defun jdibug-build ()
  (interactive)

  (semantic-grammar-batch-build-packages)

  (when (get-buffer "jdibug.el")
    (save-excursion
      (set-buffer "jdibug.el")
      (set-buffer-modified-p nil)
      (kill-buffer "jdibug.el")))
  (save-some-buffers)
  (unless (file-exists-p jdibug-build-directory)
    (make-directory jdibug-build-directory))
  (let ((root default-directory)
		bufname
		buf
		filename
		(outfile (concat jdibug-build-directory "jdibug.el"))
		output)
	(save-excursion
	  (setq output (find-file outfile))
	  (dolist (file (reverse jdibug-build-files))
		(setq bufname (concat file ".el")
			  buf (get-buffer bufname)
			  filename (concat root bufname))
		(message "inserting %s" filename)
		(with-current-buffer output
		  (insert-file-contents filename)))
      (jdibug-build-fixup-output)
	  (save-buffer))

	(if (get-buffer "*Compile-Log*")
		(kill-buffer "*Compile-Log*"))
	(byte-compile-file outfile)
	(pop-to-buffer "jdibug.el")
	(toggle-read-only 1)
	(pop-to-buffer "*Compile-Log*")
	(message "finished compilation")))

(defun jdibug-build-fixup-output ()
  (mapc (lambda (re)
		  (goto-char (point-min))
		  (while (re-search-forward re nil t)
			(replace-match "")))
		'("^(require 'jdwp).*\n"
		  "^(require 'jdi).*\n"
		  "^(require 'elog).*\n"
		  "^(require 'tree-mode).*\n"
		  "^(provide 'tree-mode).*\n"
		  "^(provide 'elog).*\n"
		  "^(provide 'jdi).*\n"
		  "^(provide 'jdwp).*\n"
		  "^(provide 'jdibug-ui).*\n"))

  (goto-char (point-max))
  (insert "(provide 'jdibug)\n"))

