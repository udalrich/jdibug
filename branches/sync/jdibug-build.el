(defconst jdibug-build-files
  '("tree-mode"
	"elog"
	"jdwp"
    "jdi"
    "jdibug"))

(defconst jdibug-build-directory
  "./build/")

(defun jdibug-build ()
  (interactive)
  (when (get-buffer "jdibug-dist.el")
    (save-excursion
      (set-buffer "jdibug-dist.el")
      (set-buffer-modified-p nil)
      (kill-buffer "jdibug-dist.el")))
  (save-some-buffers)
  (unless (file-exists-p jdibug-build-directory)
    (make-directory jdibug-build-directory))
  (let ((root default-directory)
		bufname
		buf
		filename
		(outfile (concat jdibug-build-directory "jdibug-dist.el"))
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
	(pop-to-buffer "jdibug-dist.el")
	(toggle-read-only 1)
	(pop-to-buffer "*Compile-Log*")
	(message "finished compilation")))
				
(defun jdibug-build-fixup-output ()
  ;; remove requires stuff
  (goto-char (point-min))
  (while (re-search-forward "^(require 'jdwp).*\n" nil t)
	(replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "^(require 'jdi).*\n" nil t)
	(replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "^(require 'elog).*\n" nil t)
	(replace-match ""))

  (goto-char (point-min))
  (while (re-search-forward "^(require 'tree-mode).*\n" nil t)
	(replace-match "")))


