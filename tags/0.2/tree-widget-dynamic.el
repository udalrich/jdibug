;;; tree-widget-dynamic.el --- Full dynamic tree-widget that supports reopening the opened path

;; Copyright (C) 2008 Phuah Yee Keat

;; Author: Phuah Yee Keat <ykphuah@gmail.com>
;; Maintainer: Phuah Yee Keat <ykphuah@gmail.com>
;; Created: 27 June 2008
;; Version: 0.1
;; Keywords: lisp tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; For implementing the locals browser in jdibug, I need a tree-widget that
;; is fully dynamic (everything is generated via :expander) which can
;; knows its opened path and reopen it during stepping

;; This module requires elog.el

(require 'elog)
(elog-make-logger tree-widget-dynamic)

(define-widget 'tree-widget-dynamic 'tree-widget
  "Tree Widget which is dynamic and stateful."
  :value-create    'tree-widget-dynamic-value-create
  )

(defun tree-widget-dynamic-value-create (tree)
  "Create the dynamic tree."
  (tree-widget-dynamic-info "tree-widget-dynamic-value-create")
  (tree-widget-value-create tree))

(defun tree-widget-dynamic-p (widget)
  "Return non-nil if WIDGET is a tree-widget-dynamic."
  (let ((type (widget-type widget)))
    (while (and type (not (eq type 'tree-widget-dynamic)))
      (setq type (widget-type (get type 'widget-type))))
    (eq type 'tree-widget-dynamic)))

(defun tree-widget-dynamic-save-opened (tree)
  (let ((root (tree-widget-dynamic-get-root tree)))
	(when (tree-widget-dynamic-p root)
	  (tree-widget-dynamic-info "tree-widget-dynamic-save-opened:%s" (tree-widget-dynamic-opened-path root))
	  (widget-put root 'opened-path (tree-widget-dynamic-opened-path root)))))

(defun tree-widget-dynamic-opened-path (tree)
  "Find all opened tree.
Return the tag list with the same depth."
  (tree-widget-dynamic-trace "tree-widget-dynamic-opened-path:tag=%s,no children=%d,no of args=%d" 
							 (widget-get (tree-widget-node tree) :tag)
							 (length (widget-get tree :children))
							 (length (widget-get tree :args)))
  (mapc (lambda (child)
		  (if (eq (widget-type child) 'item)
			  (tree-widget-dynamic-trace "item-type:%s" (widget-get child :value))
			(tree-widget-dynamic-trace "child-type:%s" (widget-type child))))
		(widget-get tree :children))
  (if (widget-get tree :children)
      (cons (widget-get (tree-widget-node tree) :tag)
			(delq nil
				  (mapcar (lambda (child)
							(if (eq (widget-type child) 'item)
								(list (widget-get child :value))
							  (if (tree-widget-p child)
								  (tree-widget-dynamic-opened-path child))))
						  (widget-get tree :children))))))

(defun tree-widget-dynamic-get-root (tree)
  "Return the root tree."
  (let ((root tree))
    (while (widget-get root :parent)
      (setq root (widget-get root :parent)))
    root))

(defun tree-widget-dynamic-find-child (tree tag)
  "Find the child matching the tag under this tree."
  (tree-widget-dynamic-info "tree-widget-dynamic-find-child:%s" tag)
  (catch 'found
	(dolist (child (widget-get tree :children))
	  (when (and (tree-widget-p child)
				 (string= (widget-get (tree-widget-node child) :tag)
						  tag))
		(tree-widget-dynamic-info "found")
		(throw 'found child)))))

(defun tree-widget-dynamic-reopen (tree &optional path)
  "Reopen a tree according to the saved path."
  (if (null path)
	  (setq path (widget-get tree :opened-path)))
  (tree-widget-dynamic-info "tree-widget-dynamic-reopen, children=%s, args=%d, path=%s, cdr path=%s" 
							(length (widget-get tree :children)) 
							(length (widget-get tree :args)) 
							path
							(cdr path))
  (when (and tree (cdr path))
	(if (not (widget-get tree :open))
		(progn
		  (tree-widget-dynamic-trace "widget is closed, opening:%s" tree)
		  (widget-apply-action tree))
	  (dolist (opened (cdr path))
		(let ((child (tree-widget-dynamic-find-child tree (car opened))))
		  (tree-widget-dynamic-reopen child opened))))))

(defun tree-widget-dynamic-refresh (tree)
  (tree-widget-dynamic-info "tree-widget-dynamic-refresh")
  (when (widget-get tree :open)
	(widget-put tree :opened-path (tree-widget-dynamic-opened-path tree))
	(widget-put tree :args nil)
	(widget-apply-action tree)
	(tree-widget-dynamic-reopen tree)))

(provide 'tree-widget-dynamic)

;;; tree-widget-dynamic.el ends here