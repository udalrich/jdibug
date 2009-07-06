(define-widget 'cont-tree-widget 'tree-widget
  "Tree widget that have :expander that uses cont.el"
  :value-create   'cont-tree-widget-value-create
  )

(defun cont-tree-widget-value-create (tree)
  "Create the TREE tree-widget."
  (lexical-let* ((tree tree)
				 (current-buffer (current-buffer))
				 (node   (tree-widget-node tree))
				 (flags  (widget-get tree :tree-widget--guide-flags))
				 (indent (widget-get tree :indent))
				 ;; Setup widget's image support.  Looking up for images, and
				 ;; setting widgets' :tag-glyph is done here, to allow to
				 ;; dynamically change the image theme.
				 (widget-image-enable (tree-widget-use-image-p)) ; Emacs
				 (widget-glyph-enable widget-image-enable) ; XEmacs
				 children buttons)
    (and indent (not (widget-get tree :parent))
         (insert-char ?\  indent))
    (if (widget-get tree :open)
;;;; Expanded node.
        (lexical-let ((args     (widget-get tree :args))
					  (guide    (widget-get tree :guide))
					  (noguide  (widget-get tree :no-guide))
					  (endguide (widget-get tree :end-guide))
					  (handle   (widget-get tree :handle))
					  (nohandle (widget-get tree :no-handle))
					  (guidi    (tree-widget-find-image "guide"))
					  (noguidi  (tree-widget-find-image "no-guide"))
					  (endguidi (tree-widget-find-image "end-guide"))
					  (handli   (tree-widget-find-image "handle"))
					  (nohandli (tree-widget-find-image "no-handle")))
          ;; Request children at run time, when requested.
          (when (and (widget-get tree :expander)
                     (widget-apply tree :expander-p))
			(cont-bind (args2) (widget-apply tree :expander)
			  (with-current-buffer current-buffer
				(cont-info "expanded to %s" args2)
				(setq args (mapcar 'widget-convert args2))
				(widget-put tree :args args)
				;; Defer the node widget creation after icon creation.
				(widget-put tree :node (widget-convert node))
				;; Create the icon widget for the expanded tree.
				(push (widget-create-child-and-convert
					   tree (widget-get tree (if args :open-icon :empty-icon))
					   ;; Pass the node widget to child.
					   :node (widget-get tree :node))
					  buttons)
				;; Create the tree node widget.
				(push (widget-create-child tree (widget-get tree :node))
					  children)
				;; Update the icon :node with the created node widget.
				(widget-put (car buttons) :node (car children))
				;; Create the tree children.
				(while args
				  (setq node (car args)
						args (cdr args))
				  (and indent (insert-char ?\  indent))
				  ;; Insert guide lines elements from previous levels.
				  (dolist (f (reverse flags))
					(widget-create-child-and-convert
					 tree (if f guide noguide)
					 :tag-glyph (if f guidi noguidi))
					(widget-create-child-and-convert
					 tree nohandle :tag-glyph nohandli))
				  ;; Insert guide line element for this level.
				  (widget-create-child-and-convert
				   tree (if args guide endguide)
				   :tag-glyph (if args guidi endguidi))
				  ;; Insert the node handle line
				  (widget-create-child-and-convert
				   tree handle :tag-glyph handli)
				  (if (tree-widget-p node)
					  ;; Create a sub-tree node.
					  (push (widget-create-child-and-convert
							 tree node :tree-widget--guide-flags
							 (cons (if args t) flags))
							children)
					;; Create the icon widget for a leaf node.
					(push (widget-create-child-and-convert
						   tree (widget-get tree :leaf-icon)
						   ;; At this point the node widget isn't yet created.
						   :node (setq node (widget-convert
											 node :tree-widget--guide-flags
											 (cons (if args t) flags)))
						   :tree-widget--leaf-flag t)
						  buttons)
					;; Create the leaf node widget.
					(push (widget-create-child tree node) children)
					;; Update the icon :node with the created node widget.
					(widget-put (car buttons) :node (car children))))))))
;;;; Collapsed node.
      ;; Defer the node widget creation after icon creation.
      (widget-put tree :node (widget-convert node))
      ;; Create the icon widget for the collapsed tree.
      (push (widget-create-child-and-convert
             tree (widget-get tree :close-icon)
             ;; Pass the node widget to child.
             :node (widget-get tree :node))
            buttons)
      ;; Create the tree node widget.
      (push (widget-create-child tree (widget-get tree :node))
            children)
      ;; Update the icon :node with the created node widget.
      (widget-put (car buttons) :node (car children)))
    ;; Save widget children and buttons.  The tree-widget :node child
    ;; is the first element in :children.
    (widget-put tree :children (nreverse children))
    (widget-put tree :buttons  buttons)))