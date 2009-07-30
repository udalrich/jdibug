(require 'elog)
(require 'jdwp)

(elog-make-logger jdi)

(defstruct jdi-mirror
  virtual-machine)

(defstruct jdi-virtual-machine
  jdwp

  description
  jdwp-major
  jdwp-minor
  version
  name

  (event-request-manager (make-jdi-event-request-manager))
  host 
  port
  (classes (make-hash-table :test 'equal)) ;; hash table where key=class-id value=jdi-class
  classes-by-signature ;; hash table where key=signature value=jdi-class

  threads

  suspended-thread-id

  ;; list of jdi-frame that is suspended (by breakpoint/step events)
  suspended-frames

  (objects (make-hash-table :test 'equal)) ;; hash table where key=object-id value=jdi-object
  )

;; as java returns interfaces using all-classes command, this might represent an interface
(defstruct (jdi-class (:include jdi-mirror))
  id
  ;; jni-style
  signature 
  ref-type-tag
  status
  ;; list of jdi-field
  fields    
  ;; parent jdi-class
  super	    
  ;; list of interfaces implemented by this class
  interfaces
  interfaces-count ;; we use this to see whether the interfaces field have been resolved or not
  ;; list of jdi-method
  methods)

(defstruct (jdi-method (:include jdi-mirror))
  id
  name
  signature
  mod-bits

  ;; list of jdi-location
  locations    

  ;; list of jdi-variable
  variables

  ;; link back to our containing jdi-class
  class	
  )

(defstruct (jdi-location (:include jdi-mirror))
  ;; this might have to be resolved
  line-number 
  line-code-index
  type

  ;; jdi-class
  class	 
  ;; jdi-method
  method 

  ;; String to be displayed in the frames buffer
  string)

(defstruct jdi-event-request-manager
  breakpoint-requests
  )

(defstruct (jdi-event-request (:include jdi-mirror))
  id
  data
  )

(defstruct (jdi-object (:include jdi-mirror)) ;; com.sun.jdi.ObjectReference
  id)

(defstruct (jdi-thread (:include jdi-object))
  name
  status
  suspend-status
  frames

  ; jdi-thread-group
  thread-group
  system-thread-p
  )

(defstruct (jdi-thread-group (:include jdi-object))
  name
  parent
  child-threads
  child-groups)

(defstruct (jdi-frame (:include jdi-mirror)) ;; this is actually StackFrame in JDI
  id
  values
  thread
  location)

(defstruct (jdi-variable (:include jdi-mirror)) ;; this is actually LocalVariable in JDI
  code-index
  name
  signature
  length
  slot)

;; A value retrieved from the target VM. The first byte is a signature byte which is used to identify the type. See JDWP.Tag for the possible values of this byte. It is followed immediately by the value itself. This value can be an objectID (see Get ID Sizes) or a primitive value (1 to 8 bytes). More details about each value type can be found in the next table. 
(defstruct (jdi-value (:include jdi-mirror))
  type
  value

  ;; caching purpose only
  class
  array-length)
;;   name
;;   signature
;;   generic-signature
;;   type
;;   value
;;   (string "null")

;;   ;; this is only valid when this is top level (not contained in any other object)
;;   slot

;;   ;; for values that are inside method
;;   code-index
;;   code-index-length

;;   ;; for values which are of type object
;;   class	;; jdi-class

;;   ;; for values which are of type array
;;   array-length 

;;   ;; for object/array, this is the list that are displayed when the tree is expanded
;;   ;; list of jdi-values
;;   values)

(defstruct (jdi-field (:include jdi-mirror))
  id
  name
  signature
  generic-signature
  mod-bits)

;;; Constants:
(defconst jdi-access-public        #x0001)
(defconst jdi-access-private       #x0002)
(defconst jdi-access-protected     #x0004)
(defconst jdi-access-static        #x0008)
(defconst jdi-access-final         #x0010)
(defconst jdi-access-synchronized  #x0020)
(defconst jdi-access-volatile      #x0040)
(defconst jdi-access-transient     #x0080)
(defconst jdi-access-native        #x0100)
(defconst jdi-access-abstract      #x0400)

(defun jdi-mirror-jdwp (mirror)
  (jdi-virtual-machine-jdwp (jdi-mirror-virtual-machine mirror)))

(defun jdi-event-request-manager-create-breakpoint (erm location)
  (let* ((location-data `((:type .   1)
						  (:class-id . ,(jdi-class-id (jdi-location-class location)))
						  (:method-id . ,(jdi-method-id (jdi-location-method location)))
						  (:index     . ,(jdi-location-line-code-index location))))
		 (data `((:event-kind     . ,jdwp-event-breakpoint)
				 (:suspend-policy . ,jdwp-suspend-policy-event-thread)
				 (:modifiers      . 1)
				 (:modifier       ((:mod-kind .  7)
								   (:location .  ,location-data))))))
	(make-jdi-event-request :virtual-machine (jdi-mirror-virtual-machine location) :data data)))

(unless (fboundp 'mappend)
  (defun mappend (fn &rest lsts)
	(apply 'append (apply 'mapcar fn lsts))))

(defun jdi-event-request-manager-create-step (erm thread depth)
  (let ((data `((:event-kind     . ,jdwp-event-single-step)
				(:suspend-policy . ,jdwp-suspend-policy-event-thread)
				(:modifiers      . 2)
				(:modifier       
				 ((:mod-kind . 10)
				  (:thread   . ,(jdi-thread-id thread))
				  (:size     . 1)
				  (:depth    . ,depth))
				 ((:mod-kind . 1)
				  (:count    . 1))))))
	(make-jdi-event-request :virtual-machine (jdi-mirror-virtual-machine thread) :data data)))

(defun jdi-event-request-enable (er)
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp er) "set" (jdi-event-request-data er))))
	(jdi-trace "received requestid:%s" (bindat-get-field reply :request-id))
	(setf (jdi-event-request-id er) (bindat-get-field reply :request-id))))

(defun jdi-event-request-disable (er)
  (jdwp-send-command (jdi-mirror-jdwp er) "clear" `((:event . ,jdwp-event-breakpoint) (:request-id . ,(jdi-event-request-id er))))
  (jdi-trace "cleared event request"))

(defun jdi-virtual-machine-set-standard-events (vm)
  (mapcar (lambda (event)
			(jdwp-send-command (jdi-virtual-machine-jdwp vm) "set" 
							   `((:event-kind . ,event)
								 (:suspend-policy . ,jdwp-suspend-policy-none)
								 (:modifiers . 0))))
		  (list jdwp-event-class-prepare
				jdwp-event-class-unload
				jdwp-event-thread-start
				jdwp-event-thread-end
				jdwp-event-vm-death)))

(defun jdi-virtual-machine-connect (vm)
  "[ASYNC] returns t if success, nil on failure"
  (when (jdwp-connect (jdi-virtual-machine-jdwp vm) (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
	(jdwp-put (jdi-virtual-machine-jdwp vm) 'jdi-virtual-machine vm)
	(let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "version" nil)))
	  (setf (jdi-virtual-machine-description vm) (jdwp-get-string reply :description)
			(jdi-virtual-machine-jdwp-major vm)  (bindat-get-field reply :jdwp-major)
			(jdi-virtual-machine-jdwp-minor vm)  (bindat-get-field reply :jdwp-minor)
			(jdi-virtual-machine-version vm)  (jdwp-get-string reply :vm-version)
			(jdi-virtual-machine-name vm)     (jdwp-get-string reply :vm-name))

	  (jdi-trace "description: \n%s" (jdwp-get-string reply :description))
	  (jdi-trace "major      : %s"   (bindat-get-field reply :jdwp-major))
	  (jdi-trace "minor      : %s"   (bindat-get-field reply :jdwp-minor))
	  (jdi-trace "version    : %s"   (jdwp-get-string reply :vm-version))
	  (jdi-trace "name       : %s"   (jdwp-get-string reply :vm-name))
	  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "capabilities-new" nil)))
		(jdi-trace "capabilities-new:%s" reply)
		(jdi-virtual-machine-set-standard-events vm)
		t))))

(defun jdi-virtual-machine-get-threads (vm)
  (jdi-debug "jdi-virtual-machine-get-threads")
  (if (jdi-virtual-machine-threads vm)
 	  (jdi-virtual-machine-threads vm)
	(jdi-time-start)
	(let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "all-threads" nil)))
	  (jdi-time-end "jdi-virtual-machine-get-threads:all-threads")
	  (jdi-debug "number of threads:%s" (bindat-get-field reply :threads))
	  (setf (jdi-virtual-machine-threads vm)
			(loop for thread in (bindat-get-field reply :thread)
				  collect (jdi-virtual-machine-get-object-create 
						   vm
						   (make-jdi-thread :id (bindat-get-field thread :id))))))))

(defun jdi-virtual-machine-get-top-level-thread-groups (vm)
  (jdi-debug "jdi-virtual-machine-get-top-level-thread-groups")
  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) "top-level-thread-groups" nil)))
	(jdi-debug "jdi-virtual-machine-get-top-level-thread-groups:number = %s" (bindat-get-field reply :groups))
	(loop for group in (bindat-get-field reply :group)
		  collect (jdi-virtual-machine-get-object-create vm (make-jdi-thread-group :id (bindat-get-field group :id))))))

(defun jdi-virtual-machine-get-all-thread-groups (vm)
  (jdi-debug "jdi-virtual-machine-get-all-thread-groups")
  (jdi-virtual-machine-get-top-level-thread-groups vm))

(defun jdi-virtual-machine-get-classes-by-signature (vm signature)
  (jdi-debug "jdi-virtual-machine-get-classes-by-signature:%s" signature)
  (let ((reply (jdwp-send-command (jdi-virtual-machine-jdwp vm) 
								  "classes-by-signature"
								  `((:signature . ((:length . ,(length signature))
												   (:string . ,signature)))))))
	(jdi-debug "number of classes matched:%s" (bindat-get-field reply :classes))
	(loop for class        in (bindat-get-field reply :class)
		  for type-id      =  (bindat-get-field class :type-id)
		  for ref-type-tag =  (bindat-get-field class :ref-type-tag)
		  for status       =  (bindat-get-field class :status)
		  for newclass     =  (jdi-virtual-machine-get-class-create vm type-id	
																	:signature signature
																	:ref-type-tag ref-type-tag
																	:status status)
		  collect newclass)))
														
(defun jdi-virtual-machine-get-suspended-threads (vm)
  (jdi-debug "jdi-virtual-machine-suspended-threads")
  (let* ((threads (jdi-virtual-machine-get-threads vm))
		 (suspendeds (mapcar 'jdi-thread-get-suspended-p threads)))
	(loop for suspended in suspendeds
		  for thread in threads
		  if suspended collect thread)))

(defun* jdi-virtual-machine-get-class-create (vm id &key signature ref-type-tag status)
  (let ((found (gethash id (jdi-virtual-machine-classes vm))))
	(jdi-debug "jdi-virtual-machine-get-class-create id=%s: %s" id (if found "found" "not found"))
	(if found
		(progn 
		  (if signature (setf (jdi-class-signature found) signature))
		  (if ref-type-tag (setf (jdi-class-ref-type-tag found) ref-type-tag))
		  (if status (setf (jdi-class-status found)) status))
	  (puthash id (setq found (make-jdi-class :virtual-machine vm
											  :id id
											  :signature signature
											  :ref-type-tag ref-type-tag
											  :status status))
			   (jdi-virtual-machine-classes vm)))
	found))

(defun jdi-virtual-machine-get-object-create (vm newobj)
  (jdi-debug "jdi-virtual-machine-get-object-create:%s" (jdi-object-id newobj))
  (let ((found (gethash (jdi-object-id newobj) (jdi-virtual-machine-objects vm))))
	(jdi-debug "jdi-virtual-machine-get-object-create:%s" (if found "found" "not found"))
	(unless found
	  (setf (jdi-mirror-virtual-machine newobj) vm)
	  (puthash (jdi-object-id newobj) (setq found newobj) (jdi-virtual-machine-objects vm)))
	found))

(defun jdi-thread-get-name (thread)
  (jdi-debug "jdi-thread-get-name")
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "thread-name" `((:thread . ,(jdi-thread-id thread))))))
	(setf (jdi-thread-name thread)
		  (jdwp-get-string reply :thread-name))
	(jdi-debug "thread-name=%s" (jdi-thread-name thread))
	(jdi-thread-name thread)))

(defun jdi-thread-get-status (thread)
  (jdi-debug "jdi-thread-get-status")
  (if (jdi-thread-status thread)
	  (values (jdi-thread-status thread) (jdi-thread-suspend-status thread))

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "thread-status" `((:thread . ,(jdi-thread-id thread))))))
	  (setf (jdi-thread-status thread)
			(bindat-get-field reply :thread-status)
			(jdi-thread-suspend-status thread)
			(bindat-get-field reply :suspend-status))
	  (jdi-debug "thread-status:%s suspend-status:%s" (jdi-thread-status thread) (jdi-thread-suspend-status thread))
	  (values (jdi-thread-status thread) (jdi-thread-suspend-status thread)))))

(defun jdi-thread-get-thread-group (thread)
  (jdi-debug "jdi-thread-get-thread-group:%s" (jdi-thread-id thread))
  (if (jdi-thread-thread-group thread)
	  (jdi-thread-thread-group thread)

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "thread-group" `((:thread . ,(jdi-thread-id thread))))))
	  (jdi-debug "jdi-thread-get-thread-group:group=%s" (bindat-get-field reply :group))
	  (setf (jdi-thread-thread-group thread) (jdi-virtual-machine-get-object-create 
											  (jdi-mirror-virtual-machine thread)
											  (make-jdi-thread-group :id (bindat-get-field reply :group)))))))

(defun jdi-thread-get-system-thread-p (thread)
  (jdi-debug "jdi-thread-get-system-thread-p:%s" (jdi-thread-id thread))
  (let ((group (jdi-thread-get-thread-group thread)))
	(jdi-thread-group-get-system-thread-p group)))

(defun jdi-thread-group-get-system-thread-p (thread-group)
  (jdi-debug "jdi-thread-group-get-system-thread-p:%s" (jdi-thread-group-id thread-group))
  (let ((name (jdi-thread-group-get-name thread-group)))
	(let ((parent (jdi-thread-group-get-parent thread-group)))
	  (if (string= name "main")
		  nil
		(if parent
			(jdi-thread-group-get-system-thread-p parent)
		  t)))))

(defun jdi-thread-group-get-name (thread-group)
  (jdi-debug "jdi-thread-group-get-name:%s" (jdi-thread-group-id thread-group))
  (if (jdi-thread-group-name thread-group)
	  (jdi-thread-group-name thread-group)
	
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread-group) "thread-group-name" `((:group . ,(jdi-thread-group-id thread-group))))))
	  (setf (jdi-thread-group-name thread-group) (jdwp-get-string reply :group-name)))))

(defun jdi-thread-group-get-parent (thread-group)
  (jdi-debug "jdi-thread-group-get-parent:%s" (jdi-thread-group-id thread-group))
  (if (jdi-thread-group-parent thread-group)
	  (if (equal (jdi-thread-group-parent thread-group) t)
		  nil
		(jdi-thread-group-parent thread-group))

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread-group) "thread-group-parent" `((:group . ,(jdi-thread-group-id thread-group))))))
	  (let ((group-id (bindat-get-field reply :parent-group)))
		(if (equal group-id [0 0 0 0 0 0 0 0])
			(progn
			  (setf (jdi-thread-group-parent thread-group) t)
			  nil)
		  (setf (jdi-thread-group-parent thread-group) (jdi-virtual-machine-get-object-create 
														(jdi-mirror-virtual-machine thread-group)
														(make-jdi-thread-group :id (bindat-get-field reply :parent-group)))))))))

(defun jdi-thread-group-get-children (thread-group)
  (jdi-debug "jdi-thread-group-get-children:%s" (jdi-thread-group-id thread-group))
  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread-group) "thread-group-children" `((:group . ,(jdi-thread-group-id thread-group))))))
	(setf (jdi-thread-group-child-threads thread-group) 
		  (loop for thread in (bindat-get-field reply :child-thread)
				collect (jdi-virtual-machine-get-object-create (jdi-mirror-virtual-machine thread-group)
															   (make-jdi-thread :id (bindat-get-field thread :child-thread)))))
	(setf (jdi-thread-group-child-groups thread-group) 
		  (loop for group in (bindat-get-field reply :child-group)
				collect (jdi-virtual-machine-get-object-create (jdi-mirror-virtual-machine thread-group)
															   (make-jdi-thread-group :id (bindat-get-field group :child-group)))))
	(values (jdi-thread-group-child-threads thread-group) (jdi-thread-group-child-groups thread-group))))

(defun jdi-thread-group-get-thread-groups (thread-group)
  (jdi-debug "jdi-thread-group-get-thread-groups:%s" (jdi-thread-group-id thread-group))
  (multiple-value-bind (child-threads child-groups) (jdi-thread-group-get-children thread-group)
	child-groups))

(defun jdi-thread-group-get-threads (thread-group)
  (jdi-debug "jdi-thread-group-get-threads:%s" (jdi-thread-group-id thread-group))
  (multiple-value-bind (child-threads child-groups) (jdi-thread-group-get-children thread-group)
	child-groups))

(defun jdi-thread-status-string (thread)
  (cond ((equal (jdi-thread-status thread) jdwp-thread-status-zombie) "Zombie")
		((equal (jdi-thread-status thread) jdwp-thread-status-running) "Running")
		((equal (jdi-thread-status thread) jdwp-thread-status-sleeping) "Sleeping")
		((equal (jdi-thread-status thread) jdwp-thread-status-monitor) "Monitor")
		((equal (jdi-thread-status thread) jdwp-thread-status-wait) "Wait")))

(defun jdi-thread-suspend-status-string (thread)
  (if (equal (jdi-thread-suspend-status thread) jdwp-suspend-status-suspended)
	  "Suspended"
	"Running"))

(defun jdi-virtual-machine-disconnect (vm)
  (jdwp-disconnect (jdi-virtual-machine-jdwp vm)))

(defun jdi-class-get-all-methods (class)
  (jdi-debug "jdi-class-get-all-methods")
  (let ((supers (jdi-class-get-all-super class)))
	(mappend 'jdi-class-get-methods (cons class supers))))

(defun jdi-class-get-methods (class)
  "returns a list of jdi-method in the class"
  (jdi-debug "jdi-class-get-methods:%s" (jdi-class-id class))
  (if (jdi-class-methods class)
	  (jdi-class-methods class)
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class) "methods" `((:ref-type . ,(jdi-class-id class))))))
	  (jdi-debug "number of methods:%s" (bindat-get-field reply :methods))
	  (setf (jdi-class-methods class)
			(loop for method in (bindat-get-field reply :method)
				  collect (make-jdi-method :virtual-machine (jdi-mirror-virtual-machine class)
										   :class class
										   :id (bindat-get-field method :method-id)
										   :name (jdwp-get-string method :name)
										   :signature (jdwp-get-string method :signature)
										   :mod-bits (bindat-get-field method :mod-bits))
				  do (jdi-debug "jdi-class-get-methods:name=%s:signature=%s" (jdwp-get-string method :name) (jdwp-get-string method :signature))))
	  (jdi-class-methods class))))

(defun jdi-class-get-signature (class)
  (jdi-debug "jdi-class-get-signature")
  (if (jdi-class-signature class)
	  (jdi-class-signature class)
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class) "signature" `((:ref-type . ,(jdi-class-id class))))))
	  (setf (jdi-class-signature class) (jdwp-get-string reply :signature)))))

(defun jdi-method-get-signature (method)
  (jdi-debug "jdi-method-get-signature")
  (if (jdi-method-signature method)
	  (jdi-method-signature method)
	(setf (jdi-method-signature method)
		  (jdi-method-signature (find-if (lambda (obj)
										   (equal (jdi-method-id method) (jdi-method-id obj)))
										 (jdi-class-get-methods (jdi-method-class method)))))))

(defun jdi-method-get-locations (method)
  (jdi-debug "jdi-method-get-locations:class=%s method=%s" (jdi-class-signature (jdi-method-class method)) (jdi-method-name method))
  (if (or (jdi-method-locations method)
		  (jdi-method-native-p method))
	  (jdi-method-locations method)
	(let ((reply (jdwp-send-command 
				  (jdi-mirror-jdwp method) "line-table" `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
														  (:method-id . ,(jdi-method-id method))))))
	  (jdi-trace "start=%s:end=%s:lines=%s" 
				 (jdwp-string-to-hex (bindat-get-field reply :start))
				 (jdwp-string-to-hex (bindat-get-field reply :end))
				 (bindat-get-field reply :lines))
	  (setf (jdi-method-locations method)
			(loop for line in (bindat-get-field reply :line)
				  collect (make-jdi-location :virtual-machine (jdi-mirror-virtual-machine method)
											 :class (jdi-method-class method)
											 :method method
											 :line-code-index (bindat-get-field line :line-code-index)
											 :line-number (bindat-get-field line :line-number))))
	  (dolist (location (jdi-method-locations method))
		(jdi-trace "line-number:%s line-code-index:%s" (jdi-location-line-number location) (jdi-location-line-code-index location)))
	  (jdi-method-locations method))))

(defun jdi-method-get-name (method)
  (jdi-debug "jdi-method-get-signature")
  (let ((methods (jdi-class-get-methods (jdi-method-class method))))
	(jdi-method-name (find-if (lambda (obj)
								(equal (jdi-method-id obj)
									   (jdi-method-id method)))
							  methods))))

(defvar jdi-start-time nil)

(defun jdi-time-start ()
  (setf jdi-start-time (current-time)))

(defun jdi-time-end (message)
  (jdi-info "benchmark: %s took %s seconds" message (float-time (time-subtract (current-time) jdi-start-time))))

(defun jdi-virtual-machine-set-breakpoint (vm signature line)
  "Set breakpoint and return a list of jdi-event-request"

  (jdi-debug "jdi-virtual-machine-set-breakpoint:signature=%s:line=%s" signature line)
  (jdi-time-start)
  (let ((result))
	(dolist (class (jdi-virtual-machine-get-classes-by-signature vm signature))
	  (dolist (location (jdi-class-get-locations-of-line class line))
		(let ((er (jdi-event-request-manager-create-breakpoint (jdi-virtual-machine-event-request-manager vm) location)))
		  (jdi-event-request-enable er)
		  (push er result))))
	result))

(defun jdi-method-location-by-line-code-index (method line-code-index)
  (jdi-trace "jdi-method-location-by-line-code-index:%s:%s:%s" (jdi-method-name method) line-code-index (length (jdi-method-locations method)))
  (if jdi-trace-flag
      (loop for loc in (jdi-method-locations method)
			do
			(jdi-trace "line-code-index:%s line-number:%s" (jdi-location-line-code-index loc) (jdi-location-line-number loc))))
  (let ((result))
    (loop for loc in (jdi-method-locations method)
		  while (>= (jdwp-vec-to-int line-code-index)
					(jdwp-vec-to-int (jdi-location-line-code-index loc)))
		  do (setq result loc))
    (if (null result)
		(setq result (car (last (jdi-method-locations method)))))
    (if result
		(jdi-trace "found at line-number:%s" (jdi-location-line-number result)))
    result))

(defun jdi-class-get-locations-of-line (class line-number)
  (jdi-debug "jdi-class-get-locations-of-line")
  (let ((result))
	(dolist (method (jdi-class-get-methods class))
	  (dolist (location (jdi-method-get-locations method))
		(if (equal line-number (jdi-location-line-number location))
			(push location result))))
	result))

(defun jdi-class-find-location (class method-id line-code-index)
  (jdi-debug "jdi-class-find-location:number of methods=%s" (length (jdi-class-methods class)))
  (let* ((method (find-if (lambda (method)
							(equal (jdi-method-id method) method-id))
						  (jdi-class-methods class)))
		 (location (if method (jdi-method-location-by-line-code-index method line-code-index))))
    (if location
		location
	  (jdi-error "failed to look line-code-index %s in class %s" line-code-index (jdi-class-name class)))))

(defun jdi-location-get-line-number (location)
  (jdi-debug "jdi-location-get-line-number:wanted-line-code-index=%s" (jdi-location-line-code-index location))
  (let ((methods (jdi-class-get-methods (jdi-location-class location))))
	(jdi-debug "jdi-location-get-line-number: methods=%s" (length methods))
	(setf (jdi-location-method location)
		  (find-if (lambda (obj)
					 (equal (jdi-method-id obj)
							(jdi-method-id (jdi-location-method location))))
				   methods))
	(let ((locations (jdi-method-get-locations (jdi-location-method location))))
	  (jdi-debug "jdi-location-get-line-number: locations=%s" (length locations))
	  (let ((found))
		(loop for loc in locations
			  while (>= (jdwp-vec-to-int (jdi-location-line-code-index location))
						(jdwp-vec-to-int (jdi-location-line-code-index loc)))
			  do (setq found loc))
		(jdi-debug "jdi-location-get-line-number:found=%s" (if found "yes" "no"))
		(if found (setf (jdi-location-line-number location) (jdi-location-line-number found)))
		(jdi-location-line-number location)))))
  
(defun jdi-thread-get-frames (thread)
  (jdi-debug "jdi-thread-get-frames")
  (if (jdi-thread-frames thread)
	  (jdi-thread-frames thread)
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "frame-count" `((:thread . ,(jdi-thread-id thread))))))
	  (jdi-debug "number of frames:%s" (bindat-get-field reply :frame-count))
	  (let ((reply (jdwp-send-command (jdi-mirror-jdwp thread) "frames" `((:thread . ,(jdi-thread-id thread))
																		  (:start-frame . 0)
																		  (:length . ,(bindat-get-field reply :frame-count))))))
		(setf (jdi-thread-frames thread) 
			  (loop for frame           in (bindat-get-field reply :frame)
					for class-id        = (bindat-get-field frame :location :class-id)
					for method-id       = (bindat-get-field frame :location :method-id)
					for type            = (bindat-get-field frame :location :type)
					for line-code-index = (bindat-get-field frame :location :index)

					for class           = (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine thread) class-id)
					for method          = (make-jdi-method :virtual-machine (jdi-mirror-virtual-machine thread)
														   :id method-id
														   :class class)

					for location        = (make-jdi-location :virtual-machine (jdi-mirror-virtual-machine thread)
															 :class class
															 :method method
															 :type type
															 :line-code-index line-code-index)
					collect (make-jdi-frame :virtual-machine (jdi-mirror-virtual-machine thread)
											:thread thread
											:location location
											:id (bindat-get-field frame :id))))

		(loop for frame in (jdi-thread-frames thread)
			  do (jdi-debug "thread:%s frame:%s" (jdi-thread-id thread) (jdi-frame-id frame)))
		(jdi-thread-frames thread)))))

(defun jdi-thread-get-suspended-p (thread)
  (jdi-debug "jdi-thread-get-suspended-p")
  (multiple-value-bind (status suspend-status) (jdi-thread-get-status thread)
	(= suspend-status jdwp-suspend-status-suspended)))

(defun jdi-thread-resume (thread)
  (jdi-debug "jdi-thread-resume")
  (jdwp-send-command (jdi-mirror-jdwp thread) "thread-resume" `((:thread . ,(jdi-thread-id thread)))))

(defun jdi-thread-send-step (thread depth)
  (jdi-debug "jdi-thread-send-step:depth=%s" depth)
  (let* ((vm (jdi-mirror-virtual-machine thread))
		 (erm (jdi-virtual-machine-event-request-manager vm))
		 (er (jdi-event-request-manager-create-step erm thread depth)))
	(jdi-event-request-enable er)
	(jdi-thread-resume thread)))

(defun jdi-variable-sigbyte (variable)
  (string-to-char (jdi-variable-signature variable)))

(defun jdi-method-get-variables (method)
  (jdi-debug "jdi-method-get-variables")
  (if (jdi-method-variables method)
	  (jdi-method-variables method)

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp method)
									"variable-table"
									`((:ref-type . ,(jdi-class-id (jdi-method-class method)))
									  (:method-id . ,(jdi-method-id method))))))
	  (jdi-trace "variable-table arg-count:%s slots:%s" (bindat-get-field reply :arg-cnt) (bindat-get-field reply :slots))

	  (setf (jdi-method-variables method)
			(loop for slot in (bindat-get-field reply :slot)
					
				  for code-index = (jdwp-get-int slot :code-index)
				  for name       = (jdwp-get-string slot :name)
				  for signature  = (jdwp-get-string slot :signature)
				  for length     = (bindat-get-field slot :length)
				  for slot2      = (bindat-get-field slot :slot)
				  do (jdi-trace "slot:%s code-index:%s length:%s name:%s signature:%s generic-signature:%s" 
								slot2 code-index length name signature nil)
				  collect (make-jdi-variable :virtual-machine (jdi-mirror-virtual-machine method)
											 :code-index code-index
											 :name name
											 :signature signature
											 :length length
											 :slot slot2)))
	  (jdi-method-variables method))))

(defun jdi-frame-get-visible-variables (frame)
  (jdi-debug "jdi-frame-get-visible-variables")
  ;; we don't cache the variables as the location within the same frame might change
  (let ((variables (jdi-method-get-variables (jdi-location-method (jdi-frame-location frame)))))
	(loop for variable in variables

		  for line-code = (jdwp-vec-to-int (jdi-location-line-code-index (jdi-frame-location frame)))
		  if (and (>= line-code (jdi-variable-code-index variable))
				  (<  line-code (+ (jdi-variable-code-index variable) (jdi-variable-length variable))))
		  collect variable)))

(defun jdi-frame-get-values (frame variables)
  (jdi-debug "jdi-frame-get-values")
  (let ((data `((:thread . ,(jdi-thread-id (jdi-frame-thread frame)))
				(:frame  . ,(jdi-frame-id frame))
				(:slots  . ,(length variables))
				,(nconc (list :slot)
						(mapcar (lambda (variable) 
								  `((:slot . ,(jdi-variable-slot variable))
									(:sigbyte . ,(jdi-variable-sigbyte variable))))
								variables)))))
	(let ((reply (jdwp-send-command 
				  (jdi-mirror-jdwp frame) 
				  "stack-get-values" 
				  data)))
	  (loop for variable in variables
			for value in (bindat-get-field reply :value)
			collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine frame)
									:type (bindat-get-field value :slot-value :type)
									:value (bindat-get-field value :slot-value :u :value))))))

(defun jdi-value-object-p (value)
  (equal (jdi-value-type value) jdwp-tag-object))

(defun jdi-class-name (class-or-signature)
  (jdi-debug "jdi-class-name")
  (if (null class-or-signature)
	  "null"
	(let* ((class-name (if (stringp class-or-signature) class-or-signature (jdi-class-signature class-or-signature))))
	  (cond ((string= class-name "I")
			 (setq class-name "int"))
			(t
			 (setq class-name (replace-regexp-in-string ".*/" "" class-name))
			 (setq class-name (replace-regexp-in-string ";" "" class-name))))
	  class-name)))

(defun jdi-jni-to-print (sig &optional short)
  "Convert a string of JNI signature to list of printable characters.

If second parameter short is true, it will only return the class name instead of the
fully qualified class name.

For method-type, the return type is returned as the first element of the list.

Implemented according to
http://java.sun.com/j2se/1.5.0/docs/guide/jni/spec/types.html#wp428"
  (let ((array-count 0)
		result)
	(while (> (length sig) 0)
	  (let ((first (substring sig 0 1)))
		(cond ((string= first "Z") (push "boolean" result) (setq sig (substring sig 1)))
			  ((string= first "B") (push "byte"    result) (setq sig (substring sig 1)))
			  ((string= first "C") (push "char"    result) (setq sig (substring sig 1)))
			  ((string= first "S") (push "short"   result) (setq sig (substring sig 1)))
			  ((string= first "I") (push "int"     result) (setq sig (substring sig 1)))
			  ((string= first "J") (push "long"    result) (setq sig (substring sig 1)))
			  ((string= first "F") (push "float"   result) (setq sig (substring sig 1)))
			  ((string= first "D") (push "double"  result) (setq sig (substring sig 1)))
			  ((string= first "V") (push "void"    result) (setq sig (substring sig 1)))
			  ((string= first "L") 
			   (let ((end (string-match ";" sig)))
				 (unless end
				   (error "jni class without ;"))
				 (push (if short
						   (replace-regexp-in-string ".*/" "" (substring sig 1 end))
						 (replace-regexp-in-string "/" "." (substring sig 1 end)))
					   result)
				 (setq sig (substring sig (1+ end)))))

			  ;; for array type, we will process it later, so handling of multiple dimension
			  ;; array will be easier
			  ((string= first "[") (incf array-count)      (setq sig (substring sig 1)))

			  ((string= first "(") 
			   (let ((end (string-match ")" sig)))
				 (unless end
				   (error "jni method without )"))
				 ;; we will just move the return type to the front and remove the parenthesis
				 (let ((return-type (substring sig (1+ end)))
					   (without-paren (substring sig 1 end)))
				   (setq sig (concat return-type without-paren)))))
									
			  (t (error "invalid jni signature:%s" first)))

		;; append [] at the end of arrays
		(unless (or (string= first "[")
					(= array-count 0))
		  (let ((lasttype (car result)))
			(dotimes (i array-count)
			  (setq lasttype (concat lasttype "[]")))
			(setq array-count 0)
			(setq result (cons lasttype (cdr result)))))))
	(reverse result)))

(eval-when-compile
  (assert (equal (jdi-jni-to-print "ZBCSIJFD") (list "boolean" "byte" "char" "short" "int" "long" "float" "double")))
  (assert (equal (jdi-jni-to-print "Ljava/lang/String;") (list "java.lang.String")))
  (assert (equal (jdi-jni-to-print "[I") (list "int[]")))
  (assert (equal (jdi-jni-to-print "[[I") (list "int[][]")))
  (assert (equal (jdi-jni-to-print "[[Ljava/lang/String;") (list "java.lang.String[][]")))
  (assert (equal (jdi-jni-to-print "[[Ljava/lang/String;" t) (list "String[][]")))
  (assert (equal (jdi-jni-to-print "(I)J") (list "long" "int")))
  (assert (equal (jdi-jni-to-print "(ILjava/lang/String;[I)J" t) (list "long" "int" "String" "int[]"))))

(defun jdi-class-get-name (class)
  (let ((signature (jdi-class-get-signature class)))
	(jdi-class-name signature)))

(defun jdi-value-get-class (value)
  "populate the jdi-value-class field"
  (jdi-debug "jdi-value-get-class:type=%s value=%s" (jdi-value-type value) (jdi-value-value value))
  (if (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	  nil

	(if (jdi-value-class value)
		(jdi-value-class value)

	  (let ((reply (jdwp-send-command (jdi-mirror-jdwp value) 
									  "reference-type" 
									  `((:object . ,(jdi-value-value value))))))
		(jdi-debug "jdi-value-get-class:type-id=%s" (bindat-get-field reply :type-id))
		(setf (jdi-value-class value) (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine value) (bindat-get-field reply :type-id)))
		(jdi-value-class value)))))

(defun jdi-value-array-display-string (value size)
  "for array of three dimension, return i[2][][]."
  (let ((class (jdi-value-get-class value)))
	(let ((signature (jdi-class-get-signature class)))
	  (let ((sig (string-to-list signature))
			(suffix ""))
		(pop sig)
		(loop while (equal (car sig) jdwp-tag-array)
			  do
			  (setf suffix (concat suffix "[]"))
			  (pop sig))
		(format "%s[%s]%s" 
				(jdi-class-name (concat sig))
				size
				suffix)))))

(defun jdi-value-get-array-length (value)
  (jdi-debug "jdi-value-get-array-length:%s" (jdi-value-value value))
  (if (jdi-value-array-length value)
	  (jdi-value-array-length value)

	(let ((reply (jdwp-send-command 
				  (jdi-mirror-jdwp value) 
				  "array-length" 
				  `((:array-object . ,(jdi-value-value value))))))
	  (setf (jdi-value-array-length value) (bindat-get-field reply :array-length)))))

(defun jdi-format-string (str)
  "Truncate and escape the string to be displayed."
  (with-output-to-string
	(let ((print-escape-newlines t)
		  (print-escape-nonascii t))
	  (prin1 str))))

(defun jdi-value-has-children-p (value)
  (jdi-debug "jdi-value-has-children-p")
  (and (not (equal (jdi-value-value value)
				   [0 0 0 0 0 0 0 0]))
	   (or (equal (jdi-value-type value) jdwp-tag-object)
		   (equal (jdi-value-type value) jdwp-tag-array))))

(defun jdi-field-static-p (field)
  (not (equal (logand (jdi-field-mod-bits field) jdi-access-static) 0)))

(defun jdi-field-final-p (field)
  (not (equal (logand (jdi-field-mod-bits field) jdi-access-final) 0)))

(defun jdi-remove-duplicated (fields)
  "Return a list of jdi-fields, without duplicated names, the first field is kept."
  (let (names filtered)
	(mapc (lambda (field)
			(when (not (member (jdi-field-name field) names))
			  (push (jdi-field-name field) names)
			  (push field filtered)))
		  fields)
	filtered))
	
(defun jdi-class-get-all-fields (class)
  (jdi-debug "jdi-class-get-all-fields:%s" (jdi-class-id class))
  (let ((supers (jdi-class-get-all-super class)))
	(jdi-debug "jdi-class-get-all-fields:%s:%s" (jdi-class-id class) (loop for super in supers
																		   collect (jdi-class-id super)))
	(mappend 'jdi-class-get-fields (cons class supers))))

(defun jdi-value-get-all-fields (value)
  (jdi-debug "jdi-value-get-all-fields:%s" (jdi-value-value value))
  (let ((class (jdi-value-get-class value)))
	(let ((supers (jdi-class-get-all-super class)))
	  (mappend 'jdi-class-get-fields (cons class supers)))))

(defun jdi-value-get-values (value fields)
  "Gets the value of multiple instance and/or static fields in this object. 
The Fields must be valid for this ObjectReference; that is, they must be from the mirrored object's class or a superclass of that class."

  (jdi-debug "jdi-value-get-values")
  (let ((static-fields (loop for field in fields
							 if (jdi-field-static-p field) collect field))
		(nonstatic-fields (loop for field in fields
								unless (jdi-field-static-p field) collect field))
		(results (make-hash-table)))
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp value) "object-get-values"
									`((:object . ,(jdi-value-value value))
									  (:fields . ,(length nonstatic-fields))
									  ,(nconc (list :field)
											  (mapcar (lambda (field)
														`((:id . ,(jdi-field-id field))))
													  nonstatic-fields))))))
	  (loop for field in nonstatic-fields
			for value2 in (bindat-get-field reply :value)
			do (puthash field (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
											  :type (bindat-get-field value2 :value :type)
											  :value (bindat-get-field value2 :value :u :value))
						results)
			do (jdi-debug "jdi-value-get-values: nonstatic type=%s value=%s" (bindat-get-field value2 :value :type)
						  (bindat-get-field value2 :value :u :type)))
	  (let ((class (jdi-value-get-class value)))
		(let ((reply (jdwp-send-command (jdi-mirror-jdwp value) "reference-get-values"
										`((:ref-type . ,(jdi-class-id class))
										  (:fields . ,(length static-fields))
										  ,(nconc (list :field)
												  (mapcar (lambda (field)
															`((:id . ,(jdi-field-id field))))
														  static-fields))))))
		  (loop for field in static-fields
				for value2 in (bindat-get-field reply :value)
				do (puthash field (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
												  :type (bindat-get-field value2 :value :type)
												  :value (bindat-get-field value2 :value :u :value))
							results)
				do (jdi-debug "jdi-value-get-values: static type=%s value=%s" (bindat-get-field value2 :value :type)
							  (bindat-get-field value2 :value :u :type)))
		  (loop for field in fields
				collect (gethash field results)))))))

(defun jdi-class-get-all-super (class &optional supers)
  (jdi-debug "jdi-class-get-all-super")
  (if (null class) 
	  supers
	(let ((super (jdi-class-get-super class)))
	  (if super
		  (jdi-class-get-all-super super (cons super supers))
		supers))))

(defun jdi-class-get-super (class)
  "populate the jdi-class-super of this class"
  (jdi-debug "jdi-class-get-super:id=%s" (jdi-class-id class))
  (if (jdi-class-super class)
	  (if (equal (jdi-class-super class) t)
		  nil
		(jdi-class-super class))
	(let ((reply (jdwp-send-command 
				  (jdi-mirror-jdwp class) 
				  "superclass" 
				  `((:class . ,(jdi-class-id class))))))
	  (if (equal (bindat-get-field reply :superclass)
				 [0 0 0 0 0 0 0 0])
		  (progn 
			(setf (jdi-class-super class) t)
			nil)
		(setf (jdi-class-super class) (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine class) (bindat-get-field reply :superclass)))
		(jdi-class-super class)))))

(defun jdi-method-invoke (method)
  "Invoke a simple method (do not require arguments) in the class."
  (jdi-debug "jdi-class-invoke-method:%s" (jdi-method-name method))
  (jdwp-send-command (jdi-mirror-jdwp method) "class-invoke-method"
					 `((:class . ,(jdi-class-id (jdi-method-class method)))
					   (:thread . ,(jdi-virtual-machine-suspended-thread-id (jdi-mirror-virtual-machine method)))
					   (:method-id . ,(jdi-method-id method))
					   (:arguments . 0)
					   (:options . ,jdwp-invoke-single-threaded))))

(defun jdi-class-get-interfaces (class)
  "Gets the interfaces directly implemented by this class. 
Only the interfaces that are declared with the 'implements' keyword in this class are included. "
  (jdi-debug "jdi-class-get-interfaces:id=%s" (jdi-class-id class))
  (if (or (jdi-class-interfaces-count class)
		  (string= (jdi-class-signature class) "Ljava/lang/Object;"))
	  (jdi-class-interfaces class)

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class) 
									"interfaces" 
									`((:ref-type . ,(jdi-class-id class))))))
	  (setf (jdi-class-interfaces class)
			(loop for interface in (bindat-get-field reply :interface)
				  collect (jdi-virtual-machine-get-class-create (jdi-mirror-virtual-machine class) (bindat-get-field interface :type))))
	  (setf (jdi-class-interfaces-count class) (bindat-get-field reply :interfaces))
	  (jdi-class-interfaces class))))

(defun jdi-class-get-all-interfaces (class)
  "Gets the interfaces directly and indirectly implemented by this class. 
Interfaces returned by interfaces()  are returned as well all superinterfaces."
  (jdi-debug "jdi-class-get-all-interfaces")
  (mappend 'jdi-class-get-interfaces (cons class (jdi-class-get-all-super class))))

(defun jdi-access-string (bits)
  (let ((str)
		(map `((,jdi-access-public       . "[public]")
			   (,jdi-access-private      . "[private]")
			   (,jdi-access-protected    . "[protected]")
			   (,jdi-access-static       . "[static]")
			   (,jdi-access-final        . "[final]")
			   (,jdi-access-synchronized . "[final]")
			   (,jdi-access-volatile     . "[volatile]")
			   (,jdi-access-transient    . "[transient]")
			   (,jdi-access-native       . "[native]")
			   (,jdi-access-abstract     . "[abstract]"))))
    (mapc (lambda (m)
			(if (not (equal (logand bits (car m)) 0))
				(setq str (concat str (cdr m)))))
		  map)
    str))

(defun jdi-field-mod-bits-string (field)
  (jdi-access-string (jdi-field-mod-bits field)))

(defun jdi-class-get-fields (class)
  "Get all the fields in this class only."
  (jdi-debug "jdi-class-get-fields:%s:%s" (jdi-class-id class) (if (listp (jdi-class-fields class))
																   (length (jdi-class-fields class))
																 "0"))
  (if (jdi-class-fields class)
	  (if (equal (jdi-class-fields class) t)
		  nil
		(jdi-class-fields class))

	(let ((reply (jdwp-send-command (jdi-mirror-jdwp class) "fields" `((:ref-type . ,(jdi-class-id class))))))
	  (jdi-debug "jdi-class-get-fields: %s's fields:%s" (jdi-class-id class) (bindat-get-field reply :declared))
	  (if (bindat-get-field reply :field)
		  (progn
			(setf (jdi-class-fields class)
				  (loop for field in (bindat-get-field reply :field)
						collect (make-jdi-field :virtual-machine (jdi-mirror-virtual-machine class)
												:id (bindat-get-field field :id)
												:name (jdwp-get-string field :name)
												:signature (jdwp-get-string field :signature)
												:generic-signature (jdwp-get-string field :generic-signature)
												:mod-bits (bindat-get-field field :mod-bits))
						do (jdi-debug "jdi-class-get-fields:%s id:%s name:%s signature:%s generic-signature:%s modbits:%s" 
									  (jdi-class-id class)
									  (bindat-get-field field :id) 
									  (jdwp-get-string field :name) 
									  (jdwp-get-string field :signature)
									  (jdwp-get-string field :generic-signature)
									  (bindat-get-field field :mod-bits))))
			(jdi-debug "jdi-class-get-fields:saved %s fields" (length (jdi-class-fields class)))
			(jdi-class-fields class))

		(setf (jdi-class-fields class) t)
		nil))))

(defun jdi-value-array-get-values (value)
  (jdi-debug "jdi-value-get-array-values:value=%s" (jdi-value-value value))

  (unless (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	(let* ((length (jdi-value-get-array-length value))
		   (reply (jdwp-send-command (jdi-mirror-jdwp value) "array-get-values"
									 `((:array-object . ,(jdi-value-value value))
									   (:first-index . 0)
									   (:length . ,length))))
		   (array (jdwp-unpack-arrayregion (jdi-mirror-jdwp value) reply)))
	  (jdi-trace "got array-get-values:%s" array)
	  (if (or (= (bindat-get-field array :type) jdwp-tag-object)
			  (= (bindat-get-field array :type) jdwp-tag-array))
		  (loop for value-reply in (bindat-get-field array :value)
				for i from 0 to (- (jdi-value-array-length value) 1)
				collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
										:type (bindat-get-field value-reply :value :type)
										:value (bindat-get-field value-reply :value :u :value)))
		(loop for value-reply in (bindat-get-field array :value)
			  for i from 0 to (- (jdi-value-array-length value) 1)
			  collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
									  :type (bindat-get-field array :type)
									  :value (bindat-get-field value-reply :value)))))))

(defun jdi-handle-breakpoint-event (jdwp event)
  (jdi-debug "jdi-handle-breakpoint-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (thread-id (bindat-get-field event :u :thread))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))

		 (class (jdi-virtual-machine-get-class-create vm class-id))
		 (method (make-jdi-method :virtual-machine vm
								  :id method-id
								  :class class))

		 (location (make-jdi-location :virtual-machine vm
									  :class class
									  :method method
									  :line-code-index line-code-index))
		 (thread (jdi-virtual-machine-get-object-create vm (make-jdi-thread :id thread-id)))
		 (frame (make-jdi-frame :virtual-machine vm
								:thread thread
								:location location)))

	(setf (jdi-thread-frames thread) nil)
	(setf (jdi-thread-suspend-status thread) jdwp-suspend-status-suspended)

	(setf (jdi-virtual-machine-suspended-frames vm) (nreverse (cons frame (jdi-virtual-machine-suspended-frames vm))))

	(setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
	(run-hook-with-args 'jdi-breakpoint-hooks thread location)))
  
(defun jdi-handle-step-event (jdwp event)
  (jdi-debug "jdi-handle-step-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (thread-id (bindat-get-field event :u :thread))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))

		 (class (jdi-virtual-machine-get-class-create vm class-id))
		 (method (make-jdi-method :virtual-machine vm
								  :id method-id
								  :class class))
		 (location (make-jdi-location :virtual-machine vm
									  :class class
									  :method method
									  :line-code-index line-code-index))
		 (thread (jdi-virtual-machine-get-object-create vm (make-jdi-thread :id thread-id)))
		 (frame (make-jdi-frame :virtual-machine vm
								:thread thread
								:location location)))

	(setf (jdi-thread-frames thread) nil)

	(setf (jdi-virtual-machine-suspended-frames vm) (nreverse (cons frame (jdi-virtual-machine-suspended-frames vm))))

	(setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
	(run-hook-with-args 'jdi-step-hooks thread location)))

(defun jdi-handle-class-prepare-event (jdwp event)
  (jdi-debug "jdi-handle-class-prepare-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (type-id (bindat-get-field event :u :type-id))
		 (signature (jdwp-get-string event :u :signature))
		 (newclass (jdi-virtual-machine-get-class-create vm type-id :signature signature)))
	(jdi-debug "class-loaded:%s" signature)
;; 	(puthash type-id newclass (jdi-virtual-machine-classes vm))
;; 	(puthash signature (cons newclass (gethash signature (jdi-virtual-machine-classes-by-signature vm)))
;; 			 (jdi-virtual-machine-classes-by-signature vm))
	(run-hook-with-args 'jdi-class-prepare-hooks newclass)))

(defun jdi-handle-class-unload-event (jdwp event)
  (jdi-debug "jdi-handle-class-unload-event")
  ())

(defun jdi-handle-vm-death (jdwp event)
  (jdi-debug "jdi-handle-vm-death")
  (let ((vm (jdwp-get jdwp 'jdi-virtual-machine)))
	(run-hook-with-args 'jdi-detached-hooks vm)))

(defun jdi-class-name-to-class-signature (class-name)
  "Converts a.b.c class name to JNI class signature."
  (let ((buf class-name))
	(setf buf (replace-regexp-in-string "\\." "/" buf))
	(setf buf (format "L%s;" buf))
	(jdi-debug "jdi-class-name-to-class-signature:%s:%s" class-name buf)
	buf))

(defun jdi-value-instance-of-p (value signature)
  (jdi-debug "jdi-value-instance-of-p:signature=%s" signature)
  (let* ((class (jdi-value-get-class value))
		 (supers (jdi-class-get-all-super class))
		 (interfaces (jdi-class-get-all-interfaces class))
		 (signatures (mapcar 'jdi-class-get-signature (cons class (append supers interfaces)))))
	(jdi-debug "jdi-value-instance-of-p:all-signatures=%s" signatures)
	(member signature signatures)))

(defun jdi-value-extract-generic-class-name (generic-signature)
  (string-match "<L.*/\\(.*\\);>" generic-signature)
  (match-string 1 generic-signature))

(defun jdi-method-native-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-native) 0)))

(defun jdi-method-static-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-static) 0)))

(defun jdi-value-invoke-method (value thread method arguments options)
  "Invoke the method on the value on the thread, if method is a string, it will pick the first method that matches the method-name."
  (jdi-debug "jdi-value-invoke-method")
  (when (stringp method)
	(setq method (find-if (lambda (obj)
							(equal (jdi-method-name obj) method))
						  (jdi-class-get-all-methods (jdi-value-get-class value)))))
  (when method
	(let ((reply (jdwp-send-command (jdi-mirror-jdwp value) "object-invoke-method"
									`((:object . ,(jdi-value-value value))
									  (:thread . ,(jdi-thread-id thread))
									  (:class . ,(jdi-class-id (jdi-value-get-class value)))
									  (:method-id . ,(jdi-method-id method))
									  (:arguments . 0)
									  (:options . ,jdwp-invoke-single-threaded)))))
	  (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
					  :type (bindat-get-field reply :return-value :type)
					  :value (bindat-get-field reply :return-value :u :value)))))

(defvar jdi-breakpoint-hooks nil
  "callback to be called when breakpoint is hit, called with (jdi-thread jdi-location)")

(defvar jdi-step-hooks nil
  "callback to be called when execution is stopped from stepping, called with (jdi-virtual-machine thread-id class-id method-id line-code-index)")

(defvar jdi-detached-hooks nil
  "callback to be called when debuggee detached from us, called with (jdi-virtual-machine)")

(defvar jdi-class-prepare-hooks nil
  "handler to be called when a class is prepared, called with (jdi-class)")

(defvar jdi-class-unload-hooks nil
  "handler to be called when a class is unloaded, called with (jdi-class)")

(defun jdi-handle-event (jdwp event)
  (jdi-debug "jdi-handle-event")
  (let* ((handlers (list 
					`(,jdwp-event-breakpoint    . jdi-handle-breakpoint-event)
					`(,jdwp-event-single-step   . jdi-handle-step-event)
					`(,jdwp-event-class-prepare . jdi-handle-class-prepare-event)
					`(,jdwp-event-class-unload  . jdi-handle-class-unload-event)
					`(,jdwp-event-vm-death      . jdi-handle-vm-death)
					))
		 (event-kind (if (integerp event) event (bindat-get-field event :event-kind)))
		 (handler (find-if (lambda (pair)
							 (equal event-kind (car pair)))
						   handlers)))
	(if handler
		(funcall (cdr handler) jdwp event)
	  (jdi-error "do not know how to handle event:%s" event))))

(add-hook 'jdwp-event-hooks 'jdi-handle-event)

(provide 'jdi)