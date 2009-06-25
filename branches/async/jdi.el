(require 'elog)
(require 'jdwp)
(require 'cont)

(elog-make-logger jdi)

(defstruct jdi-mirror
  virtual-machine)

(defstruct jdi-virtual-machine
  jdwp

  (event-request-manager (make-jdi-event-request-manager))
  host 
  port
  classes ;; hash table where key=class-id value=jdi-class
  classes-by-signature ;; hash table where key=signature value=jdi-class

  threads

  suspended-thread-id
  )

;; as java returns interfaces using all-classes command, this might represent an interface
(defstruct (jdi-class (:include jdi-mirror))
  id
  ;; jni-style
  signature 
  ref-type-tag
  status
  ;; list of nonstatic jdi-field
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

  ;; list of jdi-location, from variable-table command
  values

  ;; link back to our containing jdi-class
  class	
  )

(defstruct (jdi-location (:include jdi-mirror))
  ;; this might have to be resolved
  line-number 
  class-id
  method-id
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

(defstruct (jdi-thread (:include jdi-mirror))
  id
  name
  status
  suspend-status
  frames
  )

(defstruct (jdi-frame (:include jdi-mirror))
  id
  values
  thread
  location)

(defstruct (jdi-value (:include jdi-mirror)) ;; in java, this corresponds to both jdi-variable and jdi-value
  name
  signature
  generic-signature
  type
  value
  (string "null")

  ;; this is only valid when this is top level (not contained in any other object)
  slot

  ;; for values that are inside method
  code-index
  code-index-length

  ;; for values which are of type object
  class	;; jdi-class

  ;; for values which are of type array
  array-length 

  ;; for object/array, this is the list that are displayed when the tree is expanded
  ;; list of jdi-values
  values)

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
						  (:class-id . ,(jdi-location-class-id location))
						  (:method-id . ,(jdi-location-method-id location))
						  (:index     . ,(jdi-location-line-code-index location))))
		 (data `((:event-kind     . ,jdwp-event-breakpoint)
				 (:suspend-policy . ,jdwp-suspend-policy-event-thread)
				 (:modifiers      . 1)
				 (:modifier       ((:mod-kind .  7)
								   (:location .  ,location-data))))))
	(make-jdi-event-request :virtual-machine (jdi-mirror-virtual-machine location) :data data)))

(defun jdi-event-request-manager-create-step (erm thread depth)
  (let ((data `((:event-kind     . 1)
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
  (lexical-let ((er er))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp er) "set" (jdi-event-request-data er))
	  (jdi-trace "received requestid:%d" (bindat-get-field reply :request-id))
	  (setf (jdi-event-request-id er) (bindat-get-field reply :request-id))
	  (cont-values t))))

(defun jdi-event-request-disable (er)
  (lexical-let ((er er))
	(cont-bind (reply error jdwp id) 
	  (jdwp-send-command (jdi-mirror-jdwp er) "clear" `((:event . ,jdwp-event-breakpoint) (:request-id . ,(jdi-event-request-id er))))

	  (jdi-trace "cleared event request")
	  (cont-values t))))

(defun jdi-virtual-machine-connect (vm)
  "[ASYNC] returns t if success, nil on failure"
  (lexical-let ((jdwp (jdi-virtual-machine-jdwp vm))
				(vm vm))
	(cont-bind (result) (jdwp-connect jdwp (jdi-virtual-machine-host vm) (jdi-virtual-machine-port vm))
	  (jdwp-put jdwp 'jdi-virtual-machine vm)
	  (if (not (eq result t))
		  (progn
			(jdi-error "failed to connect:%s" result)
			(cont-values nil))
		(cont-bind (reply error jdwp id) (jdwp-send-command jdwp "version" nil)
		  (jdi-trace "description: \n%s" (jdwp-get-string reply :description))
		  (jdi-trace "major      : %s"   (bindat-get-field reply :jdwp-major))
		  (jdi-trace "minor      : %s"   (bindat-get-field reply :jdwp-minor))
		  (jdi-trace "version    : %s"   (jdwp-get-string reply :vm-version))
		  (jdi-trace "name       : %s"   (jdwp-get-string reply :vm-name))
		  (cont-bind (reply error jdwp id) (jdwp-send-command jdwp "capabilities-new" nil)
			(jdi-trace "capabilities-new:%s" reply)
			(cont-bind (reply error jdwp id) (jdwp-send-command jdwp "all-classes" nil)
			  (jdi-info "number of classes loaded:%d" (bindat-get-field reply :classes))
			  (setf (jdi-virtual-machine-classes vm) (make-hash-table :test 'equal))
			  (setf (jdi-virtual-machine-classes-by-signature vm) (make-hash-table :test 'equal))
			  (loop for class in       (bindat-get-field reply :class)
					for type-id      = (bindat-get-field class :type-id)
					for signature    = (jdwp-get-string class :signature)
					for ref-type-tag = (bindat-get-field class :ref-type-tag)
					for status       = (bindat-get-field class :status)
					for newclass     = (make-jdi-class 
										:virtual-machine vm
										:id type-id	
										:signature signature
										:ref-type-tag ref-type-tag
										:status status)
					do
					(jdi-trace "signature:%s id:%s" signature type-id)
					(puthash type-id newclass (jdi-virtual-machine-classes vm))
					(let ((l (gethash signature (jdi-virtual-machine-classes-by-signature vm))))
					  (if l
						  (puthash signature (cons newclass l) (jdi-virtual-machine-classes-by-signature vm))
						(puthash signature (list newclass) (jdi-virtual-machine-classes-by-signature vm)))))
			  (cont-values t))))))))

(defun jdi-virtual-machine-get-threads (vm)
  (jdi-info "jdi-virtual-machine-get-threads")
  (lexical-let ((vm vm))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-virtual-machine-jdwp vm) "all-threads" nil)
	  (jdi-info "number of threads:%s" (bindat-get-field reply :threads))
	  (setf (jdi-virtual-machine-threads vm)
			(loop for thread in (bindat-get-field reply :thread)
				  collect (make-jdi-thread :virtual-machine vm
										   :id (bindat-get-field thread :id))))
	  (cont-wait (progn
				   (mapc 'jdi-thread-get-name (jdi-virtual-machine-threads vm))
				   (mapc 'jdi-thread-get-status (jdi-virtual-machine-threads vm)))
		(cont-values)))))

(defun jdi-virtual-machine-suspended-threads (vm)
  (jdi-info "jdi-virtual-machine-suspended-threads")
  (loop for thread in (jdi-virtual-machine-threads vm)
		if (equal (jdi-thread-suspend-status thread) 1)
		collect thread))

(defun jdi-thread-get-name (thread)
  (jdi-info "jdi-thread-get-name")
  (lexical-let ((thread thread))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp thread) "thread-name" `((:thread . ,(jdi-thread-id thread))))
	  (setf (jdi-thread-name thread)
			(jdwp-get-string reply :thread-name))
	  (jdi-info "thread-name=%s" (jdi-thread-name thread))
	  (cont-values))))

(defun jdi-thread-get-status (thread)
  (jdi-info "jdi-thread-get-status")
  (lexical-let ((thread thread))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp thread) "thread-status" `((:thread . ,(jdi-thread-id thread))))
	  (setf (jdi-thread-status thread)
			(bindat-get-field reply :thread-status)
			(jdi-thread-suspend-status thread)
			(bindat-get-field reply :suspend-status))
	  (jdi-info "thread-status:%s suspend-status:%s" (jdi-thread-status thread) (jdi-thread-suspend-status thread))
	  (cont-values))))

(defun jdi-virtual-machine-disconnect (vm)
  (jdwp-disconnect (jdi-virtual-machine-jdwp vm)))

(defun jdi-class-get-methods (class)
  "[ASYNC] returns a list of jdi-method in the class"
  (jdi-info "jdi-class-get-methods:%s" (jdi-class-signature class))
  (lexical-let ((class class))
	(if (jdi-class-methods class)
		(cont-values t)
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp class) "methods" `((:ref-type . ,(jdi-class-id class))))
		(jdi-info "number of methods:%d" (bindat-get-field reply :methods))
		(setf (jdi-class-methods class)
			  (loop for method in (bindat-get-field reply :method)
					collect (make-jdi-method :virtual-machine (jdi-mirror-virtual-machine class)
											 :class class
											 :id (bindat-get-field method :method-id)
											 :name (jdwp-get-string method :name)
											 :signature (jdwp-get-string method :signature)
											 :mod-bits (bindat-get-field method :mod-bits))))
		(dolist (method (jdi-class-methods class))
		  (jdi-trace "method name:%s signature:%s id:%s" (jdi-method-name method) (jdi-method-signature method) (jdi-method-id method)))
		(if (jdi-class-super class)
			(jdi-class-get-methods (jdi-class-super class))
		  (cont-values t))))))

(defun jdi-method-get-locations (method)
  (jdi-info "jdi-method-get-locations:name=%s" (jdi-method-name method))
  (if (or (jdi-method-locations method)
		  (jdi-method-native-p method))
	  (cont-values)
	(lexical-let ((method method))
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp method) "line-table" `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
																				(:method-id . ,(jdi-method-id method))))
		(jdi-trace "start=%s:end=%s:lines=%d" 
				   (jdwp-string-to-hex (bindat-get-field reply :start))
				   (jdwp-string-to-hex (bindat-get-field reply :end))
				   (bindat-get-field reply :lines))
		(setf (jdi-method-locations method)
			  (loop for line in (bindat-get-field reply :line)
					collect (make-jdi-location :virtual-machine (jdi-mirror-virtual-machine method)
											   :class (jdi-method-class method)
											   :class-id (jdi-class-id (jdi-method-class method))
											   :method-id (jdi-method-id method)
											   :method method
											   :line-code-index (bindat-get-field line :line-code-index)
											   :line-number (bindat-get-field line :line-number))))
		(dolist (location (jdi-method-locations method))
		  (jdi-trace "line-number:%s line-code-index:%s" (jdi-location-line-number location) (jdi-location-line-code-index location)))
		(cont-values)))))

(defvar jdi-start-time nil)

(defun jdi-time-start ()
  (setf jdi-start-time (current-time)))

(defun jdi-time-end (message)
  (jdi-info "benchmark: %s took %s seconds" message (float-time (time-subtract (current-time) jdi-start-time))))

(defun jdi-class-get-all-line-locations (class)
  "[ASYNC] returns a list of jdi-location in the class"
  (jdi-info "jdi-class-get-all-line-locations")
  (lexical-let ((class class))
	(jdi-time-start)
	(cont-bind (result) (jdi-class-get-methods class)
	  (jdi-time-end "jdi-class-get-methods")
	  (jdi-info "calling jdi-method-get-locations for %s methods" (length (jdi-class-methods class)))
	  (cont-wait (mapc 'jdi-method-get-locations (jdi-class-methods class))
		(cont-values)))))

(defun jdi-virtual-machine-set-breakpoint (vm signature line)
  "[ASYNC] returns a jdi-event-request if a breakpoint is installed, nil otherwise"

  ;; TODO: handle this on class prepare also
  ;; we always return t, because there are simply too many cases to handle
  ;; 1. class not loaded yet
  ;; 2. class loaded but might be loaded again by another class loader
  ;; 3. class loaded but no code at line
  ;; eclipse simply snap the breakpoint to the nearest line, we can't do it
  ;; unless the class is loaded!

  (lexical-let ((vm vm)
				(signature signature)
				(line line)
				(classes (gethash signature (jdi-virtual-machine-classes-by-signature vm))))
	(jdi-trace "jdi-virtual-machine-set-breakpoint:signature=%s:line=%s" signature line)
	(jdi-trace "classes matched:%s" (length classes))
	(jdi-time-start)
	(cont-wait (mapc 'jdi-class-get-all-line-locations classes)
	  (jdi-time-end (format "jdi-class-get-all-line-locations for %s classes" (length classes)))
	  (let* ((methods (apply 'append (mapcar 'jdi-class-methods classes)))
			 (locations (apply 'append (mapcar 'jdi-method-locations methods)))
			 (location (find-if (lambda (loc) 
								  (equal (jdi-location-line-number loc) line))
								 locations)))
		(jdi-trace "number-of-methods:%s number-of-locations:%s found location:%s"
				   (length methods) (length locations) (if location "yes" "no"))
		(if location
			(lexical-let* ((erm (jdi-virtual-machine-event-request-manager vm))
						   (er (jdi-event-request-manager-create-breakpoint erm location)))
			  (cont-bind (result) (jdi-event-request-enable er)
				(cont-values er)))
		  (cont-values nil))))))

(defun jdi-method-location-by-line-code-index (method line-code-index)
  (jdi-trace "jdi-method-location-by-line-code-index:%s:%s:%d" (jdi-method-name method) line-code-index (length (jdi-method-locations method)))
  (if jdi-trace-flag
      (loop for loc in (jdi-method-locations method)
			do
			(jdi-trace "line-code-index:%s line-number:%d" (jdi-location-line-code-index loc) (jdi-location-line-number loc))))
  (let ((result))
    (loop for loc in (jdi-method-locations method)
		  while (>= (jdwp-vec-to-int line-code-index)
					(jdwp-vec-to-int (jdi-location-line-code-index loc)))
		  do (setq result loc))
    (if (null result)
		(setq result (car (last (jdi-method-locations method)))))
    (if result
		(jdi-trace "found at line-number:%d" (jdi-location-line-number result)))
    result))

(defun jdi-class-find-location (class method-id line-code-index)
  (let* ((method (find-if (lambda (method)
							(equal (jdi-method-id method) method-id))
						  (jdi-class-methods class)))
		 (location (if method (jdi-method-location-by-line-code-index method line-code-index))))
    (if location
		location
	  (jdi-error "failed to look line-code-index %s in class %s" line-code-index (jdi-class-name class)))))

(defun jdi-location-get-class-and-method-and-line-number (location)
  (jdi-info "jdi-location-get-class-and-method")
  (setf (jdi-location-class location) 
		(gethash (jdi-location-class-id location) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine location))))
  (when (jdi-location-class location)
	(let ((stored-loc (jdi-class-find-location (jdi-location-class location)
											   (jdi-location-method-id location)
											   (jdi-location-line-code-index location))))
	  (if stored-loc
		  (setf (jdi-location-method location)
				(jdi-location-method stored-loc)
				(jdi-location-line-number location)
				(jdi-location-line-number stored-loc))))))

(defun jdi-thread-get-frames (thread)
  (jdi-info "jdi-thread-get-frames")
  (lexical-let ((thread thread))
	(if (jdi-thread-frames thread)
		(cont-values)
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp thread) "frame-count" `((:thread . ,(jdi-thread-id thread))))
		(jdi-info "number of frames:%s" (bindat-get-field reply :frame-count))
		(cont-bind (reply error jdwp id) 
		  (jdwp-send-command (jdi-mirror-jdwp thread) "frames" `((:thread . ,(jdi-thread-id thread))
																 (:start-frame . 0)
																 (:length . ,(bindat-get-field reply :frame-count))))
		  (setf (jdi-thread-frames thread) 
				(loop for frame           in (bindat-get-field reply :frame)
					  for class-id        = (bindat-get-field frame :location :class-id)
					  for method-id       = (bindat-get-field frame :location :method-id)
					  for type            = (bindat-get-field frame :location :type)
					  for line-code-index = (bindat-get-field frame :location :index)
					  for location        = (make-jdi-location :virtual-machine (jdi-mirror-virtual-machine thread)
															   :class-id class-id
															   :method-id method-id
															   :type type
															   :line-code-index line-code-index)
					  collect (make-jdi-frame :virtual-machine (jdi-mirror-virtual-machine thread)
											  :thread thread
											  :location location
											  :id (bindat-get-field frame :id))))
		  (cont-bind () (jdi-classes-get-locations (loop for frame in (jdi-thread-frames thread)
														 for class-id = (jdi-location-class-id (jdi-frame-location frame))
														 for class = (gethash class-id (jdi-virtual-machine-classes (jdi-mirror-virtual-machine thread)))
														 if class
														 collect class))
			(mapc 'jdi-location-get-class-and-method-and-line-number 
				  (mapcar 'jdi-frame-location 
						  (jdi-thread-frames thread)))
			(cont-values)))))))

(defun jdi-thread-resume (thread)
  (jdi-info "jdi-thread-resume")
  (cont-bind (reply error jdwp id)
	(jdwp-send-command (jdi-mirror-jdwp thread) "thread-resume" `((:thread . ,(jdi-thread-id thread))))
	(cont-values t)))

(defun jdi-thread-send-step (thread depth)
  (jdi-info "jdi-thread-send-step:depth=%s" depth)
  (lexical-let* ((thread thread)
				 (vm (jdi-mirror-virtual-machine thread))
				 (erm (jdi-virtual-machine-event-request-manager vm))
				 (er (jdi-event-request-manager-create-step erm thread depth)))
	(cont-bind (result) (jdi-event-request-enable er)
	  (jdi-thread-resume thread))))

(defun jdi-value-sigbyte (value)
  (string-to-char (jdi-value-signature value)))

(defun jdi-method-get-values (method)
  (jdi-info "jdi-method-get-values")
  (if (jdi-method-values method)
	  (cont-values t)

	(lexical-let ((method method))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp method)
														  "variable-table"
														  `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
															(:method-id . ,(jdi-method-id method))))
		(jdi-trace "variable-table arg-count:%d slots:%d" (bindat-get-field reply :arg-cnt) (bindat-get-field reply :slots))

		(setf (jdi-method-values method)
			  (loop for slot in (bindat-get-field reply :slot)
					for code-index = (jdwp-get-int slot :code-index)
					for line-code-length = (bindat-get-field slot :length)
					do (jdi-trace "slot:%d code-index:%d length:%d name:%s signature:%s generic-signature:%s" 
								  (bindat-get-field slot :slot) 
								  code-index
								  line-code-length
								  (jdwp-get-string slot :name) 
								  (jdwp-get-string slot :signature)
								  (jdwp-get-string slot :generic-signature))
					collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine method)
											:slot (bindat-get-field slot :slot)
											:code-index code-index
											:code-index-length line-code-length
											:name (jdwp-get-string slot :name)
											:signature (jdwp-get-string slot :signature)
											:generic-signature (jdwp-get-string slot :generic-signature))))
		(cont-values t)))))

(defun jdi-frame-get-locals (frame location)
  "[ASYNC] returns a list of jdi-value that is visible in the current frame current location"
  (lexical-let* ((frame frame)
				 (location location)
				 (current-frame-line-code (jdwp-vec-to-int (jdi-location-line-code-index location))))
	(jdi-time-start)
	(cont-bind (result) (jdi-method-get-values (jdi-location-method location))
	  (jdi-time-end "jdi-method-get-values")
	  (setf (jdi-frame-values frame)
			(loop for method-value in (jdi-method-values (jdi-location-method location))
				  if (and (>= current-frame-line-code (jdi-value-code-index method-value))
						  (<  current-frame-line-code (+ (jdi-value-code-index method-value) (jdi-value-code-index-length method-value))))
				  collect method-value))

	  (lexical-let ((thread-id (jdi-thread-id (jdi-frame-thread frame)))
					(frame-id (jdi-frame-id frame)))
		(let ((data `((:thread . ,thread-id)
					  (:frame  . ,frame-id)
					  (:slots  . ,(length (jdi-frame-values frame)))
					  ,(nconc (list :slot)
							  (mapcar (lambda (value) 
										`((:slot . ,(jdi-value-slot value))
										  (:sigbyte . ,(jdi-value-sigbyte value))))
									  (jdi-frame-values frame))))))
		  (cont-bind (reply error jdwp id) (jdwp-send-command 
											(jdi-mirror-jdwp frame) 
											"stack-get-values" 
											data)
			(loop for value-struct in (jdi-frame-values frame)
				  for value-reply  in (bindat-get-field reply :value)
				  do
				  (setf (jdi-value-type value-struct) (bindat-get-field value-reply :slot-value :type))
				  (setf (jdi-value-value value-struct) (bindat-get-field value-reply :slot-value :u :value))
				  (jdi-info "jdi-frame-get-locals:setting value:%s to type %s" (jdi-value-name value-struct) (jdi-value-type value-struct)))
			(jdi-time-start)
			(cont-bind () (jdi-values-get-class-and-super-and-interfaces (jdi-frame-values frame))
			  (cont-wait (mapc 'jdi-value-get-string (jdi-frame-values frame))
				(jdi-time-end "mapc jdi-value-get-string")
				(cont-values (jdi-frame-values frame))))))))))

(defun jdi-value-object-p (value)
  (equal (jdi-value-type value) jdwp-tag-object))

(defun jdi-classes-get-locations (classes)
  (lexical-let ((classes classes))
	(cont-bind () (jdi-classes-get-methods classes)
	  (cont-bind () (jdi-methods-get-locations (apply 'append (mapcar 'jdi-class-methods classes)))
		(cont-values)))))

(defun jdi-values-get-class-and-super-and-interfaces (values)
  (lexical-let ((values values))
	(cont-bind () (jdi-values-get-class (remove-if-not 'jdi-value-object-p values))
	  (cont-bind () (jdi-classes-get-super-r (loop for value in values
												   if (jdi-value-class value)
												   collect (jdi-value-class value)))
		(cont-bind () (jdi-classes-get-interfaces (loop for value in values
														if (jdi-value-class value)
														append (jdi-class-all-super (jdi-value-class value))))
		  (cont-values))))))

(defun jdi-values-get-string (values)
  (jdi-info "jdi-values-get-string %s values" (length values))
  (lexical-let ((values values))
	(if (null values)
		(cont-values t)
	  (cont-bind (result) (jdi-value-get-string (car values))
		(jdi-values-get-string (cdr values))))))

(defun jdi-value-get-string (value)
  "[ASYNC] get a string to be displayed for a value"
  (jdi-info "jdi-value-get-string:name=%s:type=%s" (jdi-value-name value) (jdi-value-type value))
  (cond ((or (equal (jdi-value-type value) jdwp-tag-int)
			 (equal (jdi-value-type value) jdwp-tag-byte)
			 (equal (jdi-value-type value) jdwp-tag-char)
			 (equal (jdi-value-type value) jdwp-tag-short))
		 (setf (jdi-value-string value) (format "%d" (jdi-value-value value)))
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-long)
		 (setf (jdi-value-string value) (format "%d" (jdwp-vec-to-int (jdi-value-value value))))
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-float)
		 (setf (jdi-value-string value) (format "%f" (jdwp-vec-to-float (jdi-value-value value))))
		 (cont-values t))

;; 		((and parent
;; 			  (equal (jdi-value-type parent) jdwp-tag-object)
;; 			  (equal (jdi-value-type value) jdwp-tag-object)
;; 			  (equal (jdi-value-value parent) (jdi-value-value value)))
;; 		 (cont-values "this"))

		((equal (jdi-value-type value) jdwp-tag-boolean)
		 (setf (jdi-value-string value) (if (= 0 (jdi-value-value value)) "false" "true"))
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-void)
		 (setf (jdi-value-string value) "void")
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-class-object)
		 ;; TODO: put in the class name
		 (setf (jdi-value-string value) "class")
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-class-loader)
		 (setf (jdi-value-string value) "class-loader")
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-thread)
		 (setf (jdi-value-string value) "thread")
		 (cont-values t))

		((equal (jdi-value-type value) jdwp-tag-thread-group)
		 (setf (jdi-value-string value) "thread-group")
		 (cont-values t))

 		((equal (jdi-value-type value) jdwp-tag-object)
 		 (jdi-value-get-string-object value))

		((equal (jdi-value-type value) jdwp-tag-string)
		 (jdi-value-get-string-string value))

  		((equal (jdi-value-type value) jdwp-tag-array)
  		 (jdi-value-get-string-array value))

		(t 
		 (jdi-error "fixme: do not know how to print value of type:%s" (jdi-value-type value))
		 (setf (jdi-value-string value) "...")
		 (cont-values t))))

(defun jdi-class-name (class-or-signature)
  (jdi-info "jdi-class-name")
  (let* ((class-name (if (jdi-class-p class-or-signature) (jdi-class-signature class-or-signature) class-or-signature)))
    (cond ((string= class-name "I")
		   (setq class-name "int"))
		  (t
		   (setq class-name (replace-regexp-in-string ".*/" "" class-name))
		   (setq class-name (replace-regexp-in-string ";" "" class-name))))
    class-name))

(defmacro jdi-multiple-get-defun (name unique-id-func set-field-func accessor-func)
  `(defun ,name (structures)
	 (jdi-info "%s:%s structures" (symbol-name ',name) (length structures))
	 (lexical-let ((structures structures)
				   (structures-hash (make-hash-table :test 'equal)))
	   (dolist (structure structures)
		 (puthash (,unique-id-func structure) structure structures-hash))
	   (jdi-info "unique structures = %s" (hash-table-count structures-hash))
	   (cont-wait (maphash (lambda (key value)
							 (,set-field-func value))
						   structures-hash)
		 (dolist (structure structures)
		   (when (null (,accessor-func structure))
			 (setf (,accessor-func structure)
				   (,accessor-func (gethash (,unique-id-func structure) structures-hash)))))
		 (cont-values)))))

(jdi-multiple-get-defun jdi-values-get-class       jdi-value-value jdi-value-get-class      jdi-value-class)
(jdi-multiple-get-defun jdi-classes-get-super      jdi-class-id    jdi-class-get-super      jdi-class-super)
(jdi-multiple-get-defun jdi-classes-get-interfaces jdi-class-id    jdi-class-get-interfaces jdi-class-interfaces)

(jdi-multiple-get-defun jdi-classes-get-methods   jdi-class-id  jdi-class-get-methods    jdi-class-methods)
(jdi-multiple-get-defun jdi-methods-get-locations jdi-method-id jdi-method-get-locations jdi-method-locations)

(defun jdi-classes-get-super-r (classes)
  "recursive"
  (jdi-info "jdi-classes-get-super-r")
  (if (null classes)
	  (cont-values)
	(mapc (lambda (class)
			(jdi-info "class-signature:%s" (jdi-class-signature class)))
		  classes)
	(lexical-let ((classes classes))
	  (cont-wait (jdi-classes-get-super classes)
		(jdi-classes-get-super-r (loop for class in classes
									   if (jdi-class-super class)
									   collect (jdi-class-super class)))))))

(defun jdi-value-get-class (value)
  "populate the jdi-value-class field"
  (jdi-info "jdi-value-get-class:name=%s type=%s" (jdi-value-name value) (jdi-value-type value))
  (if (or (jdi-value-class value)
		  (equal (jdi-value-value value) [0 0 0 0 0 0 0 0]))
	  (cont-values)
	(lexical-let ((value value))
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp value) 
										"reference-type" 
										`((:object . ,(jdi-value-value value))))
		(let ((class (gethash (bindat-get-field reply :type-id) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine value)))))
		  (setf (jdi-value-class value) class)
		  (cont-values))))))

(defun jdi-value-get-string-object (value)
  (jdi-info "jdi-value-get-string-object:name=%s:type=%s:value=%s" (jdi-value-name value) (jdi-value-type value) (jdi-value-value value))
  (if (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	  (cont-values t)
	(lexical-let ((value value))
	  (cont-bind () (jdi-value-get-class value)
		(let ((setter (jdi-value-custom-set-strings-find value)))
		  (jdi-info "found setter:%s for class signature:%s" setter (jdi-class-signature (jdi-value-class value)))
		  (if setter 
			  (cond 
			   ((stringp setter)
				(jdi-value-custom-set-string-with-method value setter))
			   (t
				(funcall setter value)))
			(setf (jdi-value-string value) (format "%s {id=%s}" (jdi-class-name (jdi-value-class value)) (jdwp-vec-to-int (jdi-value-value value))))
			(jdi-info "set jdi-value-string to %s" (jdi-value-string value))
			(cont-values t)))))))

(defun jdi-value-array-display-string (value size)
  "for array of three dimension, return i[2][][]."
  (let ((sig (string-to-list (jdi-class-signature (jdi-value-class value))))
		(suffix ""))
	(pop sig)
	(loop while (equal (car sig) jdwp-tag-array)
		  do
		  (setf suffix (concat suffix "[]"))
		  (pop sig))
	(format "%s[%s]%s" 
			(jdi-class-name (concat sig))
			size
			suffix)))

(defun jdi-value-get-string-array (value)
  (jdi-info "jdi-value-get-string-array")
  (lexical-let ((value value))
	(cont-bind () (jdi-value-get-class value)
	  (jdi-info "get array-length")

	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp value) 
										"array-length" 
										`((:array-object . ,(jdi-value-value value))))
		(let ((size (bindat-get-field reply :array-length)))
		  (setf (jdi-value-array-length value) (bindat-get-field reply :array-length))
		  ;;			(setf (jdi-value-has-children-p value) (> size 0))
		  (setf (jdi-value-string value) (jdi-value-array-display-string value size))
		  (jdi-info "set jdi-value-string to %s" (jdi-value-string value))
		  (cont-values t))))))

(defun jdi-format-string (str)
  "Truncate and escape the string to be displayed."
  (with-output-to-string
	(let ((print-escape-newlines t)
		  (print-escape-nonascii t))
	  (prin1 str))))

(defun jdi-value-get-string-string (value)
  (lexical-let ((value value))
	(jdi-info "jdi-value-get-string-string:%s" (jdi-value-name value))
	(cont-bind (reply error jdwp id)
	  (jdwp-send-command (jdi-mirror-jdwp value) "string-value" `((:object . ,(jdi-value-value value))))

	  (jdi-info "jdi-value-get-string-string:%s:%s" (jdwp-get-string reply :value) (jdi-format-string (jdwp-get-string reply :value)))
	  (setf (jdi-value-string value) (jdi-format-string (jdwp-get-string reply :value)))
	  (cont-values t))))

(defun jdi-value-has-children-p (value)
  (jdi-info "jdi-value-has-children-p:name:%s type:%s array-length:%s" (jdi-value-name value) (jdi-value-type value) (jdi-value-array-length value))
  (and (not (equal (jdi-value-value value)
				   [0 0 0 0 0 0 0 0]))
	   (or (and (equal (jdi-value-type value) jdwp-tag-object)
				(not (equal (jdi-value-array-length value) 0)))
		   (and (equal (jdi-value-type value) jdwp-tag-array)
				(not (equal (jdi-value-array-length value) 0))))))

(defun jdi-field-static-p (field)
  (not (equal (logand (jdi-field-mod-bits field) jdi-access-static) 0)))

(defun jdi-remove-duplicated (fields)
  "Return a list of jdi-fields, without duplicated names, the first field is kept."
  (let (names filtered)
	(mapc (lambda (field)
			(when (not (member (jdi-field-name field) names))
			  (push (jdi-field-name field) names)
			  (push field filtered)))
		  fields)
	filtered))
	
(defun jdi-class-all-fields (class)
  "Return a list of jdi-fields including those of parents."
  (let ((all-fields (append (jdi-class-fields class)
							(if (jdi-class-super class)
								(jdi-class-all-fields (jdi-class-super class))))))
	;; since the children's field always comes first in the list, we can just remove the later duplicated ones
	(setf all-fields (jdi-remove-duplicated all-fields))
	(jdi-info "jdi-class-all-fields:%s" (mapcar 'jdi-field-name all-fields))
    (sort all-fields (lambda (a b)
					   (string< (jdi-field-name a) (jdi-field-name b))))))

(defun jdi-value-get-nonstatic-values (value)
  (jdi-info "jdi-value-get-nonstatic-values")
  (lexical-let* ((value value)
				 (class (jdi-value-class value))
				 (fields (loop for field in (jdi-class-all-fields class)
							   if (not (jdi-field-static-p field)) collect field))
				 (values (mapcar (lambda (field) (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
																 :name (jdi-field-name field)
																 :signature (jdi-field-signature field)
																 :generic-signature (jdi-field-generic-signature field)))
						 fields)))
	(jdi-info "number of nonstatic fields:%d" (length fields))
	(if (= (length fields) 0)
		(cont-values t)

	  (cont-bind (reply error jdwp id)
		(jdwp-send-command (jdi-mirror-jdwp value) "object-get-values"
						   `((:object . ,(jdi-value-value value))
							 (:fields . ,(length fields))
							 ,(nconc (list :field)
									 (mapcar (lambda (field)
											   `((:id . ,(jdi-field-id field))))
											 fields))))

		(loop for jdi-value in values
			  for value in (bindat-get-field reply :value)
			  do
			  (setf (jdi-value-type jdi-value) (bindat-get-field value :value :type))
			  (setf (jdi-value-value jdi-value) (bindat-get-field value :value :u :value)))
		(jdi-info "adding %s values to existing %s" (length values) (length (jdi-value-values value)))
		(setf (jdi-value-values value) (append (jdi-value-values value) values))
		(cont-bind () (jdi-values-get-class-and-super-and-interfaces (jdi-value-values value))
		  (cont-values t))))))

(defun jdi-value-get-static-values (value)
  "Populate the values for the static fields in jdi-value"
  (jdi-info "jdi-value-get-static-values")
  (lexical-let* ((value value)
				 (class (jdi-value-class value))
				 (fields (loop for field in (jdi-class-all-fields class)
							   if (jdi-field-static-p field) collect field))
				 (values (mapcar (lambda (field) (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
																 :name (jdi-field-name field)
																 :signature (jdi-field-signature field)
																 :generic-signature (jdi-field-generic-signature field)))
								 fields)))
	(jdi-info "number of static fields:%d" (length fields))
	(if (= (length fields) 0)
		(cont-values t)

	  (cont-bind (reply error jdwp id)
		(jdwp-send-command (jdi-mirror-jdwp value) "reference-get-values"
						   `((:ref-type . ,(jdi-class-id (jdi-value-class value)))
							 (:fields . ,(length fields))
							 ,(nconc (list :field)
									 (mapcar (lambda (field)
											   `((:id . ,(jdi-field-id field))))
											 fields))))
		(loop for jdi-value in values
			  for value in (bindat-get-field reply :value)
			  do
			  (setf (jdi-value-type jdi-value) (bindat-get-field value :value :type))
			  (setf (jdi-value-value jdi-value) (bindat-get-field value :value :u :value)))
		(setf (jdi-value-values value) (append (jdi-value-values value) values))
		(cont-bind () (jdi-values-get-class-and-super-and-interfaces (jdi-value-values value))
		  (cont-values t))))))

(defun jdi-class-all-super (class)
  "Returns all the classes in this class's hierarchy, including this class itself"
  (unless (null class)
	(cons class (jdi-class-all-super (jdi-class-super class)))))

(defun jdi-class-get-super (class)
  "populate the jdi-class-super of this class"
  (jdi-info "jdi-class-get-super:name=%s" (jdi-class-name class))
  (if (or (jdi-class-super class)
		  (string= (jdi-class-signature class) "Ljava/lang/Object;"))
	  (cont-values)
	(lexical-let ((class class))
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp class) 
										"superclass" 
										`((:class . ,(jdi-class-id class))))
		(if (equal (bindat-get-field reply :superclass)
				   [0 0 0 0 0 0 0 0])
			(cont-values)
		  (let ((superclass (gethash (bindat-get-field reply :superclass) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine class)))))
			(jdi-trace "class %s superclass:%s" (jdi-class-name class) (jdi-class-name superclass))
			(setf (jdi-class-super class) superclass)
			(cont-values)))))))

(defun jdi-method-invoke (method)
  "Invoke a simple method (do not require arguments) in the class."
  (jdi-info "jdi-class-invoke-method:%s" (jdi-method-name method))
  (jdwp-send-command (jdi-mirror-jdwp method) "class-invoke-method"
					 `((:class . ,(jdi-class-id (jdi-method-class method)))
					   (:thread . ,(jdi-virtual-machine-suspended-thread-id (jdi-mirror-virtual-machine method)))
					   (:method-id . ,(jdi-method-id method))
					   (:arguments . 0)
					   (:options . ,jdwp-invoke-single-threaded))))

(defun jdi-class-get-interfaces (class)
  "populate the jdi-class-interfaces of this class"
  (jdi-info "jdi-class-get-interfaces:name=%s" (jdi-class-name class))
  (if (or (jdi-class-interfaces-count class)
		  (string= (jdi-class-signature class) "Ljava/lang/Object;"))
	  (cont-values)

	(lexical-let ((class class)) 
	  ;; NOTE: this do not resolve the super-interfaces, TODO
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp class) 
										"interfaces" 
										`((:ref-type . ,(jdi-class-id class))))
		(setf (jdi-class-interfaces class)
			  (loop for interface in (bindat-get-field reply :interface)
					collect (gethash (bindat-get-field interface :type) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine class)))))
		(setf (jdi-class-interfaces-count class) (bindat-get-field reply :interfaces))
		(if jdi-info-flag
			(loop for interface in (jdi-class-interfaces class)
				  do (jdi-info "class:%s interface:%s"
							   (jdi-class-name class)
							   (jdi-class-name interface))))
		(cont-values)))))

;; (defun jdi-class-get-parent (class)
;;   "Get the whole parent hierarchy of this class, stop only after reaching the Object class."
;;   (jdi-info "jdi-class-get-parent:name=%s" (jdi-class-name class))
;;   (lexical-let ((class class))
;; 	(cont-wait (progn
;; 				 (jdi-class-get-super class)
;; 				 (jdi-class-get-interfaces class))
;; 	  (if (jdi-class-super class)
;; 		  (jdi-class-get-parent (jdi-class-super class))
;; 		(cont-values)))))
								
;; 	(cont-bind () (jdi-class-get-super class)
;; 	  (cont-bind () (jdi-class-get-interfaces class)
;; 	(if (jdi-class-super class)
;; 		(cont-values)

;; 	  (jdi-info "jdi-class-get-parent for %s:%s" (jdi-class-signature class) (jdi-class-id class))

;; 	  ;; NOTE: this do not resolve the super-interfaces, TODO
;; 	  (cont-bind (reply error jdwp id)
;; 		(jdwp-send-command (jdi-mirror-jdwp class) "interfaces" `((:ref-type . ,(jdi-class-id class))))
;; 		(setf (jdi-class-interfaces class)
;; 			  (loop for interface in (bindat-get-field reply :interface)
;; 					collect (gethash (bindat-get-field interface :type) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine class)))))
;; 		(if jdi-info-flag
;; 			(loop for interface in (jdi-class-interfaces class)
;; 				  do (jdi-info "class:%s interface:%s"
;; 							   (jdi-class-name class)
;; 							   (jdi-class-name interface))))

;; 		(cont-bind (reply error jdwp id)
;; 		  (jdwp-send-command (jdi-mirror-jdwp class) "superclass" `((:class . ,(jdi-class-id class))))

;; 		  (if (equal (bindat-get-field reply :superclass)
;; 					 [0 0 0 0 0 0 0 0])
;; 			  (cont-values)

;; 			(let ((superclass (gethash (bindat-get-field reply :superclass) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine class)))))
;; 			  (jdi-trace "class %s superclass:%s" (jdi-class-name class) (jdi-class-name superclass))
;; 			  (setf (jdi-class-super class) superclass)
;; 			  (if (string= (jdi-class-signature superclass) "Ljava/lang/Object;")
;; 				  (cont-values)
;; 				(jdi-class-get-parent superclass)))))))))

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
  "Get all the fields in this class, and also in the parent's class if this class have a parent."
  (lexical-let ((class class))
	(if (jdi-class-fields class)
		(cont-values)

	  (jdi-info "jdi-class-resolve-fields for %s" (jdi-class-name class))
	  (cont-bind (reply error jdwp id)
		(jdwp-send-command (jdi-mirror-jdwp class) "fields" `((:ref-type . ,(jdi-class-id class))))

		(jdi-trace "%s's fields:%d" (jdi-class-name class) (bindat-get-field reply :declared))
		(dolist (field (bindat-get-field reply :field))
		  (let ((new-field (make-jdi-field :virtual-machine (jdi-mirror-virtual-machine class)
										   :id (bindat-get-field field :id)
										   :name (jdwp-get-string field :name)
										   :signature (jdwp-get-string field :signature)
										   :generic-signature (jdwp-get-string field :generic-signature)
										   :mod-bits (bindat-get-field field :mod-bits))))
			(jdi-trace "id:%s name:%s signature:%s generic-signature:%s modbits:%s" 
					   (bindat-get-field field :id) 
					   (jdwp-get-string field :name) 
					   (jdwp-get-string field :signature)
					   (jdwp-get-string field :generic-signature)
					   (jdi-field-mod-bits-string new-field))
			(push new-field (jdi-class-fields class))))
		(cont-values)))))

(defun jdi-value-object-get-values (value)
  (jdi-info "jdi-value-object-get-values")
  (lexical-let ((value value))
	(cont-wait (mapc 'jdi-class-get-fields (jdi-class-all-super (jdi-value-class value)))

	  (let ((expander-func (jdi-value-custom-expanders-find value)))
		(if expander-func
			(cont-bind (result) (funcall expander-func value)
			  (cont-wait (mapc 'jdi-value-get-string (jdi-value-values value))
				(cont-values)))

		  (setf (jdi-value-values value) nil)
		  (cont-bind (result) (jdi-value-get-static-values value)
			(cont-bind (result) (jdi-value-get-nonstatic-values value)
			  (jdi-info "done jdi-value-object-get-values")
			  (cont-wait (mapc 'jdi-value-get-string (jdi-value-values value))
				(cont-values)))))))))

(defun jdi-value-array-get-values (value)
  (jdi-info "jdi-value-get-array-values:value=%s" (jdi-value-value value))

  (if (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	  (cont-values t)
	(lexical-let ((value value))
	  (cont-bind (reply error jdwp id)
		(jdwp-send-command (jdi-mirror-jdwp value) "array-get-values"
						   `((:array-object . ,(jdi-value-value value))
							 (:first-index . 0)
							 (:length . ,(jdi-value-array-length value))))

		(let ((array (jdwp-unpack-arrayregion jdwp reply)))
		  (jdi-trace "got array-get-values:%s" array)
		  (setf (jdi-value-values value)
				(if (or (= (bindat-get-field array :type) jdwp-tag-object)
						(= (bindat-get-field array :type) jdwp-tag-array))
					(loop for value-reply in (bindat-get-field array :value)
						  for i from 0 to (- (jdi-value-array-length value) 1)
						  collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
												  :name (format "%s[%d]" (jdi-value-name value) i)
												  :type (bindat-get-field value-reply :value :type)
												  :value (bindat-get-field value-reply :value :u :value)))
				  (loop for value-reply in (bindat-get-field array :value)
						for i from 0 to (- (jdi-value-array-length value) 1)
						collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
												:name (format "%s[%d]" (jdi-value-name value) i)
												:type (bindat-get-field array :type)
												:value (bindat-get-field value-reply :value)))))
		  (jdi-values-get-string (jdi-value-values value)))))))

(defun jdi-handle-breakpoint-event (jdwp event)
  (jdi-info "jdi-handle-breakpoint-event")
  (let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
		 (class-id (bindat-get-field event :u :location :class-id))
		 (method-id (bindat-get-field event :u :location :method-id))
		 (line-code-index (bindat-get-field event :u :location :index))
		 ;; if its a breakpoint, we assumed that we have already resolved it somehow
		 ;; any case where it isn't?
		 (class (gethash class-id (jdi-virtual-machine-classes vm)))
		 (location (jdi-class-find-location class method-id line-code-index))
		 ;; threads might be created and destroyed at the go
		 ;; and we do not handle threadstart threaddeath yet, so just create new 
		 ;; one
		 (thread (make-jdi-thread :virtual-machine vm :id (bindat-get-field event :u :thread))))
    (setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
;    (setf (jdi-current-location jdi) location)
	(jdi-info "found location:%s" (if location "yes" "no"))
	(run-hook-with-args 'jdi-breakpoint-hooks thread location)))
  
(defun jdi-handle-step-event (jdwp event)
  (jdi-info "jdi-handle-step-event")
  (lexical-let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
				 (class-id (bindat-get-field event :u :location :class-id))
				 (method-id (bindat-get-field event :u :location :method-id))
				 (line-code-index (bindat-get-field event :u :location :index))
				 (class (gethash class-id (jdi-virtual-machine-classes vm)))
				 (thread (make-jdi-thread :virtual-machine vm :id (bindat-get-field event :u :thread))))
	(cont-bind () (jdi-class-get-all-line-locations class)
	  (let ((location (jdi-class-find-location class method-id line-code-index)))
		(run-hook-with-args 'jdi-step-hooks thread location)))))

;;; Customized display and expanders:
(defvar jdi-value-custom-set-strings nil
  "a list of (class setter) where

class is a string which hold the class name of the object to be matched.
The matching will be done using something like instance of.

setter is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-string of the jdi-value. If setter
is a string, it will be the method that will be invoked on the java object
and the value that is returned is shown.")

(setq jdi-value-custom-set-strings
      '(("java.lang.Boolean"      "toString")
		("java.lang.Number"       "toString")
		("java.lang.StringBuffer" "toString")
		("java.util.Date"         "toString")
		("java.util.Collection"   jdi-value-custom-set-string-with-size)
		("java.util.Map"          jdi-value-custom-set-string-with-size)))

(defvar jdi-value-custom-expanders nil
  "a list of (instance expander-func) where

instance is a string that is matched with jdi-value-instance-of-p with the 
value

expander-func is a function that is passed (jdi jdi-value) and is expected
to populate the jdi-value-values of the jdi-value.")

(setq jdi-value-custom-expanders
      '(("java.util.Collection" jdi-value-custom-expand-collection)
		("java.util.Map"        jdi-value-custom-expand-map)))

(defun jdi-class-name-to-class-signature (class-name)
  "Converts a.b.c class name to JNI class signature."
  (let ((buf class-name))
	(setf buf (replace-regexp-in-string "\\." "/" buf))
	(setf buf (format "L%s;" buf))
	(jdi-info "jdi-class-name-to-class-signature:%s:%s" class-name buf)
	buf))

(defun jdi-value-instance-of-p (value signature)
  (catch 'found
    (let ((class (jdi-value-class value)))
      (while class
		(jdi-trace "jdi-value-instance-of-p: comparing %s with %s" (jdi-class-signature class) signature)
		(if (string= (jdi-class-signature class) signature)
			(throw 'found t))
		(dolist (interface (jdi-class-interfaces class))
		  (jdi-trace "jdi-value-instance-of-p interface: comparing %s with %s" (jdi-class-signature interface) signature)
		  (if (string= (jdi-class-signature interface) signature)
			  (throw 'found t)))
		(setq class (jdi-class-super class)))
      ;; the return value if its not found
      nil)))

(defun jdi-value-custom-set-strings-find (value)
  (let ((element (find-if (lambda (custom)
							(jdi-value-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
						  jdi-value-custom-set-strings)))
    (if element
		(cadr element))))

(defun jdi-value-custom-expanders-find (value)
  (let ((element (find-if (lambda (custom)
							(jdi-value-instance-of-p value (jdi-class-name-to-class-signature (car custom))))
						  jdi-value-custom-expanders)))
    (if element
		(cadr element))))

(defun jdi-value-custom-set-string-with-method (value method)
  (lexical-let ((value value)
				(method method))
	(cont-bind (reply error jdwp id)
	  (jdi-value-invoke-method value method "()Ljava/lang/String;")
	  (if reply
		  (let ((object-id (bindat-get-field reply :return-value :u :value)))
			(if (equal object-id [0 0 0 0 0 0 0 0])
				(progn
				  (setf (jdi-value-string value) "null")
				  (cont-values t))
			  (cont-bind (reply error jdwp id)
				(jdwp-send-command (jdi-mirror-jdwp value) "string-value" 
								   `((:object . ,object-id)))
				(setf (jdi-value-string value) (jdi-format-string (jdwp-get-string reply :value)))
				(cont-values t))))
		(setf (jdi-value-string value) (format "setter %s not found" method))
		(cont-values t)))))

(defun jdi-value-extract-generic-class-name (generic-signature)
  (string-match "<L.*/\\(.*\\);>" generic-signature)
  (match-string 1 generic-signature))

(defun jdi-value-type-with-generic (value)
  (let ((class (jdi-value-class value))
		(gs (jdi-value-generic-signature value)))
	(if (and gs (not (string= gs "")))
		(format "%s<%s>" (jdi-class-name class) (jdi-value-extract-generic-class-name (jdi-value-generic-signature value)))
	  (jdi-class-name class))))

(defun jdi-value-custom-set-string-with-size (value)
  (jdi-trace "jdi-value-custom-set-string-with-size:%s" (jdi-class-name (jdi-value-class value)))
  (lexical-let ((value value))
	(cont-bind (result) (jdi-class-get-methods (jdi-value-class value))

	  (cont-bind (reply error jdwp id)
		(jdi-value-invoke-method value "size" nil)

		(let ((size (bindat-get-field reply :return-value :u :value)))
		  (setf (jdi-value-array-length value) size)
		  (setf (jdi-value-string value) 
				(format "%s[%d]" (jdi-value-type-with-generic value) size)))
		(cont-values t)))))

(defun jdi-value-custom-expand-collection (value)
  (jdi-info "jdi-value-custom-expand-collection")
  (lexical-let ((value value))
	(cont-bind (reply error jdwp id)
	  (jdi-value-invoke-method value "toArray" nil)
	  (let ((array-value (bindat-get-field reply :return-value :u :value)))
		(setf (jdi-value-value value) array-value)
		(jdi-value-array-get-values value)))))

(defun jdi-class-all-methods (class)
  "Return a list of methods including those of parents.
Methods of child class will appear in front of parent's in the list
so finding a method by signature will return the child's method first."
  (append (jdi-class-methods class) (if (jdi-class-super class)
										(jdi-class-all-methods (jdi-class-super class)))))

(defun jdi-method-native-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-native) 0)))

(defun jdi-method-static-p (method)
  (not (equal (logand (jdi-method-mod-bits method) jdi-access-static) 0)))

(defun jdi-value-invoke-method (value method-or-name &optional method-signature)
  "Invoke a simple method (do not require arguments) in the object in jdi-value."
  (jdi-info "jdi-value-invoke-method:%s:%s" 
			(if (jdi-method-p method-or-name)
				(jdi-method-name method-or-name)
			  method-or-name)
			method-signature)
  (lexical-let ((value value)
				(method-or-name method-or-name)
				(method-signature method-signature))
	(cont-bind (result) (jdi-class-get-methods (jdi-value-class value))
	  (lexical-let ((method (if (jdi-method-p method-or-name)
								method-or-name
							  (find-if (lambda (method)
										 (and (string= (jdi-method-name method) method-or-name)
											  (or (null method-signature)
												  (string= (jdi-method-signature method) method-signature))))
									   (jdi-class-all-methods (jdi-value-class value))))))
		(if method
			(jdwp-send-command (jdi-mirror-jdwp value) "object-invoke-method"
							   `((:object . ,(jdi-value-value value))
								 (:thread . ,(jdi-virtual-machine-suspended-thread-id (jdi-mirror-virtual-machine value)))
								 (:class . ,(jdi-class-id (jdi-value-class value)))
								 (:method-id . ,(jdi-method-id method))
								 (:arguments . 0)
								 (:options . ,jdwp-invoke-single-threaded)))
		  (cont-values nil nil nil nil))))))

(defun jdi-value-custom-expand-map (value)
  (jdi-info "jdi-value-custom-expand-collection")
  (lexical-let ((value value)
				(keyset-value (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)))
				(values-value (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value))))
	
	(cont-bind (reply error jdwp id)
	  (jdi-value-invoke-method value "keySet" nil)
	  
	  (setf (jdi-value-value keyset-value) (bindat-get-field reply :return-value :u :value))
	  (setf (jdi-value-class keyset-value) (car (gethash "Ljava/util/Set;" (jdi-virtual-machine-classes-by-signature (jdi-mirror-virtual-machine value)))))

	  (cont-bind (reply error jdwp id)
		(jdi-value-invoke-method keyset-value "toArray" nil)

		(let ((key-array (bindat-get-field reply :return-value :u :value)))
		  (setf (jdi-value-name keyset-value) "key  ")
		  (setf (jdi-value-array-length keyset-value) (jdi-value-array-length value))
		  (setf (jdi-value-value keyset-value) key-array)
		  (cont-bind (result) (jdi-value-array-get-values keyset-value)

			(cont-bind (reply error jdwp id) (jdi-value-invoke-method value "values" nil)

			  (setf (jdi-value-value values-value) (bindat-get-field reply :return-value :u :value))
			  (setf (jdi-value-class values-value) (car (gethash "Ljava/util/Collection;" (jdi-virtual-machine-classes-by-signature (jdi-mirror-virtual-machine value)))))

			  (cont-bind (reply error jdwp id) (jdi-value-invoke-method values-value "toArray" nil)
				(let ((values-array (bindat-get-field reply :return-value :u :value)))
				  (setf (jdi-value-name values-value) "value")
				  (setf (jdi-value-array-length values-value) (jdi-value-array-length value))
				  (setf (jdi-value-value values-value) values-array)
				  (cont-bind (result) (jdi-value-array-get-values values-value)

					(setf (jdi-value-values value)
						  (loop for key in (jdi-value-values keyset-value)
								for value in (jdi-value-values values-value)
								append (list key value)))
					(jdi-trace "keys=%d, values=%d" 
							   (length (jdi-value-values keyset-value))
							   (length (jdi-value-values values-value)))
					(cont-values t)))))))))))

(defvar jdi-breakpoint-hooks nil
  "callback to be called when breakpoint is hit, called with (jdi-thread jdi-location)")

(defvar jdi-step-hooks nil
  "callback to be called when execution is stopped from stepping, called with (jdi-thread jdi-location)")

(defvar jdi-detached-hooks nil
  "callback to be called when debuggee detached from us, called with (jdi)")

(defvar jdi-breakpoint-resolved-hooks nil
  "handler to be called when the class is loaded for a breakpoint that wasn't resolved previously")

(defun jdi-handle-event (jdwp event)
  (jdi-info "jdi-handle-event")
  (let ((handlers (list 
				   `(,jdwp-event-breakpoint  . jdi-handle-breakpoint-event)
				   `(,jdwp-event-single-step . jdi-handle-step-event)
				   )))
	(mapc (lambda (handler)
			(jdi-trace "compare %s with %s" (car handler) (bindat-get-field event :event-kind))
			(if (equal (bindat-get-field event :event-kind)
					   (car handler))
				(funcall (cdr handler) jdwp event)))
		  handlers)))

(add-hook 'jdwp-event-hooks 'jdi-handle-event)

(provide 'jdi)