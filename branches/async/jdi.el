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

  ;; list of jdi-frame that is suspended (by breakpoint/step events)
  suspended-frames
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

(defstruct (jdi-thread (:include jdi-mirror))
  id
  name
  status
  suspend-status
  frames
  )

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
  variable)
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
  (lexical-let ((er er))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp er) "set" (jdi-event-request-data er))
	  (jdi-trace "received requestid:%s" (bindat-get-field reply :request-id))
	  (setf (jdi-event-request-id er) (bindat-get-field reply :request-id))
	  (cont-values t))))

(defun jdi-event-request-disable (er)
  (lexical-let ((er er))
	(cont-bind (reply error jdwp id) 
	  (jdwp-send-command (jdi-mirror-jdwp er) "clear" `((:event . ,jdwp-event-breakpoint) (:request-id . ,(jdi-event-request-id er))))

	  (jdi-trace "cleared event request")
	  (cont-values t))))

(defun jdi-virtual-machine-set-standard-events (vm)
  (cont-wait (mapc (lambda (event)
					 (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-virtual-machine-jdwp vm) "set" 
																		 `((:event-kind . ,event)
																		   (:suspend-policy . ,jdwp-suspend-policy-none)
																		   (:modifiers . 0)))
					   (cont-values)))
				   (list jdwp-event-class-prepare
						 jdwp-event-class-unload
						 jdwp-event-thread-start
						 jdwp-event-thread-end
						 jdwp-event-vm-death))
	(cont-values)))

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
			(cont-bind () (jdi-virtual-machine-set-standard-events vm)
			  (cont-values t))))))))

;; 			  (cont-bind () (jdi-virtual-machine-get-threads vm)
;; 				(cont-values t)))))))))

;; 			  (cont-bind (reply error jdwp id) (jdwp-send-command jdwp "all-classes" nil)
;; 				(jdi-debug "number of classes loaded:%s" (bindat-get-field reply :classes))
;; 				(setf (jdi-virtual-machine-classes vm) (make-hash-table :test 'equal))
;; 				(setf (jdi-virtual-machine-classes-by-signature vm) (make-hash-table :test 'equal))
;; 				(loop for class in       (bindat-get-field reply :class)
;; 					  for type-id      = (bindat-get-field class :type-id)
;; 					  for signature    = (jdwp-get-string class :signature)
;; 					  for ref-type-tag = (bindat-get-field class :ref-type-tag)
;; 					  for status       = (bindat-get-field class :status)
;; 					  for newclass     = (make-jdi-class 
;; 										  :virtual-machine vm
;; 										  :id type-id	
;; 										  :signature signature
;; 										  :ref-type-tag ref-type-tag
;; 										  :status status)
;; 					  do
;; 					  (jdi-trace "signature:%s id:%s" signature type-id)
;; 					  (puthash type-id newclass (jdi-virtual-machine-classes vm))
;; 					  (puthash signature (cons newclass (gethash signature (jdi-virtual-machine-classes-by-signature vm)))
;; 							   (jdi-virtual-machine-classes-by-signature vm)))
;; 				(cont-values t)))))))))

(defun jdi-virtual-machine-get-threads (vm)
  (jdi-debug "jdi-virtual-machine-get-threads")
;;   (if (jdi-virtual-machine-threads vm)
;; 	  (cont-values (jdi-virtual-machine-threads vm))
	(lexical-let ((vm vm))
	  (jdi-time-start)
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-virtual-machine-jdwp vm) "all-threads" nil)
		(jdi-time-end "jdi-virtual-machine-get-threads:all-threads")
		(jdi-debug "number of threads:%s" (bindat-get-field reply :threads))
		(setf (jdi-virtual-machine-threads vm)
			  (loop for thread in (bindat-get-field reply :thread)
					collect (make-jdi-thread :virtual-machine vm
											 :id (bindat-get-field thread :id))))
		(cont-values (jdi-virtual-machine-threads vm)))))

(defun jdi-virtual-machine-get-suspended-threads (vm)
  (jdi-debug "jdi-virtual-machine-get-suspended-threads")
  (lexical-let ((vm vm))
	(cont-bind (threads) (jdi-virtual-machine-get-threads vm)
	  (lexical-let ((threads threads))
		(cont-bind (suspendeds) (cont-mapcar 'jdi-thread-get-suspended-p threads)
		  (cont-values (loop for thread in threads
							 for suspended in suspendeds
							 if suspended
							 collect thread)))))))

(defun jdi-virtual-machine-get-classes-by-signature (vm signature)
  (jdi-debug "jdi-virtual-machine-get-classes-by-signature:%s" signature)
  (lexical-let ((vm vm)
				(signature signature))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-virtual-machine-jdwp vm) 
														"classes-by-signature"
														`((:signature . ((:length . ,(length signature))
																		 (:string . ,signature)))))
	  (jdi-debug "number of classes matched:%s" (bindat-get-field reply :classes))
	  (cont-values (loop for class        in (bindat-get-field reply :class)
						 for type-id      =  (bindat-get-field class :type-id)
						 for ref-type-tag =  (bindat-get-field class :ref-type-tag)
						 for status       =  (bindat-get-field class :status)
						 for newclass     =  (make-jdi-class 
											  :virtual-machine vm
											  :id type-id	
											  :signature signature
											  :ref-type-tag ref-type-tag
											  :status status)
						 collect newclass)))))
														
(defun jdi-virtual-machine-get-all-frames (vm)
  "We are actually only interested in suspended threads"
  (lexical-let* ((vm vm)
				 (suspended-threads (loop for frame in (jdi-virtual-machine-suspended-frames vm)
										  collect (jdi-frame-thread frame))))
	(jdi-time-start)
	(cont-wait () (progn
					(mapc 'jdi-thread-get-name suspended-threads)
					(mapc 'jdi-thread-get-status suspended-threads))
	  (jdi-time-end "jdi-virtual-machine-get-all-frames: mapc jdi-thread-get-name/status")
	  (jdi-time-start)
	  (cont-wait (mapc 'jdi-thread-get-frames suspended-threads)
		(jdi-time-end "jdi-virtual-machine-get-all-frames: mapc jdi-thread-get-frames")
		(cont-values)))))

(defun jdi-virtual-machine-suspended-threads (vm)
  (jdi-debug "jdi-virtual-machine-suspended-threads")
  (loop for frame in (jdi-virtual-machine-suspended-frames vm)
		collect (jdi-frame-thread frame)))

(defun jdi-thread-get-name (thread)
  (jdi-debug "jdi-thread-get-name")
  (lexical-let ((thread thread))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp thread) "thread-name" `((:thread . ,(jdi-thread-id thread))))
	  (setf (jdi-thread-name thread)
			(jdwp-get-string reply :thread-name))
	  (jdi-debug "thread-name=%s" (jdi-thread-name thread))
	  (cont-values (jdi-thread-name thread)))))

(defun jdi-thread-get-status (thread)
  (jdi-debug "jdi-thread-get-status")
  (lexical-let ((thread thread))
	(cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp thread) "thread-status" `((:thread . ,(jdi-thread-id thread))))
	  (setf (jdi-thread-status thread)
			(bindat-get-field reply :thread-status)
			(jdi-thread-suspend-status thread)
			(bindat-get-field reply :suspend-status))
	  (jdi-debug "thread-status:%s suspend-status:%s" (jdi-thread-status thread) (jdi-thread-suspend-status thread))
	  (cont-values (jdi-thread-status thread) (jdi-thread-suspend-status thread)))))

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
  (lexical-let ((class class))
	(cont-bind (supers) (jdi-class-get-all-super class)
	  (cont-mappend 'jdi-class-get-methods (cons class supers)))))

(defun jdi-class-get-methods (class)
  "[ASYNC] returns a list of jdi-method in the class"
  (jdi-debug "jdi-class-get-methods:%s" (jdi-class-signature class))
  (lexical-let ((class class))
	(if (jdi-class-methods class)
		(cont-values (jdi-class-methods class))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp class) "methods" `((:ref-type . ,(jdi-class-id class))))
		(jdi-debug "number of methods:%s" (bindat-get-field reply :methods))
		(setf (jdi-class-methods class)
			  (loop for method in (bindat-get-field reply :method)
					collect (make-jdi-method :virtual-machine (jdi-mirror-virtual-machine class)
											 :class class
											 :id (bindat-get-field method :method-id)
											 :name (jdwp-get-string method :name)
											 :signature (jdwp-get-string method :signature)
											 :mod-bits (bindat-get-field method :mod-bits))))
		(cont-values (jdi-class-methods class))))))

(defun jdi-class-get-signature (class)
  (jdi-debug "jdi-class-get-signature")
  (if (jdi-class-signature class)
	  (cont-values (jdi-class-signature class))
	(lexical-let ((class class))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp class) "signature" `((:ref-type . ,(jdi-class-id class))))
		(setf (jdi-class-signature class) (jdwp-get-string reply :signature))
		(cont-values (jdi-class-signature class))))))

(defun jdi-method-get-locations (method)
  (jdi-debug "jdi-method-get-locations:class=%s method=%s" (jdi-class-signature (jdi-method-class method)) (jdi-method-name method))
  (if (or (jdi-method-locations method)
		  (jdi-method-native-p method))
	  (cont-values (jdi-method-locations method))
	(lexical-let ((method method))
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp method) "line-table" `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
																				(:method-id . ,(jdi-method-id method))))
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
		(cont-values (jdi-method-locations method))))))

(defun jdi-method-get-name (method)
  (jdi-debug "jdi-method-get-signature")
  (lexical-let ((method method))
	(cont-bind (methods) (jdi-class-get-methods (jdi-method-class method))
	  (cont-values (jdi-method-name (find-if (lambda (obj)
											   (equal (jdi-method-id obj)
													  (jdi-method-id method)))
											 methods))))))

(defvar jdi-start-time nil)

(defun jdi-time-start ()
  (setf jdi-start-time (current-time)))

(defun jdi-time-end (message)
  (jdi-info "benchmark: %s took %s seconds" message (float-time (time-subtract (current-time) jdi-start-time))))

(defun jdi-class-get-all-line-locations (class)
  "[ASYNC] returns a list of jdi-location in the class"
  (jdi-debug "jdi-class-get-all-line-locations")
  (lexical-let ((class class))
	(jdi-time-start)
	(cont-bind (result) (jdi-class-get-methods class)
	  (jdi-time-end "jdi-class-get-methods")
	  (jdi-debug "calling jdi-method-get-locations for %s methods" (length (jdi-class-methods class)))
	  (cont-wait (mapc 'jdi-method-get-locations (jdi-class-methods class))
		(cont-values)))))

(defun jdi-virtual-machine-set-breakpoint (vm signature line)
  "[ASYNC] returns a jdi-event-request if a breakpoint is installed, nil otherwise"

  (lexical-let ((vm vm)
				(signature signature)
				(line line))
	(jdi-debug "jdi-virtual-machine-set-breakpoint:signature=%s:line=%s" signature line)
	(jdi-time-start)
	(cont-bind (classes) (jdi-virtual-machine-get-classes-by-signature vm signature)
	  (lexical-let ((classes classes))
		(cont-bind (locations) (cont-mappend (lambda (class)
											   (cont-values (jdi-class-get-locations-of-line class line)))
											 classes)
		  (jdi-time-end (format "jdi-class-get-locations-of-line for %s classes:matched = %s locations" (length classes) (length locations)))
		  (lexical-let* ((erm (jdi-virtual-machine-event-request-manager vm)))
			(cont-mapcar (lambda (location)
						   (lexical-let ((er (jdi-event-request-manager-create-breakpoint erm location)))
							 (cont-bind (result) (jdi-event-request-enable er)
							   (cont-values er))))
						 locations)))))))

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
  (lexical-let ((class class)
				(line-number line-number))
	(cont-bind (methods) (jdi-class-get-methods class)
	  (jdi-debug "jdi-class-get-locations-of-line: number of methods = %s" (length methods))
	  (cont-bind (locations) (cont-mappend 'jdi-method-get-locations methods)
		(jdi-debug "jdi-class-get-locations-of-line: number of locations = %s" (length locations))
		(cont-values (loop for location in locations
						   if (equal line-number (jdi-location-line-number location))
						   collect location))))))

(defun jdi-class-find-location (class method-id line-code-index)
  (jdi-debug "jdi-class-find-location:number of methods=%s" (length (jdi-class-methods class)))
  (let* ((method (find-if (lambda (method)
							(equal (jdi-method-id method) method-id))
						  (jdi-class-methods class)))
		 (location (if method (jdi-method-location-by-line-code-index method line-code-index))))
    (if location
		location
	  (jdi-error "failed to look line-code-index %s in class %s" line-code-index (jdi-class-name class)))))

(defun jdi-location-get-class-and-method-and-line-number (location)
  (jdi-debug "jdi-location-get-class-and-method-and-line-number")
  (setf (jdi-location-class location) 
		(gethash (jdi-class-id (jdi-location-class location)) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine location))))
  (when (jdi-location-class location)
	(let ((stored-loc (jdi-class-find-location (jdi-location-class location)
											   (jdi-method-id (jdi-location-method location))
											   (jdi-location-line-code-index location))))
	  (jdi-debug "after jdi-class-find-location")
	  (if stored-loc
		  (setf (jdi-location-method location)
				(jdi-location-method stored-loc)
				(jdi-location-line-number location)
				(jdi-location-line-number stored-loc))))))

(defun jdi-location-get-line-number (location)
  (jdi-debug "jdi-location-get-line-number:wanted-line-code-index=%s" (jdi-location-line-code-index location))
  (lexical-let ((location location))
	(cont-bind (methods) (jdi-class-get-methods (jdi-location-class location))
	  (jdi-debug "jdi-location-get-line-number: methods=%s" (length methods))
	  (setf (jdi-location-method location)
			(find-if (lambda (obj)
					   (equal (jdi-method-id obj)
							  (jdi-method-id (jdi-location-method location))))
					 methods))
	  (cont-bind (locations) (jdi-method-get-locations (jdi-location-method location))
		(jdi-debug "jdi-location-get-line-number: locations=%s" (length locations))
		(let ((found))
		  (loop for loc in locations
				while (>= (jdwp-vec-to-int (jdi-location-line-code-index location))
						  (jdwp-vec-to-int (jdi-location-line-code-index loc)))
				do (setq found loc))
		  (jdi-debug "jdi-location-get-line-number:found=%s" (if found "yes" "no"))
		  (if found (setf (jdi-location-line-number location) (jdi-location-line-number found)))
		  (cont-values (jdi-location-line-number location)))))))
  
(defun jdi-thread-get-frames (thread)
  (jdi-debug "jdi-thread-get-frames")
  (lexical-let ((thread thread))
	(if (jdi-thread-frames thread)
		(cont-values (jdi-thread-frames thread))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp thread) "frame-count" `((:thread . ,(jdi-thread-id thread))))
		(jdi-debug "number of frames:%s" (bindat-get-field reply :frame-count))
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

					  for class           = (make-jdi-class :virtual-machine (jdi-mirror-virtual-machine thread) 
															:id class-id)
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
		  (cont-values (jdi-thread-frames thread)))))))

(defun jdi-thread-get-suspended-p (thread)
  (jdi-debug "jdi-thread-get-suspend-status")
  (cont-bind (status suspend-status) (jdi-thread-get-status thread)
	(cont-values (= suspend-status jdwp-suspend-status-suspended))))

(defun jdi-thread-resume (thread)
  (jdi-debug "jdi-thread-resume")
  (cont-bind (reply error jdwp id)
	(jdwp-send-command (jdi-mirror-jdwp thread) "thread-resume" `((:thread . ,(jdi-thread-id thread))))
	(cont-values t)))

(defun jdi-thread-send-step (thread depth)
  (jdi-debug "jdi-thread-send-step:depth=%s" depth)
  (lexical-let* ((thread thread)
				 (vm (jdi-mirror-virtual-machine thread))
				 (erm (jdi-virtual-machine-event-request-manager vm))
				 (er (jdi-event-request-manager-create-step erm thread depth)))
	(cont-bind (result) (jdi-event-request-enable er)
	  (jdi-thread-resume thread))))

(defun jdi-value-sigbyte (value)
  (string-to-char (jdi-value-signature value)))

(defun jdi-variable-sigbyte (variable)
  (string-to-char (jdi-variable-signature variable)))

(defun jdi-method-get-variables (method)
  (jdi-debug "jdi-method-get-variables")
  (if (jdi-method-variables method)
	  (cont-values (jdi-method-variables method))

	(lexical-let ((method method))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp method)
														  "variable-table"
														  `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
															(:method-id . ,(jdi-method-id method))))
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
		(cont-values (jdi-method-variables method))))))

(defun jdi-method-get-values (method)
  (jdi-debug "jdi-method-get-values:%s" (jdi-method-name method))
  (if (jdi-method-values method)
	  (cont-values t)

	(lexical-let ((method method))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp method)
														  "variable-table"
														  `((:ref-type . ,(jdi-class-id (jdi-method-class method)))
															(:method-id . ,(jdi-method-id method))))
		(jdi-trace "variable-table arg-count:%s slots:%s" (bindat-get-field reply :arg-cnt) (bindat-get-field reply :slots))

		(setf (jdi-method-values method)
			  (loop for slot in (bindat-get-field reply :slot)
					for code-index = (jdwp-get-int slot :code-index)
					for line-code-length = (bindat-get-field slot :length)
					do (jdi-trace "slot:%s code-index:%s length:%s name:%s signature:%s generic-signature:%s" 
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

(defun jdi-frame-get-visible-variables (frame)
  (jdi-debug "jdi-frame-get-visible-variables")
  ;; we don't cache the variables as the location within the same frame might change
  (lexical-let ((frame frame))
	(cont-bind (variables) (jdi-method-get-variables (jdi-location-method (jdi-frame-location frame)))
	  (cont-values (loop for variable in variables

						 for line-code = (jdwp-vec-to-int (jdi-location-line-code-index (jdi-frame-location frame)))
						 if (and (>= line-code (jdi-variable-code-index variable))
								 (<  line-code (+ (jdi-variable-code-index variable) (jdi-variable-length variable))))
						 collect variable)))))

(defun jdi-frame-get-values (frame variables)
  (jdi-debug "jdi-frame-get-values")
  (lexical-let ((frame frame)
				(variables variables))

	(let ((data `((:thread . ,(jdi-thread-id (jdi-frame-thread frame)))
				  (:frame  . ,(jdi-frame-id frame))
				  (:slots  . ,(length variables))
				  ,(nconc (list :slot)
						  (mapcar (lambda (variable) 
									`((:slot . ,(jdi-variable-slot variable))
									  (:sigbyte . ,(jdi-variable-sigbyte variable))))
								  variables)))))
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp frame) 
										"stack-get-values" 
										data)
		(cont-values (loop for variable in variables
						   for value in (bindat-get-field reply :value)
						   collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine frame)
												   :variable variable
												   :type (bindat-get-field value :slot-value :type)
												   :value (bindat-get-field value :slot-value :u :value))))))))

(defun jdi-frame-get-locals (frame location)
  "[ASYNC] returns a list of jdi-value that is visible in the current frame current location"
  (lexical-let* ((frame frame)
				 (location location)
				 (current-frame-line-code (jdwp-vec-to-int (jdi-location-line-code-index location))))
	(jdi-debug "jdi-frame-get-locals: frame-id=%s" (jdi-frame-id frame))
	(jdi-time-start)
	(cont-bind (result) (jdi-method-get-values (jdi-location-method location))
	  (jdi-time-end "jdi-method-get-values")
	  (jdi-debug "typeof frame=%s" (type-of frame))
	  (setf (jdi-frame-values frame)
			(loop for method-value in (jdi-method-values (jdi-location-method location))
				  if (and (>= current-frame-line-code (jdi-value-code-index method-value))
						  (<  current-frame-line-code (+ (jdi-value-code-index method-value) (jdi-value-code-index-length method-value))))
				  collect method-value))

	  (loop for value in (jdi-frame-values frame)
			do (jdi-debug "valid-slot:%s" (jdi-value-name value)))

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
				  (jdi-debug "jdi-frame-get-locals:setting value:%s to type %s" (jdi-value-name value-struct) (jdi-value-type value-struct)))
			(jdi-time-start)
			(cont-bind () (jdi-values-get-class-and-super-and-interfaces (jdi-frame-values frame))
			  (cont-wait (mapc 'jdi-value-get-string (jdi-frame-values frame))
				(jdi-time-end "mapc jdi-value-get-string")
				(cont-values (jdi-frame-values frame))))))))))

(defun jdi-value-object-p (value)
  (equal (jdi-value-type value) jdwp-tag-object))

(defun jdi-classes-get-locations (classes)
  (jdi-debug "jdi-classes-get-locations")
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
  (jdi-debug "jdi-values-get-string %s values" (length values))
  (lexical-let ((values values))
	(if (null values)
		(cont-values t)
	  (cont-bind (result) (jdi-value-get-string (car values))
		(jdi-values-get-string (cdr values))))))

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

(defun jdi-class-get-name (class)
  (cont-bind (signature) (jdi-class-get-signature class)
	(cont-values (jdi-class-name signature))))

(defmacro jdi-multiple-get-defun (name unique-id-func set-field-func accessor-func)
  `(defun ,name (structures)
	 (jdi-debug "%s:%s structures" (symbol-name ',name) (length structures))
	 (lexical-let ((structures structures)
				   (structures-hash (make-hash-table :test 'equal)))
	   (dolist (structure structures)
		 (puthash (,unique-id-func structure) structure structures-hash))
	   (jdi-debug "unique structures = %s" (hash-table-count structures-hash))
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
  (jdi-debug "jdi-classes-get-super-r")
  (if (null classes)
	  (cont-values)
	(mapc (lambda (class)
			(jdi-debug "class-signature:%s" (jdi-class-signature class)))
		  classes)
	(lexical-let ((classes classes))
	  (cont-wait (jdi-classes-get-super classes)
		(jdi-classes-get-super-r (loop for class in classes
									   if (jdi-class-super class)
									   collect (jdi-class-super class)))))))

(defun jdi-value-get-class (value)
  "populate the jdi-value-class field"
  (jdi-debug "jdi-value-get-class:variable-name=%s type=%s" (jdi-variable-name (jdi-value-variable value)) (jdi-value-type value))
  (if (equal (jdi-value-value value) [0 0 0 0 0 0 0 0])
	  (cont-values nil)

	(lexical-let ((value value))
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp value) 
														  "reference-type" 
														  `((:object . ,(jdi-value-value value))))
		(jdi-debug "jdi-value-get-class:type-id=%s" (bindat-get-field reply :type-id))
		(cont-values (make-jdi-class :virtual-machine (jdi-mirror-virtual-machine value)
									 :id (bindat-get-field reply :type-id)))))))

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

(defun jdi-format-string (str)
  "Truncate and escape the string to be displayed."
  (with-output-to-string
	(let ((print-escape-newlines t)
		  (print-escape-nonascii t))
	  (prin1 str))))

(defun jdi-value-has-children-p (value)
  (jdi-debug "jdi-value-has-children-p:variable-name:%s" (jdi-variable-name (jdi-value-variable value)))
  (and (not (equal (jdi-value-value value)
				   [0 0 0 0 0 0 0 0]))
	   (or (equal (jdi-value-type value) jdwp-tag-object)
		   (equal (jdi-value-type value) jdwp-tag-array))))

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
	(jdi-debug "jdi-class-all-fields:%s" (mapcar 'jdi-field-name all-fields))
    (sort all-fields (lambda (a b)
					   (string< (jdi-field-name a) (jdi-field-name b))))))

(defun jdi-class-get-all-fields (class)
  (jdi-debug "jdi-class-get-all-fields")
  (cont-bind (supers) (jdi-class-get-all-super class)
	(cont-mapcar 'jdi-class-get-fields supers)))

(defun jdi-value-get-nonstatic-values (value)
  (jdi-debug "jdi-value-get-nonstatic-values")
  (lexical-let* ((value value)
				 (class (jdi-value-class value))
				 (fields (loop for field in (jdi-class-all-fields class)
							   if (not (jdi-field-static-p field)) collect field))
				 (values (mapcar (lambda (field) (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
																 :name (jdi-field-name field)
																 :signature (jdi-field-signature field)
																 :generic-signature (jdi-field-generic-signature field)))
						 fields)))
	(jdi-debug "number of nonstatic fields:%s" (length fields))
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
		(jdi-debug "adding %s values to existing %s" (length values) (length (jdi-value-values value)))
		(setf (jdi-value-values value) (append (jdi-value-values value) values))
		(cont-bind () (jdi-values-get-class-and-super-and-interfaces (jdi-value-values value))
		  (cont-values t))))))

(defun jdi-value-get-static-values (value)
  "Populate the values for the static fields in jdi-value"
  (jdi-debug "jdi-value-get-static-values")
  (lexical-let* ((value value)
				 (class (jdi-value-class value))
				 (fields (loop for field in (jdi-class-all-fields class)
							   if (jdi-field-static-p field) collect field))
				 (values (mapcar (lambda (field) (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
																 :name (jdi-field-name field)
																 :signature (jdi-field-signature field)
																 :generic-signature (jdi-field-generic-signature field)))
								 fields)))
	(jdi-debug "number of static fields:%s" (length fields))
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

(defun jdi-class-get-all-super (class &optional supers)
  (jdi-debug "jdi-class-get-all-super")
  (if (null class) 
	  (cont-values supers)
	(lexical-let ((class class)
				  (supers supers))
	  (cont-bind (super) (jdi-class-get-super class)
		(if super
			(jdi-class-get-all-super super (cons super supers))
		  (cont-values supers))))))

(defun jdi-class-get-super (class)
  "populate the jdi-class-super of this class"
  (jdi-debug "jdi-class-get-super:id=%s" (jdi-class-id class))
  (if (or (jdi-class-super class)
		  (string= (jdi-class-signature class) "Ljava/lang/Object;"))
	  (cont-values (jdi-class-super class))
	(lexical-let ((class class))
	  (cont-bind (reply error jdwp id) (jdwp-send-command 
										(jdi-mirror-jdwp class) 
										"superclass" 
										`((:class . ,(jdi-class-id class))))
		(if (equal (bindat-get-field reply :superclass)
				   [0 0 0 0 0 0 0 0])
			(cont-values nil)
		  (setf (jdi-class-super class) (make-jdi-class :virtual-machine (jdi-mirror-virtual-machine class)
														:id (bindat-get-field reply :superclass)))
		  (cont-values (jdi-class-super class)))))))

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
	  (cont-values (jdi-class-interfaces class))

	(lexical-let ((class class)) 
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp class) 
														  "interfaces" 
														  `((:ref-type . ,(jdi-class-id class))))
		(setf (jdi-class-interfaces class)
			  (loop for interface in (bindat-get-field reply :interface)
					collect (make-jdi-class :virtual-machine (jdi-mirror-virtual-machine class)
											:id (bindat-get-field interface :type))))
		(setf (jdi-class-interfaces-count class) (bindat-get-field reply :interfaces))
		(cont-values (jdi-class-interfaces class))))))

(defun jdi-class-get-all-interfaces (class)
  "Gets the interfaces directly and indirectly implemented by this class. 
Interfaces returned by interfaces()  are returned as well all superinterfaces."
  (jdi-debug "jdi-class-get-all-interfaces")
  (lexical-let ((class class))
	(cont-bind (interfaces) (jdi-class-get-interfaces class)
	  (jdi-class-get-all-interfaces-2 interfaces nil))))

(defun jdi-class-get-all-interfaces-2 (classes interfaces)
  (jdi-debug "jdi-class-get-all-interfaces-2")
  (if (null classes)
	  (cont-values interfaces)
	
	(lexical-let ((classes classes)
				  (interfaces interfaces))
	  (cont-bind (interfaces2) (cont-mappend 'jdi-class-get-interfaces classes)
		(jdi-class-get-all-interfaces-2 interfaces2 (append interfaces2 interfaces))))))

;; (defun jdi-class-get-parent (class)
;;   "Get the whole parent hierarchy of this class, stop only after reaching the Object class."
;;   (jdi-debug "jdi-class-get-parent:name=%s" (jdi-class-name class))
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

;; 	  (jdi-debug "jdi-class-get-parent for %s:%s" (jdi-class-signature class) (jdi-class-id class))

;; 	  ;; NOTE: this do not resolve the super-interfaces, TODO
;; 	  (cont-bind (reply error jdwp id)
;; 		(jdwp-send-command (jdi-mirror-jdwp class) "interfaces" `((:ref-type . ,(jdi-class-id class))))
;; 		(setf (jdi-class-interfaces class)
;; 			  (loop for interface in (bindat-get-field reply :interface)
;; 					collect (gethash (bindat-get-field interface :type) (jdi-virtual-machine-classes (jdi-mirror-virtual-machine class)))))
;; 		(if jdi-debug-flag
;; 			(loop for interface in (jdi-class-interfaces class)
;; 				  do (jdi-debug "class:%s interface:%s"
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
		(cont-values (jdi-class-fields class))

	  (jdi-debug "jdi-class-resolve-fields for %s" (jdi-class-name class))
	  (cont-bind (reply error jdwp id)
		(jdwp-send-command (jdi-mirror-jdwp class) "fields" `((:ref-type . ,(jdi-class-id class))))

		(jdi-trace "%s's fields:%s" (jdi-class-name class) (bindat-get-field reply :declared))
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
		(cont-values (jdi-class-fields class))))))

(defun jdi-value-object-get-values (value)
  (jdi-debug "jdi-value-object-get-values")
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
			  (jdi-debug "done jdi-value-object-get-values")
			  (cont-wait (mapc 'jdi-value-get-string (jdi-value-values value))
				(cont-values)))))))))

(defun jdi-value-array-get-values (value)
  (jdi-debug "jdi-value-get-array-values:value=%s" (jdi-value-value value))

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
												  :name (format "%s[%s]" (jdi-value-name value) i)
												  :type (bindat-get-field value-reply :value :type)
												  :value (bindat-get-field value-reply :value :u :value)))
				  (loop for value-reply in (bindat-get-field array :value)
						for i from 0 to (- (jdi-value-array-length value) 1)
						collect (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
												:name (format "%s[%s]" (jdi-value-name value) i)
												:type (bindat-get-field array :type)
												:value (bindat-get-field value-reply :value)))))
		  (jdi-values-get-string (jdi-value-values value)))))))

(defun jdi-handle-breakpoint-event (jdwp event)
  (jdi-debug "jdi-handle-breakpoint-event")
  (lexical-let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
				 (thread-id (bindat-get-field event :u :thread))
				 (class-id (bindat-get-field event :u :location :class-id))
				 (method-id (bindat-get-field event :u :location :method-id))
				 (line-code-index (bindat-get-field event :u :location :index))

				 (class (make-jdi-class :virtual-machine vm
										:id class-id))
				 (method (make-jdi-method :virtual-machine vm
										  :id method-id
										  :class class))

				 (location (make-jdi-location :virtual-machine vm
											  :class class
											  :method method
											  :line-code-index line-code-index))
				 (thread (find-if (lambda (obj)
									(equal (jdi-thread-id obj)
										   thread-id))
								  (jdi-virtual-machine-threads vm)))

				 (frame (make-jdi-frame :virtual-machine vm
										:thread thread
										:location location)))

	(setf (jdi-thread-suspend-status thread) jdwp-suspend-status-suspended)

	(setf (jdi-virtual-machine-suspended-frames vm) (nreverse (cons frame (jdi-virtual-machine-suspended-frames vm))))

	(setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
	(run-hook-with-args 'jdi-breakpoint-hooks thread location)))
  
(defun jdi-handle-step-event (jdwp event)
  (jdi-debug "jdi-handle-step-event")
  (lexical-let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
				 (thread-id (bindat-get-field event :u :thread))
				 (class-id (bindat-get-field event :u :location :class-id))
				 (method-id (bindat-get-field event :u :location :method-id))
				 (line-code-index (bindat-get-field event :u :location :index))

				 (class (make-jdi-class :virtual-machine vm
										:id class-id))
				 (method (make-jdi-method :virtual-machine vm
										  :id method-id
										  :class class))
				 (location (make-jdi-location :virtual-machine vm
											  :class class
											  :method method
											  :line-code-index line-code-index))
				 (thread (make-jdi-thread :virtual-machine vm
										  :id thread-id))
				 (frame (make-jdi-frame :virtual-machine vm
										:thread thread
										:location location)))

	(setf (jdi-virtual-machine-suspended-frames vm) (nreverse (cons frame (jdi-virtual-machine-suspended-frames vm))))

	(setf (jdi-virtual-machine-suspended-thread-id vm) (bindat-get-field event :u :thread))
	(run-hook-with-args 'jdi-step-hooks thread location)))

(defun jdi-handle-class-prepare-event (jdwp event)
  (jdi-debug "jdi-handle-class-prepare-event")
  (lexical-let* ((vm (jdwp-get jdwp 'jdi-virtual-machine))
				 (type-id (bindat-get-field event :u :type-id))
				 (signature (jdwp-get-string event :u :signature))
				 (newclass (make-jdi-class :virtual-machine vm :id type-id :signature signature)))
	(jdi-debug "class-loaded:%s" signature)
	(puthash type-id newclass (jdi-virtual-machine-classes vm))
	(puthash signature (cons newclass (gethash signature (jdi-virtual-machine-classes-by-signature vm)))
			 (jdi-virtual-machine-classes-by-signature vm))
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
  (jdi-debug "jdi-value-instance-of-p")
  (cont-bind (class) (jdi-value-get-class value)
	(if (null class)
		(cont-values nil)

	  (lexical-let ((value value)
					(signature signature))
		(cont-bind (supers) (jdi-class-get-all-super class)
		  (cont-bind (super-signatures) (cont-mapcar 'jdi-class-get-signature supers)
			(cont-values (find-if (lambda (super-signature)
									(string= signature super-signature))
								  super-signatures))))))))

;;   (catch 'found
;;     (let ((class (jdi-value-class value)))
;;       (while class
;; 		(jdi-trace "jdi-value-instance-of-p: comparing %s with %s" (jdi-class-signature class) signature)
;; 		(if (string= (jdi-class-signature class) signature)
;; 			(throw 'found t))
;; 		(dolist (interface (jdi-class-interfaces class))
;; 		  (jdi-trace "jdi-value-instance-of-p interface: comparing %s with %s" (jdi-class-signature interface) signature)
;; 		  (if (string= (jdi-class-signature interface) signature)
;; 			  (throw 'found t)))
;; 		(setq class (jdi-class-super class)))
;;       ;; the return value if its not found
;;       nil)))

(defun jdi-value-extract-generic-class-name (generic-signature)
  (string-match "<L.*/\\(.*\\);>" generic-signature)
  (match-string 1 generic-signature))

(defun jdi-value-type-with-generic (value)
  (let ((class (jdi-value-class value))
		(gs (jdi-value-generic-signature value)))
	(if (and gs (not (string= gs "")))
		(format "%s<%s>" (jdi-class-name class) (jdi-value-extract-generic-class-name (jdi-value-generic-signature value)))
	  (jdi-class-name class))))

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

;; (defun jdi-value-invoke-method (value method-or-name &optional method-signature)
;;   "Invoke a simple method (do not require arguments) in the object in jdi-value."
;;   (jdi-debug "jdi-value-invoke-method:%s:%s" 
;; 			(if (jdi-method-p method-or-name)
;; 				(jdi-method-name method-or-name)
;; 			  method-or-name)
;; 			method-signature)
;;   (lexical-let ((value value)
;; 				(method-or-name method-or-name)
;; 				(method-signature method-signature))
;; 	(cont-bind (result) (jdi-class-get-methods (jdi-value-class value))
;; 	  (lexical-let ((method (if (jdi-method-p method-or-name)
;; 								method-or-name
;; 							  (find-if (lambda (method)
;; 										 (and (string= (jdi-method-name method) method-or-name)
;; 											  (or (null method-signature)
;; 												  (string= (jdi-method-signature method) method-signature))))
;; 									   (jdi-class-all-methods (jdi-value-class value))))))
;; 		(if method
;; 			(jdwp-send-command (jdi-mirror-jdwp value) "object-invoke-method"
;; 							   `((:object . ,(jdi-value-value value))
;; 								 (:thread . ,(jdi-virtual-machine-suspended-thread-id (jdi-mirror-virtual-machine value)))
;; 								 (:class . ,(jdi-class-id (jdi-value-class value)))
;; 								 (:method-id . ,(jdi-method-id method))
;; 								 (:arguments . 0)
;; 								 (:options . ,jdwp-invoke-single-threaded)))
;; 		  (cont-values nil nil nil nil))))))

(defun jdi-value-invoke-method (value thread method arguments options)
  "Returns another jdi-value of the result."
  (jdi-debug "jdi-value-invoke-method")
  (lexical-let ((value value)
				(thread thread)
				(method method)
				(arguments arguments)
				(options options))
	(cont-bind (class) (jdi-value-get-class value)
	  (cont-bind (reply error jdwp id) (jdwp-send-command (jdi-mirror-jdwp value) "object-invoke-method"
														  `((:object . ,(jdi-value-value value))
															(:thread . ,(jdi-thread-id thread))
															(:class . ,(jdi-class-id class))
															(:method-id . ,(jdi-method-id method))
															(:arguments . 0)
															(:options . ,jdwp-invoke-single-threaded)))
		(cont-values (make-jdi-value :virtual-machine (jdi-mirror-virtual-machine value)
									 :type (bindat-get-field reply :return-value :type)
									 :value (bindat-get-field reply :return-value :u :value)))))))

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
  (let ((handlers (list 
				   `(,jdwp-event-breakpoint    . jdi-handle-breakpoint-event)
				   `(,jdwp-event-single-step   . jdi-handle-step-event)
				   `(,jdwp-event-class-prepare . jdi-handle-class-prepare-event)
				   `(,jdwp-event-class-unload  . jdi-handle-class-unload-event)
				   `(,jdwp-event-vm-death      . jdi-handle-vm-death)
				   )))
	(mapc (lambda (handler)
			(let ((event-kind (if (integerp event) event (bindat-get-field event :event-kind))))
			  (jdi-trace "compare %s with %s" (car handler) event-kind)
			  (if (equal event-kind	(car handler))
				  (funcall (cdr handler) jdwp event))))
		  handlers)))

(add-hook 'jdwp-event-hooks 'jdi-handle-event)

(provide 'jdi)