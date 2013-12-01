(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO")

;;;; REQUEST
(defgeneric overlapped-structure (request)
  (:documentation "Return the overlapped structure owned by the REQUEST."))

(defgeneric event-handle (request)
  (:documentation "Return the event that is owned by the REQUEST."))

(defgeneric descriptor (request)
  (:documentation "The descriptor bound to the REQUEST upon issuing a
  request to the operating system to perform an I/O operation."))

(defgeneric free-request (request)
  (:documentation "Free all operating system resources owned by REQUEST."))

(defgeneric obtain-results (request)
  (:documentation "Obtain all results from the operating system
  associated with REQUEST. Returns nothing."))

(defgeneric succeededp (request)
  (:documentation "Predicate to determine if the REQUEST completed successfully."))

(defgeneric failedp (request)
  (:documentation "Predicate to determine if the REQUEST failed to complete successfully."))

(defun get-overlapped-result (request &optional wait)
  (check-type request request)
  (check-type wait (or null (integer 0)))
  (cffi:with-foreign-object (bytes-transferred 'dword)
    (let ((rv (%ff-get-overlapped-result (descriptor request)
					 (overlapped-structure request)
					 bytes-transferred
					 (if wait wait +false+))))
      (values (if (= rv +false+)
		  (%ff-get-last-error)
		  :no-error)
	      (cffi:mem-ref bytes-transferred 'dword)))))

(defvar *obtaining-results* nil)

;; This around method for OBTAIN-RESULTS is needed so that the
;; functions INVALIDP, WAITINGP and COMPLETEDP can be called within
;; OBTAIN-REQUESTS methods.
(defmethod obtain-results :around ((request t))
  (unless *obtaining-results*
    (let ((*obtaining-results* t))
      (call-next-method)))
  (values))

(defclass request ()
  ((overlapped-structure
    :initarg :overlapped-structure
    :reader overlapped-structure)
   (event-handle
    :initarg :event-handle
    :reader event-handle)
   (descriptor
    :initarg :descriptor
    :accessor descriptor
    :documentation "The DESCRIPTOR slot contains the descriptor
    assigned when the request object is in the WAITING or COMPLETEDP
    state. Note this descriptor is not closed by FREE-REQUEST.")
   (succeededp
    :initarg :succeededp
    :accessor succeededp
    :documentation "Whether the operation succeeded or not."))
  (:default-initargs
   :overlapped-structure (make-empty-overlapped-structure)
   :event-handle (%ff-create-event (cffi:null-pointer) +true+ +false+ (cffi:null-pointer))
   :descriptor nil
   :succeededp nil))

(defun reset-event (request)
  (check-type request request)
  (%ff-reset-event (event-handle request)))

(defun set-event (request)
  (check-type request request)
  (%ff-set-event (event-handle request)))

(defmethod initialize-instance :after ((object request) &key)
  (let ((ptr (overlapped-structure object))
	(handle (event-handle object)))
    (reset-event object)
    (setf (cffi:foreign-slot-value ptr '(:struct overlapped) 'h-event) handle))
  nil)

(defmethod free-request ((request request))
  (with-slots (overlapped-structure event-handle descriptor) request
    (unless (cffi:null-pointer-p overlapped-structure)
      (cffi:foreign-free overlapped-structure)
      (setf overlapped-structure (cffi:null-pointer)))
    (unless (= +null+ event-handle)
      (%ff-close-handle event-handle)
      (setf event-handle +null+))

    ;; Do not close the descriptor.
    (setf descriptor nil)))

(defmethod failedp ((request request))
  (not (succeededp request)))

(defmethod obtain-results ((request request))
  (setf (succeededp request) t))

(defun do-with-request (request function)
  (unwind-protect
       (funcall function request)
    (free-request request)))

(defmacro with-request ((var request) &body body)
  `(do-with-request ,request
     #'(lambda (,var)
	 ,@body)))

;; The functions INVALIDP, WAITINGP and COMPLETEDP are all capable of
;; updating the state of the REQUEST instance via OBTAIN-RESULTS. This
;; is why they are all interconnected.
(defun invalidp (request)
  (check-type request request)
  (or (null (descriptor request))
      (and (completedp request)
	   nil)))

(defun waitingp (request)
  (check-type request request)
  (and (descriptor request)
       (not (completedp request))))

(defun completedp (request)
  (check-type request request)
  (and (descriptor request)
       (when (wait-for-single-object request 0)
	 (obtain-results request)
	 t)))

(defun event-handle-low-bit (request)
  (check-type request request)
  (ldb (byte 1 0) (event-handle request)))

(defun (setf event-handle-low-bit) (value request)
  (check-type request request)
  (check-type value bit)
  (with-slots (event-handle) request
    (setf (ldb (byte 1 0) event-handle) value)))

;;;; Synchronising Requests
(defun wait-for-single-object (request milliseconds)
  (check-type request request)
  (check-type milliseconds (integer 0))
  (ecase (%ff-wait-for-single-object (event-handle request) milliseconds)
    (:wait-abandoned
     (error "The specified object is a mutex object that was not
     released by the thread that owned the mutex object before the
     owning thread terminated. Ownership of the mutex object is
     granted to the calling thread and the mutex state is set to
     nonsignaled.

If the mutex was protecting persistent state information, you should
check it for consistency.

This error message is copied verbatim from the Microsoft
documentation.
"))
    (:wait-object-0
     t)
    (:wait-timeout
     nil)
    (:wait-failed
     (error (error-message (%ff-get-last-error))))))

(defun wait-for-request (request seconds)
  (check-type request request)
  (check-type seconds (or (real 0) (member :indefinite :immediate)))
  (let ((milliseconds (case seconds
			(:indefinite
			 +infinite+)
			(:immediate
			 0)
			(t
			 (coerce (round (* 1000 seconds)) 'integer)))))
    (when (wait-for-single-object request milliseconds)
      (obtain-results request)
      request)))

(defun wait-for-requests (requests seconds &key wait-all)
  (check-type requests sequence)
  (check-type seconds (or (real 0) (member :indefinite :immediate)))
  (let ((length (length requests))
	(pos 0))
    (assert (< length +maximum-wait-objects+))
    (cffi:with-foreign-object (ptr-handles 'handle length)
      (map nil #'(lambda (request)
		   (assert (< pos length))
		   (setf (cffi:mem-aref ptr-handles 'handle pos) (event-handle request))
		   (incf pos))
	   requests)
      (let* ((milliseconds (case seconds
			     (:indefinite
			      +infinite+)
			     (:immediate
			      0)
			     (t
			      (coerce (round (* 1000 seconds)) 'integer))))
	     (value (%ff-wait-for-multiple-objects length
						   ptr-handles
						   (if wait-all
						       +true+
						       +false+)
						   milliseconds))
	     (offset (cffi:foreign-enum-value 'wait :wait-object-0)))
	(cond
	  ((= value (cffi:foreign-enum-value 'wait :wait-failed))
	   (error (error-message (%ff-get-last-error))))
	  ((= value (cffi:foreign-enum-value 'wait :wait-timeout))
	   nil)
	  (wait-all
	   (map nil #'obtain-results requests)
	   requests)
	  (t	   
	   (let ((rv (elt requests (- value offset))))
	     (obtain-results rv)
	     rv)))))))

;; Closing things
(defun close-handle (handle)
  (%ff-close-handle handle))

(defun do-with-handle (handle function &key ignore-close-errors)
  (unwind-protect
       (funcall function handle)
    (if ignore-close-errors
	(ignore-errors (close-handle handle))
	(close-handle handle))))

(defmacro with-handle ((var handle &rest args) &body body)
  `(do-with-handle ,handle
     #'(lambda (,var)
	 ,@body)
     ,@args))

;; Cancelling IO
(defun cancel-all-io (handle)
  (%ff-cancel-io handle))

;;;; Named Pipes
(defun valid-pipe-name-p (name)
  "Check if NAME is a valid name for a pipe.

NAME must have the form \\\\.\\pipe\\pipename. This function also
accepts //./pipe/pipename."
  (check-type name string)
  (and (or (alexandria:starts-with-subseq "\\\\.\\pipe\\" name)
	   (alexandria:starts-with-subseq "//./pipe/" name))
       (> (length name) 9)
       (< (length name) 256)))

(defun canonical-windows-pipe-name (name)
  "Convert NAME to a pipe name that can be accepted by either
CreateNamedPipe or CreateFile."
  (assert (valid-pipe-name-p name))
  (if (alexandria:starts-with-subseq "//./pipe/" name)
      (concatenate 'string "\\\\.\\pipe\\" (subseq name 10))
      name))

(deftype named-pipe-handle ()
  `(satisfies valid-named-pipe-handle-p))

(defun make-named-pipe-server (name &key (max-instances +pipe-unlimited-instances+) (output-buffer-size 1000) (input-buffer-size 1000) first-instance)
  (let* ((name (canonical-windows-pipe-name name))
	 (handle (%ff-create-named-pipe name
					(if first-instance
					    '(:pipe-access-duplex :file-flag-overlapped :file-flag-first-pipe-instance)
					    '(:pipe-access-duplex :file-flag-overlapped))
					'(:pipe-type-byte :pipe-readmode-byte)
					max-instances
					output-buffer-size
					input-buffer-size
					0
					(cffi:null-pointer))))
    (check-type handle named-pipe-handle)
    handle))

(defun connect-to-named-pipe (name)
  (let* ((name (canonical-windows-pipe-name name))
	 (handle (%ff-create-file name 
				  '(:generic-read :generic-write)
				  '(:file-share-delete :file-share-read :file-share-write)
				  (cffi:null-pointer)
				  :open-existing
				  :file-flag-overlapped
				  +null+)))
    (check-type handle named-pipe-handle)
    handle))

(defclass connect-named-pipe-request (request)
  ())

(defmethod obtain-results ((request connect-named-pipe-request))
  (unless (succeededp request)
    (ecase (get-overlapped-result request)
      (:no-error
       (setf (succeededp request) t)))))

(defun connect-named-pipe (server-handle &optional (request (make-instance 'connect-named-pipe-request)))
  (check-type server-handle named-pipe-handle)
  (setf (succeededp request) nil)  
  (reset-event request)
  (multiple-value-bind (rv error) (%ff-connect-named-pipe server-handle (overlapped-structure request))
    (declare (ignore rv))
    (ecase error
      (:error-pipe-connected
       (set-event request)       
       (setf (succeededp request) t))
      (:error-io-pending
       )))
  (setf (descriptor request) server-handle)
  request)

;;;; Reading
(defclass read-file-request (request)
  ((buffer
    :initarg :buffer
    :accessor buffer
    :documentation "A pointer to a location to store the read binary data.")
   (buffer-length
    :initarg :buffer-length
    :accessor buffer-length
    :documentation "The length of the memory pointed to by BUFFER.")
   (bytes-read
    :initarg :bytes-read
    :accessor bytes-read)))

(defmethod obtain-results ((request read-file-request))
  (assert (completedp request))
  (multiple-value-bind (error bytes-read) (get-overlapped-result request)    
    (ecase error
      (:no-error
       (setf (bytes-read request) bytes-read
	     (succeededp request) t))
      (:error-broken-pipe
       (setf (bytes-read request) 0
	     (succeededp request) nil)))))

(defun read-file (handle buffer buffer-length &optional (request (make-instance 'read-file-request)))
  (check-type handle named-pipe-handle)
  (check-type buffer (satisfies cffi:pointerp))
  (check-type buffer-length (integer 0))
  (check-type request read-file-request)
  (setf (buffer request) buffer
	(buffer-length request) buffer-length
	(succeededp request) nil)
  (reset-event request)
  (cffi:with-foreign-object (ptr-bytes-read 'dword)
    (multiple-value-bind (rv error) (%ff-read-file handle buffer buffer-length
						   ptr-bytes-read (overlapped-structure request))
      (declare (ignore rv))
      (ecase error
	(:no-error
	 (setf (bytes-read request) (cffi:mem-ref ptr-bytes-read 'dword))
	 (set-event request))
	(:error-io-pending
	 (setf (bytes-read request) nil)))))
  (setf (descriptor request) handle)
  request)

(defclass write-file-request (request)
  ((buffer
    :initarg :buffer
    :accessor buffer
    :documentation "A pointer to a location which contains the binary data to write.")
   (buffer-length
    :initarg :buffer-length
    :accessor buffer-length)
   (bytes-written
    :initarg :bytes-written
    :accessor bytes-written)))

(defmethod initialize-instance :after ((object write-file-request) &key)
  (set-event object)
  (setf (succeededp object) t))

(defmethod obtain-results ((request write-file-request))
  (assert (completedp request))
  (multiple-value-bind (error bytes-written) (get-overlapped-result request)
    (ecase error
      (:no-error
       (setf (bytes-written request) bytes-written
	     (succeededp request) t)))))

(defun write-file (handle buffer buffer-length &optional (request (make-instance 'write-file-request)))
  (check-type handle named-pipe-handle)
  (check-type buffer (satisfies cffi:pointerp))
  (check-type buffer-length (integer 0))
  (check-type request write-file-request)
  (setf (buffer request) buffer
	(buffer-length request) buffer-length
	(succeededp request) nil)
  (reset-event request)
  (cffi:with-foreign-object (ptr-bytes-written 'dword)
    (multiple-value-bind (rv error) (%ff-write-file handle buffer buffer-length
						    ptr-bytes-written
						    (overlapped-structure request))
      (declare (ignore rv))
      (ecase error
	(:no-error
	 (setf (bytes-written request) (cffi:mem-ref ptr-bytes-written 'dword))
	 (set-event request))
	(:error-io-pending
	 (setf (bytes-written request) nil)))))
  (setf (descriptor request) handle)
  request)

;;;; Monitoring (I/O Completion Ports)
(defclass monitor ()
  ((port-handle
    :initarg :port-handle
    :reader port-handle
    :documentation "The handle to the underlying I/O Completion port.")
   (key-request-table
    :initarg :key-request-table
    :accessor key-request-table
    :documentation "A hash table of REQUESTS that are being
    monitored. Keys are unsigned longs and the values are the requests.")
   (request-key-table
    :initarg :request-key-table
    :reader request-key-table))  
  (:default-initargs
   :port-handle (%ff-create-io-completion-port +invalid-handle-value+
					       +null+
					       (cffi:null-pointer)
					       0)
   :key-request-table (make-hash-table)
   :request-key-table (make-hash-table)))

(defun monitor (monitor request)
  (check-type monitor monitor)
  (check-type request request)
  (with-accessors ((key-request-table key-request-table)
		   (request-key-table request-key-table)
		   (port-handle port-handle))
      monitor
    (let* ((key-ptr (overlapped-structure request))
	   (key (cffi:pointer-address key-ptr)))
      (assert (null (gethash key key-request-table)))
      (assert (null (gethash request request-key-table)))
      
      (setf (event-handle-low-bit request) 0
	    (gethash key key-request-table) request
	    (gethash request request-key-table) key)
      (%ff-create-io-completion-port (descriptor request)
				     port-handle
				     key-ptr
				     0))))

(defun unmonitor (monitor request)
  (check-type monitor monitor)
  (check-type request request)
  (with-accessors ((key-request-table key-request-table)
		   (request-key-table request-key-table)
		   (port-handle port-handle))
      monitor
    (let ((key (gethash request request-key-table)))
      (assert key)
      (remhash key key-request-table)
      (remhash request request-key-table))
    (setf (event-handle-low-bit request) 1)))

(defun pop-notification (monitor wait-seconds)
  (check-type monitor monitor)
  (check-type wait-seconds (real 0))
  (cffi:with-foreign-objects ((number-of-bytes 'dword)
			      (ptr-completion-key '(:pointer :unsigned-long))
			      (overlapped '(:struct overlapped)))
    (multiple-value-bind (rv error) (%ff-get-queued-completion-status
				     (port-handle monitor)
				     number-of-bytes
				     ptr-completion-key
				     overlapped
				     (coerce (round (* 1000 wait-seconds)) 'integer))
      (declare (ignore rv))
      (ecase error
	(:no-error
	 (let* ((key-ptr (cffi:mem-ref ptr-completion-key :pointer))
		(key (cffi:pointer-address key-ptr))
		(rv (gethash key (key-request-table monitor))))
	   (assert rv)
	   rv))
	(:wait-timeout
	 nil)))))

(defun free-monitor (monitor)
  (check-type monitor monitor)
  (with-slots (port-handle key-request-table request-key-table) monitor
    (unless (= +invalid-handle-value+ port-handle)
      (clrhash key-request-table)
      (clrhash request-key-table)
      (%ff-close-handle port-handle)
      (setf port-handle +invalid-handle-value+))))

(defun do-with-monitor (function)
  (let ((monitor (make-instance 'monitor)))
    (unwind-protect
	 (funcall function monitor)
      (free-monitor monitor))))

(defmacro with-monitor ((var) &body body)
  `(do-with-monitor #'(lambda (,var)
			,@body)))

;;;; Sockets
(defparameter +inaddr-none+ (%ff-inet-ntoa (%ff-htonl %+inaddr-none+)))
(defparameter +inaddr-any+ (%ff-inet-ntoa (%ff-htonl %+inaddr-any+)))
(defparameter +inaddr-loopback+ (%ff-inet-ntoa (%ff-htonl %+inaddr-loopback+)))

(defun make-socket (address-family type protocol)
  (%ff-socket address-family type protocol))

(defun close-socket (socket)  
  (%ff-close-socket socket))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-sockaddr-in (function &optional address port)
    (cffi:with-foreign-object (ptr '(:struct sockaddr-in))
      (dotimes (index (cffi:foreign-type-size '(:struct sockaddr-in)))
	(setf (cffi:mem-aref ptr :uint8 index) 0))
      
      (when address
	(let ((in-addr-ptr (cffi:foreign-slot-pointer ptr '(:struct sockaddr-in) 'in-addr)))
	  (setf (cffi:foreign-slot-value in-addr-ptr '(:struct in-addr) 's-addr) (%ff-inet-addr address))))
      
      (when port
	(cffi:with-foreign-slots ((sin-family sin-port) ptr (:struct sockaddr-in))
	  (setf sin-family (cffi:foreign-enum-value 'socket-address-family :af-inet)
		sin-port (%ff-htons port))))

      (funcall function ptr)))

  (defmacro with-sockaddr-in ((var &optional address port) &body body)
    `(do-with-sockaddr-in #'(lambda (,var)
			      ,@body)
       ,address ,port)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-initialising-socket-progn (socket function)
    (alexandria:unwind-protect-case ()
	(funcall function socket)
      (:abort
       (close-socket socket))))

  (defmacro initialising-socket-progn ((var form) &body body)
    `(do-initialising-socket-progn ,form
       #'(lambda (,var)
	   ,@body))))

(defun make-ipv4-server (address port &key (backlog 5))
  (initialising-socket-progn (socket (%ff-socket :af-inet :sock-stream :ipproto-tcp))
    (with-sockaddr-in (socket-address address port)
      (%ff-bind socket socket-address (cffi:foreign-type-size '(:struct sockaddr-in))))
    (%ff-listen socket backlog)
    socket))

(defclass accept-ipv4-request (request)
  ((client-descriptor
    :initarg :client-descriptor
    :accessor client-descriptor)
   (buffer
    :initarg :buffer
    :accessor buffer)
   (buffer-length
    :initarg :buffer-length
    :accessor buffer-length)
   (bytes-read
    :initarg :bytes-read
    :accessor bytes-read)
   (local-address
    :initarg :local-address
    :accessor local-address)
   (local-port
    :initarg :local-port
    :accessor local-port)
   (remote-address
    :initarg :remote-address
    :accessor remote-address)
   (remove-port
    :initarg :remote-port
    :accessor remote-port)))

(defparameter +wsaid-getacceptexsockaddrs+ #(242 125 54 181 172 203 207 17 149 202 0 128 95 72 161 146))
(defparameter +wsaid-acceptex+ #(241 125 54 181 172 203 207 17 149 202 0 128 95 72 161 146))
(defparameter +wsaid-connectex+ #(185 7 162 37 243 221 96 70 142 233 118 229 140 116 6 62))

(defun compute-socket-guid-function-pointer (socket guid)
  (cffi:with-foreign-objects ((in-buffer :uint8 (length guid))
			      (out-buffer :pointer)
			      (bytes-returned 'dword))
    (dotimes (index (length guid))
      (setf (cffi:mem-aref in-buffer :uint8 index) (aref guid index)))

    (dotimes (index (cffi:foreign-type-size :pointer))
      (setf (cffi:mem-aref out-buffer :uint8 index) 0))
    
    (%ff-wsaioctl socket :sio-get-extension-function-pointer
		  in-buffer (length +wsaid-getacceptexsockaddrs+)
		  out-buffer (cffi:foreign-type-size :pointer)
		  bytes-returned
		  (cffi:null-pointer)
		  (cffi:null-pointer))
    (assert (= (cffi:mem-ref bytes-returned 'dword) (cffi:foreign-type-size :pointer)))
    (cffi:mem-ref out-buffer :pointer)))

(defun make-getacceptexsockaddrs-function (socket)
  (let ((ptr (compute-socket-guid-function-pointer socket +wsaid-getacceptexsockaddrs+)))
    (lambda (buffer receive-data-length local-address-length remote-address-length
	     local-sockaddr local-sockaddr-length
	     remote-sockaddr remote-sockaddr-length)
      (cffi:foreign-funcall-pointer ptr nil
				    :pointer buffer
				    dword receive-data-length
				    dword local-address-length
				    dword remote-address-length
				    :pointer local-sockaddr
				    :pointer local-sockaddr-length
				    :pointer remote-sockaddr
				    :pointer remote-sockaddr-length
				    :void))))

(defun make-acceptex-function (socket)
  (let ((ptr (compute-socket-guid-function-pointer socket +wsaid-acceptex+)))
    (lambda (listen-socket accept-socket output-buffer
	     receive-data-length local-address-length remote-address-length
	     bytes-received overlapped)
      (let ((rv (cffi:foreign-funcall-pointer ptr nil
					      socket listen-socket
					      socket accept-socket
					      :pointer output-buffer
					      dword receive-data-length
					      dword local-address-length
					      dword remote-address-length
					      :pointer bytes-received
					      :pointer overlapped
					      bool)))
	(check-socket-overlapped #'make-acceptex-function "AcceptEx" rv)))))

(defun make-connectex-function (socket)
  (let ((ptr (compute-socket-guid-function-pointer socket +wsaid-connectex+)))
    (lambda (socket name name-length send-buffer send-data-length bytes-sent overlapped)
      (let ((rv (cffi:foreign-funcall-pointer ptr nil
					      socket socket
					      :pointer name
					      :int name-length
					      :pointer send-buffer
					      dword send-data-length
					      (:pointer dword) bytes-sent
					      (:pointer (:struct overlapped)) overlapped
					      bool)))
	(check-socket-overlapped #'make-connectex-function "ConnectEx" rv)))))

(defun accept-ipv4-socket-addresses (request)
  (check-type request accept-ipv4-request)
  (cffi:with-foreign-objects ((ptr-socket 'socket))
    (setf (cffi:mem-ref ptr-socket 'socket) (descriptor request))
    (%ff-setsockopt (client-descriptor request) :sol-socket
		    :so-update-accept-context
		    ptr-socket (cffi:foreign-type-size 'socket)))
  (cffi:with-foreign-objects ((ptr-name '(:struct sockaddr-in))
			      (ptr-name-length :int))
    (setf (cffi:mem-ref ptr-name-length :int) (cffi:foreign-type-size '(:struct sockaddr-in)))
    (dotimes (index (cffi:foreign-type-size '(:struct sockaddr-in)))
      (setf (cffi:mem-aref ptr-name :uint8 index) 0))
    
    (labels ((sockaddr-in/address (ptr)
	       (let ((ptr (cffi:foreign-slot-pointer ptr '(:struct sockaddr-in) 'in-addr)))
		 (%ff-inet-ntoa (cffi:foreign-slot-value ptr '(:struct in-addr) 'S-addr))))
	     (sockaddr-in/port (ptr)
	       (%ff-ntohs (cffi:foreign-slot-value ptr '(:struct sockaddr-in) 'sin-port))))
      (%ff-getpeername (client-descriptor request) ptr-name ptr-name-length)
      (setf (remote-address request) (sockaddr-in/address ptr-name)
	    (remote-port request) (sockaddr-in/port ptr-name))
      
      (%ff-getsockname (client-descriptor request) ptr-name ptr-name-length)
      (setf (local-address request) (sockaddr-in/address ptr-name)
	    (local-port request) (sockaddr-in/port ptr-name)))))

(defmethod obtain-results ((request accept-ipv4-request))
  (assert (completedp request))
  (multiple-value-bind (error bytes-read) (get-overlapped-result request)
    (ecase error
      (:no-error
       (setf (bytes-read request) bytes-read
	     (succeededp request) t)
       (accept-ipv4-socket-addresses request)))))

(defun minimum-accept-ipv4-buffer-size ()
  (* 2 (+ 16 (cffi:foreign-type-size '(:struct sockaddr-in)))))

(defun accept-ipv4 (listen-socket accept-socket buffer buffer-length &optional (request (make-instance 'accept-ipv4-request)))
  (setf (buffer request) buffer
	(buffer-length request) buffer-length
	(client-descriptor request) accept-socket
	(descriptor request) listen-socket
	(succeededp request) nil)
  (reset-event request)
  (let* ((local-address-length (+ 16 (cffi:foreign-type-size '(:struct sockaddr-in))))
	 (remote-address-length local-address-length)
	 (received-data-length (- buffer-length local-address-length remote-address-length))
	 (fn (make-acceptex-function listen-socket)))
    (unless (>= received-data-length 0)
      (error "Buffer is not big enough. Please increase the size of the buffer and/or read the documentation on the function AcceptEx."))
    (unless (zerop received-data-length)
      (warn "RECEIVED-DATA-LENGTH is not 0."))
    (cffi:with-foreign-object (ptr-bytes-read 'dword)
      (multiple-value-bind (rv error) (funcall fn
					       listen-socket accept-socket buffer
					       received-data-length local-address-length remote-address-length
					       ptr-bytes-read (overlapped-structure request))
	(cond
	  ((= rv +true+)
	   (setf (bytes-read request) (cffi:mem-ref ptr-bytes-read 'dword))
	   (set-event request)
	   (accept-ipv4-socket-addresses request))
	  ((eql error :wsa-io-pending)
	   (setf (bytes-read request) nil
		 (local-address request) nil
		 (local-port request) nil
		 (remote-address request) nil
		 (remote-port request) nil))
	  (t
	   (error "Unsupported error encountered in ACCEPT-IPV4: ~A" error))))))
  request)

(defclass connect-ipv4-request (request)
  ((local-address
    :initarg :local-address
    :accessor local-address)
   (local-port
    :initarg :local-port
    :accessor local-port)
   (remote-address
    :initarg :remote-address
    :accessor remote-address)
   (remote-port
    :initarg :remote-port
    :accessor remote-port)))

(defmethod obtain-results ((request connect-ipv4-request))
  (ecase (get-overlapped-result request)
    (:no-error
     (%ff-setsockopt (descriptor request)
		     :sol-socket
		     :so-update-connect-context
		     (cffi:null-pointer) 0)
     (setf (succeededp request) t))
    ((:error-connection-refused :error-sem-timeout)
     (setf (succeededp request) nil))))

(defun connect-ipv4 (socket address port &optional (request (make-instance 'connect-ipv4-request)) (local-address +inaddr-any+) (local-port 0))
  (check-type request connect-ipv4-request)
  (setf (local-address request) nil
	(local-port request) nil
	(remote-address request) address
	(remote-port request) port
	(succeededp request) nil)
  (reset-event request)
  (let* ((fn (make-connectex-function socket)))
    (with-sockaddr-in (name local-address local-port)
      (%ff-bind socket name (cffi:foreign-type-size '(:struct sockaddr-in))))
    
    (labels ((sockaddr-in/address (ptr)
	       (let ((ptr (cffi:foreign-slot-pointer ptr '(:struct sockaddr-in) 'in-addr)))
		 (%ff-inet-ntoa (cffi:foreign-slot-value ptr '(:struct in-addr) 'S-addr))))
	     (sockaddr-in/port (ptr)
	       (%ff-ntohs (cffi:foreign-slot-value ptr '(:struct sockaddr-in) 'sin-port))))
      (cffi:with-foreign-objects ((ptr-name '(:struct sockaddr-in))
				  (ptr-name-length :int))
	(setf (cffi:mem-ref ptr-name-length :int) (cffi:foreign-type-size '(:struct sockaddr-in)))
	
	(%ff-getsockname socket ptr-name ptr-name-length)
	(setf (local-address request) (sockaddr-in/address ptr-name)
	      (local-port request) (sockaddr-in/port ptr-name))))

    (with-sockaddr-in (name address port)
      (multiple-value-bind (rv error) (funcall fn socket name (cffi:foreign-type-size '(:struct sockaddr-in))
					       (cffi:null-pointer) 0 (cffi:null-pointer)
					       (overlapped-structure request))
	(cond
	  ((= rv +true+)
	   (set-event request))
	  ((eql error :wsa-io-pending))
	  (t
	   (error "Unsupported error encountered in CONNECT-IPV4: ~A" error)))))
    (setf (descriptor request) socket)
    request))

;;;; Helper functions
(defun make-empty-overlapped-structure ()
  (let ((ptr (cffi:foreign-alloc '(:struct overlapped))))
    (dotimes (index (cffi:foreign-type-size '(:struct overlapped)))
      (setf (cffi:mem-ref ptr :uint8 index) 0))
    ptr))

(defun valid-named-pipe-handle-p (handle)
  (and (/= handle +invalid-handle-value+)
       (plusp handle)))
