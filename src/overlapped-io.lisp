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
    state. Note this descriptor is not closed by FREE-REQUEST."))
  (:default-initargs
   :overlapped-structure (make-empty-overlapped-structure)
   :event-handle (%ff-create-event (cffi:null-pointer) +true+ +false+ "request")
   :descriptor nil))

(defmethod initialize-instance :after ((object request) &key)
  (let ((ptr (overlapped-structure object))
	(handle (event-handle object)))
    (%ff-reset-event handle)
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

(defun do-with-request (request function)
  (unwind-protect
       (funcall function request)
    (free-request request)))

(defmacro with-request ((var request) &body body)
  `(do-with-request ,request
     #'(lambda (,var)
	 ,@body)))

(defun invalidp (request)
  (check-type request request)
  (or (null (descriptor request))
      (and (waitingp request)
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

(defun make-named-pipe-server (name &key (max-instances +pipe-unlimited-instances+) (output-buffer-size 1000) (input-buffer-size 1000))
  (let* ((name (canonical-windows-pipe-name name))
	 (handle (%ff-create-named-pipe name
					'(:pipe-access-duplex :file-flag-overlapped)
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
  (values))

(defun connect-named-pipe (server-handle &optional (request (make-instance 'connect-named-pipe-request)))
  (check-type server-handle named-pipe-handle)
  (%ff-reset-event (event-handle request))
  (multiple-value-bind (rv error) (%ff-connect-named-pipe server-handle (overlapped-structure request))
    (declare (ignore rv))
    (ecase error
      (:error-pipe-connected
       (%ff-set-event (event-handle request)))
      ((:no-error :error-io-pending)
       )))
  (setf (descriptor request) server-handle)
  request)

;;;; Reading
(deftype binary-buffer ()
  `(simple-array (unsigned-byte 8) (*)))

(defvar *default-buffer-length* 2048)

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
  (cffi:with-foreign-object (ptr-bytes-read 'dword)
    (multiple-value-bind (rv error) (%ff-get-overlapped-result (descriptor request)
							       (overlapped-structure request)
							       ptr-bytes-read
							       +false+)
      (declare (ignore rv))
      (ecase error
	(:no-error
	 (setf (bytes-read request) (cffi:mem-ref ptr-bytes-read 'dword)))
	(:error-broken-pipe
	 (setf (bytes-read request) 0))))))

(defun read-file (handle buffer buffer-length &optional (request (make-instance 'read-file-request)))
  (check-type handle named-pipe-handle)
  (check-type buffer (satisfies cffi:pointerp))
  (check-type buffer-length (integer 0))
  (check-type request read-file-request)
  (setf (buffer request) buffer
	(buffer-length request) buffer-length)
  (%ff-reset-event (event-handle request))
  (cffi:with-foreign-object (ptr-bytes-read 'dword)
    (multiple-value-bind (rv error) (%ff-read-file handle buffer buffer-length
						   ptr-bytes-read (overlapped-structure request))
      (declare (ignore rv))
      (ecase error
	(:no-error
	 (setf (bytes-read request) (cffi:mem-ref ptr-bytes-read 'dword))
	 (%ff-set-event (event-handle request)))
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

(defmethod obtain-results ((request write-file-request))
  (assert (completedp request))
  (cffi:with-foreign-object (ptr-bytes-written 'dword)
    (%ff-get-overlapped-result (descriptor request)
			       (overlapped-structure request)
			       ptr-bytes-written
			       +false+)
    (setf (bytes-written request) (cffi:mem-ref ptr-bytes-written 'dword))))

(defun write-file (handle buffer buffer-length &optional (request (make-instance 'write-file-request)))
  (check-type handle named-pipe-handle)
  (check-type buffer (satisfies cffi:pointerp))
  (check-type buffer-length (integer 0))
  (check-type request write-file-request)
  (setf (buffer request) buffer
	(buffer-length request) buffer-length)
  (%ff-reset-event (event-handle request))
  (cffi:with-foreign-object (ptr-bytes-written 'dword)
    (multiple-value-bind (rv error) (%ff-write-file handle buffer buffer-length
						    ptr-bytes-written (overlapped-structure request))
      (declare (ignore rv))
      (ecase error
	(:no-error
	 (setf (bytes-written request) (cffi:mem-ref ptr-bytes-written 'dword))
	 (%ff-set-event (event-handle request)))
	(:error-io-pending
	 (setf (bytes-written request) nil)))))
  (setf (descriptor request) handle)
  request)

;;;; Helper functions
(defun make-empty-overlapped-structure ()
  (let ((ptr (cffi:foreign-alloc '(:struct overlapped))))
    (dotimes (index (cffi:foreign-type-size '(:struct overlapped)))
      (setf (cffi:mem-ref ptr :uint8 index) 0))
    ptr))

(defun valid-named-pipe-handle-p (handle)
  (and (/= handle +invalid-handle-value+)
       (plusp handle)))
