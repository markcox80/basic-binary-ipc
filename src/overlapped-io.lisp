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

(defun invalidp (request)
  (check-type request request)
  (null (descriptor request)))

(defun waitingp (request)
  (check-type request request)
  (and (descriptor request)
       (null (wait-for-single-object request 0))))

(defun completedp (request)
  (check-type request request)
  (and (descriptor request)
       (wait-for-single-object request 0)))

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
    handle))

(defclass connect-named-pipe-request (request)
  ())

(defun connect-named-pipe (server-handle &optional (request (make-instance 'connect-named-pipe-request)))
  (assert (null (wait-for-single-object request 0)))
  (%ff-connect-named-pipe server-handle (overlapped-structure request))
  (setf (descriptor request) server-handle)
  request)

;;;; Helper functions
(defun make-empty-overlapped-structure ()
  (let ((ptr (cffi:foreign-alloc '(:struct overlapped))))
    (dotimes (index (cffi:foreign-type-size '(:struct overlapped)))
      (setf (cffi:mem-ref ptr :uint8 index) 0))
    ptr))
