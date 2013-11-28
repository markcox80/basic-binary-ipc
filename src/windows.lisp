(in-package "BASIC-BINARY-IPC")

;;;; POSIX-ERROR condition (TEMPORARY)
(define-condition posix-error (error)
  ())

(define-condition posix-error/system-function-error (posix-error)
  ((system-function-error
    :initarg :system-function-error
    :reader system-function-error))
  (:report (lambda (condition stream)
	     (format stream "~A" (system-function-error condition)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-wrap-system-function-error-progn (function)
    (handler-case (funcall function)
      (system-function-error (c)
	(error 'posix-error/system-function-error :system-function-error c))))

  (defmacro wrap-system-function-error-progn (&body body)
    `(do-wrap-system-function-error-progn #'(lambda () ,@body))))

;;;; File handle stream
(defvar *default-read-buffer-size* 4098
  "The number of unsigned bytes to use to store data read from a socket.")

(defclass file-handle-stream (stream-socket)
  ((descriptor
    :initarg :descriptor
    :accessor descriptor)
   (read-request
    :initarg :read-request
    :reader read-request)
   (write-request
    :initarg :write-request
    :reader write-request)
   (read-buffer-size
    :initarg :read-buffer-size
    :reader read-buffer-size)
   (read-buffer
    :initarg :read-buffer
    :reader read-buffer)
   (interface-buffer
    :initarg :interface-buffer
    :reader interface-buffer))
  (:default-initargs
   :read-buffer-size *default-read-buffer-size*
   :read-buffer (cffi:foreign-alloc :uint8 :count *default-read-buffer-size*)
   :interface-buffer (make-array *default-read-buffer-size* :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
   :read-request (make-instance 'basic-binary-ipc.overlapped-io:read-file-request)
   :write-request (make-instance 'basic-binary-ipc.overlapped-io:write-file-request)))

(defmethod initialize-instance :after ((object file-handle-stream) &key)
  (with-accessors ((descriptor descriptor)
		   (read-buffer read-buffer)
		   (read-buffer-size read-buffer-size)
		   (read-request read-request)
		   (write-request write-request))
      object
    (setf (basic-binary-ipc.overlapped-io:descriptor read-request) descriptor
	  (basic-binary-ipc.overlapped-io:descriptor write-request) descriptor)

    (basic-binary-ipc.overlapped-io:read-file descriptor
					      read-buffer 
					      read-buffer-size
					      read-request)))

(defmethod close-socket ((socket file-handle-stream))
  (with-slots (read-buffer descriptor) socket
    (unless (cffi:null-pointer-p read-buffer)
      (basic-binary-ipc.overlapped-io:close-handle (descriptor socket))
      (setf descriptor 0)

      (cffi:foreign-free read-buffer)
      (setf read-buffer (cffi:null-pointer)))))

(defmethod ready-to-write-p ((socket file-handle-stream))
  (and (connection-succeeded-p socket)
       (basic-binary-ipc.overlapped-io:completedp (write-request socket))
       (basic-binary-ipc.overlapped-io:succeededp (write-request socket))))

(defmethod data-available-p ((socket file-handle-stream))
  (or (plusp (length (interface-buffer socket)))
      (and (basic-binary-ipc.overlapped-io:completedp (read-request socket))
	   (basic-binary-ipc.overlapped-io:succeededp (read-request socket)))))

(defmethod write-to-stream ((socket file-handle-stream) buffer &key start end)
  (check-type buffer (simple-array (unsigned-byte 8) (*)))
  (check-type start (or null (integer 0)))
  (check-type end (or null (integer 0)))
  (cond
    ((ready-to-write-p socket)
     (let* ((start (or start 0))
	    (end (or end (length buffer)))
	    (length (- end start)))
       (check-type length (integer 0))

       (when (or (minusp start) (> start (length buffer)))
	 (error "START argument is invalid. ~d" start))

       (when (> end (length buffer))
	 (error "END argument is invalid. ~d" end))

       (with-accessors ((descriptor descriptor)
			(write-request write-request))
	   socket
	 (cffi:with-pointer-to-vector-data (buffer-ptr buffer)
	   (let ((start-ptr (cffi:inc-pointer buffer-ptr start)))
	     (basic-binary-ipc.overlapped-io:write-file descriptor start-ptr length write-request)
	     length)))))
    (t
     0)))

(defun transfer-read-buffer-data (socket)
  "Copy the data from READ-BUFFER to the INTERFACE-BUFFER."
  (check-type socket file-handle-stream)  
  (with-accessors ((read-buffer read-buffer)
		   (read-buffer-size read-buffer-size)
		   (read-request read-request)
		   (interface-buffer interface-buffer))
      socket
    (assert (basic-binary-ipc.overlapped-io:succeededp read-request))
    (let ((bytes-read (basic-binary-ipc.overlapped-io:bytes-read read-request)))      
      (adjust-array interface-buffer (+ (length interface-buffer) bytes-read))
      (dotimes (index bytes-read)
	(vector-push (cffi:mem-aref read-buffer :uint8 index) interface-buffer))
      bytes-read)))

(defmethod read-from-stream ((socket file-handle-stream) buffer &key start end peek)  
  (check-type buffer (simple-array (unsigned-byte 8) (*)))
  (check-type start (or null (integer 0)))
  (check-type end (or null (integer 0)))
  (with-accessors ((interface-buffer interface-buffer)
		   (read-buffer read-buffer)
		   (read-buffer-size read-buffer-size)
		   (read-request read-request)
		   (descriptor descriptor))
      socket
    (let* ((start (or start 0))
	   (end (or end (length buffer)))
	   (maximum-bytes-to-read (- end start)))
      (check-type maximum-bytes-to-read (integer 0))

      (when (or (minusp start) (> start (length buffer)))
	(error "START argument is invalid: ~d" start))

      (when (> end (length buffer))
	(error "END ARGUMENT is invalid: ~d" end))

      ;; Fill the interface buffer
      (loop
	 :while (and (> maximum-bytes-to-read (length interface-buffer))
		     (basic-binary-ipc.overlapped-io:completedp read-request)
		     (basic-binary-ipc.overlapped-io:succeededp read-request))
	 :do
	 (transfer-read-buffer-data socket)
	 (basic-binary-ipc.overlapped-io:read-file descriptor
						   read-buffer
						   read-buffer-size
						   read-request))
      (let* ((bytes-read (min maximum-bytes-to-read (length interface-buffer)))
	     (bytes-left (- (length interface-buffer) bytes-read)))
	(check-type bytes-read (integer 0))
	(check-type bytes-left (integer 0))
	;; Copy the data over to the buffer
	(loop
	   :for index :from 0 :below bytes-read
	   :for position :from start
	   :do
	   (setf (aref buffer position) (aref interface-buffer index)))
	;; Reorganise the interface buffer.
	(unless peek
	  (loop
	     :for position :from 0 :below bytes-left
	     :for index :from bytes-read
	     :do
	     (setf (aref interface-buffer position) (aref interface-buffer index)))
	  (setf (fill-pointer interface-buffer) bytes-left))
	bytes-read))))

;;;; Local namespace
(defclass local-server (stream-server)
  ((local-pathname
    :initarg :local-pathname
    :reader local-pathname)
   (descriptor
    :initarg :descriptor
    :accessor descriptor)
   (connect-request
    :initarg :connect-request
    :reader connect-request)))

(defmethod close-socket ((socket local-server))  
  (basic-binary-ipc.overlapped-io:close-handle (descriptor socket))
  (basic-binary-ipc.overlapped-io:free-request (connect-request socket)))

(defclass local-stream (file-handle-stream)
  ((determinedp-request
    :initarg :determinedp-request
    :reader determinedp-request)
   (local-pathname
    :initarg :local-pathname
    :reader local-pathname))
  (:default-initargs
   :determinedp-request (let ((rv (make-instance 'basic-binary-ipc.overlapped-io:request)))
			  (basic-binary-ipc.overlapped-io:set-event rv)
			  rv)))

(defmethod close-socket ((socket local-stream))
  (call-next-method)
  (basic-binary-ipc.overlapped-io:free-request (determinedp-request socket)))

(defmethod determinedp ((socket local-stream))
  t)

(defmethod connection-succeeded-p ((socket local-stream))
  (let ((request (read-request socket)))
    (not (and (basic-binary-ipc.overlapped-io:completedp request)
	      (basic-binary-ipc.overlapped-io:failedp request)))))

(defmethod connection-failed-p ((socket local-stream))
  (not (connection-succeeded-p socket)))

(defmethod remote-disconnected-p ((socket local-stream))
  (not (connection-succeeded-p socket)))

(defun make-local-server (pathname &key &allow-other-keys)
  (let ((handle (wrap-system-function-error-progn
		  (basic-binary-ipc.overlapped-io:make-named-pipe-server pathname
									 :first-instance t))))
    (alexandria:unwind-protect-case ()
      (make-instance 'local-server
		     :local-pathname pathname
		     :descriptor handle
		     :connect-request (basic-binary-ipc.overlapped-io:connect-named-pipe handle))
      (:abort
       (basic-binary-ipc.overlapped-io:close-handle handle)))))

(defmethod connection-available-p ((server local-server))
  (basic-binary-ipc.overlapped-io:completedp (connect-request server)))

(defmethod accept-connection ((server local-server))
  (cond
    ((basic-binary-ipc.overlapped-io:completedp (connect-request server))
     (assert (basic-binary-ipc.overlapped-io:succeededp (connect-request server)))
     (prog1 (make-instance 'local-stream
			   :descriptor (descriptor server)
			   :local-pathname (local-pathname server))
       (let ((handle (basic-binary-ipc.overlapped-io:make-named-pipe-server (local-pathname server))))
	 (alexandria:unwind-protect-case ()	    
	     (progn
	       (basic-binary-ipc.overlapped-io:connect-named-pipe handle (connect-request server))
	       (setf (descriptor server) handle))
	   (:abort
	    (basic-binary-ipc.overlapped-io:close-handle handle))))))
    (t
     (error 'no-connection-available-error :socket server))))

(define-condition no-local-server-error (error)
  ((local-pathname
    :initarg :local-pathname
    :reader local-pathname))
  (:report (lambda (condition stream)
	     (format stream "No named pipe server exists at pathname ~S." (local-pathname condition)))))

(defun connect-to-local-server (pathname &key &allow-other-keys)
  (handler-case (let ((handle (basic-binary-ipc.overlapped-io:connect-to-named-pipe pathname)))
		  (make-instance 'local-stream
				 :descriptor handle
				 :local-pathname pathname))
    (system-function-error (c)
      (if (eql :error-file-not-found (system-function-error-value c))
	  (error 'no-local-server-error :local-pathname pathname)
	  (error c)))))

;;;; Polling
(defgeneric poll-socket-request (socket socket-event))
(defmethod poll-socket-request ((socket stream-server) (socket-event (eql 'connection-available-p)))
  (connect-request socket))

(defmethod poll-socket-request ((socket file-handle-stream) (socket-event (eql 'data-available-p)))
  (read-request socket))

(defmethod poll-socket-request ((socket file-handle-stream) (socket-event (eql 'ready-to-write-p)))
  (write-request socket))

(defmethod poll-socket-request ((socket local-stream) (socket-event (eql 'determinedp)))
  (determinedp-request socket))

(defmethod poll-socket-request ((socket local-stream) (socket-event (eql 'connection-succeeded-p)))
  (determinedp-request socket))

(defmethod poll-socket-request ((socket local-stream) (socket-event (eql 'connection-failed-p)))
  (read-request socket))

(defmethod poll-socket-request ((socket local-stream) (socket-event (eql 'remote-disconnected-p)))
  (read-request socket))

(defun poll-socket (socket socket-events timeout)
  (first (poll-sockets (list socket) (list socket-events) timeout)))

(defun poll-sockets (all-sockets all-socket-events timeout)
  (let* ((all-socket-requests (mapcar #'(lambda (socket socket-events)
					  (mapcar #'(lambda (socket-event)
						      (poll-socket-request socket socket-event))
						  (if (listp socket-events)
						      socket-events
						      (list socket-events))))
				      all-sockets all-socket-events))
	 (all-requests (reduce #'append all-socket-requests))
	 (request (basic-binary-ipc.overlapped-io:wait-for-requests all-requests timeout)))
    (mapcar #'(lambda (socket socket-events socket-requests)
		(let ((matches (loop
				  :for socket-event :in (if (listp socket-events)
							    socket-events
							    (list socket-events))
				  :for socket-request :in socket-requests
				  :when (and (eql request socket-request)
					     (ecase socket-event
					       (connection-available-p
						(connection-available-p socket))
					       (remote-disconnected-p
						(remote-disconnected-p socket))
					       (ready-to-write-p
						(ready-to-write-p socket))
					       (data-available-p
						(data-available-p socket))
					       (determinedp
						(determinedp socket))
					       (connection-failed-p
						(connection-failed-p socket))
					       (connection-succeeded-p
						(connection-succeeded-p socket))))
				  :collect socket-event)))
		  (if (listp socket-events)
		      matches
		      (first matches))))
	    all-sockets all-socket-events all-socket-requests)))
