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
   :interface-buffer (make-array *default-read-buffer-size* :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

(defmethod close-socket ((socket file-handle-stream))
  (with-slots (read-buffer descriptor) socket
    (unless (cffi:null-pointer-p read-buffer)
      (basic-binary-ipc.overlapped-io:close-handle (descriptor socket))
      (setf descriptor 0)

      (cffi:foreign-free read-buffer)
      (setf read-buffer (cffi:null-pointer)))))

(defmethod ready-to-write-p ((socket file-handle-stream))
  (basic-binary-ipc.overlapped-io:succeededp (write-request socket)))

(defmethod data-available-p ((socket file-handle-stream))
  (or (plusp (length (interface-buffer socket)))
      (basic-binary-ipc.overlapped-io:succeededp (read-request socket))))

(defmethod write-to-stream ((socket file-handle-stream) buffer &key start end)
  (check-type buffer (simple-array (unsigned-byte 8) (*)))
  (check-type start (or null (integer 0)))
  (check-type end (or null (integer 0)))
  (assert (ready-to-write-p socket))
  (let* ((start (or start 0))
	 (end (or end (length buffer)))
	 (length (- end start)))
    (check-type length (integer 0))
    (with-accessors ((descriptor descriptor)
		     (write-request write-request))
	socket
      (cffi:with-pointer-to-vector-data (buffer-ptr buffer)
	(let ((start-ptr (cffi:inc-pointer buffer-ptr start)))
	  (basic-binary-ipc.overlapped-io:write-file descriptor start-ptr length write-request)
	  length)))))

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
      (dotimes (index read-buffer-size)
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
      ;; Fill the interface buffer
      (loop
	 :while (and (> maximum-bytes-to-read (length interface-buffer))
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
	   (setf (aref buffer position) (aref interface-buffer position)))
	;; Reorganise the interface buffer.
	(unless peek
	  (loop
	     :for index :from 0 :below bytes-left
	     :for position :from bytes-read
	     :do
	     (setf (aref interface-buffer index) (aref interface-buffer position)))
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
  ())

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
     (prog1 (make-instance 'local-stream :descriptor (descriptor server))
       (let ((handle (basic-binary-ipc.overlapped-io:make-named-pipe-server (local-pathname server))))
	 (alexandria:unwind-protect-case ()	    
	     (progn
	       (basic-binary-ipc.overlapped-io:connect-named-pipe handle (connect-request server))
	       (setf (descriptor server) handle))
	   (:abort
	    (basic-binary-ipc.overlapped-io:close-handle handle))))))
    (t
     (error 'no-connection-available-error :socket server))))

(defclass failed-local-stream ()
  ())

(defun connect-to-local-server (pathname &key &allow-other-keys)
  (let ((handle (basic-binary-ipc.overlapped-io:connect-to-named-pipe pathname)))
    (if (basic-binary-ipc.overlapped-io:valid-named-pipe-handle-p handle)
	(make-instance 'local-stream :descriptor handle)
	(make-instance 'failed-local-stream))))

;;;; Polling
(defgeneric poll-socket-request (socket socket-event))
(defmethod poll-socket-request ((socket stream-server) (socket-event (eql 'connection-available-p)))
  (connect-request socket))

(defmethod poll-socket-request ((socket file-handle-stream) (socket-event (eql 'data-available-p)))
  (read-request socket))

(defmethod poll-socket-request ((socket file-handle-stream) (socket-event (eql 'ready-to-write-p)))
  (write-request socket))

(defun poll-socket (socket socket-events timeout)
  (first (poll-sockets (list socket) (list socket-events) timeout)))

(defun poll-sockets (all-sockets all-socket-events timeout)
  (let* ((all-socket-requests (mapcar #'(lambda (socket socket-events)
					  (mapcar #'(lambda (socket-event)
						      (poll-socket-request socket socket-event))
						  socket-events))
				      all-sockets all-socket-events))
	 (all-requests (reduce #'append all-socket-requests))
	 (request (basic-binary-ipc.overlapped-io:wait-for-requests all-requests timeout)))
    (mapcar #'(lambda (socket socket-events socket-requests)
		(loop
		   :for socket-event :in socket-events
		   :for socket-request :in socket-requests
		   :when (and (eql request socket-request)
			      (ecase socket-event
				(connection-available-p
				 (connection-available-p socket))
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
		   :collect socket-event))
	    all-sockets all-socket-events all-socket-requests)))
