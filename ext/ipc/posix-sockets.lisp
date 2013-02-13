(in-package "BASIC-BINARY-PACKET.IPC")

(defgeneric file-descriptor (object)
  (:documentation "Return the file descriptor of the object."))

(defgeneric namespace (posix-socket)
  (:documentation "Return the posix namespace used by the
  POSIX-SOCKET."))

(defgeneric communication-style (posix-socket)
  (:documentation "Return the posix communication style used by the
  POSIX-SOCKET."))

(defgeneric protocol (posix-socket)
  (:documentation "Return the protocol used by the POSIX-SOCKET."))

(defgeneric socket (object)
  (:documentation "Return the POSIX-SOCKET object used by OBJECT."))

(defclass posix-socket ()
  ((namespace
    :initarg :namespace
    :reader namespace)
   (communication-style
    :initarg :communication-style
    :reader communication-style)
   (protocol
    :initarg :protocol
    :reader protocol)
   (file-descriptor
    :initarg :file-descriptor
    :reader file-descriptor
    :initform (error "File descriptors must be specified."))))

(defun make-posix-socket (namespace communication-style protocol)
  (let ((fd (%ff-socket namespace communication-style protocol)))
    (make-instance 'posix-socket
		   :namespace namespace
		   :communication-style communication-style
		   :protocol protocol
		   :file-descriptor fd)))

(defmethod close-socket ((socket posix-socket))
  (%ff-close (file-descriptor socket)))

(defmethod close-socket ((object t))
  (close-socket (socket object)))

(defmethod file-descriptor ((object t))
  (file-descriptor (socket object)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro posix-socket-initialisation-progn ((socket) &body body)
    `(alexandria:unwind-protect-case ()
	 (progn
	   ,@body)
       (:abort
	(close-socket ,socket)))))

(defgeneric operating-modes (socket))
(defgeneric (setf operating-modes) (value socket))

(defmethod operating-modes ((object posix-socket))
  (cffi:foreign-bitfield-symbols 'operating-mode (%ff-fcntl-noarg (file-descriptor object) :f-getfl)))

(defmethod (setf operating-modes) (value (object posix-socket))
  (%ff-fcntl-setfl (file-descriptor object) :f-setfl (cffi:foreign-bitfield-value 'operating-mode value)))

;; Socket Option arguments
(define-socket-option-argument soa-boolean
  (:base-type :int)
  (:writer (value pointer)
    (setf (cffi:mem-ref pointer :int) (if value 1 0)))
  (:reader (pointer)
    (if (zerop (cffi:mem-ref pointer :int))
	nil
	t)))

;; Socket options
(define-socket-option (reuse-address-p :so-reuseaddr soa-boolean))
(define-socket-option (keep-alive-p :so-keepalive soa-boolean))

;;  IPv4 stream
(defclass ipv4-stream ()
  ((remote-host-address
    :initarg :remote-host-address
    :reader remote-host-address)
   (remote-port
    :initarg :remote-port
    :reader remote-port)
   (local-host-address
    :initarg :local-host-address
    :reader local-host-address)
   (local-port
    :initarg :local-port
    :reader local-port)
   (socket
    :initarg :socket
    :reader socket)))


;; IPv4 stream - future protocol
(defmethod determinedp ((future-connection ipv4-stream))
  (poll-socket future-connection 'determinedp :immediate))

(defmethod connection-failed-p ((future-connection ipv4-stream))
  (poll-socket future-connection 'connection-failed-p :immediate))

(defmethod connection-succeeded-p ((future-connection ipv4-stream))
  (poll-socket future-connection 'connection-succeeded-p :immediate))

(defmethod print-object ((object ipv4-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A:~d -> ~A:~d"
	    (local-host-address object)
	    (local-port object)
	    (remote-host-address object)
	    (remote-port object))))

;; IPv4 Stream - stream protocol
(defmethod data-available-p ((stream ipv4-stream))
  (poll-socket stream 'data-available-p :immediate))

(defmethod ready-to-write-p ((stream ipv4-stream))
  (poll-socket stream 'ready-to-write-p :immediate))

(defmethod remote-disconnected-p ((stream ipv4-stream))
  (poll-socket stream 'remote-disconnected-p :immediate))

(defmethod read-from-stream ((stream ipv4-stream) buffer &key start end peek)
  (declare (type (vector (unsigned-byte 8)) buffer))
  (unless start
    (setf start 0))
  (unless end
    (setf end (length buffer)))

  (unless (<= start end)
    (error "The value START is greater than END."))

  (unless (or (zerop (length buffer))
	      (and (>= start 0) (< start (length buffer))))
    (error "The value START is not a valid index for BUFFER."))

  (unless (and (>= end 0) (<= end (length buffer)))
    (error "The value END is not a valid end index for BUFFER."))

  (cffi:with-pointer-to-vector-data (ptr buffer)
    (%ff-recvfrom (file-descriptor stream) (cffi:mem-aptr ptr :uint8 start) (- end start)
		  (if peek
		      (cffi:foreign-bitfield-value 'message-flags '(msg-peek))
		      0)
		  (cffi:null-pointer) (cffi:null-pointer))))

(defmethod write-to-stream ((stream ipv4-stream) buffer &key start end)
  (declare (type (vector (unsigned-byte 8)) buffer))
  (unless start
    (setf start 0))
  (unless end
    (setf end (length buffer)))

  (unless (<= start end)
    (error "The value START is greater than END."))

  (unless (and (>= start 0) (< start (length buffer)))
    (error "The value START is not a valid index for BUFFER."))

  (unless (and (>= end 0) (<= end (length buffer)))
    (error "The value END is not a valid end index for BUFFER."))

  (cffi:with-pointer-to-vector-data (ptr buffer)
    (%ff-sendto (file-descriptor stream) (cffi:mem-aptr ptr :uint8 start) (- end start) 0 (cffi:null-pointer) 0)))

;; Failed IPV4 Stream - This class is for the special situation when
;; using CONNECT-TO-TCP4-SERVER to connect to a server listening on
;; the loopback device. If the server does not exist, then some
;; operating systems signal an ECONNREFUSED error. In that case an
;; object of type FAILED-IPV4-STREAM is returned.
(defclass failed-ipv4-stream ()
  ((socket
    :initarg :socket
    :reader socket)
   (remote-port
    :initarg :remote-port
    :reader remote-port)
   (remote-host-address
    :initarg :remote-host-address
    :reader remote-host-address)))

(defmethod print-object ((object failed-ipv4-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A:~d"
	    (remote-host-address object)
	    (remote-port object))))

(defmethod close-socket ((socket failed-ipv4-stream))  
  (handler-case (close-socket (socket socket))
    (posix-error (c)
      (unless (posix-error-code-p c :ebadf)
	(error c)))))

;; - stream protocol
(defmethod data-available-p ((socket-stream failed-ipv4-stream))
  nil)

(defmethod determinedp ((socket-stream failed-ipv4-stream))
  t)

(defmethod connection-failed-p ((socket-stream failed-ipv4-stream))
  t)

(defmethod connection-succeeded-p ((socket-stream failed-ipv4-stream))
  nil)

;; IPv4
(defparameter +ipv4-loopback+ (%ff-inet-ntoa (%ff-ntohl inaddr-loopback)))
(defparameter +ipv4-any+      (%ff-inet-ntoa (%ff-ntohl inaddr-any)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-sockaddr-in (function family host-address port)
    (declare (type (unsigned-byte 16) port)
	     (type string host-address))
    (cffi:with-foreign-object (sockaddr-in '(:struct sockaddr-in))
      (zero-memory sockaddr-in '(:struct sockaddr-in))
      (cffi:with-foreign-slots ((sin-family sin-port) sockaddr-in (:struct sockaddr-in))
	(setf sin-family family
	      sin-port   (%ff-htons port))
	(when (zerop (%ff-inet-aton host-address (cffi:foreign-slot-pointer sockaddr-in '(:struct sockaddr-in) 'sin-addr)))
	  (error "Host address ~S is not in standard numbers-and-dots notation." host-address))
	
	(funcall function sockaddr-in))))

  (defmacro with-sockaddr-in ((var family host-address port) &body body)
    `(do-with-sockaddr-in #'(lambda (,var)
			      ,@body)
       ,family ,host-address ,port)))

(defclass ipv4-tcp-server ()
  ((host-address
    :initarg :host-address
    :reader host-address)
   (port
    :initarg :port
    :reader port)
   (socket
    :initarg :socket
    :reader socket)))

(defun make-ipv4-tcp-server (host-address port &key reuse-address (backlog 5))
  (let ((socket (make-posix-socket :pf-inet :sock-stream 0)))
    (setf (operating-modes socket) '(o-nonblock)
	  (reuse-address-p socket) reuse-address)
    (with-accessors ((file-descriptor file-descriptor)) socket
      (posix-socket-initialisation-progn (socket)
	(with-sockaddr-in (sockaddr-in :af-inet host-address port)
	  (%ff-bind file-descriptor sockaddr-in (cffi:foreign-type-size '(:struct sockaddr-in))))
	
	(%ff-listen file-descriptor backlog)
	(make-instance 'ipv4-tcp-server
		       :host-address host-address
		       :port port
		       :socket socket)))))

(defmethod connection-available-p ((server ipv4-tcp-server))
  (let ((results (poll-socket server 'connection-available-p :immediate)))
    (if results t nil)))

(defmethod accept-connection ((server ipv4-tcp-server))
  (cffi:with-foreign-object (ptr '(:struct sockaddr-in))
    (cffi:with-foreign-object (ptr-size 'socklen-t)
      (setf (cffi:mem-ref ptr-size 'socklen-t) (cffi:foreign-type-size '(:struct sockaddr-in)))
      (let ((fd (%ff-accept (file-descriptor (socket server)) ptr ptr-size)))
	(make-instance 'ipv4-stream
		       :socket (make-instance 'posix-socket
					      :namespace (namespace (socket server))
					      :communication-style (communication-style (socket server))
					      :protocol (protocol (socket server))
					      :file-descriptor fd)
		       :local-port (port server)
		       :local-host-address (host-address server)
		       :remote-host-address (host-address-from-sockaddr-in ptr)
		       :remote-port (port-from-sockaddr-in ptr))))))

;; CONNECT-TO-IPV4-TCP-SERVER

(defun host-address-from-inaddr (in-addr)
  (%ff-inet-ntoa (cffi:foreign-slot-value in-addr '(:struct in-addr) 's-addr)))

(defun host-address-from-sockaddr-in (sockaddr-in)
  (host-address-from-inaddr (cffi:foreign-slot-pointer sockaddr-in '(:struct sockaddr-in) 'sin-addr)))

(defun port-from-sockaddr-in (sockaddr-in)
  (%ff-ntohs (cffi:foreign-slot-value sockaddr-in '(:struct sockaddr-in) 'sin-port)))

(defun connect-to-ipv4-tcp-server (host-address port &key local-host-address local-port)
  (let ((socket (make-posix-socket :pf-inet :sock-stream 0)))
    (posix-socket-initialisation-progn (socket)
      (setf (operating-modes socket) '(o-nonblock))

      ;; Bind local host address and port.
      (setf local-host-address (or local-host-address
				   +ipv4-any+))
      (setf local-port         (or local-port
				   0))
      (with-sockaddr-in (sockaddr-in :af-inet local-host-address local-port)
	(%ff-bind (file-descriptor socket) sockaddr-in (cffi:foreign-type-size '(:struct sockaddr-in))))

      ;; Connect to the host.
      (with-sockaddr-in (sockaddr-in :af-inet host-address port)
	(handler-case (%ff-connect (file-descriptor socket) sockaddr-in (cffi:foreign-type-size '(:struct sockaddr-in)))
	  (posix-error (c)
	    (case (posix-error-code c)
	      (:einprogress
	       nil)
	      (:econnrefused
	       (return-from connect-to-ipv4-tcp-server
		 (make-instance 'failed-ipv4-stream
				:socket socket
				:remote-port port
				:remote-host-address host-address)))
	      (t
	       (error c)))))
	
	(cffi:with-foreign-object (sockaddr-in-length 'socklen-t)
	  (setf (cffi:mem-ref sockaddr-in-length 'socklen-t) (cffi:foreign-type-size '(:struct sockaddr-in)))
	  (%ff-getsockname (file-descriptor socket) sockaddr-in sockaddr-in-length))

	(make-instance 'ipv4-stream
		       :socket socket
		       :local-port (port-from-sockaddr-in sockaddr-in)
		       :local-host-address (host-address-from-sockaddr-in sockaddr-in)
		       :remote-port port
		       :remote-host-address host-address)))))
