(in-package "BASIC-BINARY-IPC")

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
    :initform (error "File descriptors must be specified."))
   (closedp
    :initarg :closedp
    :initform nil
    :reader socket-closed-p)))

(defun make-posix-socket (namespace communication-style protocol)
  (let ((fd (%ff-socket namespace communication-style protocol)))
    (make-instance 'posix-socket
		   :namespace namespace
		   :communication-style communication-style
		   :protocol protocol
		   :file-descriptor fd)))

(defmethod close-socket ((socket posix-socket))
  (with-slots (closedp) socket
    (unless closedp
      (%ff-close (file-descriptor socket))
      (setf closedp t))))

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

(defclass posix-stream (stream-socket)
  ((socket
    :initarg :socket
    :reader socket)))

(defmethod socket-closed-p ((socket posix-stream))
  (socket-closed-p (socket socket)))

;; Posix stream - future protocol
(defmethod determinedp ((future-connection posix-stream))
  (poll-socket future-connection 'determinedp :immediate))

(defmethod connection-failed-p ((future-connection posix-stream))
  (poll-socket future-connection 'connection-failed-p :immediate))

(defmethod connection-succeeded-p ((future-connection posix-stream))
  (poll-socket future-connection 'connection-succeeded-p :immediate))

;; IPv4 Stream - stream protocol
(defmethod data-available-p ((stream posix-stream))
  (poll-socket stream 'data-available-p :immediate))

(defmethod ready-to-write-p ((stream posix-stream))
  (poll-socket stream 'ready-to-write-p :immediate))

(defmethod remote-disconnected-p ((stream posix-stream))
  (poll-socket stream 'remote-disconnected-p :immediate))

(defmethod read-from-stream ((stream posix-stream) buffer &key start end peek)
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
    (handler-case (%ff-recvfrom (file-descriptor stream) (cffi:mem-aptr ptr :uint8 start) (- end start)
				(if peek
				    (cffi:foreign-bitfield-value 'message-flags '(msg-peek))
				    0)
				(cffi:null-pointer) (cffi:null-pointer))
      (posix-error (c)
	(if (posix-error-code-p c :ewouldblock)
	    0
	    (error c))))))

(defmethod write-to-stream ((stream posix-stream) buffer &key start end)
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

  (handler-case (cffi:with-pointer-to-vector-data (ptr buffer)
		  (%ff-sendto (file-descriptor stream)
			      (cffi:mem-aptr ptr :uint8 start)
			      (- end start)
			      0
			      (cffi:null-pointer)
			      0))
    (posix-error (c)
      (if (posix-error-would-block-p c)
	  (error 'would-block-error :socket stream)
	  (error c)))))

;; Failed stream
(defclass failed-posix-stream (stream-socket)
  ((socket
    :initarg :socket
    :reader socket)))

(defmethod close-socket ((socket failed-posix-stream))  
  (handler-case (close-socket (socket socket))
    (posix-error (c)
      (unless (posix-error-code-p c :ebadf)
	(error c)))))

;; - stream protocol
(defmethod data-available-p ((socket-stream failed-posix-stream))
  nil)

(defmethod determinedp ((socket-stream failed-posix-stream))
  t)

(defmethod connection-failed-p ((socket-stream failed-posix-stream))
  t)

(defmethod connection-succeeded-p ((socket-stream failed-posix-stream))
  nil)

;;  IPv4 stream
(defclass ipv4-tcp-stream (posix-stream)
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
    :reader local-port)))

(defmethod print-object ((object ipv4-tcp-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A:~d -> ~A:~d"
	    (local-host-address object)
	    (local-port object)
	    (remote-host-address object)
	    (remote-port object))))

;; Failed IPV4 Stream - This class is for the special situation when
;; using CONNECT-TO-TCP4-SERVER to connect to a server listening on
;; the loopback device. If the server does not exist, then some
;; operating systems signal an ECONNREFUSED error. In that case an
;; object of type FAILED-IPV4-TCP-STREAM is returned.
(defclass failed-ipv4-tcp-stream (failed-posix-stream)
  ((remote-port
    :initarg :remote-port
    :reader remote-port)
   (remote-host-address
    :initarg :remote-host-address
    :reader remote-host-address)))

(defmethod print-object ((object failed-ipv4-tcp-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A:~d"
	    (remote-host-address object)
	    (remote-port object))))

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

(defclass ipv4-tcp-server (stream-server)
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

(defun posix-error-would-block-p (condition)
  "The predicate POSIX-ERROR-WOULD-BLOCK-P handles the potential
  sitation where EAGAIN and EWOULDBLOCK are defined to be the same
  ERRNO constant (this is true on OSX). This causes difficulties with
  the mapping between error codes and keywords in the enum
  ERRNO-ENUM."
  (let ((code (posix-error-code condition)))
    (cond
      ((= (cffi:foreign-enum-value 'errno-enum :eagain)
	  (cffi:foreign-enum-value 'errno-enum :ewouldblock))
       (or (eql code :eagain)
	   (eql code :ewouldblock)))

      (t
       (eql code :ewouldblock)))))

(defmethod accept-connection ((server ipv4-tcp-server))
  (cffi:with-foreign-object (ptr '(:struct sockaddr-in))
    (cffi:with-foreign-object (ptr-size 'socklen-t)
      (setf (cffi:mem-ref ptr-size 'socklen-t) (cffi:foreign-type-size '(:struct sockaddr-in)))
      (let* ((fd (handler-case (%ff-accept (file-descriptor (socket server)) ptr ptr-size)
		   (posix-error (c)
		     (if (posix-error-would-block-p c)
			 (error 'no-connection-available-error :socket server)
			 (error c)))))
	     (socket (make-instance 'posix-socket
				    :namespace (namespace (socket server))
				    :communication-style (communication-style (socket server))
				    :protocol (protocol (socket server))
				    :file-descriptor fd)))
	;; This shouldn't be necessary but on some systems the socket
	;; options are not inherited.
	(setf (operating-modes socket) '(o-nonblock))
	(make-instance 'ipv4-tcp-stream
		       :socket socket
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
		 (make-instance 'failed-ipv4-tcp-stream
				:socket socket
				:remote-port port
				:remote-host-address host-address)))
	      (t
	       (error c)))))
	
	(cffi:with-foreign-object (sockaddr-in-length 'socklen-t)
	  (setf (cffi:mem-ref sockaddr-in-length 'socklen-t) (cffi:foreign-type-size '(:struct sockaddr-in)))
	  (%ff-getsockname (file-descriptor socket) sockaddr-in sockaddr-in-length))

	(make-instance 'ipv4-tcp-stream
		       :socket socket
		       :local-port (port-from-sockaddr-in sockaddr-in)
		       :local-host-address (host-address-from-sockaddr-in sockaddr-in)
		       :remote-port port
		       :remote-host-address host-address)))))

;; Local Sockets
(defgeneric local-pathname (socket)
  (:documentation "The pathname to the local socket."))

(defgeneric delete-on-close-p (socket)
  (:documentation "Delete the pathname when closing the socket."))

;; - Local Streams
(defclass local-stream (posix-stream)
  ((local-pathname
    :initarg :local-pathname
    :reader local-pathname)))

(defmethod print-object ((object local-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (local-pathname object))))

(defclass local-server (stream-server)
  ((socket
    :initarg :socket
    :reader socket)
   (local-pathname
    :initarg :local-pathname
    :reader local-pathname)
   (delete-on-close-p
    :initarg :delete-on-close-p
    :reader delete-on-close-p)))

(defmethod close-socket ((socket local-server))
  (close-socket (socket socket))
  (when (delete-on-close-p socket)
    (delete-file (local-pathname socket))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-sockaddr-un (function pathname)
    (let ((pathname (namestring pathname))
	  (maximum-pathname-length (or #+darwin 104
				       #+freebsd 104
				       #+linux 108
				       #-(or darwin freebsd linux)
				       (error "Maximum pathname length not specified for this operating system. Please inspect sys/un.h."))))
      (assert (<= (+ maximum-pathname-length (cffi:foreign-type-size 'posix-socket-address-family))
		  (cffi:foreign-type-size '(:struct sockaddr-un))))
      (unless (<= (1+ (length pathname)) maximum-pathname-length)
	(error "Pathname for local socket exceeds the allowable length of ~d characters." maximum-pathname-length))
      (cffi:with-foreign-object (sockaddr-un '(:struct sockaddr-un))
	(zero-memory sockaddr-un '(:struct sockaddr-un))
	(setf (cffi:foreign-slot-value sockaddr-un '(:struct sockaddr-un) 'sun-family) :af-local)
	(let ((sun-name-ptr (cffi:foreign-slot-pointer sockaddr-un '(:struct sockaddr-un) 'sun-path)))
	  (dotimes (i (length pathname))
	    (setf (cffi:mem-aref sun-name-ptr :char i) (char-code (elt pathname i)))))
	(funcall function sockaddr-un (+ (cffi:foreign-type-size 'posix-socket-address-family)
					 (length pathname)
					 1)))))

  (defmacro with-sockaddr-un ((ptr-var length-var pathname) &body body)
    `(do-with-sockaddr-un #'(lambda (,ptr-var ,length-var)
			      ,@body)
       ,pathname)))

(defun make-local-server (pathname &key (backlog 5) (delete-on-close t))
  (let ((socket (make-posix-socket :pf-local :sock-stream 0)))
    (setf (operating-modes socket) '(o-nonblock))
    (with-accessors ((file-descriptor file-descriptor)) socket
      (posix-socket-initialisation-progn (socket)
	(with-sockaddr-un (sockaddr-un sockaddr-length pathname)
	  (%ff-bind file-descriptor sockaddr-un sockaddr-length))
	
	(alexandria:unwind-protect-case ()
	    (progn
	      (%ff-listen file-descriptor backlog)
	      (make-instance 'local-server
			     :local-pathname pathname
			     :socket socket
			     :delete-on-close-p delete-on-close))
	  (:abort
	   (delete-file pathname)))))))

(defmethod connection-available-p ((server local-server))
  (let ((results (poll-socket server 'connection-available-p :immediate)))
    (if results t nil)))

(defmethod accept-connection ((server local-server))
  (cffi:with-foreign-object (ptr '(:struct sockaddr-un))
    (cffi:with-foreign-object (ptr-size 'socklen-t)
      (setf (cffi:mem-ref ptr-size 'socklen-t) (cffi:foreign-type-size '(:struct sockaddr-un)))
      (let* ((fd (handler-case (%ff-accept (file-descriptor (socket server)) ptr ptr-size)
		   (posix-error (c)
		     (if (posix-error-would-block-p c)
			 (error 'no-connection-available-error :socket server)
			 (error c)))))
	     (socket (make-instance 'posix-socket
				    :namespace (namespace (socket server))
				    :communication-style (communication-style (socket server))
				    :protocol (protocol (socket server))
				    :file-descriptor fd)))
	;; This shouldn't be necessary but on some systems the socket
	;; options are not inherited.
	(setf (operating-modes socket) '(o-nonblock))
	(make-instance 'local-stream
		       :socket socket
		       :local-pathname (local-pathname server))))))

(define-condition no-local-server-error ()
  ((local-pathname
    :initarg :local-pathname
    :reader local-pathname))
  (:report (lambda (condition stream)
	     (format stream "No local server exists at pathname ~S." (local-pathname condition)))))

(defun connect-to-local-server (pathname &key)
  (let ((socket (make-posix-socket :pf-local :sock-stream 0)))
    (posix-socket-initialisation-progn (socket)
      (setf (operating-modes socket) '(o-nonblock))

      ;; Connect to the host.
      (with-sockaddr-un (sockaddr-un sockaddr-length pathname)
	(handler-case (%ff-connect (file-descriptor socket) sockaddr-un sockaddr-length)
	  (posix-error (c)
	    (if (posix-error-code-p c :enoent)
		(error 'no-local-server-error :local-pathname pathname)
		(error c)))))

      (make-instance 'local-stream
		     :socket socket
		     :local-pathname pathname))))
