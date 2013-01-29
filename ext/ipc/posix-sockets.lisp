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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro posix-socket-initialisation-progn ((socket) &body body)
    `(alexandria:unwind-protect-case ()
	 (progn
	   ,@body)
       (:abort
	(close-socket ,socket)))))

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

(defclass ipv4-tcp-server (posix-socket)
  ((host-address
    :initarg :host-address
    :reader host-address)
   (port
    :initarg :port
    :reader port)))

(defun make-ipv4-tcp-server (host-address port &key (backlog 5))
  (let ((socket (make-posix-socket :pf-inet :sock-stream 0)))
    (with-accessors ((file-descriptor file-descriptor)) socket
      (posix-socket-initialisation-progn (socket)
	(with-sockaddr-in (sockaddr-in :af-inet host-address port)
	  (%ff-bind file-descriptor sockaddr-in (cffi:foreign-type-size '(:struct sockaddr-in))))
	
	(%ff-listen file-descriptor backlog)
	socket))))
