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

(defgeneric host-address (server))
(defgeneric port (server))

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
