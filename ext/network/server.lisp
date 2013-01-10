(in-package "BASIC-BINARY-PACKET.NETWORK")

;; The server protocol
;; - Utilises the socket management protocol too.
(defgeneric on-new-connection (server)
  (:documentation "Obtain the callback that is invoked when SERVER
  accepts a new client connection."))

(defgeneric (setf on-new-connection) (function server)
  (:documentation "Obtain the callback that is invoked when SERVER
  accepts a new client connection. FUNCTION must accept two arguments,
  the SERVER object that accepted the new connection and an object
  that implements the stream protocol."))

;; The server implementation.

(defclass server ()
  ((socket
    :initarg :socket
    :reader socket)
   (event-base
    :initarg :event-base
    :reader event-base)

   (on-new-connection
    :initarg :on-new-connection
    :accessor on-new-connection
    :initform (error "The callback ON-NEW-CONNECTION must have a function."))
   (on-error
    :initarg :on-error
    :accessor on-error)

   (state
    :accessor state))
  (:default-initargs
   :event-base (default-event-base)
   :on-error nil))

(defun server/error (server exception)
  (close server)
  (call-callback (on-error server) server exception))

(defun server/read (server)
  (assert (on-new-connection server))
  (call-callback (on-new-connection server) server
		 (accept-remote-client (socket server)
				       :event-base (event-base server))))

(defmethod initialize-instance :after ((self server) &key)
  (labels ((handler (fd event exception)
	     (assert (eql fd (socket-os-fd self)))
	     (ecase event
	       (:read
		(server/read self))
	       (:error
		(server/error self exception)))))
    (with-accessors ((event-base event-base)
		     (socket-os-fd socket-os-fd)
		     (state state))
	self
      (setf state :listening)
      (set-io-handler event-base socket-os-fd :read #'handler)
      (set-error-handler event-base socket-os-fd #'handler))))

(defmethod close ((server server) &key abort)
  (unless (eql (state server) :closed)
    (remove-fd-handlers (event-base server) (socket-os-fd server) :error t :read t)
    (close (socket server) :abort abort)
    (setf (state server) :closed))
  nil)

(defmethod socket-os-fd ((server server))
  (let ((rv (socket-os-fd (socket server))))
    (assert rv)
    rv))

(defmethod (setf on-new-connection) ((value null) (object server))
  (error "The callback function ON-NEW-CONNECTION cannot be NIL."))

(defun make-server (address port on-new-connection &key (reuse-address t) (backlog 5))
  "Start a server that will accept connections to PORT on the
interface ADDRESS. The return value of the function is an object that
implements the SERVER protocol.

The callback function ON-NEW-CONNECTION will be invoked when the
server accepts the connection. The function must accept two arguments,
the SERVER object that accepted the connection and an OBJECT that
implements the stream protocol.

If the keyword argument REUSE-ADDRESS is non-NIL then MAKE-SERVER will
not signal an error if the PORT and ADDRESS pair were recently used by
another process.

The BACKLOG argument specifies the maximum number of pending
connections that will be queued before the operating system will start
dropping new connections.
"
  (let ((s (make-socket :connect :passive
			:type :stream
			:address-family :internet
			:ipv6 nil)))
    (alexandria:unwind-protect-case ()
	(progn
	  (bind-address s address :reuse-address reuse-address :port port)
	  (listen-on s :backlog backlog)
	  (make-instance 'server
			 :socket s
			 :on-new-connection on-new-connection))
      (:abort
       (close s)))))
