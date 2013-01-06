(in-package "BASIC-BINARY-PACKET.NETWORK")

(defclass server ()
  ((socket
    :initarg :socket
    :reader socket)
   (event-base
    :initarg :event-base
    :reader event-base)

   (on-new-connection
    :initarg :on-new-connection
    :accessor on-new-connection)
   (on-error
    :initarg :on-error
    :accessor on-error)

   (state
    :accessor state))
  (:default-initargs
   :event-base (default-event-base)
   :on-new-connection nil
   :on-error nil))

(defun server/error (server exception)
  (close server)
  (call-callback (on-error server) server exception))

(defun server/read (server)  
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

(defun make-server (address port &key (reuse-address t) (backlog 5))
  (let ((s (make-socket :connect :passive
			:type :stream
			:address-family :internet
			:ipv6 nil)))
    (alexandria:unwind-protect-case ()
	(progn
	  (bind-address s address :reuse-address reuse-address :port port)
	  (listen-on s :backlog backlog)
	  (make-instance 'server :socket s))
      (:abort
       (close s)))))
