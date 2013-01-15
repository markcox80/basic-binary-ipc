(in-package "BASIC-BINARY-PACKET.NETWORK")

(defclass client ()
  ((socket
    :initarg :socket
    :reader socket)
   (event-base
    :initarg :event-base
    :reader event-base)
   (on-error
    :initarg :on-error
    :accessor on-error)
   (on-connection
    :initarg :on-connection
    :accessor on-connection)
   (on-object
    :initarg :on-object
    :accessor on-object)

   (state
    :accessor state)
   (read-buffer
    :reader read-buffer
    :initform (make-array '(1000) :element-type '(unsigned-byte 8)))
   (packet-accumulator
    :reader packet-accumulator
    :initform (basic-binary-packet:make-packet-reader-function)))
  (:default-initargs
   :event-base (default-event-base)
   :on-error nil
   :on-connection nil
   :on-object nil))

(defgeneric client/read (client state))
(defgeneric client/write (client state))

;; CONNECTING STATE
 
(defmethod client/read ((client client) (state (eql :connecting)))
  (handler-case (progn
		  (listen (socket client))
		  (error "Should not get here."))
    (socket-connection-refused-error (c)
      (client/error client c))))

(defmethod client/write ((client client) (state (eql :connecting)))
  (handler-case (progn
		  ;; It appears that client/write can be called in the
		  ;; event that the connection is refused
		  ;; too. Therefore we must check that the socket is
		  ;; functional in order to not transition to the
		  ;; CONNECTED state. If LISTEN does not signal an
		  ;; error then we can transition to the next state.
		  (listen (socket client))
		  (setf (state client) :connected)
		  (remove-fd-handlers (event-base client) (socket-os-fd client) :write t)
		  (call-callback (on-connection client) client))
    (socket-connection-refused-error (c)
      (client/error client c))))

;; CONNECTED STATE
(defmethod client/read ((client client) (state (eql :connected)))
  (labels ((process-stream (stream)
	     (multiple-value-bind (payload identifier) (funcall (packet-accumulator client) stream)
	       (when payload
		 (flexi-streams:with-input-from-sequence (in payload)
		   (let ((obj (basic-binary-packet:decode-object in)))
		     (call-callback (on-object client) client obj identifier)))))))
    (multiple-value-bind (buffer number-of-bytes) (receive-from (socket client)
								:buffer (read-buffer client)
								:size (length (read-buffer client)))
      (if (zerop number-of-bytes)
	  (remote-client/error client (make-instance 'end-of-file :stream (socket client)))
	  (flexi-streams:with-input-from-sequence (in buffer :end number-of-bytes)
	    (dotimes (i number-of-bytes)
	      (process-stream in)))))))

;; All other states should generate an error.
(defmethod connectedp ((client client))
  (and (eql (state client) :connected)
       t))

(defmethod write-object ((client client) object &key (identifier 0) binary-type (force t))
  (assert (connectedp client))
  (basic-binary-packet:write-object (socket client) object
				    :identifier identifier
				    :binary-type binary-type)
  (when force
    (force-output (socket client))))

(defun client/error (client exception)
  (close client)
  (call-callback (on-error client) client exception))

(defmethod initialize-instance :after ((self client) &key)
  (setf (state self) :connecting)
  (labels ((handler (fd event exception)
	     (declare (ignore fd))
	     (ecase event
	       (:read
		(client/read self (state self)))
	       (:write
		(client/write self (state self)))
	       (:error
		(client/error self exception)))))
    (set-io-handler (event-base self) (socket-os-fd self) :read  #'handler)
    (set-io-handler (event-base self) (socket-os-fd self) :write #'handler)
    (set-error-handler (event-base self) (socket-os-fd self) #'handler)))

(defmethod close ((stream client) &key abort)
  (unless (eql (state stream) :closed)
    (remove-fd-handlers (event-base stream) (socket-os-fd stream) :error t :read t :write t)
    (close (socket stream) :abort abort)
    (setf (state stream) :closed))
  nil)

(defmethod socket-os-fd ((object client))
  (let ((rv (socket-os-fd (socket object))))
    (assert rv)
    rv))

(defun make-client (address port)
  "Start a connection to a basic binary packet server listening on
port number PORT at Internet ADDRESS. The function will return an
object that implements the STREAM protocol."
  (let ((s (make-socket :connect :active
			:type :stream
			:address-family :internet
			:ipv6 nil)))
    (alexandria:unwind-protect-case ()
	(progn
	  (connect s address :wait nil :port port)
	  (make-instance 'client :socket s))
      (:abort
       (close s)))))
