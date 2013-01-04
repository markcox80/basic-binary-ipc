(in-package "BASIC-BINARY-PACKET.NETWORK")

(defclass remote-client ()
  ((socket
    :initarg :socket
    :reader socket)
   (event-base
    :initarg :event-base
    :reader event-base)
   
   (on-object
    :initarg :on-object
    :accessor on-object)
   (on-error
    :initarg :on-error
    :accessor on-error)
   
   (state
    :accessor state)
   (packet-accumulator
    :initform (basic-binary-packet:make-packet-reader-function)
    :reader packet-accumulator)
   (read-buffer
    :initform (make-array '(1000) :element-type '(unsigned-byte 8))
    :reader read-buffer))
  (:default-initargs
   :event-base (default-event-base)
   :on-object nil
   :on-error nil))

(defun remote-client/error (client exception)
  (close client)
  (call-callback (on-error client) client exception))

(defgeneric remote-client/read (client state))
(defgeneric remote-client/write (client state))

(defmethod remote-client/read ((client remote-client) (state (eql :accepting)))
  (listen (socket client))
  (remove-fd-handlers (event-base client) (socket-os-fd client) :write t)
  (setf (state client) :connected))

(defmethod remote-client/write ((client remote-client) (state (eql :accepting)))
  (listen (socket client))
  (remove-fd-handlers (event-base client) (socket-os-fd client) :write t)
  (setf (state client) :connected))

(defmethod remote-client/read ((client remote-client) (state (eql :connected)))
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

(defmethod socket-os-fd ((client remote-client))
  (let ((rv (socket-os-fd (socket client))))
    (assert rv)
    rv))

(defmethod close ((client remote-client) &key abort)
  (with-accessors ((state state)
		   (event-base event-base)
		   (socket socket)
		   (socket-os-fd socket-os-fd))
      client
    (unless (eql state :closed)
      (remove-fd-handlers event-base socket-os-fd :error t :read t :write t)
      (close socket :abort abort)
      (setf state :closed))
    nil))

(defmethod initialize-instance :after ((self remote-client) &key)
  (with-accessors ((state state)
		   (event-base event-base)
		   (socket-os-fd socket-os-fd))
      self

    (labels ((handler (fd event exception)
	       (assert (eql fd (socket-os-fd self)))
	       (ecase event
		 (:read
		  (remote-client/read self state))
		 (:write
		  (remote-client/write self state))
		 (:error
		  (remote-client/error self exception)))))
      (set-io-handler event-base socket-os-fd :read #'handler)
      (set-io-handler event-base socket-os-fd :write #'handler)
      (set-error-handler event-base socket-os-fd #'handler))

    (setf state :accepting)))

(defun accept-remote-client (socket)
  (make-instance 'remote-client
		 :socket (accept-connection socket :wait nil)))

(defmethod basic-binary-packet:write-object ((client remote-client) object &key (identifier 0) binary-type)
  (assert (eql (state client) :connected))
  (basic-binary-packet:write-object (socket client) object
				    :identifier identifier
				    :binary-type binary-type))
