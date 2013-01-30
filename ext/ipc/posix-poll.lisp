(in-package "BASIC-BINARY-PACKET.IPC")

(defgeneric poll (socket events timeout))

(defmethod poll (socket (events symbol) timeout)
  (poll socket (list events) timeout))

(defmethod poll ((socket posix-socket) (poll-events list) timeout)
  (declare (type (integer 0) timeout))
  (cffi:with-foreign-object (ptr '(:struct pollfd))
    (cffi:with-foreign-slots ((fd events revents) ptr (:struct pollfd))
      (setf fd     (file-descriptor socket)
	    events (cffi:foreign-bitfield-value 'poll-events poll-events))
      (%ff-poll ptr 1 timeout)
      (cffi:foreign-bitfield-symbols 'poll-events revents))))

