(in-package "BASIC-BINARY-PACKET.NETWORK")

(defvar *event-base* nil
  "The default IOLIB:EVENT-BASE object that is to be used for all new
  instances of the SERVER and CLIENT classes.")

(defun default-event-base ()
  "Obtain the default IOLIB:EVENT-BASE object that is to be used for
all new instances of the SERVER and CLIENT classes."
  (unless *event-base*
    (setf *event-base* (make-instance 'event-base)))
  *event-base*)

(defun call-callback (callback &rest args)
  "If CALLBACK is non-NIL, then invoke the CALLBACK function using
ARGS as arguments."
  (when callback
    (apply callback args)))

(defun process-events (&key one-shot timeout)
  "Enter a loop that processes any network events involving basic
binary packet network sockets.

A non null ONE-SHOT keyword specifies that only one invocation of the
loop should occur.

A non null TIMEOUT keyword specifies that the loop should stop if no
event has been processed within TIMEOUT milliseconds.
"
  (event-dispatch (default-event-base) :one-shot one-shot :timeout timeout))

;; Socket management protocol.
(defgeneric on-error (object)
  (:documentation "Obtain the callback that is invoked when an
  asynchronous error occurs with the socket managed by OBJECT."))

(defgeneric (setf on-error) (function stream)
  (:documentation "Obtain the callback that is invoked when an
  asynchronous error occurs when using the socket managed by
  STREAM. FUNCTION must accept two arguments, the STREAM in error and
  a CONDITION object representing the error."))

;; The stream protocol

(defgeneric write-object (stream object &key binary-type identifier force &allow-other-keys)
  (:documentation "Write OBJECT to STREAM using the basic binary
  packet protocol. The binary encoding used is determined by
  BINARY-TYPE. The argument IDENTIFIER represents the 32 bit unsigned
  number used to identify the transmitted packet at the receiver. A
  non-NIL FORCE specifies that the assembled packet is to be sent
  immediately."))

(defgeneric connectedp (stream)
  (:documentation "Returns non-NIL if OBJECT is in the connected state."))

(defgeneric on-connection (stream)
  (:documentation "Obtain the callback that is invoked when the
  connection used by STREAM is fully established."))

(defgeneric (setf on-connection) (function stream)
  (:documentation "Assign the callback that is invoked when the
  connection used by STREAM is fully established. FUNCTION must be a
  function that accepts a single argument, representing the stream
  that has entered the connected state."))

(defgeneric on-object (stream)
  (:documentation "Obtain the callback that is invoked when STREAM
  successfully decodes an object."))

(defgeneric (setf on-object) (function stream)
  (:documentation "Assign the callback that is invoked when STREAM
  successfully decodes an object. FUNCTION must accept three
  arguments, the STREAM that received the object, the received OBJECT
  and the IDENTIFIER assigned to the packet by the sender."))
