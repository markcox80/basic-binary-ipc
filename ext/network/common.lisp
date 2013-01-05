(in-package "BASIC-BINARY-PACKET.NETWORK")

(defvar *event-base* nil)

(defun default-event-base ()
  (unless *event-base*
    (setf *event-base* (make-instance 'event-base)))
  *event-base*)

(defun call-callback (callback &rest args)
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
