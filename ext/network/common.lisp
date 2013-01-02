(in-package "BASIC-BINARY-PACKET.NETWORK")

(defvar *event-base* (make-instance 'event-base))

(defun default-event-base ()
  *event-base*)

(defun call-callback (callback &rest args)
  (when callback
    (apply callback args)))
