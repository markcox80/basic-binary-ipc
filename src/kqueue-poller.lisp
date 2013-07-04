(in-package "BASIC-BINARY-IPC")

(defgeneric kqueue-descriptor (object)
  (:documentation "The OS descriptor for the KQUEUE."))

(defclass kqueue-poller (poller)
  ((kqueue-descriptor
    :initarg :kqueue-descriptor
    :reader kqueue-descriptor)
   (closedp
    :initarg :closedp
    :accessor closedp))
  (:default-initargs
   :closedp nil))

(defun make-poller ()
  (make-instance 'kqueue-poller 
		 :kqueue-descriptor (%ff-kqueue)))

(defmethod close-poller ((object kqueue-poller))
  (unless (closedp object)
    (%ff-close (kqueue-descriptor object))
    (setf (closedp object) t)))

