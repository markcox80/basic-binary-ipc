(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO")

;;;; REQUEST
(defgeneric overlapped-structure (request)
  (:documentation "Return the overlapped structure owned by the REQUEST."))

(defgeneric event-handle (request)
  (:documentation "Return the event that is owned by the REQUEST."))

(defgeneric descriptor (request)
  (:documentation "The descriptor bound to the REQUEST upon issuing a
  request to the operating system to perform an I/O operation."))

(defclass request ()
  ((overlapped-structure
    :initarg :overlapped-structure
    :reader overlapped-structure)
   (event-handle
    :initarg :event-handle
    :reader event-handle)
   (descriptor
    :initarg :descriptor
    :reader descriptor)))

(defun invalidp (request)
  (check-type request request)
  (null (descriptor request)))

(defun waitingp (request)
  (check-type request request)
  (and (descriptor request)
       (null (%ff-wait-for-single-object (event-handle request) 0))))

(defun completedp (request)
  (check-type request request)
  (and (descriptor request)
       (%ff-wait-for-single-object (event-handle request) 0)))
