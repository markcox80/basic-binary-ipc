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

;;;; Named Pipes
(defun valid-pipe-name-p (name)
  "Check if NAME is a valid name for a pipe.

NAME must have the form \\\\.\\pipe\\pipename. This function also
accepts //./pipe/pipename."
  (check-type name string)
  (and (or (alexandria:starts-with-subseq "\\\\.\\pipe\\" name)
	   (alexandria:starts-with-subseq "//./pipe/" name))
       (> (length name) 9)
       (< (length name) 256)))

(defun canonical-windows-pipe-name (name)
  "Convert NAME to a pipe name that can be accepted by either
CreateNamedPipe or CreateFile."
  (assert (valid-pipe-name-p name))
  (if (alexandria:starts-with-subseq "//./pipe/" name)
      (concatenate 'string "\\\\.\\pipe\\" (subseq name 10))
      name))

(defun start-named-pipe (name &key (max-instances +pipe-unlimited-instances+) (output-buffer-size 1000) (input-buffer-size 1000))
  (let* ((name (canonical-windows-pipe-name name))
	 (handle (%ff-create-named-pipe name
					'(:pipe-access-duplex :file-flag-overlapped)
					'(:pipe-type-byte :pipe-readmode-byte)
					max-instances
					output-buffer-size
					input-buffer-size
					0
					(cffi:null-pointer))))
    handle))
