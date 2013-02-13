(in-package "BASIC-BINARY-PACKET.IPC")

;; All of the ERRNO wrapper stuff is inspired by the one in OSICAT.

(cffi:defcvar (%ff-errno "errno") :int)
(cffi:defcfun (%ff-strerror "strerror_r") :string
  (errnum :int)
  (buffer :pointer)
  (buffer-size :unsigned-int))

(defun strerror (errnum)
  "Obtain the POSIX error string for the error with integer ERRNUM."
  (let ((buffer (make-array 1000 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr buffer)
      (%ff-strerror errnum ptr (length buffer)))
    (babel:octets-to-string buffer :end (position 0 buffer))))

(defun errnum-symbol (errnum)
  "Map the error with integer ERRNUM to a symbol from the grovelled
ERRNO-ENUM type."
  (cffi:foreign-enum-keyword 'errno-enum errnum))

(defgeneric lisp-function-name (object)
  (:documentation "The name of the LISP function that wraps the
    foreign posix function."))

(defgeneric c-function-name (object)
  (:documentation "The name of the foreign function that signalled the
    posix error."))

(define-condition posix-error (error)
  ((errnum
    :initarg :errnum
    :reader errnum)
   (lisp-function-name
    :initarg :lisp-function-name
    :reader lisp-function-name)
   (c-function-name
    :initarg :c-function-name
    :reader c-function-name))
  (:documentation
   "This error class provides a lisp representation of POSIX errors.")
  (:report
   (lambda (condition stream)
     (format stream "The system call ~S (~S) signalled ~A: ~S"
	     (lisp-function-name condition)
	     (c-function-name condition)
	     (errnum-symbol (errnum condition))
	     (strerror (errnum condition))))))

(defun posix-error-code (condition)
  (cffi:foreign-enum-keyword 'errno-enum (errnum condition)))

(defun posix-error-code-p (condition code)
  (declare (type keyword code))
  (eql (posix-error-code condition)
       code))

(defgeneric base-type (object)
  (:documentation "The CFFI return type of the posix system call."))

(cffi:define-foreign-type errno-wrapper ()
  ((base-type
    :initarg :base-type
    :reader base-type
    :documentation "The CFFI return type of the posix function.")
   (lisp-function-name
    :initarg :lisp-function-name
    :reader lisp-function-name
    :documentation "The name of the LISP function that wraps the
    foreign posix function.")
   (c-function-name
    :initarg :c-function-name
    :reader c-function-name
    :documentation "The name of the foreign function that signalled
    the posix error."))
  (:documentation
   "The ERRONO-WRAPPER CFFI type provides a convenient method of
   signalling a POSIX-ERROR when posix system call functions fail."))

(cffi:define-parse-method errno-wrapper (&key (base-type :int) lisp-function-name c-function-name)
  (make-instance 'errno-wrapper
		 :actual-type base-type
		 :base-type base-type
		 :lisp-function-name lisp-function-name
		 :c-function-name c-function-name))

(defmethod cffi:translate-from-foreign ((value number) (type errno-wrapper))
  (if (/= -1 value)
      value
      (error 'posix-error
	     :lisp-function-name (lisp-function-name type)
	     :c-function-name (c-function-name type)
	     :errnum %ff-errno)))

(defmacro define-system-call (name return-value &body arguments)
  (destructuring-bind (lisp-function-name c-function-name) name
    `(cffi:defcfun ,name (errno-wrapper
			  :base-type ,return-value
			  :lisp-function-name ,lisp-function-name
			  :c-function-name ,c-function-name)
       ,@arguments)))

;; Memory
(defun zero-memory (pointer cffi-type)
  (dotimes (i (cffi:foreign-type-size cffi-type))
    (setf (cffi:mem-aref pointer :uint8 i) 0))
  pointer)
