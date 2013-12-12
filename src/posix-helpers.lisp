(in-package "BASIC-BINARY-IPC")

;; All of the ERRNO wrapper stuff is inspired by the one in OSICAT.

#+ (or darwin freebsd)
(cffi:defcfun (%ff-get-errno-pointer "__error") (:pointer :int))
#+linux
(cffi:defcfun (%ff-get-errno-pointer "__errno_location") (:pointer :int))

(defun %ff-get-errno ()
  (cffi:mem-ref (%ff-get-errno-pointer) :int))

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

(define-condition posix-error (socket-error system-function-error)
  ()
  (:documentation
   "This error class provides a lisp representation of POSIX errors."))

(defun posix-error-code (condition)
  (cffi:foreign-enum-keyword 'errno-enum (system-function-error-value condition)))

(defun posix-error-code-p (condition code)
  (declare (type keyword code))
  (eql (posix-error-code condition)
       code))

(define-check-system-call check-posix (caller foreign-name return-value)
  (if (/= -1 return-value)
      return-value
      (let ((v (%ff-get-errno)))
	(error 'posix-error
	       :name foreign-name
	       :caller caller
	       :error-value v
	       :error-message (strerror v)))))

(defmacro define-posix-system-call (name return-value &body arguments)
  `(define-system-call ,name (check-posix ,return-value)
     ,@arguments))

;; Memory
(defun zero-memory (pointer cffi-type)
  (dotimes (i (cffi:foreign-type-size cffi-type))
    (setf (cffi:mem-aref pointer :uint8 i) 0))
  pointer)
