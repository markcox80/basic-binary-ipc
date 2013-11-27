(in-package "BASIC-BINARY-IPC")

;; The code in this file provides an abstraction for invoking
;; functions provided by the operating system, or more succinctly,
;; system calls. A common pattern with system calls is the manner in
;; which errors are communicated from the operating system to the
;; calling process. Below are examples on different operating systems
;; and also different APIs on the same operating system.
;;
;;    // On Posix based systems:
;;    ssize_t rv = read(fd, buffer, buffer_size);
;;    if (rv == -1) {
;;      printf("ERROR: %s\n", strerror(errno));
;;      return FAILED;
;;    }
;;
;;    // On Windows (Winsock):
;;    SOCKET s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
;;    if (s == INVALID_SOCKET) {
;;      int last_error = WSAGetLastError();
;;      WCHAR buffer[1024];
;;      FormatMessage(FORMAT_MESSAGE_FROM_STRING | FORMAT_MESSAGE_IGNORE_INSERTS,
;;                    0, last_error, 0, buffer, 1024, NULL);
;;      return FAILED;
;;    }
;;    
;;    // On Windows (Named Pipes)
;;    HANDLE h = CreateFileA(pipe_name, GENERIC_READ | GENERIC_WRITE, ... );
;;    if (h == INVALID_HANDLE_ERROR) {
;;      DWORD last_error = GetLastError();
;;      WCHAR buffer[1024];
;;      FormatMessage(FORMAT_MESSAGE_FROM_STRING | FORMAT_MESSAGE_IGNORE_INSERTS,
;;                    0, last_error, 0, buffer, 1024, NULL);
;;      return FAILED;
;;    }
;;
;; The goal of this file is to provide a convenient method of defining
;; a foreign function which signals an appropriate Lisp condition when
;; an error occurs rather than the style of programming demonstrated
;; above.
;;
;; All examples can use the common foreign function interface (CFFI)
;; to interface with the operating system. Customisation is needed to
;; determine if an error has occurred, and if so, signal the
;; appropriate condition.
;; 
;; The class of the signalled condition should be a subclass of
;; SYSTEM-FUNCTION-ERROR. Instances of this class encapsulate the
;; information pertinent to the user: the foreign function that
;; failed, the caller of the foreign function, the value of the error
;; returned, and a nice user friendly message describing the error
;; returned.

(defgeneric system-function-caller (condition)
  (:documentation "The name of the function which invoked the
  system function that failed."))

(defgeneric system-function-name (condition)
  (:documentation "The name of the system call that failed."))

(defgeneric system-function-error-value (condition)
  (:documentation "The error value communicated by the system call."))

(defgeneric system-function-error-message (condition)
  (:documentation "A user friendly message describing the system call
  failure."))

(define-condition system-function-error (error)
  ((caller
    :initarg :caller
    :reader system-function-caller)
   (name
    :initarg :name
    :reader system-function-name)
   (error-value
    :initarg :error-value
    :reader system-function-error-value)
   (error-message
    :initarg :error-message
    :reader system-function-error-message))
  (:report (lambda (condition stream)
	     (format stream "The system function ~S failed. (~S ~A)"
		     (system-function-name condition)
		     (system-function-error-value condition)
		     (system-function-error-message condition)))))

;; Creating a Lisp function which performs the system call and checks
;; the return value requires two macros, DEFINE-SYSTEM-CALL and
;; DEFINE-CHECK-SYSTEM-CALL. The DEFINE-SYSTEM-CALL macro defines the
;; lisp function and DEFINE-CHECK-SYSTEM-CALL defines a function that
;; is used to evaluate the return value of the foreign function, and
;; if required, signal a condition. 
;; 
;; The prototypes for the two macros are:
;;
;; (defmacro define-system-call name-and-options (name-of-check-system-call return-type &rest args &key) &body args)
;; (defmacro define-check-system-call (name lambda-args &body body))
;;
;; DEFINE-SYSTEM-CALL requires a name of a system call checker created
;; with DEFINE-CHECK-SYSTEM-CALL.
;;
;; An example of DEFINE-CHECK-SYSTEM-CALL is as follows
;;
;; (define-check-system-call check-posix (caller name return-value)
;;   (if (/= return-value -1)
;;     return-value
;;     (let ((errno (%ff-get-errno)))
;;       (error 'posix-error
;; 	        :caller caller
;; 	        :name name
;; 	        :error-value 
;; 	        :error-message (strerror errno)))))
;;
;; With a "checker" defined, the DEFINE-SYSTEM-CALL macro can be
;; used. The only difference between DEFINE-SYSTEM-CALL and
;; CFFI:DEFCFUN is in the processing of the RETURN-TYPE argument. The
;; return type argument is used to select the "checker" for the system
;; call.
;;
;; (define-system-call (%ff-read "read") (check-posix :int)
;;   (fd :int)
;;   (buffer (:pointer :uint8))
;;   (length :unsigned-int))

(defmacro define-check-system-call (name (caller foreign-name return-value &rest args) &body body)
  `(defun ,name (,caller ,foreign-name ,return-value ,@args)
     ,@body))

(defmacro define-system-call (name-and-options return-type-info &body args)
  (destructuring-bind (lisp-name ff-name) (if (stringp (first name-and-options))
					      (reverse name-and-options)
					      name-and-options)
    (destructuring-bind (checker-name return-type &rest checker-args) return-type-info
      (let ((act-ff-name (gensym (symbol-name lisp-name)))
	    (argument-names (mapcar #'first args)))
	`(progn
	   (cffi:defcfun (,act-ff-name ,ff-name) ,return-type ,@args)
	   (defun ,lisp-name ,argument-names
	     (let ((rv (,act-ff-name ,@argument-names)))
	       (,checker-name ',lisp-name ,ff-name rv ,@checker-args))))))))
