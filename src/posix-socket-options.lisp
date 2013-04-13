(in-package "BASIC-BINARY-IPC")

;; Socket option arguments
(defgeneric soa-base-type (soa))
(defgeneric soa-size (soa))
(defgeneric soa-translate-from-memory (soa pointer))
(defgeneric soa-translate-to-memory (soa value pointer))

(defclass socket-option-argument ()
  ((base-type
    :initarg :base-type
    :reader soa-base-type)))

(defmethod soa-size ((object socket-option-argument))
  (cffi:foreign-type-size (soa-base-type object)))

(defun socket-option-argument (symbol)
  (get symbol 'socket-option-argument))

(defun (setf socket-option-argument) (value symbol)
  (setf (get symbol 'socket-option-argument) value))

(defun ensure-socket-option-argument (name &key base-type)
  (setf (socket-option-argument name) (make-instance 'socket-option-argument
						     :base-type base-type)))

(defmacro define-socket-option-argument (name &body options)
  (labels ((option-values (key)
	     (let ((v (find key options :key #'first)))
	       (assert v)
	       (rest v)))
	   (option-value (key)
	     (let ((v (option-values key)))
	       (assert (= 1 (length v)))
	       (first v))))
    (let ((soa-object (gensym)))
      `(progn
	 (ensure-socket-option-argument ',name :base-type ,(option-value :base-type))
	 ,(destructuring-bind ((value pointer) &rest body) (option-values :writer)
			      `(defmethod soa-translate-to-memory ((,soa-object (eql ',name)) ,value ,pointer)
				 ,@body))
	 ,(destructuring-bind ((pointer) &rest body) (option-values :reader)
			      `(defmethod soa-translate-from-memory ((,soa-object (eql ',name)) ,pointer)
				 ,@body))))))

;; Socket options
(defun do-define-socket-option/reader (name socket-option-name socket-option-argument &key (level 0))
  `(defmethod ,name ((object posix-socket))
     (let ((soa (socket-option-argument ',socket-option-argument)))
       (cffi:with-foreign-object (ptr (soa-base-type soa))
	 (cffi:with-foreign-object (ptr-length 'socklen-t)
	   (setf (cffi:mem-ref ptr-length 'socklen-t) (soa-size soa))
	   (%ff-getsockopt (file-descriptor object) ,level ,socket-option-name ptr ptr-length)
	   (assert (= (cffi:mem-ref ptr-length 'socklen-t)
		      (soa-size soa)))
	   (soa-translate-from-memory ',socket-option-argument ptr))))))

(defun do-define-socket-option/writer (name socket-option-name socket-option-argument &key (level 0))
  `(defmethod (setf ,name) (value (object posix-socket))
     (let ((soa (socket-option-argument ',socket-option-argument)))
       (cffi:with-foreign-object (ptr (soa-base-type soa))
	 (soa-translate-to-memory ',socket-option-argument value ptr)
	 (%ff-setsockopt (file-descriptor object) ,level ,socket-option-name ptr (soa-size soa)))
       value)))

(defmacro define-socket-option ((name socket-option-name socket-option-argument) &rest args &key (level 0))
  (declare (ignore level))
  `(progn
     ,(apply #'do-define-socket-option/reader name socket-option-name socket-option-argument args)
     ,(apply #'do-define-socket-option/writer name socket-option-name socket-option-argument args)))


