(in-package "BASIC-BINARY-PACKET")

(defvar *custom-binary-type-identifier* 65536)
(defvar *binary-object-identifier-map* nil)

(defun binary-type-for-identifier (object-identifier)
  (let ((v (find object-identifier *binary-object-identifier-map* :key #'car)))
    (if v
	(cdr v)
	(error "Unable to find binary-type for identifier ~d" object-identifier))))

(defun binary-object-identifier-for-type (binary-type)
  (let ((v (find binary-type *binary-object-identifier-map* :key #'cdr)))
    (if v
	(car v)
	*custom-binary-type-identifier*)))

(defun install-binary-identifier (identifier binary-type)
  (assert (/= identifier *custom-binary-type-identifier*))
  (let ((v (find identifier *binary-object-identifier-map* :key #'car)))
    (if v
	(setf (cdr v) binary-type)
	(push (cons identifier binary-type) *binary-object-identifier-map*)))
  (values))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-binary-object-identifier-map (&body body)
    `(progn
       ,@(loop :for item :in body :collect
	    (destructuring-bind (object-identifier binary-type) item
	      `(install-binary-identifier ,object-identifier ',binary-type))))))

(define-binary-object-identifier-map
  (  0 binary-NIL)
  (  1 binary-T)
  (  2 binary-keyword)
  (  3 binary-symbol)
  (  4 binary-cons)
  (  5 binary-utf8-string)
  ( 10 binary-uint8)
  ( 11 binary-int8)
  ( 12 binary-uint32)
  ( 13 binary-int32)
  ( 14 binary-uint64)
  ( 15 binary-int64)
  ( 16 binary-single-float)
  ( 17 binary-double-float)
  (100 binary-list-generic)
  (101 binary-list-fixed))

(defun custom-binary-type-identifier-p (identifier)
  (= identifier *custom-binary-type-identifier*))

(defgeneric binary-type-for-object (object)
  (:documentation "Return the binary type to use for OBJECT."))

(define-binary-type binary-object-identifier ()
  (:reader (in)
    (read-value 'binary-uint32 in))
  (:writer (out value)
    (write-value 'binary-uint32 out value)))

(define-binary-type binary-type ()
  (:reader (in)
    (let ((identifier (read-value 'binary-object-identifier in)))
      (if (custom-binary-type-identifier-p identifier)
	  (read-value 'custom-binary-type in)
	  (binary-type-for-identifier identifier))))
  (:writer (out value)
    (let ((identifier (binary-object-identifier-for-type value)))      
      (write-value 'binary-object-identifier out identifier)
      (when (custom-binary-type-identifier-p identifier)
	(write-value 'custom-binary-type out value)))))

(define-binary-type custom-binary-type ()
  (:reader (in)
    (read-value 'binary-symbol in))
  (:writer (out value)
    (write-value 'binary-symbol out value)))

(define-binary-type binary-object (binary-type)
  (:reader (in)
    (read-value (read-value 'binary-type in) in))
  (:writer (out value)
    (let ((binary-type (if binary-type
			   binary-type
			   (binary-type-for-object value))))
      (write-value 'binary-type out binary-type)
      (write-value binary-type  out value))))

;; binary-type-for-object methods
(defmethod binary-type-for-object ((object symbol))
  (cond
    ((null object)
     'binary-nil)
    ((eql object t)
     'binary-T)
    ((keywordp object)
     'binary-keyword)
    (t
     'binary-symbol)))

(defmethod binary-type-for-object ((object string))
  'binary-utf8-string)

(defmethod binary-type-for-object ((object cons))
  'binary-cons)

(defmethod binary-type-for-object ((object integer))
  (cond
    ((typep object '(unsigned-byte 8))
     'binary-uint8)
    ((typep object '(unsigned-byte 16))
     'binary-uint16)
    ((typep object '(unsigned-byte 32))
     'binary-uint32)
    ((typep object '(unsigned-byte 64))
     'binary-uint64)
    ((typep object '(signed-byte 8))
     'binary-int8)
    ((typep object '(signed-byte 16))
     'binary-int16)
    ((typep object '(signed-byte 32))
     'binary-int32)
    ((typep object '(signed-byte 64))
     'binary-int64)
    (t
     (error "Do not know how to encode integer ~d" object))))

;; binary types built on the above
(define-binary-type binary-nil ()
  (:reader (in)
    (declare (ignore in))
    nil)
  (:writer (out value)
    (declare (ignore out))
    (assert (null value))))

(define-binary-type binary-t ()
  (:reader (in)
    (declare (ignore in))
    t)
  (:writer (out value)
    (declare (ignore out))
    (assert (eql value t))))

(define-binary-type binary-cons ()
  (:reader (in)
    (cons (read-value 'binary-object in)
	  (read-value 'binary-object in)))
  (:writer (out value)
    (write-value 'binary-object out (car value))
    (write-value 'binary-object out (cdr value))))

(define-binary-type binary-list-generic ()
  (:reader (in)
    (let ((number-of-elements (read-value 'binary-uint32 in)))
      (loop :for i :from 0 :below number-of-elements :collect
	 (read-value 'binary-object in))))
  (:writer (out value)
    (write-value 'binary-uint32 out (length value))
    (dolist (item value)
      (write-value 'binary-object out item))))

(define-binary-type binary-list-fixed (item-binary-type)
  (:reader (in)
    (let ((number-of-elements (read-value 'binary-uint32 in))
	  (binary-type        (read-value 'binary-type in)))
      (loop :for i :from 0 :below number-of-elements :collect
	 (read-value binary-type in))))
  (:writer (out value)
    (assert value)
    (write-value 'binary-uint32 out (length value))
    (let ((item-binary-type (or item-binary-type
				(binary-type-for-object (first value)))))
      (write-value 'binary-type out item-binary-type)
      (dolist (item value)
	(write-value item-binary-type out item)))))
