(in-package "BASIC-BINARY-PACKET")

;; COM.GIGAMONKEYS.BINARY-DATA definitions for reading and writing the
;; primitive types
;; - unsigned integers 8,16,32 and 64 bit
;; - signed integers 8,16,32 and 64 bit
;; - floating point 32 and 64 bit.
;; - boolean 32 bits!

(define-binary-type binary-unsigned-integer (number-of-bytes bits-per-byte)
  (:reader (in)
    (assert (and (subtypep (list 'unsigned-byte bits-per-byte) (stream-element-type in))
		 (subtypep (stream-element-type in) (list 'unsigned-byte bits-per-byte))))
    (let ((rv 0)
	  (number-of-bits (* number-of-bytes bits-per-byte))
	  (bytes (make-array number-of-bytes :element-type (list 'unsigned-byte bits-per-byte) :initial-element 0)))
      (unless (= number-of-bytes (read-sequence bytes in))
	(error "Not enough bytes read from stream."))
      (loop
	 :for byte-position :downfrom (* (1- number-of-bytes) bits-per-byte) :to 0 :by bits-per-byte
	 :for x :from 0 :below number-of-bytes
	 :do
	 (setf (ldb (byte bits-per-byte byte-position) rv) (elt bytes x)))
      (assert (typep rv (list 'unsigned-byte number-of-bits)))
      rv))
  (:writer (out value)
    (assert (and (subtypep (list 'unsigned-byte bits-per-byte) (stream-element-type out))
		 (subtypep (stream-element-type out) (list 'unsigned-byte bits-per-byte))))
    (let ((number-of-bits (* number-of-bytes bits-per-byte))
	  (bytes (make-array number-of-bytes :element-type (list 'unsigned-byte bits-per-byte) :initial-element 0)))
      (assert (typep value (list 'unsigned-byte number-of-bits)))
      (loop
	 :for byte-position :downfrom (* (1- number-of-bytes) bits-per-byte) :to 0 :by bits-per-byte
	 :for x :from 0 :below number-of-bytes
	 :do
	 (setf (elt bytes x) (ldb (byte bits-per-byte byte-position) value)))
      (write-sequence bytes out))))

(define-binary-type binary-uint8 () (binary-unsigned-integer :number-of-bytes 1 :bits-per-byte 8))
(define-binary-type binary-uint16 () (binary-unsigned-integer :number-of-bytes 2 :bits-per-byte 8))
(define-binary-type binary-uint32 () (binary-unsigned-integer :number-of-bytes 4 :bits-per-byte 8))
(define-binary-type binary-uint64 () (binary-unsigned-integer :number-of-bytes 8 :bits-per-byte 8))

(define-binary-type binary-signed-integer (number-of-bytes bits-per-byte)
  (:reader (in)
    (let* ((value (read-value 'binary-unsigned-integer in :number-of-bytes number-of-bytes :bits-per-byte bits-per-byte))
	   (number-of-bits   (* number-of-bytes bits-per-byte))
	   (rv               (if (zerop (ldb (byte 1 (1- number-of-bits)) value))	   
				 value
				 (- value (expt 2 number-of-bits)))))
       (assert (typep rv (list 'signed-byte number-of-bits)))
       rv))
  (:writer (out value)
     (let ((number-of-bits (* number-of-bytes bits-per-byte)))
       (assert (typep value (list 'signed-byte number-of-bits)))
       (if (minusp value)	 
	   (write-value 'binary-unsigned-integer out (+ (expt 2 number-of-bits) value) :number-of-bytes number-of-bytes :bits-per-byte bits-per-byte)
	   (write-value 'binary-unsigned-integer out value :number-of-bytes number-of-bytes :bits-per-byte bits-per-byte))
       value)))

(define-binary-type binary-int8 ()  (binary-signed-integer :number-of-bytes 1 :bits-per-byte 8))
(define-binary-type binary-int16 () (binary-signed-integer :number-of-bytes 2 :bits-per-byte 8))
(define-binary-type binary-int32 () (binary-signed-integer :number-of-bytes 4 :bits-per-byte 8))
(define-binary-type binary-int64 () (binary-signed-integer :number-of-bytes 8 :bits-per-byte 8))

(define-binary-type binary-boolean ()
  (:reader (in)
    (not (zerop (read-value 'binary-uint32 in))))
  (:writer (out value)
    (write-value 'binary-uint32 out (if value 1 0))))

(define-binary-type binary-double-float ()
  (:reader (in)
    (ieee-floats:decode-float64 (read-value 'binary-uint64 in)))
  (:writer (out value)
    (write-value 'binary-uint64 out (ieee-floats:encode-float64 (coerce value 'double-float)))))

(define-binary-type binary-single-float ()
  (:reader (in)
    (ieee-floats:decode-float32 (read-value 'binary-uint32 in)))
  (:writer (out value)
    (write-value 'binary-uint32 out (ieee-floats:encode-float32 (coerce value 'single-float)))))
