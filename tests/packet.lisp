(in-package "BASIC-BINARY-PACKET.TESTS")

(define-test magic-header-search
  (let* ((magic-header (make-array 4
				   :element-type '(unsigned-byte 8)
				   :initial-contents '(0 0 255 255)))
	 (fn (basic-binary-packet::make-magic-header-search-function magic-header)))
    (flexi-streams:with-input-from-sequence (in (make-array 10
							    :element-type '(unsigned-byte 8)
							    :initial-contents '(0 0 255 128 0 0 255 255 12 128)))
      (dotimes (i 7)
	(assert-false (funcall fn in)))
      (assert-true (funcall fn in))
      (assert-equal 12 (read-byte in))
      (assert-false (funcall fn in))
      (assert-error 'error (funcall fn in)))))

(defun sequence-equal-p (a b &key (test #'eql))
  (and (= (length a)
	  (length b))
       (every test a b)))

(define-test sequence-equal-p
  (assert-false (sequence-equal-p #(1 2 3) '(1 2)))
  (assert-false (sequence-equal-p '(1 2) '(2 1 3)))
  (assert-false (sequence-equal-p '(1 2) '(2 1)))
  (assert-true (sequence-equal-p '(1 2) '(1 2)))
  (assert-true (sequence-equal-p '(1 2 3) #(1 2 3)))
  (assert-true (sequence-equal-p #(1 2 3) #(1 2 3))))

(define-test payload-accumulator
  (flexi-streams:with-input-from-sequence (in (make-array 10
							  :element-type '(unsigned-byte 8)
							  :initial-contents '(0 1 2 3 4 5 6 7 8 9)))
    (let ((fn (basic-binary-packet::make-payload-accumulator-function 4)))
      (dotimes (i 3)
	(assert-false (funcall fn in)))
      (assert-true (sequence-equal-p #(0 1 2 3) (funcall fn in)))

      (dotimes (i 3)
	(assert-false (funcall fn in)))
      (assert-true (sequence-equal-p #(4 5 6 7) (funcall fn in)))

      (dotimes (i 2)
	(assert-false (funcall fn in)))
      (assert-error 'error (funcall fn in)))))

(define-test decode-32-bit-unsigned-integer
  (labels ((four-bytes (a b c d)
	     (make-array 4 :element-type '(unsigned-byte 8)
			 :initial-contents (list a b c d)))
	   (trial (a b c d)
	     (basic-binary-packet::decode-32-bit-unsigned-integer (four-bytes a b c d))))
    (assert-equal      0 (trial 0 0 0 0))
    (assert-equal      1 (trial 0 0 0 1))
    (assert-equal    512 (trial 0 0 2 0))
    (assert-equal 262144 (trial 0 4 0 0))
    (assert-equal 262148 (trial 0 4 0 4))))

(define-test make-packet-reader-function
  (labels ((send (value stream)
	     (declare (type vector value))
	     (if (subtypep (array-element-type value) '(unsigned-byte 8))
		 (write-sequence value stream)
		 (send (make-array (length value)
				   :element-type '(unsigned-byte 8)
				   :initial-contents (map 'list #'identity value))
		       stream))))
    (let ((fn (basic-binary-packet::make-packet-reader-function))
	  (bytes (flexi-streams:with-output-to-sequence (out)
		   ;; packet 1
		   (send basic-binary-packet::*magic-header* out)
		   (send #(0 0 0 1) out) ;; identifier
		   (send #(0 0 0 2) out) ;; 2 bytes of payload
		   (send #(10 11) out)

		   ;; rubbish
		   (send #(255 0 101) out)
		   
		   ;; packet 2
		   (send basic-binary-packet::*magic-header* out)
		   (send #(0 0 0 2) out) ;; identifier
		   (send #(0 0 0 1) out) ;; 1 bytes of payload
		   (send #(15) out))))
      (flexi-streams:with-input-from-sequence (in bytes)
	;; packet 1
	(dotimes (i (1- (+ (length basic-binary-packet::*magic-header*)
			   4 4 2)))
	  (assert-false (funcall fn in)))
	(multiple-value-bind (payload identifier) (funcall fn in)
	  (assert-true (sequence-equal-p #(10 11) payload))
	  (assert-equal 1 identifier))

	;; packet 2
	(dotimes (i (1- (+ 3 ;; rubbish
			   (length basic-binary-packet::*magic-header*)
			   4 4 1)))
	  (assert-false (funcall fn in)))
	(multiple-value-bind (payload identifier) (funcall fn in)
	  (assert-true (sequence-equal-p #(15) payload))
	  (assert-equal 2 identifier))))))

(define-test make-packet-reader-function/insufficient
  (labels ((send (value stream)
	     (declare (type vector value))
	     (if (subtypep (array-element-type value) '(unsigned-byte 8))
		 (write-sequence value stream)
		 (send (make-array (length value)
				   :element-type '(unsigned-byte 8)
				   :initial-contents (map 'list #'identity value))
		       stream))))
    (let ((fn (basic-binary-packet::make-packet-reader-function))
	  (bytes (flexi-streams:with-output-to-sequence (out)
		   ;; packet 1
		   (send basic-binary-packet::*magic-header* out)
		   (send #(0 0 0 1) out) ;; identifier
		   (send #(0 0 0 2) out) ;; 2 bytes of payload
		   )))
      (flexi-streams:with-input-from-sequence (in bytes)
	;; packet 1
	(dotimes (i (length bytes))
	  (assert-false (funcall fn in)))
	(assert-error 'error (funcall fn in))))))
