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
