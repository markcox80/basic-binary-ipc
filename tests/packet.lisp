(in-package "BASIC-BINARY-PACKET.TESTS")

(define-test magic-header-search
  (let* ((magic-header (make-array 4
				   :element-type '(unsigned-byte 8)
				   :initial-contents '(0 0 255 255)))
	 (fn (basic-binary-packet::make-magic-header-search-function magic-header)))
    (flexi-streams:with-input-from-sequence (in (make-array 9
							    :element-type '(unsigned-byte 8)
							    :initial-contents '(0 0 255 128 0 0 255 255 12)))
      (dotimes (i 7)
	(assert-false (funcall fn in)))
      (assert-true (funcall fn in))
      (assert-equal 12 (read-byte in)))))
