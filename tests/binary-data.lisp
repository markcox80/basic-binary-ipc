(in-package "BASIC-BINARY-PACKET.TESTS")

(define-test binary-nil-test
  (let ((bytes (flexi-streams:with-output-to-sequence (out)
		 (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out nil))))
    (assert-equal 4 (length bytes))
    (flexi-streams:with-input-from-sequence (in bytes)
      (assert-true (null (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-object in))))))

(define-test binary-t-test
  (let ((bytes (flexi-streams:with-output-to-sequence (out)
		 (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out t))))
    (assert-equal 4 (length bytes))
    (flexi-streams:with-input-from-sequence (in bytes)
      (assert-equal t (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-object in)))))

(define-test binary-cons-test
  (let* ((item  '((a . 5) (b . 6) (:here . 2) (:garbage . t) nil))
	 (bytes (flexi-streams:with-output-to-sequence (out)
		  (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out item))))
    (flexi-streams:with-input-from-sequence (in bytes)
      (assert-equal item (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-object in)))))

(define-test binary-list-generic-test
  (let* ((item '((a . 5) "hello"))
	 (bytes (flexi-streams:with-output-to-sequence (out)
		  (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out item :binary-type 'basic-binary-packet::binary-list-generic))))
    (flexi-streams:with-input-from-sequence (in bytes)
      (assert-equal item (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-object in)))))

(define-test binary-list-fixed-test
  (let* ((item '("hello" "mate"))
	 (bytes (flexi-streams:with-output-to-sequence (out)
		  (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out item :binary-type 'basic-binary-packet::binary-list-fixed))))
    (flexi-streams:with-input-from-sequence (in bytes)
      (assert-equal item (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-object in))))

  ;; an error should occur with this one because it is the wrong object type.
  (flexi-streams:with-output-to-sequence (out)
    (assert-error 'error (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out (list "hello" 5) :binary-type 'basic-binary-packet::binary-list-fixed))))

(com.gigamonkeys.binary-data:define-binary-type my-custom-binary-type ()
  (:reader (in)
    (let ((v (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-uint8 in)))
      (ecase v
	(1 :one)
	(2 :two)
	(3 :three))))
  (:writer (out value)
    (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-uint8
					     out
					     (ecase value
					       (:one 1)
					       (:two 2)
					       (:three 3)))))

(define-test custom-binary-type-test
  (let* ((item  :one)
	 (bytes (flexi-streams:with-output-to-sequence (out)
		 (com.gigamonkeys.binary-data:write-value 'basic-binary-packet::binary-object out item :binary-type 'my-custom-binary-type))))
    (flexi-streams:with-input-from-sequence (in bytes)
      (assert-equal item (com.gigamonkeys.binary-data:read-value 'basic-binary-packet::binary-object in)))))
