(in-package "BASIC-BINARY-PACKET.TESTS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-encode-decode-output (function object &key binary-type)
    (let* ((bytes (flexi-streams:with-output-to-sequence (out)
		    (encode-object out object :binary-type binary-type)))
	   (value (flexi-streams:with-input-from-sequence (in bytes)
		    (decode-object in))))
      (funcall function object value bytes)))

  (defmacro with-encode-decode-output ((expected actual bytes &rest args) object &body body)
    `(do-with-encode-decode-output #'(lambda (,expected ,actual ,bytes)
				       ,@body)
       ,object ,@args)))

(define-test binary-nil-test
  (with-encode-decode-output (exp act bytes) nil
    (assert-equal 4 (length bytes))
    (assert-equal exp act)
    (assert-true (null act))))

(define-test binary-t-test
  (with-encode-decode-output (exp act bytes) t
    (assert-equal 4 (length bytes))
    (assert-equal exp act)))

(define-test binary-cons-test
  (with-encode-decode-output (exp act bytes) '((a . 5) (b . 6) (:here . 2) (:garbage . t) nil)
    (declare (ignore bytes))
    (assert-equal exp act)))

(define-test binary-list-generic-test
  (with-encode-decode-output (exp act bytes :binary-type 'binary-list-generic) '((a . 5) "hello")
    (declare (ignore bytes))
    (assert-equal exp act)))

(define-test binary-list-fixed-test
  (with-encode-decode-output (exp act bytes :binary-type 'binary-list-fixed) '("hello" "mate")
    (declare (ignore bytes))
    (assert-equal exp act))

  ;; Check for an error if one item in the list is the wrong type.
  (flexi-streams:with-output-to-sequence (out)
    (assert-error 'error (encode-object out (list "hello" 5) :binary-type 'binary-list-fixed))))

(com.gigamonkeys.binary-data:define-binary-type my-custom-binary-type ()
  (:reader (in)
    (let ((v (read-value 'binary-uint8 in)))
      (ecase v
	(1 :one)
	(2 :two)
	(3 :three))))
  (:writer (out value)
    (write-value 'binary-uint8 out (ecase value
				     (:one 1)
				     (:two 2)
				     (:three 3)))))

(define-test custom-binary-type-test
  (with-encode-decode-output (exp act bytes :binary-type 'my-custom-binary-type) :one
    (declare (ignore bytes))
    (assert-equal exp act)))
