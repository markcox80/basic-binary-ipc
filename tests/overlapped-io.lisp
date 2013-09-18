(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO.TESTS")

#-windows
(error "The file ~A should only be compiled/loaded on Windows." (or *compile-file-truename*
								    *load-truename*))

(defun random-string (&optional (number-of-characters 10))  
  (let ((rv (make-string number-of-characters))
	(offset (char-code #\a))
	(range (- (char-code #\z) (char-code #\a))))
    (dotimes (index number-of-characters)
      (let ((code (random range)))
	(setf (elt rv index) (code-char (+ offset code)))))
    rv))

(defun random-pipe-name (&optional (number-of-characters 10))
  (concatenate 'string
	       "//./pipe/"
	       (random-string number-of-characters)))

(define-test named-pipe/connection/nothing
  (with-handle (server (make-named-pipe-server (random-pipe-name)))
    (with-request (connect-request (connect-named-pipe server))
      (assert-false (invalidp connect-request))
      (assert-true (waitingp connect-request))
      (assert-false (completedp connect-request))

      (cancel-all-io server))))

(define-test named-pipe/connection/wait
  (let ((pipe-name (random-pipe-name)))
    (with-handle (server (make-named-pipe-server pipe-name))
      (with-request (connect-request (connect-named-pipe server))
	(assert-false (invalidp connect-request))
	(assert-true (waitingp connect-request))
	(assert-false (completedp connect-request))

	(with-handle (client (connect-to-named-pipe pipe-name))
	  (declare (ignore client))
	  (assert-false (waitingp connect-request))
	  (assert-true (completedp connect-request)))))))

(define-test named-pipe/connection/no-wait
  (let ((pipe-name (random-pipe-name)))
    (with-handle (server (make-named-pipe-server pipe-name))
      (with-handle (client (connect-to-named-pipe pipe-name))
	(declare (ignore client))
	(with-request (connect-request (connect-named-pipe server))
	  (assert-false (invalidp connect-request))
	  (assert-false (waitingp connect-request))
	  (assert-true (completedp connect-request)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-connected-pipe (function)
    (let ((pipe-name (random-pipe-name)))
      (with-handle (server (make-named-pipe-server pipe-name))
	(with-handle (client (connect-to-named-pipe pipe-name))
	  (with-request (connect-request (connect-named-pipe server))
	    (assert-true (completedp connect-request))
	    (funcall function server client))))))
  
  (defmacro with-connected-pipe ((server client) &body body)
    `(do-with-connected-pipe #'(lambda (,server ,client)
				 ,@body))))

(define-test named-pipe/read/no-data
  (with-connected-pipe (server client)
    (declare (ignore client))
    (cffi:with-foreign-object (buffer :uint8 100)
      (with-request (read-request (read-file server buffer 100))
	(assert-false (completedp read-request))
	(cancel-all-io server)))))

(define-test named-pipe/read/immediate
  (with-connected-pipe (server client)
    (cffi:with-foreign-objects ((buffer-to-write :uint8 100)
				(buffer-to-read :uint8 100))
      (dotimes (index 100)
	(setf (cffi:mem-aref buffer-to-write :uint8 index) index
	      (cffi:mem-aref buffer-to-read :uint8 index) 0))

      (with-request (write-request (write-file client buffer-to-write 100))
	(wait-for-single-object write-request 1000)
	(assert-true (completedp write-request))
	(obtain-results write-request)
	(assert-equal 100 (bytes-written write-request)))

      (with-request (read-request (read-file server buffer-to-read 100))
	(wait-for-single-object read-request 1000)
	(assert-true (completedp read-request))
	(obtain-results read-request)
	(assert-equal 100 (bytes-read read-request))

	(dotimes (index 100)
	  (assert-equal index (cffi:mem-aref buffer-to-read :uint8 index))
	  (assert-equal index (cffi:mem-aref (buffer read-request) :uint8 index)))))))

(define-test named-pipe/read/wait
  (with-connected-pipe (server client)
    (cffi:with-foreign-objects ((buffer-to-write :uint8 100)
				(buffer-to-read :uint8 100))
      (dotimes (index 100)
	(setf (cffi:mem-aref buffer-to-write :uint8 index) index
	      (cffi:mem-aref buffer-to-read :uint8 index) 0))
      
      (with-request (read-request (read-file server buffer-to-read 100))
	(assert-false (completedp read-request))
	(assert-equal nil (bytes-read read-request))

	(with-request (write-request (write-file client buffer-to-write 100))
	  (wait-for-single-object write-request 1000)
	  (assert-true (completedp write-request))
	  (obtain-results write-request)
	  (assert-equal 100 (bytes-written write-request)))

	(wait-for-single-object read-request 1000)
	(assert-true (completedp read-request))
	(obtain-results read-request)
	(assert-equal 100 (bytes-read read-request))

	(dotimes (index 100)
	  (assert-equal index (cffi:mem-aref buffer-to-read :uint8 index))
	  (assert-equal index (cffi:mem-aref (buffer read-request) :uint8 index)))))))
