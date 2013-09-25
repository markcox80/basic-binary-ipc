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
  (defun do-with-connected-pipe (function &key ignore-close-errors)
    (let ((pipe-name (random-pipe-name)))
      (with-handle (server (make-named-pipe-server pipe-name) :ignore-close-errors (if (eql ignore-close-errors :client-only)
										       nil
										       t))
	(with-handle (client (connect-to-named-pipe pipe-name) :ignore-close-errors (if (eql ignore-close-errors :server-only)
											nil
											t))
	  (with-request (connect-request (connect-named-pipe server))
	    (assert (completedp connect-request))
	    (funcall function server client))))))
  
  (defmacro with-connected-pipe ((server client &rest args) &body body)
    `(do-with-connected-pipe #'(lambda (,server ,client)
				 ,@body)
       ,@args)))

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
	(wait-for-request write-request 1)
	(assert-true (completedp write-request))
	(assert-equal 100 (bytes-written write-request)))

      (with-request (read-request (read-file server buffer-to-read 100))
	(wait-for-request read-request 1)
	(assert-true (completedp read-request))
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
	  (wait-for-request write-request 1)
	  (assert-true (completedp write-request))
	  (assert-equal 100 (bytes-written write-request)))

	(wait-for-request read-request 1)
	(assert-true (completedp read-request))
	(assert-equal 100 (bytes-read read-request))

	(dotimes (index 100)
	  (assert-equal index (cffi:mem-aref buffer-to-read :uint8 index))
	  (assert-equal index (cffi:mem-aref (buffer read-request) :uint8 index)))))))

(define-test named-pipe/disconnect/close-client
  (with-connected-pipe (server client :ignore-close-errors :client-only)
    (cffi:with-foreign-objects ((buffer-to-read :uint8 100))
      (with-request (read-request (read-file server buffer-to-read 100))
	(assert-false (completedp read-request))	
	(close-handle client)
	(assert-true (completedp read-request))
	(assert-equal 0 (bytes-read read-request))))))

(define-test named-pipe/disconnect/close-server
  (with-connected-pipe (server client :ignore-close-errors :server-only)
    (cffi:with-foreign-objects ((buffer-to-read :uint8 100))
      (with-request (read-request (read-file client buffer-to-read 100))
	(assert-false (completedp read-request))	
	(close-handle server)
	(assert-true (completedp read-request))
	(assert-equal 0 (bytes-read read-request))))))

(define-test wait-for-requests/one
  (with-connected-pipe (server-1 client-1)
    (declare (ignore client-1))
    (with-connected-pipe (server-2 client-2)
      (cffi:with-foreign-objects ((buffer-1 :uint8 10)
				  (buffer-2 :uint8 10)
				  (write-buffer :uint8 10))
	(dotimes (index 10)
	  (setf (cffi:mem-aref write-buffer :uint8) index))
	
	(with-request (read-1 (read-file server-1 buffer-1 10))
	  (with-request (read-2 (read-file server-2 buffer-2 10))
	    (assert-equal nil (wait-for-requests (list read-1 read-2) 0))
	    
	    (with-request (write-2 (write-file client-2 write-buffer 10))
	      (assert-true (completedp write-2)))

	    (assert-equal read-2 (wait-for-requests (list read-1 read-2) 1))
	    (assert-equal 10 (bytes-read read-2)))
	  (cancel-all-io server-1))))))

(define-test wait-for-requests/all
  (with-connected-pipe (server-1 client-1)
    (with-connected-pipe (server-2 client-2)
      (cffi:with-foreign-objects ((buffer-1 :uint8 10)
				  (buffer-2 :uint8 10)
				  (write-buffer :uint8 10))
	(dotimes (index 10)
	  (setf (cffi:mem-aref write-buffer :uint8) index))
	
	(with-request (read-1 (read-file server-1 buffer-1 10))
	  (with-request (read-2 (read-file server-2 buffer-2 10))
	    (assert-equal nil (wait-for-requests (list read-1 read-2) 0 :wait-all t))
	    
	    (with-request (write-1 (write-file client-1 write-buffer 10))
	      (assert-true (completedp write-1)))

	    (assert-equal nil (wait-for-requests (list read-1 read-2) 1 :wait-all t))

	    (with-request (write-2 (write-file client-2 write-buffer 10))
	      (assert-true (completedp write-2)))

	    (assert-equal (list read-1 read-2) (wait-for-requests (list read-1 read-2) 1 :wait-all t))
	    
	    (assert-equal 10 (bytes-read read-1))
	    (assert-equal 10 (bytes-read read-2))))))))

(define-test monitor
  (with-connected-pipe (server-1 client-1)
    (with-connected-pipe (server-2 client-2)
      (cffi:with-foreign-objects ((buffer-1 :uint8 10)
				  (buffer-2 :uint8 10)
				  (write-buffer :uint8 10))
	(dotimes (index 10)
	  (setf (cffi:mem-aref write-buffer :uint8) index))
	
	(with-monitor (monitor)
	  (with-request (read-1 (read-file server-1 buffer-1 10))
	    (with-request (read-2 (read-file server-2 buffer-2 10))
	      (monitor monitor read-1)
	      (monitor monitor read-2)
	      
	      (with-request (write-1 (make-instance 'write-file-request :descriptor client-1))
		(with-request (write-2 (make-instance 'write-file-request :descriptor client-2))
		  (monitor monitor write-1)
		  (monitor monitor write-2)

		  (write-file client-1 write-buffer 10 write-1)
		  (write-file client-2 write-buffer 10 write-2)

		  (let ((requests (list read-1 read-2 write-1 write-2)))
		    (dotimes (attempt 4)
		      (let ((v (pop-notification monitor 0)))
			(assert-true (find v requests))
			(setf requests (remove v requests))))
		    (assert-false (pop-notification monitor 0))))))))))))
