(in-package "BASIC-BINARY-PACKET.NETWORK.TESTS")

(defvar *test-address* iolib:+loopback+)
(defvar *test-port*    4444)

(defun make-test-server (on-new-connection &rest args &key &allow-other-keys)
  (apply #'make-server *test-address* *test-port* on-new-connection args))

(defun make-test-client ()
  (make-client *test-address* *test-port*))

(define-test server-no-new-connection
  (process-events :timeout 1)
  (assert-error 'error (print (make-test-server nil))))

(define-test server-startup
  (process-events :timeout 1)
  (let ((s (make-test-server #'(lambda (server connection)
				 (declare (ignore server connection))
				 (error "This should not be called.")))))
    (unwind-protect
	 (process-events :timeout 1)
      (close s))))

(define-test client-connection-refused
  (process-events :timeout 1)
  (let ((s (make-test-client))
	(c nil))
    (setf (on-error s) #'(lambda (stream condition)
			   (assert-equal s stream)
			   (setf c condition)))
    (unwind-protect
	 (progn
	   (process-events :timeout 1)
	   (assert-true (typep c 'iolib:socket-connection-refused-error)))
      (close s))))

(define-test server-to-client
  (process-events :timeout 1)
  (let* ((received-object nil)
	 (received-identifier nil)
	 (remote-client nil)
	 (remote-client-connected nil)
	 (client-connected nil)
	 (server (make-test-server #'(lambda (server stream)
				       (declare (ignore server))
				       (setf remote-client stream
					     (on-connection remote-client) #'(lambda (stream)
									       (declare (ignore stream))
									       (setf remote-client-connected t))
					     (on-object remote-client) #'(lambda (stream object identifier)
									   (declare (ignore stream))
									   (setf received-object object
										 received-identifier identifier))))))
	 (client (make-test-client)))
    (setf (on-connection client) #'(lambda (stream)
				     (declare (ignore stream))
				     (setf client-connected t)))
    (unwind-protect
	 (progn
	   (dotimes (i 10)
	     (process-events :timeout 1)
	     (when (and ; remote-client-connected
			client-connected)
	       (print :writing)
	       (write-object client 5 :identifier 2)
	       (setf remote-client-connected nil
		     client-connected nil))
	     (sleep 0.5))
	   (assert-equal 5 received-object)
	   (assert-equal 2 received-identifier)
	   (assert-true remote-client))
      (close server)      
      (close client)
      (when remote-client
	(close remote-client)))))
