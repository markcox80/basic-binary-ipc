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
	  (assert-true (completedp connect-request))
	  (assert-true (succeededp connect-request)))))))

(define-test named-pipe/connection/no-wait
  (let ((pipe-name (random-pipe-name)))
    (with-handle (server (make-named-pipe-server pipe-name))
      (with-handle (client (connect-to-named-pipe pipe-name))
	(declare (ignore client))
	(with-request (connect-request (connect-named-pipe server))
	  (assert-false (invalidp connect-request))
	  (assert-false (waitingp connect-request))
	  (assert-true (completedp connect-request))
	  (assert-true (succeededp connect-request)))))))

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
	(assert-true (succeededp write-request))
	(assert-equal 100 (bytes-written write-request)))

      (with-request (read-request (read-file server buffer-to-read 100))
	(wait-for-request read-request 1)
	(assert-true (completedp read-request))
	(assert-true (succeededp read-request))
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
	  (assert-true (succeededp write-request))
	  (assert-equal 100 (bytes-written write-request)))

	(wait-for-request read-request 1)
	(assert-true (completedp read-request))
	(assert-true (succeededp read-request))
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
	(assert-false (succeededp read-request))
	(assert-equal 0 (bytes-read read-request))))))

(define-test named-pipe/disconnect/close-server
  (with-connected-pipe (server client :ignore-close-errors :server-only)
    (cffi:with-foreign-objects ((buffer-to-read :uint8 100))
      (with-request (read-request (read-file client buffer-to-read 100))
	(assert-false (completedp read-request))	
	(close-handle server)
	(assert-true (completedp read-request))
	(assert-false (succeededp read-request))
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
  (let ((number-of-bytes 20))
    (with-connected-pipe (server-1 client-1)
      (with-connected-pipe (server-2 client-2)
	(cffi:with-foreign-objects ((buffer-1 :uint8 number-of-bytes)
				    (buffer-2 :uint8 number-of-bytes)
				    (write-buffer :uint8 number-of-bytes))
	  (dotimes (index number-of-bytes)
	    (setf (cffi:mem-aref write-buffer :uint8 index) index
		  (cffi:mem-aref buffer-1 :uint8 index) 0
		  (cffi:mem-aref buffer-2 :uint8 index) 0))
	  
	  (with-monitor (monitor)
	    (with-request (read-1 (read-file server-1 buffer-1 number-of-bytes))
	      (with-request (read-2 (read-file server-2 buffer-2 number-of-bytes))
		(monitor monitor read-1)
		(monitor monitor read-2)
		
		(with-request (write-1 (make-instance 'write-file-request :descriptor client-1))
		  (with-request (write-2 (make-instance 'write-file-request :descriptor client-2))
		    (monitor monitor write-1)
		    (monitor monitor write-2)

		    (write-file client-1 write-buffer number-of-bytes write-1)
		    (write-file client-2 write-buffer number-of-bytes write-2)

		    (let ((requests (list read-1 read-2 write-1 write-2)))
		      (dotimes (attempt 4)
			(let ((v (pop-notification monitor 1)))
			  (assert-true (find v requests))
			  (setf requests (remove v requests))))
		      (assert-false (pop-notification monitor 0)))

		    (assert-true (completedp write-1))
		    (assert-true (completedp write-2))
		    (assert-true (completedp read-1))
		    (assert-true (completedp read-2)))))))

	  (dotimes (index number-of-bytes)
	    (assert-equal index (cffi:mem-aref buffer-1 :uint8 index))
	    (assert-equal index (cffi:mem-aref buffer-2 :uint8 index))))))))

;;;; Sockets
(defvar *ports* nil)
(defun random-port ()
  (loop
     :for port := (+ 41000 (random 1000))
     :while (find port *ports* :test #'=)
     :finally (progn
		(push port *ports*)
		(return port))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-with-socket (socket function)
    (unwind-protect
	 (funcall function socket)
      (ignore-errors (close-socket socket))))

  (defmacro with-socket ((var form) &body body)
    `(do-with-socket ,form
       #'(lambda (,var)
	   ,@body))))

(define-test ipv4-connection
  (let ((port (random-port))
	(address "127.0.0.1")
	(buffer-length (minimum-accept-ipv4-buffer-size)))
    (with-socket (server (make-ipv4-server address port))
      (with-socket (remote-client (make-socket :af-inet :sock-stream :ipproto-tcp))
	(cffi:with-foreign-objects ((buffer :uint8 buffer-length))
	  (with-request (accept (accept-ipv4 server remote-client buffer buffer-length))
	    (multiple-value-bind (client connection-request) (connect-ipv4 address port)
	      (with-request (connection-request connection-request)
		(with-socket (client client)
		  (declare (ignore client))
		  (wait-for-request accept 10)
		  (assert-true (completedp accept))
		  (assert-true (succeededp accept))
		  (assert-true (completedp connection-request))
		  (assert-true (succeededp connection-request))
		  (assert-equal address (remote-address connection-request))
		  (assert-equal port (remote-port connection-request))
		  (assert-equal address (local-address accept))
		  (assert-equal port (local-port accept)))))))))))

(define-test ipv4-connection/no-server/remote
  (let ((address "169.254.1.1")
	(port (random-port)))
    (multiple-value-bind (client connection-request) (connect-ipv4 address port)
      (with-request (connection-request connection-request)
	(with-socket (client client)
	  (declare (ignore client))
	  (wait-for-request connection-request 60)
	  (assert-true (completedp connection-request))
	  (assert-false (succeededp connection-request))
	  (assert-equal nil (remote-address connection-request)))))))

(define-test ipv4-connection/no-server/local
  (let ((address "127.0.0.1")
	(port (random-port)))
    (multiple-value-bind (client connection-request) (connect-ipv4 address port)
      (with-request (connection-request connection-request)
	(with-socket (client client)
	  (declare (ignore client))
	  (wait-for-request connection-request 60)
	  (assert-true (completedp connection-request))
	  (assert-false (succeededp connection-request))
	  (assert-equal nil (remote-address connection-request)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defun do-with-ipv4 (function address port)
    (let ((buffer-length (minimum-accept-ipv4-buffer-size)))
      (with-socket (server (make-ipv4-server address port))
	(with-socket (remote-client (make-socket :af-inet :sock-stream :ipproto-tcp))
	  (cffi:with-foreign-object (buffer :uint8 buffer-length)
	    (with-request (accept (accept-ipv4 server remote-client buffer buffer-length))
	      (multiple-value-bind (client connection-request) (connect-ipv4 address port)
		(wait-for-request connection-request 10)
		(assert (and (completedp connection-request)
			     (succeededp connection-request)))
		(assert (completedp accept))
		(with-socket (client client)
		  (funcall function client remote-client)))))))))

  (defmacro with-ipv4 ((client remote-client &optional (address "127.0.0.1") (port '(random-port))) &body body)
    `(do-with-ipv4 #'(lambda (,client ,remote-client)
		       ,@body)
       ,address ,port)))

(define-test ipv4-connection/communication/nowait
  (let ((buffer-length 10))
    (with-ipv4 (client remote-client)
      (cffi:with-foreign-objects ((read-buffer :uint8 buffer-length)
				  (write-buffer :uint8 buffer-length))
	(dotimes (index buffer-length)
	  (setf (cffi:mem-aref write-buffer :uint8 index) index
		(cffi:mem-aref read-buffer :uint8 index) 0))
	(with-request (writing (write-file client write-buffer buffer-length))
	  (wait-for-request writing 10)
	  (assert-true (completedp writing)))

	(with-request (reading (read-file remote-client read-buffer buffer-length))
	  (assert-true (completedp reading))
	  (dotimes (index buffer-length)
	    (assert-equal (cffi:mem-aref write-buffer :uint8 index)
			  (cffi:mem-aref read-buffer :uint8 index))))))))

(define-test ipv4-connection/communication/wait
  (let ((buffer-length 10))
    (with-ipv4 (client remote-client)
      (cffi:with-foreign-objects ((read-buffer :uint8 buffer-length)
				  (write-buffer :uint8 buffer-length))
	(dotimes (index buffer-length)
	  (setf (cffi:mem-aref write-buffer :uint8 index) index
		(cffi:mem-aref read-buffer :uint8 index) 0))
	(with-request (reading (read-file remote-client read-buffer buffer-length))
	  (assert-false (completedp reading))

	  (with-request (writing (write-file client write-buffer buffer-length))
	    (wait-for-request writing 10)
	    (assert-true (and (completedp writing)
			      (succeededp writing)))
	    
	    (assert-true (and (completedp reading)
			      (succeededp reading)))
	    (dotimes (index buffer-length)
	      (assert-equal (cffi:mem-aref write-buffer :uint8 index)
			    (cffi:mem-aref read-buffer :uint8 index)))))))))

(define-test ipv4-connection/communication/disconnect
  (let ((buffer-length 10))
    (with-ipv4 (client remote-client)
      (cffi:with-foreign-objects ((write-buffer :uint8 buffer-length)
				  (read-buffer :uint8 buffer-length))
	(dotimes (index buffer-length)
	  (setf (cffi:mem-aref write-buffer :uint8 index) index
		(cffi:mem-aref read-buffer :uint8 index) 0))

	(close-socket remote-client)

	(with-request (writing (write-file client write-buffer buffer-length))
	  (wait-for-request writing 10)
	  (assert-true (and (completedp writing)
			    (succeededp writing))))))))
