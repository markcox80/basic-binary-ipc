(in-package "BASIC-BINARY-PACKET.IPC.TESTS")

(defun local-socket-pathname ()
  "/tmp/test.socket")

(define-test make-local-server
  (let ((server (make-local-server (local-socket-pathname))))
    (unwind-protect
	 (progn
	   (assert-true server)
	   (assert-error 'no-connection-available-error (accept-connection server))
	   (assert-error 'posix-error (make-local-server (local-socket-pathname))))
      (close-socket server))))

(define-test local-test/sockets
  (labels ((channel-test (client remote-client)
	     (assert-true (poll-socket client '(determinedp connection-succeeded-p) 10))
	     (assert-true (poll-socket remote-client 'determinedp 10))
	     (assert-false (connection-failed-p client))
	     (assert-false (connection-failed-p remote-client))
	     (assert-true (connection-succeeded-p client))
	     (assert-true (connection-succeeded-p remote-client)))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test local-test/stream
  (labels ((channel-test (client remote-client)
	     (assert-true (poll-socket client 'ready-to-write-p 0))
	     (assert-false (poll-socket remote-client 'data-available-p 0))

	     (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
	       (dotimes (i (length buffer))
		 (setf (elt buffer i) i))
	       (assert-error 'error (write-to-stream client buffer :start -1))
	       (assert-error 'error (write-to-stream client buffer :end -1))
	       (assert-error 'error (write-to-stream client buffer :start 3 :end 1))
	       (assert-error 'error (write-to-stream client buffer :start 3 :end 11))

	       (assert-equal 2 (write-to-stream client buffer :start 3 :end 5))
	       (assert-true (poll-socket remote-client 'data-available-p 10))
	       
	       (assert-error 'error (read-from-stream remote-client buffer :start -1))
	       (assert-error 'error (read-from-stream remote-client buffer :end -1))
	       (assert-error 'error (read-from-stream remote-client buffer :start 3 :end 1))
	       (assert-error 'error (read-from-stream remote-client buffer :start 3 :end 11))

	       (assert-equal 2 (read-from-stream remote-client buffer :start 7 :end 10))
	       (dotimes (i 7)
		 (assert-equal i (elt buffer i)))
	       (assert-equal 3 (elt buffer 7))
	       (assert-equal 4 (elt buffer 8))
	       (assert-equal 9 (elt buffer 9))

	       (assert-false (data-available-p remote-client))
	       (assert-true (ready-to-write-p client))))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))
	       (assert-true (poll-socket remote-client 'connection-succeeded-p 10))	       
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test local-test/remote-disconnected
  (labels ((channel-test (client remote-client)
	     (assert-true (poll-socket client 'ready-to-write-p 0))
	     (assert-false (poll-socket remote-client 'data-available-p 0))

	     (close-socket remote-client)
	     (assert-error 'error (poll-socket remote-client 'ready-to-write-p 10))

	     (assert-false (poll-socket client 'connection-succeeded-p 10))
	     (assert-true (poll-socket client 'remote-disconnected-p 10))
	     (assert-false (poll-socket client 'ready-to-write-p 0))
	     (assert-false (poll-socket client 'data-available-p 0)))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))
	       (assert-true (poll-socket remote-client 'connection-succeeded-p 10))	       

	       (channel-test client remote-client))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test connect-to-local-server/does-not-exist  
  (assert-error 'no-local-server-error (connect-to-local-server (local-socket-pathname))))

(define-test local-test/pathname
  (labels ((establish-channel (server client)
	     (assert-true (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))

	       (assert-true (pathname-match-p (local-pathname server)
					      (local-pathname client)))
	       (assert-true (pathname-match-p (local-pathname server)
					      (local-pathname remote-client))))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))
