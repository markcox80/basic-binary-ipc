(in-package "BASIC-BINARY-IPC.TESTS")

(define-test poller/ipv4
  (let ((port (random-server-port)))
    (labels ((transmit-data (poller client remote-client)
	       (monitor-socket poller client '(connection-succeeded-p data-available-p ready-to-write-p))
	       (monitor-socket poller remote-client '(connection-succeeded-p data-available-p ready-to-write-p))
	       (let ((events (wait-for-events poller 10)))
		 (assert-equal '(connection-succeeded-p ready-to-write-p) (second (find client events :key #'first)))
		 (assert-equal '(connection-succeeded-p ready-to-write-p) (second (find remote-client events :key #'first))))

	       (setf (monitored-events poller client) '(data-available-p)
		     (monitored-events poller remote-client) '(data-available-p))

	       (let ((buffer (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 4))))
		 (write-to-stream client buffer))

	       (let ((events (wait-for-events poller 2)))
		 (assert-equal 1 (length events))
		 (assert-equal '(data-available-p) (second (find remote-client events :key #'first))))
	       
	       (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
		 (assert-equal 5 (read-from-stream remote-client buffer)))

	       (assert-equal nil (wait-for-events poller :immediate)))

	     (use-client (poller tcp-server  client)
	       (let* ((events (wait-for-events poller 2))
		      (v (find tcp-server events :key #'first)))
		 (assert-true (second v))
		 (let ((remote-client (accept-connection tcp-server)))
		   (unwind-protect
			(transmit-data poller client remote-client)
		     (close-socket remote-client))))

	       (setf (monitored-events poller client) '(connection-failed-p connection-succeeded-p remote-disconnected-p))
	       (let* ((events (wait-for-events poller 2))
		      (v (find client events :key #'first)))
		 (assert-true (find 'remote-disconnected-p (second v)))
		 (assert-true (find 'connection-failed-p (second v)))
		 (assert-false (find 'connection-succeeded-p (second v)))))

	     (create-client (poller tcp-server)
	       (assert-true (null (wait-for-events poller 0)))
	       (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ port)))
		 (unwind-protect
		      (use-client poller tcp-server  client)
		   (close-socket client))))

	     (start (tcp-server)
	       (let ((poller (make-poller)))
		 (monitor-socket poller tcp-server 'connection-available-p)
		 (unwind-protect
		      (create-client poller tcp-server)
		   (close-poller poller)))))

      (let ((tcp-server (make-ipv4-tcp-server +ipv4-loopback+ port)))	
	(unwind-protect
	     (start tcp-server)
	  (close-socket tcp-server))))))

(define-test poller/local
  (let ((path (local-socket-pathname)))
    (labels ((transmit-data (poller client remote-client)
	       (monitor-socket poller client '(connection-succeeded-p data-available-p ready-to-write-p))
	       (monitor-socket poller remote-client '(connection-succeeded-p data-available-p ready-to-write-p))
	       (let ((events (wait-for-events poller 10)))
		 (assert-equal '(connection-succeeded-p ready-to-write-p) (second (find client events :key #'first)))
		 (assert-equal '(connection-succeeded-p ready-to-write-p) (second (find remote-client events :key #'first))))

	       (setf (monitored-events poller client) '(data-available-p)
		     (monitored-events poller remote-client) '(data-available-p))

	       (let ((buffer (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3 4))))
		 (write-to-stream client buffer))

	       (let ((events (wait-for-events poller 2)))
		 (assert-equal 1 (length events))
		 (assert-equal '(data-available-p) (second (find remote-client events :key #'first))))
	       
	       (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
		 (assert-equal 5 (read-from-stream remote-client buffer)))

	       (assert-equal nil (wait-for-events poller :immediate)))

	     (use-client (poller local-server  client)
	       (let* ((events (wait-for-events poller 2))
		      (v (find local-server events :key #'first)))
		 (assert-true (second v))
		 (let ((remote-client (accept-connection local-server)))
		   (unwind-protect
			(transmit-data poller client remote-client)
		     (close-socket remote-client))))

	       (setf (monitored-events poller client) '(connection-failed-p connection-succeeded-p remote-disconnected-p))
	       (let* ((events (wait-for-events poller 2))
		      (v (find client events :key #'first)))
		 (assert-true (find 'remote-disconnected-p (second v)))
		 (assert-true (find 'connection-failed-p (second v)))
		 (assert-false (find 'connection-succeeded-p (second v)))))

	     (create-client (poller local-server)
	       (assert-true (null (wait-for-events poller 0)))
	       (let ((client (connect-to-local-server path)))
		 (unwind-protect
		      (use-client poller local-server  client)
		   (close-socket client))))

	     (start (local-server)
	       (let ((poller (make-poller)))
		 (monitor-socket poller local-server 'connection-available-p)
		 (unwind-protect
		      (create-client poller local-server)
		   (close-poller poller)))))

      (let ((local-server (make-local-server path)))	
	(unwind-protect
	     (start local-server)
	  (close-socket local-server))))))

(define-test poller/no-server
  (labels ((run-test (poller client)
	     (format *standard-output* "~&; This test pauses for a maximum of 2 minutes, do not panic.~%")
	     (let ((events (wait-for-events poller 120)))
	       (assert-true events)
	       (destructuring-bind (&optional socket matched-events) (first events)
		 (assert-equal client socket)
		 (assert-true (find 'determinedp matched-events))
		 (assert-false (find 'connection-succeeded-p matched-events))
		 (assert-true  (find 'connection-failed-p matched-events))
		 
		 (assert-false (connection-succeeded-p client))
		 (assert-true (connection-failed-p client))))))
    (let ((client (connect-to-ipv4-tcp-server +ipv4-address-with-no-server+ (random-server-port)))
	  (poller (make-poller)))
      (monitor-socket poller client '(determinedp connection-succeeded-p connection-failed-p))
      (unwind-protect
	   (run-test poller client)
	(close-socket client)
	(close-poller poller)))))

#-freebsd
(define-test poller/no-server/loopback
  (labels ((run-test (poller client)
	     (format *standard-output* "~&; This test pauses for a maximum of 2 minutes, do not panic.~%")
	     (let ((events (wait-for-events poller 120)))
	       (assert-equal 1 (length events))
	       (destructuring-bind (&optional socket matched-events) (first events)
		 (assert-equal client socket)
		 (assert-true (find 'determinedp matched-events))
		 (assert-false (find 'connection-succeeded-p matched-events))
		 (assert-true  (find 'connection-failed-p matched-events))
		 
		 (assert-false (connection-succeeded-p client))
		 (assert-true (connection-failed-p client))))))
    (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (random-server-port)))
	  (poller (make-poller)))
      (unwind-protect
	   (progn
	     (monitor-socket poller client '(determinedp connection-succeeded-p connection-failed-p))
	     (run-test poller client))
	(close-socket client)
	(close-poller poller)))))

#+freebsd
(define-test poller/no-server/loopback
  (warn "POLLER/NO-SERVER-LOOPBACK on FreeBSD is different to other
  hosts. For some reasonEINVAL is signalled during MONITOR-SOCKET when
  it shouldn't be. I need to investigate this further, but for now, I
  just test for the presence of the bug.")
  (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (random-server-port)))
	(poller (make-poller)))    
    (unwind-protect
	 (assert-error 'error (monitor-socket poller client '(determinedp connection-succeeded-p connection-failed-p)))
      (close-socket client)
      (close-poller poller))))
