(in-package "BASIC-BINARY-IPC.TESTS")

(define-test poll-sockets
  (:tag :poll-sockets)
  (let ((port (random-server-port)))
    (labels ((wait-for-clients (tcp-server local-server)
	       (let ((sockets (list tcp-server local-server))
		     (events  '(connection-available-p connection-available-p)))
		 (assert-equal '(nil nil) (poll-sockets sockets events 0))
		 (let ((tcp-client (connect-to-ipv4-tcp-server +ipv4-loopback+ port)))
		   (unwind-protect
			(progn
			  (assert-equal '(connection-available-p nil) (poll-sockets sockets events 1))
			  (let ((local-client (connect-to-local-server (local-socket-pathname))))
			    (unwind-protect
				 (progn
				   (assert-equal events (poll-sockets sockets events 1))
				   (close-socket (accept-connection tcp-server))
				   (assert-equal '(nil connection-available-p) (poll-sockets sockets events 1))
				   (close-socket (accept-connection local-server))
				   (assert-equal '(nil nil) (poll-sockets sockets events 0)))
			      (close-socket local-client))))
		     (close-socket tcp-client))))))
      (let ((tcp-server (make-ipv4-tcp-server +ipv4-loopback+ port)))
	(unwind-protect
	     (let ((local-server (make-local-server (local-socket-pathname))))
	       (unwind-protect
		    (wait-for-clients tcp-server local-server)
		 (close-socket local-server)))
	  (close-socket tcp-server))))))

#+(and thread-support (not windows))
(define-test poll-sockets/interrupt
  (let ((finished-properly nil))
    (labels ((start-thread ()	     
	       (let ((port (random-server-port)))
		 (bordeaux-threads:make-thread #'(lambda ()
						   (with-socket (s (make-ipv4-tcp-server +ipv4-loopback+ port))
						     (poll-socket s 'connection-available-p 60)
						     (setf finished-properly t)))))))
      (let ((thread (start-thread)))
	(sleep 5)
	(assert-true (bordeaux-threads:thread-alive-p thread))
	(bordeaux-threads:interrupt-thread thread #'(lambda ()
						      nil))	
	(ignore-errors (bordeaux-threads:join-thread thread))
	(assert-true finished-properly)))))
