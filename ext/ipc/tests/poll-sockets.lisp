(in-package "BASIC-BINARY-PACKET.IPC.TESTS")

(define-test poll-sockets
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
      (let ((tcp-server (make-ipv4-tcp-server +ipv4-loopback+ port))
	    (local-server (make-local-server (local-socket-pathname))))
	(unwind-protect
	     (wait-for-clients tcp-server local-server)
	  (close-socket tcp-server)
	  (close-socket local-server))))))
