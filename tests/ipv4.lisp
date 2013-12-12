(in-package "BASIC-BINARY-IPC.TESTS")

(defvar *used-server-ports* nil
  "A list of all server ports returned by RANDOM-SERVER-PORT.")

(defvar +ipv4-address-with-no-server+ "169.254.0.1")

(defun random-server-port ()
  (let ((port (loop
		 :for port := (+ 30000 (random 10000))
		 :while (find port *used-server-ports* :test #'=)
		 :finally (return port))))
    (push port *used-server-ports*)
    port))

(define-test make-ipv4-tcp-server
  (:tag :ipv4-tcp-socket)
  (let* ((port   (random-server-port))
	 (server (make-ipv4-tcp-server +ipv4-loopback+ port)))
    (unwind-protect
	 (progn
	   (assert-true server)
	   (assert-error 'no-connection-available-error (accept-connection server))
	   (assert-error 'socket-error (make-ipv4-tcp-server +ipv4-loopback+ port)))
      (close-socket server))))

(define-test ipv4-tcp-test/sockets
  (:tag :ipv4-tcp-socket)
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
	       (assert-true (typep server 'stream-server))
	       (assert-true (typep client 'stream-socket))
	       (assert-true (typep remote-client 'stream-socket))
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-ipv4-tcp-server +ipv4-loopback+ (random-server-port))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (port server))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test ipv4-tcp-test/stream
  (:tag :ipv4-tcp-socket)
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
    (let ((server (make-ipv4-tcp-server +ipv4-loopback+ (random-server-port))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (port server))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test ipv4-tcp-test/remote-disconnected
  (:tag :ipv4-tcp-socket)
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
    (let ((server (make-ipv4-tcp-server +ipv4-loopback+ (random-server-port))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (port server))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test connect-to-ipv4-server/does-not-exist
  (:tag :ipv4-tcp-socket)
  (labels ((perform-test (client)
	     (format *standard-output* "~&; This test pauses for a maximum of 2 minutes, do not panic.~%")
	     (let ((results (poll-socket client '(determinedp connection-failed-p connection-succeeded-p) 120)))
	       (assert-equal 2 (length results))
	       (assert-true (find 'determinedp results))
	       (assert-true (find 'connection-failed-p results))
	       (assert-false (find 'connection-succeeded-p results)))
	     (assert-true (connection-failed-p client))))
    (let ((client (connect-to-ipv4-tcp-server +ipv4-address-with-no-server+ (random-server-port))))
      (unwind-protect
	   (perform-test client)
	(close-socket client)))))

(define-test connect-to-ipv4-server/does-not-exist/loopback
  (:tag :ipv4-tcp-socket)
  (labels ((perform-test (client)
	     (assert-true (typep client 'stream-socket))
	     (format *standard-output* "~&; This test pauses for a maximum of 2 minutes, do not panic.~%")
	     (let ((results (poll-socket client '(determinedp connection-failed-p connection-succeeded-p) 120)))
	       (assert-equal 2 (length results))
	       (assert-true (find 'determinedp results))
	       (assert-true (find 'connection-failed-p results))
	       (assert-false (find 'connection-succeeded-p results)))
	     (assert-true (connection-failed-p client))))
    (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (random-server-port))))
      (unwind-protect
	   (perform-test client)
	(close-socket client)))))

(define-test ipv4-tcp-test/host-address-and-ports
  (:tag :ipv4-tcp-socket)
  (let ((client-port (random-server-port))
	(server-port (random-server-port)))
    (labels ((establish-channel (server client)
	       (assert-true (poll-socket server 'connection-available-p 10))
	       (let ((remote-client (accept-connection server)))
		 (assert-true (poll-socket remote-client 'connection-succeeded-p 10))
		 (assert-true (poll-socket client 'connection-succeeded-p 10))
		 
		 ;; remote client tests
		 (assert-equal server-port      (local-port remote-client))
		 (assert-equal +ipv4-loopback+  (local-host-address remote-client))
		 (assert-equal client-port      (remote-port remote-client))
		 (assert-equal +ipv4-loopback+  (remote-host-address remote-client))

		 ;; client tests
		 (assert-equal client-port      (local-port client))
		 (assert-equal +ipv4-loopback+  (local-host-address client))
		 (assert-equal server-port      (remote-port client))
		 (assert-equal +ipv4-loopback+  (remote-host-address client)))))
      (let ((server (make-ipv4-tcp-server +ipv4-loopback+ server-port)))
	(assert-false (poll-socket server 'connection-available-p 0))
	(unwind-protect
	     (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ (port server) :local-port client-port)))
	       (unwind-protect
		    (establish-channel server client)
		 (close-socket client)))
	  (close-socket server))))))
