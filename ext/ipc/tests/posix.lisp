(in-package "BASIC-BINARY-PACKET.IPC.TESTS")

(define-test define-system-call
  (let ((fd (basic-binary-packet.ipc::%ff-socket :pf-inet :sock-stream 0)))
    (assert-true (plusp fd))
    (assert-true (zerop (basic-binary-packet.ipc::%ff-close fd)))
    (assert-error 'posix-error (basic-binary-packet.ipc::%ff-close fd))))

(define-test make-ipv4-tcp-server
  (let ((server (make-ipv4-tcp-server +ipv4-loopback+ 4545)))
    (unwind-protect
	 (progn
	   (assert-true server)
	   (assert-error 'posix-error (make-ipv4-tcp-server +ipv4-loopback+ 4545)))
      (close-socket server))))

(define-test poll-fd-event-test
  (labels ((true (expression revents)
	     (assert-true (basic-binary-packet.ipc::poll-fd-event-test expression revents)))
	   (false (expression revents)
	     (assert-false (basic-binary-packet.ipc::poll-fd-event-test expression revents))))
    (true 'pollin '(pollin))
    (false 'pollhup '(pollin))
    (true '(or pollin pollhup) '(pollin))
    (true '(or pollin pollhup) '(pollhup))
    (false '(or pollin pollhup) '())
    (true '(or pollin (not pollhup)) '())
    (false '(and pollin (not pollhup)) '())
    (true  '(and pollin (not pollhup)) '(pollin))
    (false  '(and pollin (not pollhup)) '(pollin pollhup))

    (true '(or pollin pollhup) '(pollin))
    (true '(not (and (not pollin) (not pollhup))) '(pollin))
    (true '(or pollin pollhup) '(pollhup))
    (true '(not (and (not pollin) (not pollhup))) '(pollhup))))

(define-test ipv4-tcp-test
  (labels ((channel-test (client remote-client)
	     (let ((results (poll-sockets (list client remote-client) '(determinedp determinedp) 10)))
	       (assert-equal '(determinedp determinedp) results)
	       (assert-false (connection-failed-p client))
	       (assert-false (connection-failed-p remote-client))))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-ipv4-tcp-server +ipv4-loopback+ 4455)))
      (unwind-protect
	   (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ 4455)))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test connect-to-ipv4-server/does-not-exist
  (labels ((perform-test (client)
	     (let ((results (poll-socket client '(determinedp connection-failed-p) 10)))
	       (assert-equal 2 (length results))
	       (assert-true (find 'determinedp results))
	       (assert-true (find 'connection-failed-p results)))
	     (assert-true (connection-failed-p client))))
    (let ((client (connect-to-ipv4-tcp-server +ipv4-loopback+ 5566)))
      (unwind-protect
	   (perform-test client)
	(close-socket client)))))
