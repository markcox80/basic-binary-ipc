(in-package "BASIC-BINARY-PACKET.IPC.TESTS")

(defvar *used-server-ports* nil
  "A list of all server ports returned by RANDOM-SERVER-PORT.")

(defun random-server-port ()
  (let ((port (loop
		 :for port := (+ 30000 (random 10000))
		 :while (find port *used-server-ports* :test #'=)
		 :finally (return port))))
    (push port *used-server-ports*)
    port))

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
