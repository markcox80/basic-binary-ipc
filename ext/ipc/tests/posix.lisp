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
