(in-package "BASIC-BINARY-PACKET.IPC.TESTS")

(define-test define-system-call
  (let ((fd (basic-binary-packet.ipc::%ff-socket :pf-inet :sock-stream 0)))
    (assert-true (plusp fd))
    (assert-true (zerop (basic-binary-packet.ipc::%ff-close fd)))
    (assert-error 'posix-error (basic-binary-packet.ipc::%ff-close fd))))

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
