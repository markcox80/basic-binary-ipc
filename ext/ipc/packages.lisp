(defpackage "BASIC-BINARY-PACKET.IPC"
  (:use "COMMON-LISP")
   
  ;; Sockets
  (:export #:close-socket)

  ;; Stream servers
  (:export #:accept-connection
	   #:connection-available-p)

  ;; Future connections
  (:export #:determinedp
	   #:connection-stream)

  ;; Streams
  (:export #:remote-disconnected-p
	   #:ready-to-write-p
	   #:data-available-p

	   #:read-from-stream
	   #:write-to-stream)

  ;; Polling
  (:export #:poll-socket
	   #:poll-sockets)

  ;; IPv4
  (:export #:make-ipv4-tcp-server
	   #:connect-to-ipv4-tcp-server))
