(defpackage "BASIC-BINARY-PACKET.IPC"
  (:use "COMMON-LISP")
   
  ;; Sockets
  (:export #:close-socket)

  ;; Stream servers
  (:export #:accept-connection
	   #:connection-available-p)

  ;; Future connections
  (:export #:determinedp
	   #:connection-failed-p
	   #:connection-succeeded-p
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
	   #:connect-to-ipv4-tcp-server
	   #:host-address
	   #:port

	   #:+ipv4-loopback+
	   #:+ipv4-any+

	   #:local-host-address
	   #:local-port
	   #:remote-host-address
	   #:remote-port))
