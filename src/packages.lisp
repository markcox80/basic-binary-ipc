(defpackage "BASIC-BINARY-IPC"
  (:use "COMMON-LISP")
   
  ;; Errors
  (:export #:posix-error)

  ;; Sockets
  (:export #:socket
	   #:close-socket
	   #:socket-closed-p)

  ;; Stream servers
  (:export #:stream-server
	   #:accept-connection
	   #:connection-available-p
	   #:no-connection-available-error
	   #:socket)

  ;; Streams
  (:export #:stream-socket
	   
	   ;; - connecting state
	   #:determinedp
	   #:connection-failed-p
	   #:connection-succeeded-p
	   
	   ;; - connected state
	   #:remote-disconnected-p
	   #:ready-to-write-p
	   #:data-available-p

	   #:read-from-stream
	   #:write-to-stream

	   #:would-block-error)

  ;; Polling
  (:export #:poll-socket
	   #:poll-sockets)

  ;; Pollers
  (:export #:make-poller
	   #:wait-for-events
	   #:monitor-socket
	   #:unmonitor-socket
	   #:monitored-events
	   #:monitored-sockets
	   #:close-poller)

  ;; IPv4
  (:export #:ipv4-tcp-server
	   #:make-ipv4-tcp-server

	   #:ipv4-tcp-stream
	   #:connect-to-ipv4-tcp-server
	   #:host-address
	   #:port

	   #:+ipv4-loopback+
	   #:+ipv4-any+

	   #:local-host-address
	   #:local-port
	   #:remote-host-address
	   #:remote-port)

  ;; Local Sockets
  (:export #:local-server
	   #:make-local-server

	   #:local-stream
	   #:connect-to-local-server
	   #:no-local-server-error
	   
	   #:local-pathname))
