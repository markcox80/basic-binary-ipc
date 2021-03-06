(defpackage "BASIC-BINARY-IPC.OVERLAPPED-IO"
  (:use "COMMON-LISP")
  (:import-from "BASIC-BINARY-IPC"
		#:system-function-error
		#:define-check-system-call
		#:define-system-call)
  
  ;; Requests
  (:export #:request
	   #:descriptor
	   #:free-request
	   #:invalidp
	   #:waitingp
	   #:completedp
	   #:succeededp
	   #:failedp
	   #:reset-event
	   #:set-event

	   #:with-request
	   #:do-with-request)

  ;; Synchronising
  (:export #:wait-for-request
	   #:wait-for-requests

	   #:+infinite+)

  ;; Monitor Synchronising
  (:export #:monitor
	   #:unmonitor
	   #:pop-notification
	   #:free-monitor
	   #:do-with-monitor
	   #:with-monitor)

  ;; Generic handle stuff
  (:export #:close-handle
	   #:with-handle
	   #:do-with-handle
	   #:cancel-all-io)

  ;; Named Pipes
  (:export #:connect-to-named-pipe

	   #:valid-pipe-name-p
	   #:canonical-windows-pipe-name
	   
	   #:valid-named-pipe-handle-p

	   ;; Servers
	   #:make-named-pipe-server
	   #:connect-named-pipe

	   #:buffer
	   #:buffer-length

	   #:read-file
	   #:read-file-request
	   #:bytes-read

	   #:write-file
	   #:write-file-request
	   #:bytes-written)

  ;; Sockets
  (:export #:make-socket
	   #:close-socket)

  ;; IPv4 Sockets
  (:export #:+inaddr-none+
	   #:+inaddr-any+
	   #:+inaddr-loopback+
	   
	   #:with-sockaddr-in
	   #:do-with-sockaddr-in
	   
	   #:make-ipv4-server
	   
	   #:accept-ipv4-request
	   #:local-address
	   #:local-port
	   #:remote-address
	   #:remote-port
	   #:client-descriptor
	   #:buffer
	   #:buffer-length
	   #:bytes-read

	   #:minimum-accept-ipv4-buffer-size
	   #:accept-ipv4
	   
	   #:connect-ipv4-request
	   #:connect-ipv4

	   #:resolve-ipv4-address))
