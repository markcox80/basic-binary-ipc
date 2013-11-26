(defpackage "BASIC-BINARY-IPC.OVERLAPPED-IO"
  (:use "COMMON-LISP")
  (:import-from "BASIC-BINARY-IPC"
		#:define-check-system-call
		#:define-system-call)
  
  ;; Errors
  (:export #:foreign-function-error-function-name
	   #:foreign-function-error-code
	   #:foreign-function-error-message
	   #:foreign-function-error
	   #:socket-foreign-function-error)
  
  ;; Requests
  (:export #:request
	   #:free-request
	   #:invalidp
	   #:waitingp
	   #:completedp
	   #:succeededp
	   #:failedp

	   #:with-request
	   #:do-with-request)

  ;; Synchronising
  (:export #:wait-for-request
	   #:wait-for-requests)

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
	   #:connect-ipv4))
