(defpackage "BASIC-BINARY-PACKET.NETWORK"
  (:use "COMMON-LISP"
	"IOLIB")
  (:export #:process-events)

  ;; The stream protocol
  (:export #:write-object
	   #:on-object
	   #:force-output
	   #:connectedp
	   #:on-connection
	   #:close
	   #:on-error

	   ;; errors
	   #:end-of-file)

  ;; The server protocol
  (:export #:make-server

	   #:on-new-connection
	   #:on-error
	   #:close)

  ;; The client protocol
  (:export #:make-client)

  
  ;; Advanced API
  (:export #:*event-base*
	   #:event-base
	   #:socket

	   #:client
	   #:server))
