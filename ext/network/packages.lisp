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
	   #:end-of-file))
