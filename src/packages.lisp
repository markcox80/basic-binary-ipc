(defpackage "BASIC-BINARY-PACKET"
  (:use "COMMON-LISP")
  (:export #:write-packet
	   #:create-payload
	   #:encode-object
	   #:decode-object))
