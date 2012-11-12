(defpackage "BASIC-BINARY-PACKET"
  (:use "COMMON-LISP"
	"COM.GIGAMONKEYS.BINARY-DATA")
  (:export #:write-packet
	   #:create-payload
	   #:encode-object
	   #:decode-object))
