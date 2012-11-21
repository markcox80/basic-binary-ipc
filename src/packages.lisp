(defpackage "BASIC-BINARY-PACKET"
  (:use "COMMON-LISP"
	"COM.GIGAMONKEYS.BINARY-DATA")
  (:export #:write-packet-for-payload
	   #:write-object
	   #:make-packet-reader-function
	   #:encode-object
	   #:decode-object)

  ;; Extensibility
  (:export #:binary-type-for-object)
  
  ;; Functions for reading and writing values
  (:export #:read-value
	   #:write-value
	   #:define-binary-type)

  ;; special binary types
  (:export #:binary-type
	   #:binary-object)

  ;; binary types
  (:export #:binary-uint8
	   #:binary-uint16
	   #:binary-uint32
	   #:binary-uint64
	   #:binary-int8
	   #:binary-int16
	   #:binary-int32
	   #:binary-int64
	   #:binary-boolean
	   #:binary-single-float
	   #:binary-double-float
	   #:binary-utf8-string
	   #:binary-keyword
	   #:binary-symbol
	   #:binary-nil
	   #:binary-t
	   #:binary-cons
	   #:binary-list-generic
	   #:binary-list-fixed))
