(in-package "BASIC-BINARY-PACKET.IPC")

(cffi:defctype posix-socket-protocol :int)

(define-system-call (%ff-socket "socket") :int
  (domain   posix-socket-namespace)
  (type     posix-socket-type)
  (protocol posix-socket-protocol))

(define-system-call (%ff-close "close") :int
  (file-descriptor :int))

(define-system-call (%ff-bind "bind") :int
  (socket :int)
  (socket-address :pointer)
  (socket-address-length socklen-t))

(define-system-call (%ff-listen "listen") :int
  (socket :int)
  (backlog :int))

(cffi:defcfun (%ff-inet-aton "inet_aton") :int
  (name :string)
  (addr (:pointer (:struct in-addr))))

(cffi:defcfun (%ff-htons "htons") :uint16
  (host-short :uint16))

(cffi:defcfun (%ff-ntohl "ntohl") :uint32
  (network-long :uint32))

;; This is a potential source of problems.
;; The prototype for inet_ntoa is
;; char *inet_ntoa(struct in_addr addr)
(cffi:defcfun (%ff-inet-ntoa "inet_ntoa") :string
  (addr :uint32))

;; This should at least check if the hack with %ff-inet-ntoa (above)
;; works.
(assert (= (cffi:foreign-type-size :uint32)
	   (cffi:foreign-type-size '(:struct in-addr))))

(define-system-call (%ff-fcntl-noarg "fcntl") :int
  (file-descriptor :int)
  (command fcntl-command))

(define-system-call (%ff-fcntl-setfl "fcntl") :int
  (file-descriptor :int)
  (command fcntl-command)
  (mode operating-mode))

