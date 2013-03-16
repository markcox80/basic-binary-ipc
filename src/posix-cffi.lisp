(in-package "BASIC-BINARY-IPC")

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

(cffi:defcfun (%ff-ntohs "ntohs") :uint16
  (network-short :uint16))

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

;; FCNTL
(define-system-call (%ff-fcntl-noarg "fcntl") :int
  (file-descriptor :int)
  (command fcntl-command))

(define-system-call (%ff-fcntl-setfl "fcntl") :int
  (file-descriptor :int)
  (command fcntl-command)
  (mode operating-mode))


;; Socket options
(define-system-call (%ff-getsockopt "getsockopt") :int
  (socket :int)
  (level :int)
  (option-name socket-option)
  (option-value :pointer)
  (option-length (:pointer socklen-t)))

(define-system-call (%ff-setsockopt "setsockopt") :int
  (socket :int)
  (level :int)
  (option-name socket-option)
  (option-value :pointer)
  (option-length socklen-t))


;; connecting
(define-system-call (%ff-connect "connect") :int
  (socket :int)
  (address :pointer)
  (address-len socklen-t))

(define-system-call (%ff-getsockname "getsockname") :int
  (socket :int)
  (address :pointer)
  (address-length (:pointer socklen-t)))

;; accept
(define-system-call (%ff-accept "accept") :int
  (socket :int)
  (address :pointer)
  (address-len (:pointer socklen-t)))

;; poll
(define-system-call (%ff-poll "poll") :int
  (fds (:pointer (:struct pollfd)))
  (nfds nfds-t)
  (timeout :int))

;; reading
(define-system-call (%ff-recvfrom "recvfrom") :int
  (socket :int)
  (buffer :pointer)
  (length size-t)
  (flags message-flags)
  (address :pointer)
  (address-len (:pointer socklen-t)))

;; writing
(define-system-call (%ff-sendto "sendto") :int
  (socket :int)
  (buffer :pointer)
  (length size-t)
  (flags message-flags)
  (dest-addr :pointer)
  (dest-len socklen-t))
