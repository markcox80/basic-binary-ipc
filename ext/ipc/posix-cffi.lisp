(in-package "BASIC-BINARY-PACKET.IPC")

(cffi:defctype posix-socket-protocol :int)

(define-system-call (%ff-socket "socket") :int
  (domain   posix-socket-namespace)
  (type     posix-socket-type)
  (protocol posix-socket-protocol))

(define-system-call (%ff-close "close") :int
  (file-descriptor :int))

