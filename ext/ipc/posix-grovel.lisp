(in-package "BASIC-BINARY-PACKET.IPC")

(include "errno.h")

(constantenum (errno-enum :base-type :int)
  ((:ebadf "EBADF"))
  ((:eintr "EINTR"))
  ((:eio   "EIO"))
  ((:eacces "EACCES"))
  ((:eafnosupport "EAFNOSUPPORT"))
  ((:eisconn "EISCONN"))
  ((:emfile "EMFILE"))
  ((:enfile "ENFILE"))
  ((:enobufs "ENOBUFS"))
  ((:enomem "ENOMEM"))
  ((:eprotonosupport "EPROTONOSUPPORT"))
  ((:eprototype "EPROTOTYPE")))

(include "sys/socket.h")

(constantenum (posix-socket-namespace :base-type :int)
  ((:pf-inet "PF_INET")))

(constantenum (posix-socket-type :base-type :int)
  ((:sock-stream "SOCK_STREAM"))
  ((:sock-dgram  "SOCK_DGRAM")))
