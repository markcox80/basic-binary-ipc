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
  ((:eprototype "EPROTOTYPE"))
  ((:eaddrinuse "EADDRINUSE"))
  ((:eaddrnotavail "EADDRNOTAVAIL"))
  ((:edestaddrreq "EDESTADDRREQ"))
  ((:efault "EFAULT"))
  ((:einval "EINVAL"))
  ((:enotsock "ENOTSOCK"))
  ((:eopnotsupp "EOPNOTSUPP")))

(include "fcntl.h")

(constantenum (fcntl-command :base-type :int)
  ((:f-getfl "F_GETFL"))
  ((:f-setfl "F_SETFL")))

(bitfield (operating-mode :base-type :int)
  ((o-nonblock "O_NONBLOCK")))

(include "sys/socket.h")

(constantenum (posix-socket-namespace :base-type :int)
  ((:pf-inet "PF_INET")))

(constantenum (posix-socket-type :base-type :int)
  ((:sock-stream "SOCK_STREAM"))
  ((:sock-dgram  "SOCK_DGRAM")))

(constantenum (socket-option :base-type :int)
  ((:so-reuseaddr "SO_REUSEADDR"))
  ((:so-keepalive "SO_KEEPALIVE")))

(ctype socklen-t "socklen_t")

(include "netinet/in.h")

(ctype sa-family-t "sa_family_t")
(constantenum (posix-socket-address-family :base-type sa-family-t)
  ((:af-inet "AF_INET")))

(cstruct in-addr "struct in_addr"
  (s-addr "s_addr" :type :uint32))

(cstruct sockaddr-in "struct sockaddr_in"
  (sin-family "sin_family" :type posix-socket-address-family)
  (sin-addr   "sin_addr"   :type (:struct in-addr))
  (sin-port   "sin_port"   :type :unsigned-short))

(constant (inaddr-loopback  "INADDR_LOOPBACK"))
(constant (inaddr-any       "INADDR_ANY"))
(constant (inaddr-broadcast "INADDR_BROADCAST"))
(constant (inaddr-none      "INADDR_NONE"))

; poll
(include "poll.h")

(ctype nfds-t "nfds_t")

(cstruct pollfd "struct pollfd"
  (fd "fd" :type :int)
  (events "events" :type :short)
  (revents "revents" :type :short))

(bitfield (poll-events :base-type :short)
  ((pollerr    "POLLERR"))
  ((pollhup    "POLLHUP"))
  ((pollin     "POLLIN"))
  ((pollnval   "POLLNVAL"))
  ((pollout    "POLLOUT"))
  ((pollpri    "POLLPRI"))
  ((pollrdband "POLLRDBAND"))
  ((pollrdnorm "POLLRDNORM"))
  ((pollwrband "POLLWRBAND"))
  ((pollwrnorm "POLLWRNORM")))
