(in-package "BASIC-BINARY-IPC")

(include "sys/epoll.h")

(cunion epoll-data "union epoll_data"
  (ptr "ptr" :type :pointer)
  (fd "fd" :type :int)
  (u32 "u32" :type :uint32)
  (u64 "u64" :type :uint64))

(constantenum (epoll-operation :base-type :int)
  ((:epoll-ctl-add "EPOLL_CTL_ADD"))
  ((:epoll-ctl-mod "EPOLL_CTL_MOD"))
  ((:epoll-ctl-del "EPOLL_CTL_DEL")))

(bitfield (epoll-events :base-type :uint32)
  ((:epollin "EPOLLIN"))
  ((:epollout "EPOLLOUT"))
  ((:epollrdhup "EPOLLRDHUP"))
  ((:epollerr "EPOLLERR"))
  ((:epollhup "EPOLLHUP")))

(cstruct epoll-event "struct epoll_event"
  (events "events" :type epoll-events)
  (data "data" :type (:union epoll-data)))