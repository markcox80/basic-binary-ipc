(in-package "BASIC-BINARY-IPC")

(include "sys/types.h")
(include "sys/event.h")
(include "sys/time.h")

(cstruct kevent64-s "struct kevent64_s"
  (ident "ident" :type :uint64)
  (filter "filter" :type :int16)
  (flags "flags" :type :uint16)
  (fflags "fflags" :type :uint32)
  (data "data" :type :int64)
  (udata "udata" :type :uint64)
  (ext "ext" :type :uint64 :count 2))

(ctype time-t "time_t")

(cstruct timespec "struct timespec"
  (tv-sec "tv_sec" :type time-t)
  (tv-usec "tv_nsec" :type :long))
