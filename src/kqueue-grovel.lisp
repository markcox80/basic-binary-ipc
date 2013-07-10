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
  (tv-nsec "tv_nsec" :type :long))

(constantenum (kevent-flags :base-type :uint16)
  ((:ev-add "EV_ADD"))
  ((:ev-enable "EV_ENABLE"))
  ((:ev-disable "EV_DISABLE"))
  ((:ev-delete "EV_DELETE"))
  ((:ev-clear "EV_CLEAR"))
  ((:ev-eof "EV_EOF"))
  ((:ev-error "EV_ERROR")))

(constantenum (kevent-filters :base-type :int16)
  ((:evfilt-read "EVFILT_READ"))
  ((:evfilt-write "EVFILT_WRITE")))
