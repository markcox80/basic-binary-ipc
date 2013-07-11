(in-package "BASIC-BINARY-IPC")

(include "sys/types.h")
(include "sys/event.h")
(include "sys/time.h")

#+freebsd
(progn
  (ctype uintptr-t "uintptr_t")
  (ctype intptr-t "intptr_t")
  (ctype u-short "u_short")
  (ctype u-int "u_int")

  (cstruct kevent "struct kevent"
    (ident "ident" :type uintptr-t)
    (filter "filter" :type :short)
    (flags "flags" :type u-short)
    (fflags "fflags" :type u-int)
    (data "data" :type intptr-t)
    (udata "udata" :type :pointer)))

#+darwin
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

(constantenum (kevent-flags :base-type
			    #+darwin :uint16
			    #+freebsd u-short)
  ((:ev-add "EV_ADD"))
  ((:ev-enable "EV_ENABLE"))
  ((:ev-disable "EV_DISABLE"))
  ((:ev-delete "EV_DELETE"))
  ((:ev-clear "EV_CLEAR"))
  ((:ev-eof "EV_EOF"))
  ((:ev-error "EV_ERROR")))

(constantenum (kevent-filters :base-type
			      #+darwin :int16
			      #+freebsd :short)
  ((:evfilt-read "EVFILT_READ"))
  ((:evfilt-write "EVFILT_WRITE")))
