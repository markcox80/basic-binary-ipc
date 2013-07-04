(in-package "BASIC-BINARY-IPC")

(define-system-call (%ff-kqueue "kqueue") :int)
(define-system-call (%ff-kevent64 "kevent64") :int
  (kq :int)
  (change-list (:pointer (:struct kevent64-s)))
  (number-of-changes :int)
  (event-list (:pointer (:struct kevent64-s)))
  (number-of-events :int)
  (flags :unsigned-int)
  (timeout (:pointer (:struct timespec))))
