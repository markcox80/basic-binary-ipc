(in-package "BASIC-BINARY-IPC")

(define-system-call (%ff-kqueue "kqueue") :int)

#+freebsd
(define-system-call (%ff-kevent "kevent") :int
  (kq :int)
  (change-list (:pointer (:struct kevent)))
  (number-of-changes :int)
  (event-list (:pointer (:struct kevent)))
  (number-of-events :int)
  (timeout (:pointer (:struct timespec))))

#+darwin
(define-system-call (%ff-kevent64 "kevent64") :int
  (kq :int)
  (change-list (:pointer (:struct kevent64-s)))
  (number-of-changes :int)
  (event-list (:pointer (:struct kevent64-s)))
  (number-of-events :int)
  (flags :unsigned-int)
  (timeout (:pointer (:struct timespec))))

(defun kevent-wrapper (kq change-list number-of-changes event-list number-of-events timeout)
  #+darwin
  (%ff-kevent64 kq change-list number-of-changes event-list number-of-events 0 timeout)
  #+freebsd
  (%ff-kevent kq change-list number-of-changes event-list number-of-events timeout))
