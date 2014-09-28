(in-package "BASIC-BINARY-IPC")

(defgeneric socket (object))

;; Socket protocol
(defclass socket ()
  ())

(defgeneric close-socket (socket))
(defgeneric socket-closed-p (socket))

(define-condition socket-error (error)
  ((socket
    :initarg :socket
    :reader socket)))

;; Stream server protocol
(defclass stream-server (socket)
  ())

(defgeneric accept-connection (server))
(defgeneric connection-available-p (server))

(define-condition no-connection-available-error (error)
  ((socket
    :initarg :socket
    :reader socket)))

;; Stream socket protocol
(defclass stream-socket (socket)
  ())

;; - Future connection protocol
(defgeneric determinedp (stream-socket))
(defgeneric connection-failed-p (stream-socket))
(defgeneric connection-succeeded-p (stream-socket))
(defgeneric connection-stream (stream-socket))

;; - Connected protocol
(defgeneric data-available-p (stream-socket))
(defgeneric ready-to-write-p (stream-socket))
(defgeneric remote-disconnected-p (stream-socket))
(defgeneric read-from-stream (stream-socket buffer &key start end))
(defgeneric write-to-stream (stream-socket buffer &key start end))

;; Polling protocol
(defgeneric poll-socket (socket socket-events timeout))
(defgeneric poll-sockets (all-sockets all-socket-events timeout))

;; IPv4 protocol
(defgeneric host-address (server))
(defgeneric port (server))
(defgeneric local-host-address (stream))
(defgeneric local-port (stream))
(defgeneric remote-host-address (stream))
(defgeneric remote-port (stream))

;; Poller protocol
(defclass poller ()
  ())

(defgeneric wait-for-events (poller timeout))
(defgeneric monitoredp (poller socket))
(defgeneric monitor-socket (poller socket socket-events))
(defgeneric unmonitor-socket (poller socket))
(defgeneric monitored-events (poller socket))
(defgeneric (setf monitored-events) (value poller socket))
(defgeneric monitored-sockets (poller))
(defgeneric close-poller (poller))

;; Helper functions
(defun do-with-socket (socket function)
  (unwind-protect
       (funcall function socket)
    (close-socket socket)))

(defmacro with-socket ((var form) &body body)
  `(do-with-socket ,form #'(lambda (,var)
			     ,@body)))

(defun do-with-poller (function poller)
  (unwind-protect
       (funcall function poller)
    (close-poller poller)))

(defmacro with-poller ((var form) &body body)
  `(do-with-poller #'(lambda (,var)
		       ,@body)
     ,form))
