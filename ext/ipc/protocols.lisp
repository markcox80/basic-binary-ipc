(in-package "BASIC-BINARY-PACKET.IPC")

;; Socket protocol
(defclass socket ()
  ())

(defgeneric close-socket (socket))

;; Stream server protocol
(defclass stream-server (socket)
  ())

(defgeneric accept-connection (server))
(defgeneric connection-available-p (server))

(defgeneric socket (object))
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
