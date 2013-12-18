(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "basic-binary-ipc"))

(defpackage "ECHO-EXAMPLE"
  (:use "COMMON-LISP"
	"BASIC-BINARY-IPC")
  (:export #:run-server
	   #:send-to-server))
(in-package "ECHO-EXAMPLE")

(defun run-server (port &optional (host-address +ipv4-loopback+))
  (check-type port (unsigned-byte 16))
  (check-type host-address string)
  
  (with-socket (server (make-ipv4-tcp-server host-address port :reuse-address t))
    (loop
       :with buffer-size := 10000
       :with buffer := (make-array buffer-size :element-type '(unsigned-byte 8))       
       :for result := (poll-socket server 'connection-available-p 10)
       :when result
       :do
       (with-socket (client (accept-connection server))
	 (loop
	    :for attempts :from 0 :below 3
	    :for data-available := (poll-socket client 'data-available-p 10)
	    :when data-available
	    :do
	    (let ((bytes-read (read-from-stream client buffer)))
	      (write-to-stream client buffer :end bytes-read)))))))

(defun send-to-server (string host-address port)
  (check-type string string)
  (check-type host-address string)
  (check-type port (unsigned-byte 16))
  
  (with-socket (client (connect-to-ipv4-tcp-server host-address port))
    (unless (poll-socket client 'connection-succeeded-p 10)
      (error "Failed to connect to address ~A:~d" host-address port))

    (write-to-stream client (babel:string-to-octets string))
    
    (loop
       :for attempts :from 0 :below 3
       :for data-available := (poll-socket client 'data-available-p 10)
       :until data-available
       :finally (unless data-available
		  (error "Echo server not working.")))
    
    (let* ((buffer (make-array 100000 :element-type '(unsigned-byte 8)))
	   (bytes-read (read-from-stream client buffer)))
      (babel:octets-to-string buffer :end bytes-read))))
