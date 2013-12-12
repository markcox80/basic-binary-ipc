(in-package "BASIC-BINARY-IPC.TESTS")

(defun local-socket-pathname ()  
  #-windows
  "/tmp/test.socket"
  #+windows
  "//./pipe/test.socket")

(define-test make-local-server
  (:tag :local-socket)
  (let ((server (make-local-server (local-socket-pathname))))
    #-windows
    (assert-true (probe-file (local-socket-pathname)))
    (unwind-protect
	 (progn
	   (assert-true server)
	   (assert-error 'no-connection-available-error (accept-connection server))
	   (assert-error 'socket-error (make-local-server (local-socket-pathname))))
      (close-socket server)
      #-windows
      (assert-false (probe-file (local-socket-pathname))))))

(define-test local-test/sockets
  (:tag :local-socket)
  (labels ((channel-test (client remote-client)
	     (assert-true (poll-socket client '(determinedp connection-succeeded-p) 10))
	     (assert-true (poll-socket remote-client 'determinedp 10))
	     (assert-false (connection-failed-p client))
	     (assert-false (connection-failed-p remote-client))
	     (assert-true (connection-succeeded-p client))
	     (assert-true (connection-succeeded-p remote-client)))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (typep server 'stream-server))
	       (assert-true (typep client 'stream-socket))
	       (assert-true (typep remote-client 'stream-socket))
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (unwind-protect
	   (progn
	     (assert-false (poll-socket server 'connection-available-p 0))
	     (let ((client (connect-to-local-server (local-socket-pathname))))
	       (unwind-protect
		    (establish-channel server client)
		 (close-socket client))))
	(close-socket server)))))

(define-test local-test/stream
  (:tag :local-socket)
  (labels ((channel-test (client remote-client)
	     (assert-true (poll-socket client 'ready-to-write-p 0))
	     (assert-false (poll-socket remote-client 'data-available-p 0))

	     (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
	       (dotimes (i (length buffer))
		 (setf (elt buffer i) i))

	       (assert-error 'error (write-to-stream client buffer :start -1))
	       (assert-error 'error (write-to-stream client buffer :end -1))
	       (assert-error 'error (write-to-stream client buffer :start 3 :end 1))
	       (assert-error 'error (write-to-stream client buffer :start 3 :end 11))

	       (assert-equal 2 (write-to-stream client buffer :start 3 :end 5))
	       (assert-true (poll-socket remote-client 'data-available-p 10))
	       
	       (assert-error 'error (read-from-stream remote-client buffer :start -1))
	       (assert-error 'error (read-from-stream remote-client buffer :end -1))
	       (assert-error 'error (read-from-stream remote-client buffer :start 3 :end 1))
	       (assert-error 'error (read-from-stream remote-client buffer :start 3 :end 11))

	       (assert-equal 2 (read-from-stream remote-client buffer :start 7 :end 10))
	       (dotimes (i 7)
		 (assert-equal i (elt buffer i)))
	       (assert-equal 3 (elt buffer 7))
	       (assert-equal 4 (elt buffer 8))
	       (assert-equal 9 (elt buffer 9))

	       (assert-false (data-available-p remote-client))
	       (assert-true (ready-to-write-p client))))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))
	       (assert-true (poll-socket remote-client 'connection-succeeded-p 10))	       
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test local-test/remote-disconnected
  (:tag :local-socket)
  (labels ((channel-test (client remote-client)
	     (assert-true (poll-socket client 'ready-to-write-p 0))
	     (assert-false (poll-socket remote-client 'data-available-p 0))

	     (close-socket remote-client)

	     (assert-false (poll-socket client 'connection-succeeded-p 10))
	     (assert-true (poll-socket client 'remote-disconnected-p 10))
	     (assert-false (poll-socket client 'ready-to-write-p 0))
	     (assert-false (poll-socket client 'data-available-p 0)))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))
	       (assert-true (poll-socket remote-client 'connection-succeeded-p 10))	       

	       (channel-test client remote-client))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

(define-test connect-to-local-server/does-not-exist
  (:tag :local-socket)
  (assert-error 'no-local-server-error (connect-to-local-server (local-socket-pathname))))

(define-test local-test/pathname
  (:tag :local-socket)
  (labels ((perform-test (server client remote-client)
	     (assert-true (poll-socket client 'connection-succeeded-p 10))	     
	     (assert-true (pathname-match-p (local-pathname server)
					    (local-pathname client)))
	     (assert-true (pathname-match-p (local-pathname server)
					    (local-pathname remote-client))))
	   (establish-channel (server client)
	     (assert-true (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (unwind-protect
		    (perform-test server client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (unwind-protect
	   (progn
	     (assert-false (poll-socket server 'connection-available-p 0))
	     (let ((client (connect-to-local-server (local-socket-pathname))))
	       (unwind-protect
		    (establish-channel server client)
		 (close-socket client))))
	(close-socket server)))))

(define-test local-test/no-data
  (:tag :local-socket)
  (labels ((channel-test (client remote-client)
	     (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
	       (assert-equal 0 (read-from-stream client buffer))
	       (assert-equal 0 (read-from-stream remote-client buffer))))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))
	       (assert-true (poll-socket remote-client 'connection-succeeded-p 10))	       
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))

#-windows
(define-test local-test/full-write-buffer
  (:tag :local-socket)
  (labels ((channel-test (client remote-client)
	     (declare (ignore remote-client))
	     (let ((buffer (make-array 100000 :element-type '(unsigned-byte 8)))
		   (write-buffer-full nil))
	       (loop
		  :for attempt :from 0 :below 1000
		  :until write-buffer-full
		  :do
		  (when (zerop (write-to-stream client buffer))
		    (setf write-buffer-full t)))
	       (assert-true write-buffer-full)))
	   (establish-channel (server client)
	     (assert-equal 'connection-available-p (poll-socket server 'connection-available-p 10))
	     (let ((remote-client (accept-connection server)))
	       (assert-true (poll-socket client 'connection-succeeded-p 10))
	       (assert-true (poll-socket remote-client 'connection-succeeded-p 10))	       
	       (unwind-protect
		    (channel-test client remote-client)
		 (close-socket remote-client)))))
    (let ((server (make-local-server (local-socket-pathname))))
      (assert-false (poll-socket server 'connection-available-p 0))
      (unwind-protect
	   (let ((client (connect-to-local-server (local-socket-pathname))))
	     (unwind-protect
		  (establish-channel server client)
	       (close-socket client)))
	(close-socket server)))))
