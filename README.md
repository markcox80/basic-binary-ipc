Basic Binary IPC
================

The basic binary IPC system provides an interface for performing inter
process communication using IPv4 or local streams. The interface
follows a non-blocking pattern which allows applications to
communicate either synchronously or asynchronously.

The interface has been implemented for the following platforms:
- Linux (poll and epoll)
- OSX (poll and kqueue)
- FreeBSD (poll and kqueue)
- Windows 8 (Overlapped I/O)

The complete documentation to this system can be found in
`doc/basic-binary-ipc.html`.

The only requirement for this system is `CFFI`.

Example
-------
The file `examples/echo-example.lisp` contains an example echo server
and echo client. The code for the server and client is shown below.

The echo server can be started using
```common-lisp
(load "examples/echo-example.lisp")
(echo-example:run-server 12345)
```

The echo client can be executed using
```common-lisp
(load "examples/echo-example.lisp")
(echo-example:send-to-server "Hello World" basic-binary-ipc:+ipv4-loopback+ 12345)
```

### Server

```common-lisp
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
```

### Client

```common-lisp
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
```