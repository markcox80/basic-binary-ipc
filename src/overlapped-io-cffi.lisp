(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO")

;;;; Type size checks.
;;;;
;;;; The CFFI groveller can't use types that it has already found for
;;;; some reason.
(assert (= (cffi:foreign-type-size 'dword)
	   (cffi:foreign-type-size :unsigned-int)))

;;;; Handles
(define-system-call (%ff-close-handle "CloseHandle") (check-true bool)
  (object handle))

;;;; Overlapped Structures
(cffi:defcfun (%ff-wait-for-single-object "WaitForSingleObject") wait
  (handle handle)
  (milliseconds dword))

(cffi:defcfun (%ff-wait-for-multiple-objects "WaitForMultipleObjects") dword
  (number-of-handles dword)
  (handles (:pointer handle))
  (wait-for-all bool)
  (milliseconds dword))

(define-system-call (%ff-cancel-io "CancelIo") (check-true bool)
  (object handle))

(define-system-call (%ff-get-overlapped-result "GetOverlappedResult")
    (check-overlapped bool :pass-errors '(:error-broken-pipe))
  (handle handle)
  (overlapped (:pointer (:struct overlapped)))
  (ptr-bytes-transferred (:pointer dword))
  (wait bool))

;;;; Named Pipes
(define-system-call (%ff-create-named-pipe "CreateNamedPipeA") (check-valid-handle handle)
  (name :string)
  (open-mode named-pipe-open-mode)
  (mode named-pipe-mode)
  (max-instances dword)
  (output-buffer-size dword)
  (in-buffer-size dword)
  (default-timeout dword)
  (security-attributes :pointer))

(define-system-call (%ff-create-file "CreateFileA") (check-valid-handle handle)
  (name :string)
  (desired-access file-desired-access)
  (share-mode file-share-mode)
  (security-attributes :pointer)
  (creation-disposition file-creation-disposition)
  (flags-and-attributes file-attribute)
  (template-file handle))

(define-system-call (%ff-connect-named-pipe "ConnectNamedPipe")
    (check-overlapped bool :pass-errors '(:error-io-pending :error-pipe-connected))
  (server-handle handle)
  (overlapped (:pointer (:struct overlapped))))

(define-system-call (%ff-read-file "ReadFile") (check-overlapped bool)
  (handle handle)
  (buffer (:pointer :uint8))
  (number-of-bytes-to-read dword)
  (number-of-bytes-read (:pointer dword))
  (overlapped (:pointer (:struct overlapped))))

(define-system-call (%ff-write-file "WriteFile") (check-overlapped bool)
  (handle handle)
  (buffer (:pointer :uint8))
  (number-of-bytes-to-write dword)
  (number-of-bytes-written (:pointer dword))
  (overlapped (:pointer (:struct overlapped))))

;;;; Events
(define-system-call (%ff-create-event "CreateEventA") (check-non-null handle)
  (security-attributes :pointer)
  (manual-reset bool)
  (initial-state bool)
  (name :string))

(define-system-call (%ff-reset-event "ResetEvent") (check-true bool)
  (h-event handle))

(define-system-call (%ff-set-event "SetEvent") (check-true bool)
  (h-event handle))

;;;; I/O Completion ports
(define-system-call (%ff-create-io-completion-port "CreateIoCompletionPort")
    (check-valid-handle handle)
  (file-handle handle)
  (existing-completion-port handle)
  (completion-key (:pointer :unsigned-long))
  (number-of-concurrent-threads dword))

(define-system-call (%ff-get-queued-completion-status "GetQueuedCompletionStatus")
    (check-overlapped bool :pass-errors '(:wait-timeout))
  (completion-port handle)
  (ptr-number-of-bytes (:pointer dword))
  (ptr-completion-key (:pointer (:pointer :unsigned-long)))
  (overlapped (:pointer (:struct overlapped)))
  (milliseconds dword))

;;;; Sockets
(cffi:defcfun (%%ff-wsa-socket "WSASocketA") socket
  (address-family socket-address-family)
  (type socket-type)
  (protocol socket-protocol)
  (protocol-info :pointer)
  (group socket-group)
  (flags socket-flags))

(defun %ff-socket (address-family type protocol)
  (let ((rv (%%ff-wsa-socket address-family type protocol
			     (cffi:null-pointer) 0 :wsa-flag-overlapped)))
    (when (= rv +invalid-socket+)
      (signal-socket-foreign-function-error '%ff-socket "WSASocketA"))
    rv))

(cffi:defcfun (%%ff-close-socket "closesocket") :int
  (socket socket))

(defun %ff-close-socket (socket)
  (let ((rv (%%ff-close-socket socket)))
    (unless (zerop rv)
      (signal-socket-foreign-function-error '%ff-close-socket "closesocket"))
    rv))

(define-system-call (%ff-bind "bind") (check-socket-zero :int)
  (socket socket)
  (socket-address :pointer)
  (address-length :int))

(define-system-call (%ff-listen "listen") (check-socket-zero :int)
  (socket socket)
  (backlog :int))

;; AcceptEx and GetAcceptExSockaddrs
;; http://msdn.microsoft.com/en-us/library/windows/desktop/ms738516(v=vs.85).aspx
;; http://msdn.microsoft.com/en-us/library/windows/desktop/ms737524(v=vs.85).aspx
;;
;; See the NOTE paragraphs in the above links.
;;
;; Microsoft! OMFG!!! YTF is it this hard?!!  This is the last library
;; I am writing for your platform in my spare time. FormatMessage was
;; bad enough, now this BS! 

;; I would like to be able to use this, but you can't. I'll leave it
;; here as a reminder of what things should have been like.
#- (and)
(define-system-call (%ff-accept-ex "AcceptEx") (check-socket-overlapped bool)
  (listen-socket socket)
  (accept-socket socket)
  (output-buffer (:pointer :uint8))
  (received-data-length dword)
  (local-address-length dword)
  (remote-address-length dword)
  (ptr-bytes-received (:pointer dword))
  (overlapped (:pointer (:struct overlapped))))

#- (and)
(cffi:defcfun (%ff-get-accept-ex-sockaddrs "GetAcceptExSockaddrs") :void
  (buffer (:pointer :uint8))
  (receive-data-length dword)
  (local-address-length dword)
  (remote-address-length dword)
  (local-sockaddr :pointer)
  (local-sockaddr-length dword)
  (remote-sockaddr :pointer)
  (remote-sockaddr-length dword))

(define-system-call (%ff-wsaioctl "WSAIoctl") (check-socket-zero :int)
  (socket socket)
  (io-control-code io-control-code)
  (in-buffer :pointer)
  (in-buffer-size dword)
  (out-buffer :pointer)
  (out-buffer-size dword)
  (bytes-returned-pointer (:pointer dword))
  (overlapped (:pointer (:struct overlapped)))
  (completion-routine :pointer))

(define-system-call (%ff-setsockopt "setsockopt") (check-socket-zero :int)
  (socket socket)
  (level socket-level)
  (option-name socket-option)
  (value :pointer)
  (option-length :int))

(define-system-call (%ff-getpeername "getpeername") (check-socket-zero :int)
  (socket socket)
  (name :pointer)
  (name-length :pointer))

(define-system-call (%ff-getsockname "getsockname") (check-socket-zero :int)
  (socket socket)
  (name :pointer)
  (name-length :pointer))

;; Socket number format stuff. I have never understood why the
;; application writer has to care about this crap. Even posix does it.
(cffi:defcfun (%ff-htons "htons") :unsigned-short
  (host-short :unsigned-short))

(cffi:defcfun (%ff-ntohs "ntohs") :unsigned-short
  (network-short :unsigned-short))

(cffi:defcfun (%ff-htonl "htonl") :unsigned-long
  (host-long :unsigned-long))

(cffi:defcfun (%ff-ntohl "ntohl") :unsigned-long
  (network-long :unsigned-long))

(cffi:defcfun (%%ff-inet-addr "inet_addr") :unsigned-long
  (dotted-decimal :string))

(defun %ff-inet-addr (dotted-decimal &optional (error t) error-value)
  (let ((rv (%%ff-inet-addr dotted-decimal)))
    (cond
      ((and error (= rv %+inaddr-none+))
       (error "Cannot convert value ~A to a network long using \"inet_addr\"." dotted-decimal))
      ((= rv %+inaddr-none+)
       error-value)
      (t
       rv))))

(assert (= (cffi:foreign-type-size :unsigned-long)
	   (cffi:foreign-type-size '(:struct in-addr))))
(cffi:defcfun (%ff-inet-ntoa "inet_ntoa") :string
  (in-addr :unsigned-long))

