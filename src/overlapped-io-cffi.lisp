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
