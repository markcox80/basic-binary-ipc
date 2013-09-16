(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO")

;; Type size checks.
;;
;; The CFFI groveller can't use types that it has already found for
;; some reason.
(assert (= (cffi:foreign-type-size 'dword)
	   (cffi:foreign-type-size :unsigned-int)))

;; %FF-WAIT-FOR-SINGLE-OBJECT

(cffi:defcfun (%ff-wait-for-single-object "WaitForSingleObject") wait
  (handle handle)
  (milliseconds dword))

(define-system-call (%ff-close-handle "CloseHandle") (check-true bool)
  (object handle))

(define-system-call (%ff-cancel-io "CancelIo") (check-true bool)
  (object handle))

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

(define-system-call (%ff-connect-named-pipe "ConnectNamedPipe") (check-true/overlapped bool)
  (server-handle handle)
  (overlapped (:pointer (:struct overlapped))))

;;;; Events
(define-system-call (%ff-create-event "CreateEventA") (check-non-null handle)
  (security-attributes :pointer)
  (manual-reset bool)
  (initial-state bool)
  (name :string))

(define-system-call (%ff-reset-event "ResetEvent") (check-true bool)
  (h-event handle))
