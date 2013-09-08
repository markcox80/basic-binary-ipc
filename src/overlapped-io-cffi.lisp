(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO")

;; Type size checks.
;;
;; The CFFI groveller can't use types that it has already found for
;; some reason.
(assert (= (cffi:foreign-type-size 'dword)
	   (cffi:foreign-type-size :unsigned-int)))

;; %FF-WAIT-FOR-SINGLE-OBJECT

(cffi:defcfun (%%ff-wait-for-single-object "WaitForSingleObject") wait
  (handle handle)
  (milliseconds dword))

(defun %ff-wait-for-single-object (handle milliseconds)
  (ecase (%%ff-wait-for-single-object handle milliseconds)
    (:wait-abandoned
     (error "The specified object is a mutex object that was not
     released by the thread that owned the mutex object before the
     owning thread terminated. Ownership of the mutex object is
     granted to the calling thread and the mutex state is set to
     nonsignaled.

If the mutex was protecting persistent state information, you should
check it for consistency.

This error message is copied verbatim from the Microsoft
documentation.
"))
    (:wait-object-0
     t)
    (:wait-timeout
     nil)
    (:wait-failed
     (error (error-message (%ff-get-last-error))))))
