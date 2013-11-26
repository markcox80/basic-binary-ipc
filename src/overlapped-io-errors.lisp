(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO")

;;;; Size checks
;; nSize in %FF-FORMAT-MESSAGE is the number of TCHARs in
;; LPTSTR. Make sure TCHAR is an unsigned byte.
(assert (= 1 (cffi:foreign-type-size 'tchar)))

;; DWORD is a pain. The Micorsoft documentation states that it should
;; be a 32 bit unsigned integer. Unfortunately, the groveller (as of
;; 2013-09-08) does not handle unsigned types properly. I have sent a
;; patch to CFFI to fix CONSTANTENUM to print the correct value but it
;; relies on :UNSIGNED-INT rather than the stricter :uint32. These two
;; assertions check that this hack is correct.
(assert (= 4 (cffi:foreign-type-size :unsigned-int)))
(assert (= 4 (cffi:foreign-type-size 'dword)))

;;;; Obtaining the error message for a given ERRNUM.
(cffi:defcfun (%ff-format-message "FormatMessageA") dword
  (dwFlags dword)
  (lpSource lpcvoid)
  (dwMessageId dword)
  (dwLanguageId dword)
  (lpBuffer lptstr)
  (nSize dword)
  (arguments :pointer))

(defun %error-message (errnum)
  "Obtain the message for the error represented by ERRNUM."
  (check-type errnum (integer 0))
  (let ((buffer (make-array 1000 :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (ptr buffer)
      (let ((number-of-characters
	     (%ff-format-message (cffi:foreign-bitfield-value 'format-message-flags '(:format-message-from-system
										      :format-message-ignore-inserts))
				 0
				 errnum
				 0
				 (cffi:pointer-address ptr) (length buffer)
				 (cffi:null-pointer))))
	(assert (plusp number-of-characters))
	(string-trim '(#\Return #\Newline) (babel:octets-to-string buffer :end number-of-characters))))))

;;;; Functions for obtaining the last error
(cffi:defcfun (%ff-wsa-get-last-error "WSAGetLastError") winsock-error-codes)
(cffi:defcfun (%ff-get-last-error "GetLastError") error-codes)

(defun error-message (error-code)
  (check-type error-code keyword)
  (%error-message (cffi:foreign-enum-value 'error-codes error-code)))

(defun winsock-error-message (error-code)
  (check-type error-code keyword)
  (%error-message (cffi:foreign-enum-value 'winsock-error-codes error-code)))

;;;; System call checkers
(defun signal-foreign-function-error (caller name)
  (let ((v (%ff-get-last-error)))
    (error 'system-function-error
	   :caller caller
	   :name name	   
	   :error-value v
	   :error-message (error-message v))))

(defun signal-socket-foreign-function-error (caller name)
  (let ((error-code (%ff-wsa-get-last-error)))
    (error 'system-function-error
	   :caller caller
	   :name name
	   :error-value error-code
	   :error-message (winsock-error-message error-code))))

;; File system call checkers
(define-check-system-call check-valid-handle (caller name return-value)
  (if (eql return-value +invalid-handle-value+)
      (signal-foreign-function-error caller name)
      return-value))

(define-check-system-call check-true (caller name return-value)
  (if (eql return-value +false+)
      (signal-foreign-function-error caller name)
      return-value))

(define-check-system-call check-non-null (caller name return-value)
  (if (= +null+ return-value)
      (signal-foreign-function-error caller name)
      return-value))

(define-check-system-call check-overlapped (caller name return-value &key (pass-errors '(:error-io-pending)))
  (cond
    ((= +false+ return-value)
     (let ((v (%ff-get-last-error)))
       (if (find v pass-errors)
	   (values +false+ v)
	   (signal-foreign-function-error caller name))))
    (t
     (values +true+ :no-error))))

(define-check-system-call check-socket-zero (caller name return-value)
  (if (/= 0 return-value)
      (signal-socket-foreign-function-error caller name)
      return-value))

(define-check-system-call check-socket-overlapped (caller name return-value &key (pass-errors '(:wsa-io-pending)))

  (cond
    ((= +false+ return-value)
     (let ((v (%ff-wsa-get-last-error)))
       (if (find v pass-errors)
	   (values +false+ v)
	   (signal-socket-foreign-function-error caller name))))
    (t
     (values +true+ :no-error))))
