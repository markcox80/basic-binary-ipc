(in-package "BASIC-BINARY-IPC.OVERLAPPED-IO.TESTS")

#-windows
(error "The file ~A should only be compiled/loaded on Windows." (or *compile-file-truename*
								    *load-truename*))

(defun random-string (&optional (number-of-characters 10))  
  (let ((rv (make-string number-of-characters))
	(offset (char-code #\a))
	(range (- (char-code #\z) (char-code #\a))))
    (dotimes (index number-of-characters)
      (let ((code (random range)))
	(setf (elt rv index) (code-char (+ offset code)))))
    rv))

(defun random-pipe-name (&optional (number-of-characters 10))
  (concatenate 'string
	       "//./pipe/"
	       (random-string number-of-characters)))

(define-test named-pipe/connection/nothing
  (with-handle (server (make-named-pipe-server (random-pipe-name)))
    (with-request (connect-request (connect-named-pipe server))
      (assert-false (invalidp connect-request))
      (assert-true (waitingp connect-request))
      (assert-false (completedp connect-request))

      (cancel-all-io server))))

(define-test named-pipe/connection/wait
  (let ((pipe-name (random-pipe-name)))
    (with-handle (server (make-named-pipe-server pipe-name))
      (with-request (connect-request (connect-named-pipe server))
	(assert-false (invalidp connect-request))
	(assert-true (waitingp connect-request))
	(assert-false (completedp connect-request))

	(with-handle (client (connect-to-named-pipe pipe-name))
	  (declare (ignore client))
	  (assert-false (waitingp connect-request))
	  (assert-true (completedp connect-request)))))))

(define-test named-pipe/connection/no-wait
  (let ((pipe-name (random-pipe-name)))
    (with-handle (server (make-named-pipe-server pipe-name))
      (with-handle (client (connect-to-named-pipe pipe-name))
	(declare (ignore client))
	(with-request (connect-request (connect-named-pipe server))
	  (assert-false (invalidp connect-request))
	  (assert-false (waitingp connect-request))
	  (assert-true (completedp connect-request)))))))
