(in-package "BASIC-BINARY-IPC.TESTS")

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (asdf:find-system "basic-binary-ipc-tests"))))
  (dolist (pkg (list "BASIC-BINARY-IPC.TESTS"
		     #+windows "BASIC-BINARY-IPC.OVERLAPPED-IO.TESTS"))
    (format t "~&;;;; Running tests in package ~A~%" pkg)
    (let ((report (lisp-unit:run-tests :all pkg)))
      (print-failures report)
      (print-errors report))))
