(in-package "BASIC-BINARY-IPC.TESTS")

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (asdf:find-system "basic-binary-ipc-tests"))))
  (lisp-unit:run-tests :all "BASIC-BINARY-IPC.TESTS"))
