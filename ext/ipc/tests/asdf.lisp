(in-package "BASIC-BINARY-PACKET.IPC.TESTS")

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (asdf:find-system "basic-binary-packet-ipc-tests"))))
  (lisp-unit:run-tests :all "BASIC-BINARY-PACKET.IPC.TESTS"))
