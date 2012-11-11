(in-package "BASIC-BINARY-PACKET.TESTS")

(defmethod asdf:perform ((op asdf:test-op) (component (eql (asdf:find-system "basic-binary-packet-tests"))))
  (lisp-unit:run-tests :all "BASIC-BINARY-PACKET.TESTS"))
