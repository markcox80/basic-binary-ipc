(in-package "BASIC-BINARY-PACKET.NETWORK.TESTS")

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (asdf:find-system "basic-binary-packet-network-tests"))))
  (lisp-unit:run-tests :all "BASIC-BINARY-PACKET.NETWORK.TESTS"))
