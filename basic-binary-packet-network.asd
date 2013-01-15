(in-package "ASDF")

(defsystem "basic-binary-packet-network"
  :author "Mark Cox"
  :description "An extension to the basic binary packet library that
  supports transmitting and receiving packets over a network."
  :depends-on ("basic-binary-packet" "iolib")
  :components ((:module "ext/network/"
			:serial t
			:components ((:file "packages")
				     (:file "common")
				     (:file "client")
				     (:file "remote-client")
				     (:file "server"))))
  :in-order-to ((test-op (test-op "basic-binary-packet-network-tests"))))
