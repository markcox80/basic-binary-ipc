(in-package "ASDF")

(defsystem "basic-binary-packet-network-tests"
  :author "Mark Cox"
  :description "The collection of tests for the
  basic-binary-packet-network system."
  :depends-on ("basic-binary-packet-network" "lisp-unit")
  :components ((:module "ext/network/tests/"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "network")))))
