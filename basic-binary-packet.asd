(in-package "ASDF")

(defsystem "basic-binary-packet"
  :author "Mark Cox"
  :description "A simple and extensible API for communication objects over a stream."
  :depends-on ("babel" "com.gigamonkeys.binary-data" "flexi-streams" "ieee-floats")
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "packet")
				     (:file "raw-binary-data")
				     (:file "binary-data"))))
  :in-order-to ((test-op (test-op "basic-binary-packet-tests"))))
