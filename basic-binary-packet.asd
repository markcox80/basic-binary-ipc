(in-package "ASDF")

(defsystem "basic-binary-packet"
  :author "Mark Cox"
  :description "A simple and extensible API for communication objects over a stream."
  :depends-on ("com.gigamonkeys.binary-data")
  :components ((:module "src"
			:serial t
			:components ((:file "packages"))))
  :in-order-to ((test-op (test-op "basic-binary-packet-tests"))))
