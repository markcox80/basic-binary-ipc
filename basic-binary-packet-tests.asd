(in-package "ASDF")

(defsystem "basic-binary-packet-tests"
  :author "Mark Cox"
  :description "Unit tests for the basic-binary-packet system."
  :depends-on ("basic-binary-packet" "lisp-unit")
  :components ((:module "tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "packet")))))
