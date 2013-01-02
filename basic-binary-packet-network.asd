(in-package "ASDF")

(defsystem "basic-binary-packet-network"
  :author "Mark Cox"
  :description ""
  :depends-on ("basic-binary-packet" "iolib")
  :components ((:module "ext/network/"
			:serial t
			:components ((:file "packages")
				     (:file "common")
				     (:file "client")))))
