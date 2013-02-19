(in-package "ASDF")

(defsystem "basic-binary-packet-ipc-tests"
  :author "Mark Cox"
  :description "A collection of tests for the BASIC-BINARY-PACKET-IPC system."
  :depends-on ("basic-binary-packet-ipc" "lisp-unit")
  :serial t
  :components ((:module "ext/ipc/tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     (:file "posix")
				     (:file "ipv4")
				     (:file "local")
				     (:file "poll-sockets")))))
