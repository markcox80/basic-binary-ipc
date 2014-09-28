(in-package "ASDF")

(defsystem "basic-binary-ipc-tests"
  :author "Mark Cox"
  :description "A collection of tests for the BASIC-BINARY-IPC system."
  :depends-on ("basic-binary-ipc" "bordeaux-threads" "lisp-unit")
  :serial t
  :components ((:module "tests"
			:serial t
			:components ((:file "packages")
				     (:file "asdf")
				     #-windows
				     (:file "posix")
				     (:file "ipv4")
				     (:file "local")
				     (:file "poll-sockets")
				     (:file "poller")))
	       
	       #+windows
	       (:module "tests/windows"
			:serial t
			:pathname "tests/"
			:components ((:file "overlapped-io")))))
