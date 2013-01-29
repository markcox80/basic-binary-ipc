(in-package "ASDF")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "cffi-grovel"))

(defsystem "basic-binary-packet-ipc"
  :author "Mark Cox"
  :description "The inter-process communication library used by the
  BASIC-BINARY-PACKET-NETWORK system."
  :depends-on ("cffi-grovel" "cffi-libffi")
  :serial t
  :components ((:module "ext/ipc"
			:serial t
			:components ((:file "packages")
				     (:file "protocols")
				     (:file "posix-helpers")
				     (cffi-grovel:grovel-file "posix-grovel")
				     (:file "posix-cffi")
				     (:file "posix-socket-options")
				     (:file "posix-sockets"))))
  :in-order-to ((test-op (test-op "basic-binary-packet-ipc-tests"))))
