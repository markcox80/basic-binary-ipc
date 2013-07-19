(in-package "ASDF")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "cffi-grovel"))

(defsystem "basic-binary-ipc"
  :author "Mark Cox"
  :description "A inter-process communication library for transmitting binary data over a stream."
  :depends-on ("cffi-grovel")
  :serial t
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "protocols")
				     (:file "posix-helpers")
				     (cffi-grovel:grovel-file "posix-grovel")
				     (:file "posix-cffi")
				     (:file "posix-socket-options")
				     (:file "posix-sockets")
				     (:file "posix-poll")))
	       
	       #+(or darwin freebsd)
	       (:module "src/kqueue"
			:serial t
			:pathname "src"
			:components ((cffi-grovel:grovel-file "kqueue-grovel")
				     (:file "kqueue-cffi")
				     (:file "kqueue-poller")))
	       #+linux
	       (:module "src/epoll"
			:serial t
			:pathname "src"
			:components ((cffi-grovel:grovel-file "epoll-grovel")
				     (:file "epoll-cffi")
				     (:file "epoll-poller"))))
  :in-order-to ((test-op (test-op "basic-binary-ipc-tests"))))
