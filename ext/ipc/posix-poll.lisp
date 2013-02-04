(in-package "BASIC-BINARY-PACKET.IPC")

;; POLL-SOCKETS helpers
(defgeneric compute-poll-fd-events (socket socket-events)
  (:documentation "Return the list of POLL-EVENTS symbols required for
  the events field in the pollfd structure. SOCKET-EVENTS can be a
  single symbol or a list of symbols that represent events that can be
  detected using poll(2)."))

(defmethod compute-poll-fd-events (socket (socket-events list))
  (loop
     :for socket-event :in socket-events
     :append (compute-poll-fd-events socket socket-event)))

(defgeneric parse-poll-fd-result (socket socket-events revents)
  (:documentation "Parse the POLL-EVENTS symbols returned by poll(2)
  and determine if any event in SOCKET-EVENTS occurred. SOCKET-EVENTS
  may be a symbol or a list of symbols."))

(defmethod parse-poll-fd-result (socket (socket-events list) revents)
  (loop
     :for socket-event :in socket-events
     :for result := (parse-poll-fd-result socket socket-event revents)
     :when result
     :collect result))

(define-condition poll-socket-error (error)
  ((socket
    :initarg :socket
    :reader socket)
   (message
    :initarg :message
    :reader message))
  (:report (lambda (condition stream)
	     (write-string (message condition) stream))))

(defmethod parse-poll-fd-result :before (socket socket-events revents)
  (when (find 'pollerr revents)    
    (error 'poll-socket-error
	   :message (format nil "An exceptional condition has occurred on socket ~A" socket)
	   :socket socket))

  (when (find 'pollnval revents)
    (error 'poll-socket-error
	   :message (format nil "File descriptor for socket ~A is not open." socket)
	   :socket socket)))

;; POLL-SOCKETS Implementation
(defmethod poll-socket (socket socket-events timeout)
  (first (poll-sockets (list socket) (list socket-events) timeout)))

(defmethod poll-sockets ((all-sockets list) (all-socket-events list) timeout)
  (declare (type (or (member :immediate :indefinite)
		     (real 0))
		 timeout))
  (assert (= (length all-sockets)
	     (length all-socket-events)))
  (let ((number-of-sockets (length all-sockets)))
    (cffi:with-foreign-object (poll-fd-array '(:struct pollfd) number-of-sockets)
      ;; Build up the structure to pass to poll(2)
      (loop
	 :for socket :in all-sockets
	 :for socket-events :in all-socket-events
	 :for index :from 0
	 :for poll-fd := (cffi:mem-aptr poll-fd-array '(:struct pollfd) index)
	 :do
	 (cffi:with-foreign-slots ((fd events) poll-fd (:struct pollfd))
	   (setf fd (file-descriptor socket))
	   (setf events (compute-poll-fd-events socket socket-events))))

      ;; Call poll(2)
      (%ff-poll poll-fd-array number-of-sockets (case timeout
						  (:immediate   0)
						  (:indefinite -1)
						  ;; Convert to milliseconds
						  (t           (max 0 (coerce (round (* timeout 1000)) 'integer)))))

      ;; Parse the revents field to determine events.
      (loop
	 :for socket :in all-sockets
	 :for socket-events :in all-socket-events
	 :for index :from 0
	 :for poll-fd := (cffi:mem-aptr poll-fd-array '(:struct pollfd) index)
	 :collect
	 (cffi:with-foreign-slots ((revents) poll-fd (:struct pollfd))
	   (parse-poll-fd-result socket socket-events revents))))))

;; Posix Poll FD event definitions helpers
(defun poll-fd-event-test (expression events)
  (labels ((eventp (event)
	     (find event events))
	   (evaluate (s-exp)
	     (cond
	       ((symbolp s-exp)
		(eventp s-exp))
	       ((listp s-exp)
		(ecase (first s-exp)
		  (or
		   (some #'evaluate (rest s-exp)))
		  (and
		   (every #'evaluate (rest s-exp)))
		  (not
		   (assert (= 2 (length s-exp)))
		   (not (eventp (second s-exp))))
		  (t
		   (error "Invalid poll-fd-event-test form: ~A" s-exp))))
	       (t
		(error "Invalid poll-fd-event-test form: ~A" s-exp)))))
    (if (evaluate expression)
	t
	nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-poll-fd-event (name &body body)
    "Provide implementations for COMPUTE-POLL-FD-EVENTS and
PARSE-POLL-FD-EVENTS for the poll-fd event macro NAME. BODY consists
of expressions starting with :CLASSES, :INPUT or :TEST. 

  (:CLASSES <class1> ... <class N>) represents the classes in which
  this event macro should be defined.

  (:INPUT <input1> <input2>) is the input events that poll(2) should
  look for.

  (:TEST <expression>) is an expression which is T to indicate that
  the event is present.
"
    (labels ((body-values (key)
	       (let ((v (find key body :key #'first)))
		 (unless v
		   (error "Unable to find expression starting with ~A in ~A" key body))
		 (rest v)))
	     (body-value (key)
	       (first (body-values key))))
      `(progn
	 ,@(mapcar #'(lambda (class)
		       `(defmethod compute-poll-fd-events ((object ,class) (socket-events (eql ',name)))
			  ',(body-values :input)))
		   (body-values :classes))
	 
	 (labels ((do-test (events)
		    (poll-fd-event-test ',(body-value :test) events)))
	   ,@(mapcar #'(lambda (class)
			 `(defmethod parse-poll-fd-result ((object ,class) (socket-events (eql ',name)) revents)
			    (if (do-test revents)
				',name
				nil)))
		     (body-values :classes)))))))

(define-poll-fd-event determinedp
  (:classes ipv4-stream)
  (:input pollout)
  (:test (or pollout pollhup)))

(define-poll-fd-event connection-available-p
  (:classes ipv4-tcp-server)
  (:input pollin)
  (:test pollin))

(define-poll-fd-event connection-failed-p
  (:classes ipv4-stream)
  (:input pollin)
  (:test pollhup))

(define-poll-fd-event connection-succeeded-p
  (:classes ipv4-stream)
  (:input pollout pollin)
  (:test (and pollout (not pollhup))))

(define-poll-fd-event data-available-p
  (:classes ipv4-stream)
  (:input pollin)
  (:test (and pollin (not pollhup))))

(define-poll-fd-event ready-to-write-p
  (:classes ipv4-stream)
  (:input pollout pollin)
  (:test (and pollout (not pollhup))))

(define-poll-fd-event remote-disconnected-p
  (:classes ipv4-stream)
  (:input pollin)
  (:test pollhup))
