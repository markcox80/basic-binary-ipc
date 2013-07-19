(in-package "BASIC-BINARY-IPC")

(defgeneric epoll-descriptor (object)
  (:documentation "The descriptor for the EPOLL object."))

(defgeneric monitor-table (object)
  (:documentation "A table mapping sockets to epoll poller specific information."))

(defgeneric descriptor-socket-table (object)
  (:documentation "A table mapping file descriptors to socket objects."))

(defgeneric maximum-number-of-events (object)
  (:documentation "The maximum number of events to retrieve when
  invoking epoll_wait."))

(defclass epoll-poller (poller)
  ((epoll-descriptor
    :initarg :epoll-descriptor
    :reader epoll-descriptor)
   (closedp
    :initarg :closedp
    :accessor closedp)
   (monitor-table
    :initarg :monitor-table
    :reader monitor-table)
   (descriptor-socket-table
    :initarg :descriptor-socket-table
    :reader descriptor-socket-table)
   (maximum-number-of-events
    :initarg :maximum-number-of-events
    :reader maximum-number-of-events))
  (:default-initargs
   :closedp nil
   :maximum-number-of-events 100
   :monitor-table (make-hash-table)
   :descriptor-socket-table (make-hash-table)))

(defun make-poller ()
  (let ((epfd (%ff-epoll-create 1)))
    ;; 1 is the size argument. From the NOTES section in
    ;; epoll_create(2).
    ;;
    ;;   In the initial epoll_create() implementation, the size
    ;;   argument informed the kernel of the number of file
    ;;   descriptors that the caller expected to add to the epoll
    ;;   instance.  The kernel used this information as a hint for the
    ;;   amount of space to initially allocate in internal data
    ;;   structures describing events. .... Nowadays, this hint is no
    ;;   longer required, ...., but size must still be greater than
    ;;   zero, in order to ensure backward compatibility when new
    ;;   epoll applications are run on older kernels.

    (make-instance 'epoll-poller 
		   :epoll-descriptor epfd)))

(defmethod close-poller ((poller epoll-poller))
  (unless (closedp poller)
    (%ff-close (epoll-descriptor poller))
    (setf (closedp poller) t)))

(defmethod wait-for-events ((poller epoll-poller) timeout)
  (assert (typep timeout '(or (member :indefinite :immediate) (real 0))))
  (with-accessors ((epfd epoll-descriptor)
		   (maximum-number-of-events maximum-number-of-events))
      poller
    (cffi:with-foreign-objects ((events '(:struct epoll-event) maximum-number-of-events))
      (let* ((number-of-events (%ff-epoll-wait epfd events maximum-number-of-events
					       (cond
						 ((eql timeout :immediate)
						  0)
						 ((eql timeout :indefinite)
						  -1)
						 (t
						  (* timeout 1000))))))
	(process-events poller events number-of-events)))))

(defmethod monitor-socket ((poller epoll-poller) socket socket-events)
  (with-accessors ((monitor-table monitor-table)
		   (descriptor-socket-table descriptor-socket-table))
      poller
    (multiple-value-bind (value present?) (gethash socket monitor-table)
      (declare (ignore value))
      (when present?
	(error "Socket ~A is already monitored by poller ~A." socket poller))
      
      (setf (gethash socket monitor-table) nil
	    (gethash (file-descriptor socket) descriptor-socket-table) socket
	    (monitored-events poller socket) socket-events))))

(defmethod unmonitor-socket ((poller epoll-poller) socket)
  (with-accessors ((monitor-table monitor-table)
		   (descriptor-socket-table descriptor-socket-table))
      poller
    (multiple-value-bind (value present?) (gethash socket monitor-table)
      (declare (ignore value))
      (when present?
	(setf (monitored-events poller socket) nil)
	(remhash (file-descriptor socket) descriptor-socket-table)
	(remhash socket monitor-table)))))

(defgeneric epoll-events (object)
  (:documentation "Return the list of events required for epoll to
  notify when the event OBJECT occurs."))

(defgeneric epoll-match-p (object events socket)
  (:documentation "Inspect EVENTS and SOCKET to see if the OBJECT
  event has occurred."))

(defmethod (setf monitored-events) (value (poller epoll-poller) socket)
  (setf value (if (listp value)
		  value
		  (list value)))

  (with-accessors ((monitor-table monitor-table)
		   (epoll-descriptor epoll-descriptor))
      poller
    (let ((fd (file-descriptor socket)))
      (cffi:with-foreign-object (event '(:struct epoll-event))
	(cffi:with-foreign-slots ((events data) event (:struct epoll-event))
	  (setf (cffi:foreign-slot-value data '(:union epoll-data) 'fd) fd
		events (remove-duplicates (reduce #'append value :key #'epoll-events)))
	  (cond
	    ((null value)
	     (%ff-epoll-ctl epoll-descriptor :epoll-ctl-del fd event))
	    ((null (gethash socket monitor-table))
	     (%ff-epoll-ctl epoll-descriptor :epoll-ctl-add fd event))
	    (t
	     (%ff-epoll-ctl epoll-descriptor :epoll-ctl-mod fd event))))))    
    (setf (gethash socket monitor-table) value)))

(defun process-events (poller events number-of-events)
  (with-accessors ((monitor-table monitor-table)
		   (descriptor-socket-table descriptor-socket-table))
      poller
    (let ((data (loop
		   :for index :from 0 :below number-of-events
		   :collect
		   (let ((event (cffi:mem-aptr events '(:struct epoll-event) index)))
		     (cffi:with-foreign-slots ((events data) event (:struct epoll-event))
		       (cffi:with-foreign-slots ((fd) data (:union epoll-data))
			 (let* ((socket (gethash fd descriptor-socket-table))
				(monitored-events (gethash socket monitor-table)))
			   (assert (and socket monitored-events))
			   (list socket (loop
					   :for monitored-event :in monitored-events
					   :when (epoll-match-p monitored-event events socket)
					   :collect
					   monitored-event)))))))))
      (remove-if #'null data :key #'second))))

(defun epoll-event-data (symbol)
  (when (null symbol)
    (error "Attempting to retrieve EPOLL-EVENT-DATA for NIL."))
  (get symbol 'epoll-event-data))

(defun (setf epoll-event-data) (value symbol)
  (setf (get symbol 'epoll-event-data) value))

(defclass epoll-event-data ()
  ((events
    :initarg :events
    :reader epoll-events)
   (test-function
    :initarg :test-function
    :reader test-function)))

(defmethod epoll-match-p ((object epoll-event-data) events socket)
  (funcall (test-function object) events socket))

(defun ensure-epoll-event-data (name events test-function)
  (check-type name (and (not null) symbol))
  (check-type events list)
  (check-type test-function function)
  (assert (every #'keywordp events))
  (setf (epoll-event-data name) (make-instance 'epoll-event-data
					       :events events
					       :test-function test-function)))

(defmethod epoll-events ((object symbol))
  (epoll-events (epoll-event-data object)))

(defmethod epoll-match-p ((object symbol) events socket)
  (epoll-match-p (epoll-event-data object) events socket))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun prepare-epoll-event-test-function/expander (sexp events-var socket-var)
    (labels ((recurse (sexp)
	       (prepare-epoll-event-test-function/expander sexp events-var socket-var))
	     (unsupported-form ()
	       (error "Unsupported form for DEFINE-EPOLL-EVENT test function: ~A" sexp)))
      (cond
	((keywordp sexp)
	 (assert (cffi:foreign-bitfield-value 'epoll-events (list sexp)))
	 `(find ,sexp ,events-var))
	((listp sexp)
	 (alexandria:destructuring-case sexp
	   ((and &rest args)
	    `(and ,@(mapcar #'recurse args)))
	   ((or &rest args)
	    `(or ,@(mapcar #'recurse args)))
	   ((not arg)
	    `(not ,(recurse arg)))
	   ((lambda (socket) &body body)
	    `(funcall (function (lambda (,socket)
			,@body))
		      ,socket-var))
	   ((t &rest args)
	    (declare (ignore args))
	    (unsupported-form))))
	(t
	 (unsupported-form)))))

  (defun prepare-epoll-event-test-function (sexp)
    (let ((events-var (gensym))
	  (socket-var (gensym)))
      `(lambda (,events-var ,socket-var)
	 (declare (ignorable ,events-var ,socket-var))
	 ,(prepare-epoll-event-test-function/expander sexp events-var socket-var))))

  (defmacro define-epoll-event (name &body body)
    (labels ((body-values (key)
	       (let ((v (find key body :key #'first)))
		 (unless v
		   (error "Unable to find entry with key ~A" key))
		 (rest v)))
	     (body-value (key)
	       (first (body-values key))))
      `(ensure-epoll-event-data ',name
				(list ,@(body-values :events))
				(function ,(prepare-epoll-event-test-function (body-value :test)))))))

(define-epoll-event connection-available-p
  (:events :epollin)
  (:test (and :epollin (not :epollhup))))

(define-epoll-event determinedp
  (:events :epollin :epollout)
  (:test (or :epollin :epollout)))

(define-epoll-event connection-succeeded-p
  (:events :epollout)
  (:test (and :epollout
	      (not :epollhup)
	      (not :epollerr)
	      (not (and :epollin
			(lambda (socket)
			  (let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
			    (zerop (read-from-stream socket buffer :peek t)))))))))

(define-epoll-event connection-failed-p
  (:events :epollin)
  (:test (and :epollin
	      (or :epollhup
		  :epollerr
		  :epollrdhup
		  (lambda (socket)
		    (let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
		      (zerop (read-from-stream socket buffer :peek t))))))))

(define-epoll-event data-available-p
  (:events :epollin)
  (:test (and :epollin (not :epollrdhup) (not :epollhup))))

(define-epoll-event ready-to-write-p
  (:events :epollout)
  (:test (and :epollout
	      (not :epollhup)
	      (not :epollrdhup))))

(define-epoll-event remote-disconnected-p
  (:events :epollin)
  (:test (and :epollin
	      (lambda (socket)
		(let ((buffer (make-array 1 :element-type '(unsigned-byte 8))))
		  (zerop (read-from-stream socket buffer :peek t)))))))