(in-package "BASIC-BINARY-IPC")

(defparameter *kevent-struct* 
  #+darwin '(:struct kevent64-s)
  #+freebsd '(:struct kevent)
  #-(or darwin freebsd) (error "Unsupported platform for KQUEUE."))

(defgeneric kqueue-descriptor (kqueue-poller)
  (:documentation "The OS descriptor for the KQUEUE."))

;; CLOSEDP is defined elsewhere.
#- (and)
(defgeneric closedp (object)
  (:documentation "A predicate for determining whether or not the KQUEUE is closed."))

(defgeneric monitor-table (kqueue-poller)
  (:documentation "A table containing information about each monitored socket."))

(defgeneric maximum-number-of-events (kqueue-poller)
  (:documentation "The maximum number of events that the kevent syscall can return."))

(defclass kqueue-poller (poller)
  ((kqueue-descriptor
    :initarg :kqueue-descriptor
    :reader kqueue-descriptor)
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
   :monitor-table (make-hash-table)
   :descriptor-socket-table (make-hash-table)
   :maximum-number-of-events 100))

(defun make-poller ()
  (make-instance 'kqueue-poller 
		 :kqueue-descriptor (%ff-kqueue)))

(defmethod close-poller ((object kqueue-poller))
  (unless (closedp object)
    (%ff-close (kqueue-descriptor object))
    (setf (closedp object) t)))

(defmethod monitoredp ((poller kqueue-poller) socket)
  (multiple-value-bind (value present?) (gethash socket (monitor-table poller))
    (declare (ignore value))
    present?))

(defmethod monitor-socket ((poller kqueue-poller) socket socket-events)
  (with-accessors ((monitor-table monitor-table)
		   (descriptor-socket-table descriptor-socket-table))
      poller
    (multiple-value-bind (value present-p) (gethash socket monitor-table)
      (declare (ignore value))
      (when present-p
	(error "Socket ~A is already being monitored by poller ~A." socket poller))
      
      (setf (gethash socket monitor-table) nil)
      (setf (gethash (file-descriptor socket) descriptor-socket-table) socket)
      (setf (monitored-events poller socket) socket-events))))

(defmethod unmonitor-socket ((poller kqueue-poller) socket)
  (with-accessors ((monitor-table monitor-table)
		   (kqueue-descriptor kqueue-descriptor))
      poller
    (multiple-value-bind (events present-p) (gethash monitor-table socket)
      (declare (ignore events))
      (when present-p
	(setf (monitored-events poller socket) nil)
	(remhash socket monitor-table)
	(remhash (file-descriptor socket) (descriptor-socket-table socket))))))

(defmethod monitored-events ((poller kqueue-poller) socket)
  (with-accessors ((monitor-table monitor-table)) poller
    (gethash socket monitor-table)))

(defgeneric compute-change-list (socket current-events new-events))

(defgeneric kevent-filters (object)
  (:documentation "Return the kevent filters needed to match the event
  represented by OBJECT."))

(defgeneric match-kqueue-events (object events))

(defmethod (setf monitored-events) (value (poller kqueue-poller) socket)
  (assert (monitoredp poller socket))
  (setf value (if (listp value)
		  value
		  (list value)))

  (with-accessors ((monitor-table monitor-table)
		   (kqueue-descriptor kqueue-descriptor))
      poller
    (let* ((current-events (gethash socket monitor-table))
	   (change-list (compute-change-list socket current-events value))
	   (kevent-struct *kevent-struct*))      
      (cffi:with-foreign-objects ((ptr-change-list kevent-struct (length change-list))
				  (ptr-event-list kevent-struct 0)
				  (timeout '(:struct timespec)))
	(setf (cffi:foreign-slot-value timeout '(:struct timespec) 'tv-sec) 0
	      (cffi:foreign-slot-value timeout '(:struct timespec) 'tv-nsec) 0)
	
	(loop
	   :for change :in change-list
	   :for index :from 0
	   :for ptr := (cffi:mem-aptr ptr-change-list kevent-struct index)
	   :do
	   (prepare-kevent-struct change ptr))
	
	(let ((number-of-events (kevent-wrapper kqueue-descriptor
						ptr-change-list (length change-list)
						ptr-event-list 0 timeout)))
	  (assert (zerop number-of-events))))

      (setf (gethash socket (monitor-table poller)) value))))

(defmethod compute-change-list ((socket socket) current new)
  (let ((current (if (listp current) current (list current)))
	(new     (if (listp new) new (list new))))
    (assert (and (listp current) (listp new)))

    (labels ((valid-event-p (event)
	       (if (typep socket 'stream-server)
		   (member event '(connection-available-p))
		   (member event '(connection-failed-p connection-succeeded-p determinedp
				   data-available-p ready-to-write-p
				   remote-disconnected-p))))
	     (reduce-kevent-filters (events)
	       (remove-duplicates (reduce #'append events :key #'kevent-filters))))
      (assert (every #'valid-event-p current))
      (assert (every #'valid-event-p new))

      (let* ((current-filters (reduce-kevent-filters current))
	     (new-filters (reduce-kevent-filters new))
	     (filters-to-remove (set-difference current-filters new-filters))
	     (filters-to-add (set-difference new-filters current-filters))
	     rv)
	(dolist (filter filters-to-remove)
	  (push (list (file-descriptor socket) filter :ev-delete) rv))
	(dolist (filter filters-to-add)
	  (push (list (file-descriptor socket) filter :ev-add) rv))
	rv))))

(defmethod wait-for-events ((poller kqueue-poller) timeout)
  (assert (typep timeout '(or (member :indefinite :immediate)
			      (real 0))))
  (let ((kevent-struct *kevent-struct*))
    (cffi:with-foreign-objects ((change-list-ptr kevent-struct 0)
				(event-list-ptr kevent-struct (maximum-number-of-events poller))
				(timespec-ptr '(:struct timespec)))
      
      (cffi:with-foreign-slots ((tv-sec tv-nsec) timespec-ptr (:struct timespec))
	(cond
	  ((realp timeout)
	   (setf tv-sec (floor timeout)
		 tv-nsec (floor (* 1e9 (- timeout (floor timeout))))))
	  ((eql timeout :immediate)
	   (setf tv-sec 0
		 tv-nsec 0))
	  ((eql timeout :indefinite) ;; do nothing.
	   ))
	(let ((n (kevent-wrapper (kqueue-descriptor poller)
				 change-list-ptr 0
				 event-list-ptr (maximum-number-of-events poller)
				 (if (eql timeout :indefinite)
				     (cffi:null-pointer)
				     timespec-ptr))))
	  (process-kqueue-events poller
				 (loop
				    :for index :from 0 :below n
				    :collect
				    (parse-kevent-struct (cffi:mem-aptr event-list-ptr kevent-struct index)))))))))

(defun process-kqueue-events (poller events)
  (let* ((unique-file-descriptors (remove-duplicates (mapcar #'first events) :test #'=))
	 (unique-sockets (mapcar #'(lambda (fd)
				     (gethash fd (descriptor-socket-table poller)))
				 unique-file-descriptors)))
    (assert (not (find nil unique-sockets)))
    (loop
       :for current-fd :in unique-file-descriptors
       :for current-socket :in unique-sockets
       :for socket-events := (remove-if-not #'(lambda (fd)
						(= current-fd fd))
					    events :key #'first)
       :for matched-events := (loop
				 :for monitored-event :in (monitored-events poller current-socket)
				 :when (match-kqueue-events monitored-event socket-events)
				 :collect monitored-event)
       :when matched-events
       :collect
       (list current-socket matched-events))))

;; KQUEUE EVENTS
(defun kevent/ident (object)
  (first object))

(defun kevent/filter (object)
  (second object))

(defun kevent/flags (object)
  (third object))

(defun kevent/data (object)
  (fifth object))

;; - connection-available-p
(defmethod kevent-filters ((object (eql 'connection-available-p)))
  '(:evfilt-read))

(defmethod match-kqueue-events ((object (eql 'connection-available-p)) events)
  (when (and events (= 1 (length events)))
    (equal (kevent/filter (first events)) :evfilt-read)))

;; - determinedp
(defmethod kevent-filters ((object (eql 'determinedp)))
  '(:evfilt-read :evfilt-write))

(defmethod match-kqueue-events ((object (eql 'determinedp)) events)
  (not (null events)))

;; - connection-succeeded-p
(defmethod kevent-filters ((object (eql 'connection-succeeded-p)))
  '(:evfilt-write :evfilt-read))

(defmethod match-kqueue-events ((object (eql 'connection-succeeded-p)) events)
  (let ((write (find :evfilt-write events :key #'kevent/filter))
	(read (find :evfilt-read events :key #'kevent/filter)))
    (and write
	 (or (not read)
	     (not (find :ev-eof (kevent/flags read)))))))

;; - connection-failed-p
(defmethod kevent-filters ((object (eql 'connection-failed-p)))
  '(:evfilt-read))

(defmethod match-kqueue-events ((object (eql 'connection-failed-p)) events)
  (let ((read (find :evfilt-read events :key #'kevent/filter)))
    (and read
	 (find :ev-eof (kevent/flags read)))))

;; - data-available-p
(defmethod kevent-filters ((object (eql 'data-available-p)))
  '(:evfilt-read))

(defmethod match-kqueue-events ((object (eql 'data-available-p)) events)
  (let ((read (find :evfilt-read events :key #'kevent/filter)))
    (and read
	 (not (find :ev-eof (kevent/flags read)))
	 (plusp (kevent/data read)))))

;; - read-to-write-p
(defmethod kevent-filters ((object (eql 'ready-to-write-p)))
  '(:evfilt-write))

(defmethod match-kqueue-events ((object (eql 'ready-to-write-p)) events)
  (let ((write (find :evfilt-write events :key #'kevent/filter)))
    (and write
	 (plusp (kevent/data write)))))

;; - remote-disconnected-p
(defmethod kevent-filters ((object (eql 'remote-disconnected-p)))
  '(:evfilt-read))

(defmethod match-kqueue-events ((object (eql 'remote-disconnected-p)) events)
  (let ((read (find :evfilt-read events :key #'kevent/filter)))
    (and read
	 (find :ev-eof (kevent/flags read)))))

;; KEVENT STRUCTS
#+darwin
(progn
  (defun parse-kevent-struct (kevent-struct-ptr)
    (cffi:with-foreign-slots ((ident filter flags fflags data udata ext) kevent-struct-ptr (:struct kevent64-s))
      (list ident
	    (cffi:foreign-enum-keyword 'kevent-filters filter)
	    (remove-if #'(lambda (keyword)
			   (zerop (logand flags (cffi:foreign-enum-value 'kevent-flags keyword))))
		       (cffi:foreign-enum-keyword-list 'kevent-flags))
	    fflags
	    data 
	    udata
	    (list (cffi:mem-aref ext :uint64 0)
		  (cffi:mem-aref ext :uint64 1)))))

  (defun prepare-kevent-struct (change kevent-struct-ptr)
    (cffi:with-foreign-slots ((ident filter flags fflags data udata ext) kevent-struct-ptr (:struct kevent64-s))
      (destructuring-bind (change-ident change-filter change-flags) change
	(setf ident change-ident
	      filter (cffi:foreign-enum-value 'kevent-filters change-filter)
	      flags (cffi:foreign-enum-value 'kevent-flags change-flags)
	      fflags 0
	      data 0
	      udata 0)

	(setf (cffi:mem-aref ext :uint64 0) 0
	      (cffi:mem-aref ext :uint64 1) 0)))))

#+freebsd
(progn
  (defun parse-kevent-struct (kevent-struct-ptr)
    (cffi:with-foreign-slots ((ident filter flags fflags data udata) kevent-struct-ptr (:struct kevent))
      (list ident
	    (cffi:foreign-enum-keyword 'kevent-filters filter)
	    (remove-if #'(lambda (keyword)
			   (zerop (logand flags (cffi:foreign-enum-value 'kevent-flags keyword))))
		       (cffi:foreign-enum-keyword-list 'kevent-flags))
	    fflags
	    data 
	    udata)))

  (defun prepare-kevent-struct (change kevent-struct-ptr)
    (cffi:with-foreign-slots ((ident filter flags fflags data udata) kevent-struct-ptr (:struct kevent))
      (destructuring-bind (change-ident change-filter change-flags) change
	(setf ident change-ident
	      filter (cffi:foreign-enum-value 'kevent-filters change-filter)
	      flags (cffi:foreign-enum-value 'kevent-flags change-flags)
	      fflags 0
	      data 0
	      udata (cffi:null-pointer))))))
