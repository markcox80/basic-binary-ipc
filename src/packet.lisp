(in-package "BASIC-BINARY-PACKET")

(defvar *magic-header* (babel:string-to-octets "basic-binary-packet" :encoding :utf-8))

(defun make-magic-header-search-function (magic-header)
  "Returns a function that searches for MAGIC-HEADER in a binary
stream. The returned function accepts a single argument, STREAM, which
is used to conduct the search. The function is a state machine,
processing only one character at a time and the returning control to
the caller. This function should be repeatedly called until it returns
non NIL, indicating that MAGIC-HEADER has been read from the stream."
  (declare (type (array (unsigned-byte 8) (*)) magic-header))
  (assert (plusp (length magic-header)))
  (let ((current-position 0))
    (lambda (stream)
      (labels ((process (v)
		 (when (>= current-position (length magic-header))
		   (setf current-position 0))

		 (cond
		   ((= v (elt magic-header current-position))
		    (incf current-position)
		    (if (= current-position (length magic-header))
			t
			nil))
		   ((plusp current-position)
		    (setf current-position 0)
		    (process v)))))
	(process (read-byte stream))))))

(defun make-payload-accumulator-function (number-of-bytes)
  "Return a function that accumulates NUMBER-OF-BYTES bytes from a
stream. The function returned accepts a single argument. The function
is a state machine, only processing a single byte from stream at one
time."
  (let ((s (flexi-streams:make-in-memory-output-stream :element-type '(unsigned-byte 8)))
	(byte-count 0))
    (lambda (stream)
      (when (>= byte-count number-of-bytes)
	(setf byte-count 0))
      
      (write-byte (read-byte stream) s)
      (incf byte-count)
      (if (= byte-count number-of-bytes)
	  (flexi-streams:get-output-stream-sequence s)
	  nil))))

(defun decode-32-bit-unsigned-integer (bytes)
  "Convert the vector of 4 bytes to a 32 bit unsigned integer. Byte 0
of BYTES is the most significant byte."
  (declare (type vector bytes))
  (assert (= 4 (length bytes)))
  (let ((rv 0))
    (declare (type (unsigned-byte 32) rv))
    (setf (ldb (byte 8 24) rv) (elt bytes 0)
	  (ldb (byte 8 16) rv) (elt bytes 1)
	  (ldb (byte 8  8) rv) (elt bytes 2)
	  (ldb (byte 8  0) rv) (elt bytes 3))
    rv))

(defun make-packet-reader-function ()
  "Return a function that reads packets from a STREAM. The function
  returned accepts a single argument which is a STREAM. The function
  is a state machine that only processes a single byte at a time. This
  function should be repeatedly called as bytes become available on
  the stream. The function will return a vector containing the payload
  and the packet identifier as values upon accumulation of a complete
  packet."
  (let ((state :magic-header)
	(magic-header-fn (make-magic-header-search-function *magic-header*))
	(four-byte-fn (make-payload-accumulator-function 4))
	(payload-fn nil)
	(identifier nil)
	(number-of-bytes nil))
    (lambda (stream)
      (case state
	(:magic-header
	 (when (funcall magic-header-fn stream)
	   (setf state :identifier))
	 nil)
	(:identifier
	 (alexandria:when-let ((v (funcall four-byte-fn stream)))
	   (setf identifier (decode-32-bit-unsigned-integer v)
		 state :number-of-bytes))
	 nil)
	(:number-of-bytes
	 (alexandria:when-let ((v (funcall four-byte-fn stream)))
	   (setf number-of-bytes (decode-32-bit-unsigned-integer v)
		 payload-fn (make-payload-accumulator-function number-of-bytes)
		 state :payload))
	 nil)
	(:payload
	 (alexandria:when-let ((v (funcall payload-fn stream)))
	   (setf state :magic-header
		 payload-fn nil)
	   (values v identifier)))))))
