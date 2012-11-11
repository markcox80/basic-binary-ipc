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
