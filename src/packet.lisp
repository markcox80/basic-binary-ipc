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

