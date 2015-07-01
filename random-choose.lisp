(setf *random-state* (make-random-state t))

(destructuring-bind (n in out) ext:*args*
  (defparameter *to-take* (parse-integer n :junk-allowed t))
  (defparameter *in-file* in)
  (defparameter *out-file* out))

(defconstant +whitespace+ '(#\Space #\Tab #\Newline #\Return #\Rubout #\Page))

(defun do-the-knuth-shuffle (arr)
  "Shuffle the array in place. Returns the array for convenience."
  (let ((len (length arr)))
    (dotimes (i len)
      (let ((rand (+ i (random (- len i)))))
	(psetf (aref arr i) (aref arr rand)
	       (aref arr rand) (aref arr i)))))
  arr)

(with-open-file (in *in-file*)
  (with-open-file (out *out-file* :direction :output
		                  :if-exists :overwrite
				  :if-does-not-exist :create)
		  (let ((elements (make-array *to-take* :adjustable t :fill-pointer 0)))
		    (loop for line = (read-line in nil nil)
			  while line
			  do (vector-push-extend (string-trim +whitespace+ line) elements))
		    (map nil (lambda (val) (write-line val out))
			 (subseq (do-the-knuth-shuffle elements) 0 *to-take*)))))
