(defclass record () ()
  (:documentation "Base type for all classes defined with DEFRECORD."))

(defmacro defrecord (name (&rest superrecords) &rest slots)
  (destructuring-bind (slot-names default-vals)
      (transpose (map 'list #'normalize-slot slots))
    
    (let ((slot-names (map 'list #'string slot-names)))
      `(progn
	 (defclass ,name ,(if superrecords superrecords '(record))
	   ,(loop for slot-name in slot-names
	       for default-val in default-vals
	       collect `(,(intern slot-name)
			  :accessor ,(intern slot-name)
			  :initarg  ,(intern slot-name :keyword)
			  :initform ,default-val)))
	 (defmethod %copy-record progn ((record ,name) (instance ,name))
	   ,@(loop for slot-name in slot-names
		collect `(setf (slot-value instance ',(intern slot-name))
			       (slot-value record   ',(intern slot-name)))))
	 (defmethod copy-record ((record ,name))
	   (let ((copy (make-instance ',name)))
	     (%copy-record record copy)
	     copy))
	 ',name))))

(defun transpose (list)
  (apply #'map 'list #'list list))
(defun normalize-slot (slot) (if (listp slot) slot (list slot nil)))

(defgeneric copy-record (record))
(defgeneric %copy-record (record instance)
  (:method-combination progn :most-specific-last))
