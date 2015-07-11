#|
 Macro for defining a record type.

 William Yao (c) 2015
|#

(defclass record () ()
  (:documentation "Base type for all classes defined with DEFRECORD."))

(defmacro defrecord (name (&rest superrecords) &body slots)

  "Defines a Record Type.

   Most classes are just lists of accessors, initargs, and initforms.
   DEFRECORD serves to abstract this and make it a little simpler.
   Consider:

   (defrecord dog () 
     breed (age 0) name (alive? t))

   versus

   (defclass dog ()
     ((breed  :accessor breed  :initarg :breed  :initform nil)
      (age    :accessor age    :initarg :age    :initform 0)
      (name   :accessor name   :initarg :name   :initform nil)
      (alive? :accessor alive? :initarg :alive? :initform t)))

   Essentially: As concise as DEFSTRUCT, but hooks into the same
   inheritance and generic function machinery that DEFCLASS does.

   Also defines an automatic shallow copy COPY-RECORD on the newly
   defined record type, which copies all superclass slots as well."
  
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
	 (defun ,name ,(map 'list #'intern slot-names)
	   (make-instance ',name
			  ,@(mapcan (lambda (slot-name)
				      `(,(intern slot-name :keyword)
					 ,(intern slot-name)))
				    slot-names)))
	 ',name))))

(defun transpose (list)
  (apply #'map 'list #'list list))
(defun normalize-slot (slot) (if (listp slot) slot (list slot nil)))

(defgeneric copy-record (record))
(defgeneric %copy-record (record instance)
  (:method-combination progn :most-specific-last))
