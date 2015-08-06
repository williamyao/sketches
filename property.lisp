;;;; Property pattern implementation.

(defconstant +removed+ :removed)

(defclass object ()
  ((prototype :accessor prototype :initarg :prototype :initform nil)
   (properties :accessor properties :initform (make-hash-table :test 'equal))))

(defun @ (object property-name)
  (when object
    (let ((property (gethash property-name (properties object))))
      (cond
	((eql property +removed+) nil)
	(property property)
	(t (@ (prototype object) property-name))))))

(defun (setf @) (value object property-name)
  (setf (gethash property-name (properties object))
	(if (null value) +removed+ value)))


