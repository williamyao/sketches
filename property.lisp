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
  (setf (gethash property-name (properties object)) value))

(defun remove@ (object property-name)
  (setf (@ object property-name) +removed+))

(defun %copy (object new-object)
  (when object
    (%copy (prototype object) new-object)
    (loop for key being each hash-key of (properties object)
	    using (hash-value value)
	  do (unless (eql value +removed+)
	       (setf (@ new-object key) value)))))

;;; as an odd consequence of the way they're written,
;;; COPY, COPY-WITH, PROTOTYPE, and PROTOTYPE-WITH
;;; all return empty objects when called with a unary NIL.

;;; a useful idiom for shortening the creation of new
;;; objects...?
(defun copy (object)
  "Return a newly-created `object' based on OBJECT and its prototype
chain. All properties are newly set; the returned `object' will not see
changes in the copied `object' or its prototype chain."
  (let ((new-object (make-instance 'object)))
    (%copy object new-object)
    new-object))

(defun copy-with (object &rest keys-and-values)
  "Return a newly-created `object' as by COPY, but with the
specified slots set to the specified values."
  (let ((new-object (copy object)))
    (loop for (key value) on keys-and-values by #'cddr
	  do (setf (@ new-object key) value))
    new-object))

(defun prototype (object)
  "Return a newly-created `object' that prototypes from
OBJECT."
  (make-instance 'object :prototype object))

(defun prototype-with (object &rest keys-and-values)
  "Return a newly-created `object' with the specified slots set to
the specified values, with its PROTOTYPE set to OBJECT i.e. the
returned object will see changes in the input object and its
prototype chain."
  (let ((new-object (prototype object)))
    (loop for (key value) on keys-and-values by #'cddr
	  do (setf (@ new-object key) value))
    new-object))
