;;;; Implementation of foldl and foldr, in Common Lisp.

(defgeneric foldl (fn init elements)
  (:documentation "Reduce FN acress ELEMENTS, starting from the left
of whatever the implementing structure is. Starts with INIT."))

(defmethod foldl (fn init (elements list))
  (if elements
      (foldl fn (funcall fn init (first elements)) (rest elements))
      init))

(defgeneric foldr (fn init elements)
  (:documentation "Reduce FN across ELEMENTS, starting from the right
of whatever the implementing structure is. Starts with INIT."))

(defmethod foldr (fn init (elements list))
  (if elements
      (funcall fn (first elements) (foldr fn init (rest elements)))
      init))
