(defrecord rbnode () (color :red) key data left right)
(defrecord rbtree () root sortfn)

(defmethod initialize-instance :after ((instance rbtree) &rest initargs &key)
  (declare (ignore initargs))
  (unless (sortfn instance)
    (error "You need to specify a sorting function for a red-black tree!")))

(defparameter *colors* '(:red :black))

(defmethod initialize-instance :after ((instance rbnode) &rest initargs &key)
  (declare (ignore initargs))
  (unless (member (color instance) *colors*)
    (error "Color must be either RED or BLACK.")))

(defmethod (setf color) :before (color (node rbnode))
  (unless (member color *colors*)
    (error "Color must be either RED or BLACK.")))


;;;-----

(defun rb-find (rbtree key &optional default)
  (with-slots (root sortfn) rbtree
    (labels ((rb-find* (node)
	       (if node
		   (with-slots ((key* key) data left right) node
		     (cond
		       ((funcall sortfn key key*) (rb-find* left))
		       ((funcall sortfn key* key) (rb-find* right))
		       (t (values data t))))
		   (values default nil))))
      (rb-find* root))))
