#|
 Implementation of red-black trees.
|#

(require :optima)

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

(defun %node-find (node key sortfn default)
  (if node
      (with-slots ((key* key) data left right) node
	(cond ((funcall sortfn key key*) (%node-find left key sortfn default))
	      ((funcall sortfn key* key) (%node-find right key sortfn default))
	      (t (values data t))))
      (values default nil)))

(defun rb-find (rbtree key &optional default)
  (%node-find (root rbtree) key (sortfn rbtree) default))

(optima:defpattern rbnode (color key data left right)
  `(class rbnode :color ,color :key ,key :data ,data :left ,left :right ,right))

(defun %balance (root)
  (optima:match root
    ((or (rbnode :black z z* (rbnode :red y y* (rbnode :red x x* a b) c) d)
	 (rbnode :black z z* (rbnode :red x x* a (rbnode :red y y* b c)) d)
	 (rbnode :black x x* a (rbnode :red y y* b (rbnode :red z z* c d)))
	 (rbnode :black x x* a (rbnode :red z z* (rbnode :red y y* b c) d)))
     (rbnode :red y y* (rbnode :black x x* a b) (rbnode :black z z* c d)))
    (_ root)))

(defun %node-insert (node key data sortfn)
  (if node
      (with-slots (color (key* key) (data* data) left right) node
	(cond ((funcall sortfn key key*)
	       (%balance (rbnode color key* data*
				 (%node-insert left key data sortfn) right)))
	      ((funcall sortfn key* key)
	       (%balance (rbnode color key* data*
				 left (%node-insert right key data sortfn))))
	      (t (rbnode color key data left right))))
      (rbnode :red key data nil nil)))

(defun rb-insert (tree key data)
  (with-slots (root sortfn) tree
    (setf (root tree)
	  (optima:match (%node-insert root key data sortfn)
	    ((rbnode :red key data left right)
	     (rbnode :black key data left right))
	    (other other)))))

