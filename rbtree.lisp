(defrecord rbnode () color parent left right key data)
(defrecord rbtree () root sortfn)

(defmethod color ((object null)) :black)

(defmethod initialize-instance ((instance rbtree) &rest initargs &key)
  (declare (ignore initargs))
  (unless (sortfn instance)
    (error "You didn't supply a sorting function for a red-black tree!")))

(defmacro letrec ((&rest arguments) form &body body)
  `(labels ((recurse ,arguments ,form))
     ,@body))

(defun %new-rbnode (key data)
  (make-instance 'rbnode
		 :color :red
		 :key key
		 :data data))

(defun %rb-insert (tree key data)
   (with-slots (root sortfn) tree
     (let ((new-node (%new-rbnode key data)))
       (letrec
	   (node)
	   (with-slots (left right (key* key)) node
	     (cond ((funcall sortfn key key*)
		    (if left
			(recurse left)
			(progn (setf (parent new-node) node)
			       (setf left new-node))))
		   ((funcall sortfn key* key)
		    (if right
			(recurse right)
			(progn (setf (parent new-node) node)
			       (setf right new-node))))
		   (t (setf (data node) data)
		      :do-not-restructure)))
	 (recurse root)))))
  
(defun %left-rotate (root)
  (let ((pivot (right root)))
    (shiftf (parent pivot) (parent root) pivot)
    (shiftf (right root) (left pivot) root)
    (let ((grandparent (parent pivot)))
      (when grandparent
	(cond
	  ((eq (left grandparent) root)
	   (setf (left grandparent) pivot))
	  (t (setf (right grandparent) pivot)))))))

(defun %right-rotate (root)
  (let ((pivot (left root)))
    (shiftf (parent pivot) (parent root) pivot)
    (shiftf (left root) (right pivot) root)
    (let ((grandparent (parent pivot)))
      (when grandparent
	(cond
	  ((eq (left grandparent) root)
	   (setf (left grandparent) pivot))
	  (t (setf (right grandparent) pivot)))))))

(defun %rbnode-restructure (node)
  (let ((parent (parent node)))
    (unless (eql (color parent) :black) ;; NODE will only ever be red
      (let* ((grandparent (parent parent))
	     (uncle (if (eq parent (left grandparent))
			(right grandparent)
			(left grandparent))))
	(if (eql (color uncle) :red)
	    (progn (setf (color parent) :black
			 (color uncle) :black
			 (color grandparent) :red)
		   (%rbnode-restructure grandparent))
	    (cond
	      ((and (eq node (left parent))
		    (eq parent (left grandparent)))
	       (%right-rotate grandparent)
	       (setf (color parent) :black
		     (color grandparent) :red))
	      ((and (eq node (right parent))
		    (eq parent (right grandparent)))
	       (%left-rotate grandparent)
	       (setf (color parent) :black
		     (color grandparent) :red))
	      ((eq node (right parent))
	       (%left-rotate parent)
	       (%rbnode-restructure parent))
	      ((eq node (left parent))
	       (%right-rotate parent)
	       (%rbnode-restructure parent))))))))

(defun rb-insert (tree key data)
  (if (root tree)
      (let ((node (%rb-insert tree key data)))
	(unless (eql node :do-not-restructure)
	  (%rbnode-restructure node)
	  (when (eql (color (root tree)) :red)
	    (setf (color (root tree)) :black))))
      (setf (root tree)
	    (let ((new-node (%new-rbnode key data)))
	      (setf (color new-node) :black)
	      new-node)))
  tree)

(defun rb-find (tree key)
  (with-slots (root sortfn) tree
    (letrec (node)
	    (if node
		(with-slots (left right (key* key) data) node
		  (cond
		    ((funcall sortfn key key*) (recurse left))
		    ((funcall sortfn key* key) (recurse right))
		    (t (values data t))))
		(values nil nil))
      (recurse root))))


;;;-----

(defmethod print-object ((object rbnode) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "[~a] ~s:~s ~s ~s"
	    (color object)
	    (key object) (data object)
	    (left object) (right object))))
