;;;; A place to put things that I need, or that I think
;;;; I'll need, but don't really fit anywhere else.
;;;; A lot of these are stolen from other languages.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mklist (obj)
    "Place OBJ into a one-item list, if it's not already a list."
    (if (listp obj) obj (list obj))))

(defun partial (fn &rest args)
  "Return a new function by currying FN with ARGS."
  (if (null args)
      fn
      (lambda (&rest args2) (apply fn (append args args2)))))

(defun comp/2 (fn1 fn2)
  "Return the compose of two functions."
  (lambda (x) (funcall fn1 (funcall fn2 x))))

(defun comp (fn &rest fns)
  "Return the compose of an arbitrary number of functions."
  (labels ((comp* (acc fns)
             (if (null fns)
                 acc
                 (comp* (comp/2 (first fns) acc)
                        (rest fns)))))
    (let ((fns (reverse (cons fn fns))))
      (comp* (first fns) (rest fns)))))

(defun flip (fn)
  "Return a new binary function that does the same thing as FN, but
   with the argument order flipped."
  (lambda (x y) (funcall fn y x)))

(defgeneric foldl (fn init foldable)
  (:documentation "Accumulate a value of type INIT by continuously calling FN on
   the accumulator and successive values of SEQ. Tail recursive."))

(defmethod foldl (fn init (seq list))
  (if (null seq)
      init
      (foldl fn (funcall fn init (first seq)) (rest seq))))

(defmethod foldl (fn init (seq vector))
  (let ((length (length seq)))
    (labels ((foldl* (acc index)
               (if (= index length)
                   acc
                   (foldl* (funcall fn acc (aref seq index)) (+ index 1)))))
      (foldl* init 0))))

(defgeneric foldr (fn init foldable)
  (:documentation "Return a value of type INIT by recursively calling FN on
   INIT and values of SEQ, starting from the right of SEQ."))

(defmethod foldr (fn init (seq list))
  (if (null seq)
      init
      (funcall fn (first seq) (foldr fn init (rest seq)))))

(defmethod foldr (fn init (seq vector))
  (labels ((foldr* (acc index)
             (if (minusp index)
                 acc
                 (foldr* (funcall fn (aref seq index) acc) (- index 1)))))
    (foldr* init (- (length seq) 1))))

(defmacro -> (init &rest forms)
  "'Thread' return values through FORMS, starting with INIT.

   Places each return value into the second position of the following form,
   returning the value of the last form."
  (let ((forms (mapcar #'mklist forms)))
    (labels ((thread (acc forms)
               (if (null forms)
                   acc
                   (destructuring-bind (head &rest tail) (first forms)
                     (thread `(,head ,acc ,@tail) (rest forms))))))
      (if (not (every #'identity forms))
          (error "Erroneous NIL in -> body.")
          (thread init forms)))))

(defmacro ->> (init &rest forms)
  "'Thread' return values through FORMS, starting with INIT.

   Places each return value into the last position of the following form,
   returning the value of the last form."
  (let ((forms (mapcar #'mklist forms)))
    (labels ((thread (acc forms)
               (if (null forms)
                   acc
                   (thread `(,@(first forms) ,acc) (rest forms)))))
      (if (not (every #'identity forms))
          (error "Erroneous NIL in ->> body.")
          (thread init forms)))))
