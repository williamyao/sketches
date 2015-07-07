#|
 OCaml style pattern matching

 William Yao (c) 2015
|#

(defconstant +ignore-sym+ '_)

(deftype constant ()
  '(or keyword number character null))

;; example usage: defining recursive append

;; (defun append* (l1 l2)
;;   (match l1
;;     (cons head tail) (cons head (append* tail l2))
;;     ()               l2))
(defmacro match (place &body clauses)
  "OCaml style pattern matching

   See: http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora016.html
   
   Matches constants and strings through equality and other objects
   structurally; allows for destructuring of arbitrary objects into
   variable bindings.

   Example:

   ```
   (match '(1 2)
     (list _) :list-of-one-element
     (list _ _) :list-of-two-elements
     (list _ _ _) :list-of-three-elements
     _ :other)
   
   ==> :LIST-OF-TWO-ELEMENTS

   (defclass some-class ((slot1 :initarg :slot1) (slot2 :initarg :slot2)))
   (match (make-instance 'some-class :slot1 t :slot2 nil)
     (some-class slot1 nil slot2 a) (list :slot1-is-false a)
     (some-class slot1 t   slot2 a) (list :slot1-is-true  a))

   ==> (:SLOT1-IS-TRUE NIL)
   ```

   The body of MATCH is pairs of match specifications and clauses.
   Non-keyword symbols are bound to their respective values, and other
   atomic values are used as conditional barriers. List specifications
   are used as structural indicators; the first value describes how to
   destructure. Currently only these destructuring options are explicitly
   defined:

   (CONS A B)
   (LIST ...)
   (VECTOR ...)

   For other list specifiers, the first element will be used as a class name
   and the rest of the elements form (slot-name value-spec) pairs.

   Specifications are recursive, i.e. the following is perfectly valid:
   ```
   (match ...
     (cons (list a b) (list c d)) (list a b c d))
   ```
   It will only match a cons with lists of two elements in both the car and
   cdr."
  (let ((place-sym (gensym "PLACE")))
    `(let ((,place-sym ,place))
       (cond
	 ,@(loop for (spec clause) on clauses by #'cddr
	      collect (compile-match-clause (gen-match-conditions place-sym spec)
					    (gen-match-bindings place-sym spec)
					    clause))
	 (t (error "Fell through match case: ~s" ,place-sym))))))


;;; To figure out which branch to take, MATCH turns each specification into a
;;; list of conditions that must be met for the branch to be chosen, and a
;;; list of variable bindings to effect if the branch is chosen.

;;; For example:

;;; (match '(1 . 2)
;;;   (cons 1 b) (+ 1 b)
;;;   _          0)

;;; is equivalent to

;;; (cond
;;;   ((and (consp '(1 . 2))
;;;         (= (car '(1 . 2)) 1))
;;;    (let ((b (cdr '(1 . 2))))
;;;      (+ 1 b)))
;;;   (t 0))

;;; (cons 1 b) is turned into a list of conditions, ((CONSP '(1 . 2)) (= (CAR '(1 . 2)) 1)),
;;; and a list of bindings, ((B (CDR '(1 . 2)))), and these are used to generate the
;;; conditional form for each clause.

(defun compile-match-clause (conditions bindings clause)
  `((and ,@conditions) (let (,@bindings)
			 ,clause)))

(defun gen-match-conditions (place valplace)
  (etypecase valplace
    (constant (list `(eql ,place ,valplace)))
    (string   (list `(string= ,place ,valplace)))
    (symbol   '())
    (list     (gen-list-match-conditions place
					 (first valplace)
					 (rest valplace)))))

(defgeneric gen-list-match-conditions (place designator values))

(defmethod gen-list-match-conditions (place (designator (eql 'cons)) values)
  (destructuring-bind (car cdr) values
    (cons `(consp ,place)
	  (append (gen-match-conditions `(car ,place) car)
		  (gen-match-conditions `(cdr ,place) cdr)))))

(defmethod gen-list-match-conditions (place (designator (eql 'list)) values)
  (consn `(listp ,place)
	 `(haslength ,place ,(length values))
	 (loop for valplace in values
	    for i upfrom 0
	    append (gen-match-conditions `(nth ,i ,place) valplace))))

(defmethod gen-list-match-conditions (place (designator (eql 'vector)) values)
  (consn `(vectorp ,place)
	 `(= (length ,place) ,(length values))
	 (loop for valplace in values
	    for i upfrom 0
	    append (gen-match-conditions `(aref ,place ,i) valplace))))

(defmethod gen-list-match-conditions (place designator values)
  (cons `(typep ,place ',designator)
	(loop for (slot value) on values by #'cddr
	   append (gen-match-conditions `(slot-value ,place ',slot) value))))

(defun gen-match-bindings (place valplace)
  (etypecase valplace
    (constant '())
    (string   '())
    (symbol   (if (eql valplace +ignore-sym+)
		  '()
		  (list `(,valplace ,place))))
    (list     (gen-list-match-bindings place
				       (first valplace)
				       (rest valplace)))))

(defgeneric gen-list-match-bindings (place designator values))

(defmethod gen-list-match-bindings (place (designator (eql 'cons)) values)
  (destructuring-bind (car cdr) values
    (append (gen-match-bindings `(car ,place) car)
	    (gen-match-bindings `(cdr ,place) cdr))))

(defmethod gen-list-match-bindings (place (designator (eql 'list)) values)
  (loop for valplace in values
     for i upfrom 0
     append (gen-match-bindings `(nth ,i ,place) valplace)))

(defmethod gen-list-match-bindings (place (designator (eql 'vector)) values)
  (loop for valplace in values
     for i upfrom 0
     append (gen-match-bindings `(aref ,place ,i) valplace)))

(defmethod gen-list-match-bindings (place designator values)
  (declare (ignore designator))
  (loop for (slot valplace) on values by #'cddr
     append (gen-match-bindings `(slot-value ,place ',slot) valplace)))


;;; utilities

(declaim (inline drop take))
(defun drop (list n)
  (nthcdr n list))
(defun take (list n)
  (loop for element in list
     repeat n
     collect element))

(defun group (list n)
  (when (not (zerop n))
    (do ((list list (drop list n))
	 (acc  '()  (cons (take list n) acc)))
	((null list) (nreverse acc)))))

(defun haslength (list n)
  "(= (LENGTH LIST) N), without the potential efficiency headaches"
  (cond
    ((and (zerop n) (null list)) t)
    ((or  (zerop n) (null list)) nil)
    (t (haslength (rest list) (- n 1)))))

(defun consn (&rest elements)
  "(CONSN A B C D) ≡ (CONS A (CONS B (CONS C (CONS D))))"
  (do ((elements elements (cdr elements))
       (acc      '()      (cons (car elements) acc)))
      ((null (cdr elements)) (append (nreverse acc) (car elements)))))
