;; See bottom for example usage.

(defconstant +ignore-sym+ '_)

(deftype constant ()
  '(or keyword number character null))

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
	 (acc  ()   (cons (take list n) acc)))
	((null list) (nreverse acc)))))

(defun dictionary (&rest keys-and-values)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (k . v) on keys-and-values by #'cddr
       do (setf (gethash k hash) v))
    hash))

(defun compile-match-clause (conditions bindings clause)
  `((and ,@conditions) (let (,@bindings)
			 ,clause)))

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
  (consn `(= (length ,place) ,(length values))
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
  (loop for (slot valplace) on values by #'cddr
     append (gen-match-bindings `(slot-value ,place ',slot) valplace)))

(defmacro match (place &body clauses)
  (let ((place-sym (gensym "PLACE")))
    `(let ((,place-sym ,place))
       (cond
	 ,@(loop for (spec clause) on clauses by #'cddr
	      collect (compile-match-clause (gen-match-conditions place-sym spec)
					    (gen-match-bindings place-sym spec)
					    clause))
	 (t (error "Fell through match case: ~s" ,place-sym))))))

;; example usage: defining recursive append

(defun append* (l1 l2)
  (match l1
    (cons head tail) (cons head (append* tail l2))
    ()               l2))
