(defvar *word-features* (make-hash-table :test 'equal))
(defvar *total-seen* (make-hash-table))

(defun reset-seen ()
  (setf *word-features* (make-hash-table :test 'equal)
	*total-seen* (make-hash-table)))

(defrecord word-feature ()
  (seen (make-hash-table)))

(defun intern-feature (word)
  (or (gethash word *word-features*)
      (setf (gethash word *word-features*)
	    (make-instance 'word-feature))))

(defun get-numeric-hash (table key)
  (gethash key table 0))
(defun (setf get-numeric-hash) (value table key)
  (setf (gethash key table) value))

(defun train-word (word category)
  (incf (get-numeric-hash (seen (intern-feature word)) category))
  (incf (get-numeric-hash *total-seen* category)))

(defun train-words (words category)
  (mapc (lambda (word)
	  (train-word word category))
	words))


;;;-----

(defparameter *curr-string* nil)
(defparameter *curr-index* nil)

(defun next-char ()
  (when (< *curr-index* (length *curr-string*))
    (prog1 (schar *curr-string* *curr-index*)
      (incf *curr-index*))))

(defmacro with-parse-env (string &body body)
  `(let ((*curr-string* (copy-seq ,string))
	 (*curr-index* 0))
     ,@body))


;;;-----

(defun parse-token ()
  (let ((token (string-downcase 
		(with-output-to-string (s)
		  (loop for ch = (next-char)
			do (cond
			     ((not ch) (return-from parse-token :eof))
			     ((alpha-char-p ch) (write-char ch s))
			     (t (return))))))))
    (when (>= (length token) 3)
      token)))

(defun split-tokens (string)
  (with-parse-env string
    (loop for token = (parse-token)
	  while (not (eql token :eof))
	  if token
	  collect token)))

(defun train-document (string category)
  (train-words (split-tokens string) category))


;;;-----

(defun filter (predicate list)
  (loop for element in list
	if (funcall predicate element)
	  collect element))

(defun extract-known-features (document)
  (filter #'identity
	  (map 'list
	       (lambda (word)
		 (gethash word *word-features*))
	       (split-tokens document))))

(defun count-other (table category)
  (loop for key being each hash-key of table
	when (not (eql key category))
	sum (get-numeric-hash table key)))

(defun basic-probability (feature category)
  (let ((seen (seen feature)))
    (let ((category-probability (/ (get-numeric-hash seen category)
				   (max 1 (get-numeric-hash *total-seen* category))))
	  (other-probability (/ (count-other seen category)
				(max 1 (count-other *total-seen* category)))))
      (/ category-probability (+ category-probability other-probability)))))

(defun expected-probability ()
  (/ (loop for key being each hash-key of *total-seen*
	   count key)))

(defun weighted-probability (feature category
			     &optional
			       (base-probability (expected-probability))
			       (weight 1))
  (let* ((seen (seen feature))
	 (basic-probability (basic-probability feature category))
	 (total-seen (+ (get-numeric-hash seen category)
			(count-other seen category))))
    (/ (+ (* weight base-probability)
	  (* total-seen basic-probability))
       (+ total-seen weight))))

(defun logarithmic-combine (probabilities)
  (- (reduce #'+ (map 'list #'log probabilities))))

(defun score (document category)
  (logarithmic-combine
   (map 'list
	(lambda (feature) (- 1 (weighted-probability feature category)))
	(extract-known-features document))))


;;;-----

(defun rank (document)
  (let ((scores (sort (map 'list
			   (lambda (category)
			     (cons category
				   (score document category)))
			   '(:one :two :three :four :five :six :seven :eight :nine :ten))
		      #'>
		      :key #'cdr)))
    (values (caar scores)
	    scores)))
