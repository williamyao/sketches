;; An implementation of a character trie using cons cells.
;; A single node in the trie:

;; +--+--+
;; |  |  |--> alist of keys to nodes
;; +--+--+       |
;;  |            |
;;  v            v
;; data         +---+----+
;;              |key|node|
;;              +---+----+

(defun assoc1 (item alist &key (key #'identity) (test #'eql))
  (cdr (assoc item alist :key key :test test)))

(defun trie-get (trie keys &key (key #'identity) (test #'eql))
  (when trie
    (if keys
	(trie-get (assoc1 (car keys) (cdr trie)
			  :key key
			  :test test)
		  (cdr keys))
	(car trie))))

(defun trie-put (trie keys value &key (key #'identity) (test #'eql))
  (if keys
      (let* ((trie (if trie
		       (cons (car trie) (cdr trie))
		       (cons nil nil)))
	     (alist (cdr trie)))
	(rplacd trie
		(acons (car keys)
		       (trie-put (assoc1 (car keys) alist
					 :key key
					 :test test)
				 (cdr keys)
				 value)
		       (remove (car keys) alist :key #'car))))
      (cons value (cdr trie))))

(defmacro trie-putf (place keys value &key (key #'identity) (test #'eql))
  `(setf ,place (trie-put ,place ',keys ,value :key ,key :test ,test)))
