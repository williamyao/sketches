#|
 Generating pseudowords with the characteristics of natural language.

 William Yao (c) 2015
|#

;; try it with
;; (process-file "words.txt")
;; (loop repeat 5 collect (gen-pseudoword 7))

(defclass sampler ()
  ((samples :accessor samples :initform (make-hash-table :test 'equal))
   (size    :accessor size    :initform 0)))

(defun sample-add (sampler obj)
  "Add a random sample to the sampler."
  (macrolet ((getsamp (obj) `(gethash ,obj (samples sampler))))
    (if (getsamp obj)
      (incf (getsamp obj))
      (setf (getsamp obj) 1))
    (incf (size sampler))))

(defun sample-get (sampler)
  "Get a random sample from the sampler."
  (loop with pos = (random (size sampler))
        for key being each hash-key of (samples sampler) using (hash-value val)
        do (if (> val pos)
             (return key)
             (decf pos val))))


(defmacro aif (condition clause1 clause2)
  "Anaphoric IF."
  `(let ((it ,condition))
     (if it
       ,clause1
       ,clause2)))


(defparameter character-adjacency (make-hash-table :test 'eql)
  "Association of characters to following characters.")
(defparameter character-beginning (make-instance 'sampler)
  "Frequency of characters that begin a word.")

(defparameter whitespace '(#\Space #\Tab #\Newline #\Return #\Rubout #\Page))


(defun adjacent-char (ch)
  (aif (gethash ch character-adjacency)
    (sample-get it)
    (sample-get character-beginning)))

(defun gen-pseudoword (length)
  (with-output-to-string (s)
    (loop repeat length
          for ch = (sample-get character-beginning) then (adjacent-char ch)
          do (write-char ch s))))

(defun process-word (word)
  (when (> (length word) 0)
    (sample-add character-beginning (char word 0))
    (loop for ch1 across (subseq word 0 (1- (length word)))
          for ch2 across (subseq word 1)
          do (aif (gethash ch1 character-adjacency)
               (sample-add it ch2)
               (setf (gethash ch1 character-adjacency)
                     (let ((sampler (make-instance 'sampler)))
                       (sample-add sampler ch2)
                       sampler))))))

(defun process-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil nil)
          while line
          do (process-word (string-trim whitespace line)))))


(defun hash-keys (hash-table)
  (loop for key being each hash-key of hash-table collect key))

(defmethod print-object ((obj hash-table) stream)
  (format stream "{~{~<~%~2:;~s: ~s~>~^, ~}}"
          (loop for key in (hash-keys obj)
                collect key
                collect (gethash key obj))))

(defmethod print-object ((obj sampler) stream)
  (print-object (samples obj) stream))
