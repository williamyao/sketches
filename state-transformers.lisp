;;; Monads in Lisp. We don't really _need_ typeclasses to take advantage of
;;; the conceptual elegance.

;; StateTransformer a :: State -> (a, State)
;; Obviously.

;; >identity :: a -> StateTransformer a
(defun >identity (obj)
  (lambda (state) (cons obj state)))

;; >bind :: StateTransformer a -> (a -> StateTransformer b) -> StateTransformer b
(defun >bind (st sf)
  (lambda (state)
    (destructuring-bind (val . state*) (funcall st state)
      (funcall (funcall sf val) state*))))

;; >get :: StateTransformer State
(defun >get ()
  (lambda (state) (cons state state)))

;; >put :: State -> StateTransformer ()
(defun >put (state)
  (lambda (state*)
    (declare (ignore state*))
    (cons nil state)))

;; >update :: (State -> State) -> StateTransformer ()
(defun >update (f)
  (>bind (>get)
         (lambda (state)
           (>put (funcall f state)))))

;; run-state :: StateTransformer a -> State -> a
(defun run-state (st s)
  (car (funcall st s)))

;; Proof of monad laws

;;; Left identity

;; (>bind (>identity x) f) =
;; (>bind (lambda (state) (cons x state)) f) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall (lambda (state) (cons x state)) state)
;;     (funcall (funcall f val) state*))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (cons x state)
;;     (funcall (funcall f val) state*))) =
;; (lambda (state)
;;   (funcall (funcall f x) state)) =
;; \state -> f x state =
;; f x =
;; (funcall f x)

;;; Right identity

;; (>bind m #'>identity) =
;; (>bind m (lambda (obj)
;;            (lambda (state) (cons obj state)))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall m state)
;;     (funcall (funcall (lambda (obj)
;;                         (lambda (state) (cons obj state)))
;;                       val)
;;              state*))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall m state)
;;     (funcall (lambda (state) (cons val state)) state*))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall m state)
;;     `(,val . ,state*))) =
;; (lambda (state)
;;   (funcall m state)) =
;; \state -> m state =
;; m

;;; Associativity

;; (>bind (>bind m f1) f2) =
;; (>bind (lambda (state)
;;          (destructuring-bind (val . state*) (funcall m state)
;;            (funcall (funcall f1 val) state*)))
;;        f2) =
;; (lambda (state)
;;   (destructuring-bind (val . state*)
;;       (funcall (lambda (state)
;;                  (destructuring-bind (val . state*) (funcall m state)
;;                    (funcall (funcall f1 val) state*)))
;;                state)
;;     (funcall (funcall f2 val) state*))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*)
;;       (destructuring-bind (val* . state**) (funcall m state)
;;         (funcall (funcall f1 val*) state**))
;;     (funcall (funcall f2 val state*)))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall m state)
;;     (destructuring-bind (val* . state**) (funcall (funcall f1 val) state*)
;;       (funcall (funcall f2 val*) state**)))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall m state)
;;     (funcall (lambda (state)
;;                (destructuring-bind (val* . state*) (funcall (funcall f1 val) state)
;;                  (funcall (funcall f2 val*) state*)))
;;              state*))) =
;; (lambda (state)
;;   (destructuring-bind (val . state*) (funcall m state)
;;     (funcall (funcall (lambda (x) (lambda (state)
;;                                     (destructuring-bind (val . state*) (funcall (funcall f1 x) state)
;;                                       (funcall (funcall f2 val) state*))))
;;                       val)
;;               state*))) =
;; (>bind m (lambda (x) (lambda (state)
;;                        (destructuring-bind (val . state*) (funcall (funcall f1 x) state)
;;                          (funcall (funcall f2 val) state*))))) =
;; (>bind m (lambda (x) (>bind (funcall f1 x) f2)))

;;;; QED.
