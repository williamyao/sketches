(defclass champion ()
  ((effects :accessor effects :initform ())))

(defclass effect ()
  ((priority :accessor priority :initarg :priority)
   (fn       :accessor fn       :initarg :fn)
   (type     :accessor type     :initarg :type)))

(defclass attacker (champion)
  ((attack-damage :accessor attack-damage :initarg :attack-damage)
   (attack-speed  :accessor attack-speed  :initarg :attack-speed)
   (ability-power :accessor ability-power :initarg :ability-power)))

(defclass defender (champion)
  ((armor            :accessor armor            :initarg :armor)
   (magic-resistance :accessor magic-resistance :initarg :magic-resistance)))

(defclass action ()
  ((span :accessor span :initarg :span)
   (effect :accessor effect :initarg :effect)))

(defmacro define-constant (name val &optional doc)
  `(defconstant ,name (if (boundp ',name) ,name ,val)
                ,@(if doc (list doc))))

(define-constant +lucian+ (make-instance 'attacker
                            :attack-damage 103.0
                            :attack-speed  0.638
                            :ability-power 0.0))

(define-constant +maokai+ (make-instance 'defender
                            :armor            96.7
                            :magic-resistance 30.0))

(define-constant +block+ (make-instance 'effect
                           :type :pdef
                           :priority 0
                           :fn (lambda (amt) (1- amt))))


;;; functions

(defun add-effect (champion effect)
  (with-slots (effects) champion
    (push effect effects)
    (setf effects (sort effects #'> :key #'priority))))

(defun compose (&rest functions)
  (labels ((compose* (functions)
             (if (null functions)
               (lambda (x) x)
               (let ((fn (compose* (cdr functions))))
                 (lambda (x) (funcall (car functions)
                                      (funcall fn x)))))))
    (compose* functions)))

(defun filter (predicate list &key (key (lambda (x) x)))
  (labels ((filter* (list)
             (cond
               ((null list) ())
               ((funcall predicate (funcall key (car list)))
                 (cons (car list) (filter* (cdr list))))
               (t (filter* (cdr list))))))
    (filter* list)))

(defgeneric reduce-dmg (target amt type)
  (:documentation "Calculate damage dealt after resistances and such."))

(defmethod reduce-dmg ((target defender) (amt number) (type (eql :true)))
  amt)

(defmethod reduce-dmg ((target defender) (amt number) (type (eql :physical)))
  (funcall (apply #'compose (mapcar #'fn
                                    (filter (lambda (x) (eql x :pdef))
                                            (effects target)
                                            :key #'type)))
           (/ amt (1+ (/ (armor target) 100.0)))))
