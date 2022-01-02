;;;; src/vsclass.lisp

(uiop:define-package #:value-semantics-class
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:m #:closer-mop))
  (:export
   ;; #EQV
   #:eqv-default-method-called #:eqv
   ;; CLASS-WITH-VALUE-SEMANTICS
   #:class-with-value-semantics
   ;; TYPECHECKED-CLASS
   #:typechecked-class #:typechecked-slot #:typechecked-direct-slot-definition
   #:typechecked-effective-slot-definition #:typecheck-function
   ;; TYPECHECKED-CLASS-WITH-VALUE-SEMANTICS
   #:typechecked-class-with-value-semantics))

(in-package #:value-semantics-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EQV

(defun sequence-eqv (x y)
  (and (= (length x) (length y)) (every #'eqv x y)))

(defun array-eqv (x y)
  (and (equal (array-dimensions x) (array-dimensions y))
       (loop for i below (array-total-size x)
             always (eqv (row-major-aref x i)
                         (row-major-aref y i)))))

(define-condition eqv-default-method-called (simple-warning) ()
  (:default-initargs
   :format-control
   "EQV default method called with ~S and ~S; possible type error?"
   :format-arguments (a:required-argument :arguments)))

(defgeneric eqv (x y)
  (:method (x y) (warn 'eqv-default-method-called :format-arguments (list x y)))
  (:method ((x function) (y function)) (eq x y))
  (:method ((x number) (y number)) (= x y))
  (:method ((x symbol) (y symbol)) (eq x y))
  (:method ((x string) (y string)) (string= x y))
  (:method ((x list) (y list)) (sequence-eqv x y))
  (:method ((x vector) (y vector)) (sequence-eqv x y))
  (:method ((x array) (y array)) (array-eqv x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLASS-WITH-VALUE-SEMANTICS

(defclass class-with-value-semantics (standard-class) ())

(defmethod m:validate-superclass
    ((class class-with-value-semantics) (superclass standard-class))
  t)

(defmethod m:validate-superclass
    ((class standard-class) (superclass class-with-value-semantics))
  t)

(defun slot-based-object-eqv (object-1 object-2)
  (let* ((slots (m:class-slots (class-of object-1)))
         (slot-names (mapcar #'m:slot-definition-name slots)))
    (dolist (slot-name slot-names t)
      (let ((boundp-1 (slot-boundp object-1 slot-name))
            (boundp-2 (slot-boundp object-2 slot-name)))
        (flet ((fail () (return nil)))
          (cond ((a:xor boundp-1 boundp-2) (fail))
                ((not (or boundp-1 boundp-2)))
                ((and boundp-1 boundp-2
                      (eqv (slot-value object-1 slot-name)
                           (slot-value object-2 slot-name))))
                (t (fail))))))))

(defmethod shared-initialize :after
    ((class class-with-value-semantics) slots &key)
  (let* ((method-lambda
           (m:make-method-lambda
            (m:class-prototype (find-class 'standard-generic-function))
            (m:class-prototype (find-class 'standard-method))
            '(lambda (x y) (slot-based-object-eqv x y))
            nil))
         (method (make-instance 'standard-method
                                :specializers (list class class)
                                :lambda-list '(object-1 object-2)
                                :function (compile nil method-lambda))))
    (add-method #'eqv method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPECHECKED-CLASS

(defclass typechecked-class (standard-class) ())

(defclass typechecked-slot (m:standard-slot-definition) ())

(defclass typechecked-direct-slot-definition
    (m:standard-direct-slot-definition typechecked-slot)
  ())

(defclass typechecked-effective-slot-definition
    (m:standard-effective-slot-definition typechecked-slot)
  ((typecheck-function :reader typecheck-function)))

(defmethod m:validate-superclass ((c typechecked-class) (s standard-class)) t)

(defmethod m:validate-superclass ((c standard-class) (s typechecked-class)) t)

(defmethod m:direct-slot-definition-class
    ((class typechecked-class) &key (type nil typep) &allow-other-keys)
  (declare (ignore type))
  (if typep
      (find-class 'typechecked-direct-slot-definition)
      (call-next-method)))

(defmethod m:effective-slot-definition-class
    ((class typechecked-class) &key &allow-other-keys)
  (find-class 'typechecked-effective-slot-definition))

(defmethod m:compute-effective-slot-definition
    ((class typechecked-class) name dsds)
  (let* ((esd (call-next-method))
         (type (m:slot-definition-type esd))
         (typecheck-function
           (compile nil `(lambda (,name) (check-type ,name ,type) ,name))))
    (setf (slot-value esd 'typecheck-function) typecheck-function)
    esd))

(defmethod (setf m:slot-value-using-class) :around
    (new-value (class typechecked-class) object
     (slot typechecked-effective-slot-definition))
  (setf new-value (funcall (typecheck-function slot) new-value))
  (call-next-method new-value class object slot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMMUTABLE-CLASS

;;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPECHECKED-CLASS-WITH-VALUE-SEMTANTICS

(defclass typechecked-class-with-value-semantics
    (typechecked-class class-with-value-semantics)
  ())
