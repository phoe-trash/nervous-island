
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EQV

(defclass test-simple-class () ()
  (:metaclass class-with-value-semantics))

(defun test-simple-class ()
  (let ((instance-1 (make-instance 'test-simple-class))
        (instance-2 (make-instance 'test-simple-class)))
    (assert (not (eql instance-1 instance-2)))
    (assert (eqv instance-1 instance-2))))

(defclass test-class-with-one-slot-1 ()
  ((foo :initarg :foo :reader foo))
  (:metaclass class-with-value-semantics))

(defclass test-class-with-one-slot-2 ()
  ((foo :initarg :foo :reader foo))
  (:metaclass class-with-value-semantics))

(defun test-class-with-one-slot ()
  (handler-bind ((warning #'error))
    (let ((instance-1 (make-instance 'test-class-with-one-slot-1 :foo 42))
          (instance-2 (make-instance 'test-class-with-one-slot-1 :foo 42))
          (instance-3 (make-instance 'test-class-with-one-slot-1 :foo 24))
          (instance-4 (make-instance 'test-class-with-one-slot-1))
          (instance-5 (make-instance 'test-class-with-one-slot-1 :foo "42"))
          (instance-6 (make-instance 'test-class-with-one-slot-2 :foo 42)))
      (assert (not (eql instance-1 instance-2)))
      (assert (eqv instance-1 instance-2))
      (assert (not (eqv instance-1 instance-3)))
      (assert (not (eqv instance-1 instance-4)))
      (assert (handler-bind ((eqv-default-method-called #'muffle-warning))
                (not (eqv instance-1 instance-5))))
      (assert (handler-bind ((eqv-default-method-called #'muffle-warning))
                (not (eqv instance-1 instance-6)))))))

;; TODO nested classes with value semantics