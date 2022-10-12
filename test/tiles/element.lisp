;;;; test/tiles/army.lisp

(in-package #:nervous-island.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element

(define-class element-test-element-container (nel:element-container) ())

(define-class element-test-element (nel:element) ())

(define-class element-test-hq-element (nel:hq-element) ())

(define-test element-instantiation
  (let ((element (make-instance 'element-test-element)))
    (true (typep element 'nel:element)))
  (let ((container (make-instance 'element-test-element-container :name 'test)))
    (true (typep container 'nel:element-container))
    (is eqv 'test (nel:name container))
    (let ((element-1 (make-instance 'element-test-element))
          (hq-element-1 (make-instance 'element-test-hq-element))
          (element-2 (make-instance 'element-test-element :owner container))
          (hq-element-2 (make-instance 'element-test-hq-element
                                       :owner container)))
      (true (null (nel:owner element-1)))
      (true (null (nel:owner hq-element-1)))
      (is eqv container (nel:owner element-2))
      (is eqv container (nel:owner hq-element-2)))))

(define-test element-copy
  (let* ((container-1 (make-instance 'element-test-element-container :name 'a))
         (element-1 (make-instance 'element-test-element :owner container-1))
         (hq-element-1 (make-instance 'element-test-hq-element
                                      :owner container-1))
         (container-2 (make-instance 'element-test-element-container :name 'a))
         (element-2 (ncom:copy element-1 :owner container-2))
         (hq-element-2 (ncom:copy hq-element-1 :owner container-2)))
    (is eqv container-1 container-2)
    (is eqv element-1 element-2)
    (is eqv hq-element-1 hq-element-2)
    (is eqv container-1 (nel:owner element-1))
    (is eqv container-1 (nel:owner hq-element-1))
    (is eqv container-2 (nel:owner element-2))
    (is eqv container-2 (nel:owner hq-element-2))))

(define-test element-designator
  (flet ((test-true (x) (true (typep x 'nel:element-designator)))
         (test-false (x) (false (typep x 'nel:element-designator))))
    (mapc #'test-true
          '(element-test-element
            element-test-hq-element
            (element-test-element 1)
            (element-test-hq-element 1)
            (element-test-element 10)
            (element-test-hq-element 10)))
    (mapc #'test-false '(element-test-element-container test 42 "42"))))
