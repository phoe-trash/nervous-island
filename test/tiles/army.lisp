;;;; test/tiles/army.lisp

(in-package #:nervous-island/test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element

(ncom:define-class army-test-element-container (nr:element-container) ())

(ncom:define-class army-test-element (nr:element) ())

(ncom:define-class army-test-hq-element (nr:hq-element) ())

(define-test army-element-instantiation
  (let ((element (make-instance 'army-test-element)))
    (true (typep element 'nr:element)))
  (let ((container (make-instance 'army-test-element-container)))
    (true (typep container 'nr:element-container))
    (let ((element-1 (make-instance 'army-test-element))
          (hq-element-1 (make-instance 'army-test-hq-element))
          (element-2 (make-instance 'army-test-element :owner container))
          (hq-element-2 (make-instance 'army-test-hq-element :owner container)))
      (true (null (nr:owner element-1)))
      (true (null (nr:owner hq-element-1)))
      (is eqv container (nr:owner element-2))
      (is eqv container (nr:owner hq-element-2)))))

;; TODO add a second R-I call in all other tests, one that actually makes use of
;;      kwargs to be passed around
(define-test army-reinitialize
  (let* ((army (make-instance
                'army-test-army
                :hq-elements (list (make-instance 'army-test-hq-element))
                :elements (loop repeat 34
                                collect (make-instance 'army-test-element))))
         (name (nr:name army))
         (element-count (nr:element-count army))
         (hq-elements (nr:hq-elements army))
         (elements (nr:elements army)))
    (is eqv army (reinitialize-instance army))
    (is eqv name (nr:name army))
    (is eqv element-count (nr:element-count army))
    (is eqv hq-elements (nr:hq-elements army))
    (is eqv elements (nr:elements army))
    (let ((hq-elements (loop repeat 3
                             collect (make-instance 'army-test-hq-element)))
          (elements (loop repeat 32
                          collect (make-instance 'army-test-element))))
      (is eqv army (reinitialize-instance army :hq-elements hq-elements
                                               :elements elements))
      (is eqv name (nr:name army))
      (is eqv element-count (nr:element-count army))
      (is = 3 (length (nr:hq-elements army)))
      (is = 32 (length (nr:elements army))))))

(define-test army-element-copy
  (let* ((container-1 (make-instance 'army-test-element-container))
         (element-1 (make-instance 'army-test-element :owner container-1))
         (hq-element-1 (make-instance 'army-test-hq-element :owner container-1))
         (container-2 (make-instance 'army-test-element-container))
         (element-2 (ncom:copy element-1 :owner container-2))
         (hq-element-2 (ncom:copy hq-element-1 :owner container-2)))
    (is eqv element-1 element-2)
    (is eqv hq-element-1 hq-element-2)
    (is eqv container-1 (nr:owner element-1))
    (is eqv container-1 (nr:owner hq-element-1))
    (is eqv container-2 (nr:owner element-2))
    (is eqv container-2 (nr:owner hq-element-2))))

(define-test army-element-designator
  (flet ((test-true (x) (true (typep x 'nr:element-designator)))
         (test-false (x) (false (typep x 'nr:element-designator))))
    (mapc #'test-true
          '(army-test-element
            army-test-hq-element
            (army-test-element 1)
            (army-test-hq-element 1)
            (army-test-element 10)
            (army-test-hq-element 10)))
    (mapc #'test-false '(army-test-element-container test 42 "42"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

;;; TODO test armies with other sorts of kwarg combinations
(ncom:define-class army-test-army (nr:army) ()
  (:default-initargs :name :test-army))

(define-test army-instantiation
  (flet ((test (army &optional (nelements 35))
           (is eqv :test-army (nr:name army))
           (is = nelements (nr:element-count army))
           (let ((hq-elements (nr:hq-elements army)))
             (is = 1 (length hq-elements))
             (is eqv 'army-test-hq-element (type-of (first hq-elements)))
             (a:when-let ((owner (nr:owner (first hq-elements))))
               (is eqv army owner)))
           (let ((elements (nr:elements army)))
             (is = (1- nelements) (length elements))
             (is eqv 'army-test-element (type-of (first elements)))
             (dolist (element elements)
               (a:when-let ((owner (nr:owner element)))
                 (is eqv army owner)))))
         (make-elements (n)
           (loop repeat n collect (make-instance 'army-test-element)))
         (make (&rest args)
           (apply #'make-instance 'army-test-army args)))
    (test (make :designators '(army-test-hq-element (army-test-element 34))))
    (test (make :designators '(army-test-hq-element (army-test-element 9))
                :element-count 10)
          10)
    (test (make :hq-elements (list (make-instance 'army-test-hq-element))
                :elements (make-elements 34)))
    (test (make :hq-elements (list (make-instance 'army-test-hq-element))
                :elements (make-elements 9)
                :element-count 10)
          10)))

(define-test army-instantiation-negative
  (fail (make-instance
         'nr:army
         :name :test
         :hq-elements (list (make-instance 'army-test-hq-element))
         :elements (loop repeat 34
                         collect (make-instance 'army-test-element)))
      p:protocol-object-instantiation)
  (flet ((make (&rest args)
           (apply #'make-instance 'army-test-army args)))
    (fail (make :name 0) (or type-error program-error))
    (fail (make :element-count :zero) (or type-error program-error))
    (fail (make :hq-elements '(0)) (or type-error program-error))
    (fail (make :elements '(0)) (or type-error program-error))
    (fail (make :designators '(0)) type-error)
    (handler-bind ((style-warning #'muffle-warning))
      (fail (make :designators '(#:foo)) type-error)
      (fail (make :designators '(army-test-hq-element (army-test-element #:foo)))
          type-error)
      (fail (make :hq-elements '(#:foo)) (or type-error program-error))
      (fail (make :elements '((#:foo 34))) (or type-error program-error)))
    (fail (make :hq-elements (list (make-instance 'army-test-hq-element))
                :elements ())
        nr:element-count-error)))
