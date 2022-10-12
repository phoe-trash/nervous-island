;;;; test/tiles/army.lisp

(in-package #:nervous-island.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army-test-element-container (nel:element-container) ())

(define-class army-test-element (nel:element) ())

(define-class army-test-hq-element (nel:hq-element) ())

(define-class army-test-army (nr:army) ()
  (:default-initargs :name :test-army))

(define-test army-instantiation
  (flet ((test (army &optional (nelements 35))
           (is eqv :test-army (nel:name army))
           (is = nelements (nr:total-element-count army))
           (let ((hq-elements (nr:hq-elements army)))
             (is = 1 (length hq-elements))
             (is eqv 'army-test-hq-element (type-of (first hq-elements)))
             (a:when-let ((owner (nel:owner (first hq-elements))))
               (is eqv army owner)))
           (let ((elements (nr:elements army)))
             (is = (1- nelements) (length elements))
             (is eqv 'army-test-element (type-of (first elements)))
             (dolist (element elements)
               (a:when-let ((owner (nel:owner element)))
                 (is eqv army owner)))))
         (make-elements (n)
           (loop repeat n collect (make-instance 'army-test-element)))
         (make (&rest args)
           (apply #'make-instance 'army-test-army args)))
    (test (make :designators '(army-test-hq-element (army-test-element 34))))
    (test (make :designators '(army-test-hq-element (army-test-element 9))
                :total-element-count 10 :element-count 9)
          10)
    (test (make :hq-elements (list (make-instance 'army-test-hq-element))
                :elements (make-elements 34)))
    (test (make :hq-elements (list (make-instance 'army-test-hq-element))
                :elements (make-elements 9)
                :total-element-count 10
                :element-count 9)
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

(define-test army-reinitialize
  (let* ((army (make-instance
                'army-test-army
                :hq-elements (list (make-instance 'element-test-hq-element))
                :elements (loop repeat 34
                                collect (make-instance
                                         'element-test-element))))
         (name (nel:name army))
         (element-count (nr:total-element-count army))
         (hq-elements (nr:hq-elements army))
         (elements (nr:elements army)))
    (is eqv army (reinitialize-instance army))
    (is eqv name (nel:name army))
    (is eqv element-count (nr:total-element-count army))
    (is eqv hq-elements (nr:hq-elements army))
    (is eqv elements (nr:elements army))
    (let ((hq-elements (loop repeat 3
                             collect (make-instance 'element-test-hq-element)))
          (elements (loop repeat 32
                          collect (make-instance 'element-test-element))))
      ;; TODO add a second R-I call in all other tests, one that actually makes
      ;;      use of kwargs to be passed around - like below
      (is eqv army (reinitialize-instance army :hq-elements hq-elements
                                               :hq-element-count 3
                                               :elements elements
                                               :element-count 32))
      (is eqv name (nel:name army))
      (is eqv element-count (nr:total-element-count army))
      (is = 3 (nr:hq-element-count army))
      (is = 3 (length (nr:hq-elements army)))
      (is = 32 (nr:element-count army))
      (is = 32 (length (nr:elements army))))))
