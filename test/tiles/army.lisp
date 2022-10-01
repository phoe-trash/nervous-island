;;;; test/tiles/army.lisp

(in-package #:nervous-island/test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Element

(defclass army-test-element-container (nr:element-container) ())

(defclass army-test-element (nr:element) ())

(defclass army-test-hq-element (nr:hq-element) ())

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
      (is eq container (nr:owner element-2))
      (is eq container (nr:owner hq-element-2)))))

(define-test army-element-copy
  (let* ((container-1 (make-instance 'army-test-element-container))
         (element-1 (make-instance 'army-test-element :owner container-1))
         (hq-element-1 (make-instance 'army-test-hq-element :owner container-1))
         (container-2 (make-instance 'army-test-element-container))
         (element-2 (nr:copy-element element-1 :owner container-2))
         (hq-element-2 (nr:copy-element element-2 :owner container-2)))
    (isnt eq element-1 element-2)
    (isnt eq hq-element-1 hq-element-2)
    (is eq container-1 (nr:owner element-1))
    (is eq container-1 (nr:owner hq-element-1))
    (is eq container-2 (nr:owner element-2))
    (is eq container-2 (nr:owner hq-element-2))))

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
(defclass army-test-army (nr:army) ()
  (:default-initargs :name :test-army))

(define-test army-instantiation
  (flet ((test (army &optional (nelements 35))
           (is eq :test-army (nr:name army))
           (is = nelements (nr:element-count army))
           (let ((hq-elements (nr:hq-elements army)))
             (is = 1 (length hq-elements))
             (is eq 'army-test-hq-element (type-of (first hq-elements)))
             (is eq army (nr:owner (first hq-elements))))
           (let ((elements (nr:elements army)))
             (is = (1- nelements) (length elements))
             (is eq 'army-test-element (type-of (first elements)))
             (dolist (element elements)
               (is eq army (nr:owner element)))))
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
    (fail (make :name 0) 'type-error)
    (fail (make :element-count :zero) 'type-error)
    (fail (make :hq-elements '(0)) 'type-error)
    (fail (make :elements '(0)) 'type-error)
    (fail (make :hq-elements '(#:foo)))
    (fail (make :elements '((#:foo 34))))
    (fail (make :hq-elements (list (make-instance 'army-test-hq-element))
                :elements (loop repeat 34
                                collect (make-instance 'army-test-element))
                :element-count 36)
        'nr:element-count-error)))

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
    (is eq army (reinitialize-instance army))
    (is eq name (nr:name army))
    (is eql element-count (nr:element-count army))
    (is eq hq-elements (nr:hq-elements army))
    (is eq elements (nr:elements army))))
