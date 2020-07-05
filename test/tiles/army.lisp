;;;; test/armies/army.lisp

(in-package #:nervous-island.test)

(defclass army-test-hq (nt:hq) ())

(defclass army-test-warrior (nt:warrior) ())

(defclass army-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :hq-tiles '(army-test-hq)
   :tiles '((army-test-warrior 34))))

(define-test army-instantiation
  (flet ((make () (make-instance 'nr:army :hq-tiles '() :name nil :tiles '())))
    (fail (make) p:protocol-object-instantiation))
  (let ((army (make-instance 'army-test-army)))
    (is eq :test-army (nr:name army))
    (is = 35 (nr:tile-count army))
    (let ((hq-tiles (nr:hq-tiles army)))
      (is = 1 (length hq-tiles))
      (is eq 'army-test-hq (type-of (first hq-tiles))))
    (let ((tiles (nr:tiles army)))
      (is = 34 (length tiles))
      (is eq 'army-test-warrior (type-of (first tiles)))))
  (fail (make-instance 'army-test-army :tile-count 36) 'nr:tile-count-error))
