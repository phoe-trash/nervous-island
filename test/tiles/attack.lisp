;;;; test/tiles/attack.lisp

(in-package #:nervous-island.test)

(defclass attack-test-attack (na:attack) ())

(define-test attack-instantiation
  (fail (make-instance 'na:attack :direction :w)
      'p:protocol-object-instantiation)
  (fail (make-instance 'attack-test-attack :direction :w :strength 0)
      'type-error)
  (true (make-instance 'attack-test-attack :direction :w)))
