;;;; test/tiles/attack.lisp

(in-package #:nervous-island/test)

(define-class attack-test-attack (na:attack) ())

(define-test attack-instantiation
  (fail (make-instance 'na:attack :direction :w)
      'p:protocol-object-instantiation)
  (fail (make-instance 'attack-test-attack :direction :w :strength 0)
      'type-error)
  (fail (make-instance 'attack-test-attack :direction :w :strength :true)
      'type-error)
  (true (make-instance 'attack-test-attack :direction :w :strength t))
  (true (make-instance 'attack-test-attack :direction :w))
  (true (make-instance 'na:melee :direction :w))
  (true (make-instance 'na:ranged :direction :w))
  (true (make-instance 'na:gauss-cannon :direction :w)))
