;;;; test/state/choice.lisp

(in-package #:nervous-island.test)

(define-test choice-instantiation
  (fail (make-instance 'nch:choice) 'p:protocol-object-instantiation))
