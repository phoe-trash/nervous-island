;;;; test/tiles/token.lisp

(in-package #:nervous-island.test)

(define-test token-instantiation
  (fail (make-instance 'nto:token) p:protocol-object-instantiation)
  (true (make-instance 'nto:damage))
  (true (make-instance 'nto:net)))
