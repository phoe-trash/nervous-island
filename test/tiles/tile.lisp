;;;; test/armies/tile.lisp

(in-package #:nervous-island.test)

(defclass tile-test-tile (nt:tile) ())

(define-test tile-instantiation
  (let ((tile (make-instance 'tile-test-tile)))
    (is eq nil (nt:owner tile)))
  (let* ((army (make-instance 'army-test-army))
         (tile (make-instance 'tile-test-tile :owner army)))
    (is eq army (nt:owner tile)))
  (fail (make-instance 'tile-test-tile :owner 42) type-error))

(define-test tile-skill-having)

(define-test tile-hq)
