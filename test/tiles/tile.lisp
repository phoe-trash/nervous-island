;;;; test/armies/tile.lisp

(in-package #:nervous-island.test)

(defclass tile-test-tile (nt:tile) ())

(defclass tile-test-hq (nt:hq) ())

(defclass tile-test-warrior (nt:warrior) ())

(defclass tile-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :hq-tiles '(tile-test-hq)
   :tiles '((tile-test-warrior 34))))

(define-test tile-instantiation
  (let ((tile (make-instance 'tile-test-tile)))
    (is eq nil (nt:owner tile)))
  (let* ((army (make-instance 'tile-test-army))
         (tile (make-instance 'tile-test-tile :owner army)))
    (is eq army (nt:owner tile)))
  (fail (make-instance 'tile-test-tile :owner 42) type-error))

(defclass tile-test-skill-having (nt:skill-having) ())

(define-test tile-skill-having
  (fail (make-instance 'tile-test-skill-having :skills 42) type-error)
  (fail (make-instance 'tile-test-skill-having :skills '(42)) type-error)
  (true (make-instance 'tile-test-skill-having))
  (true (make-instance 'tile-test-skill-having :skills '())))

(defclass tile-test-hq (nt:hq) ())

(define-test tile-hq
  (fail (make-instance 'tile-test-hq :starting-hp nil) type-error)
  (true (make-instance 'tile-test-hq))
  (true (make-instance 'tile-test-hq :starting-hp 42)))
