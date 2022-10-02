;;;; test/tiles/tile.lisp

(in-package #:nervous-island/test)

(ncom:define-class tile-test-tile (nt:tile) ())

(ncom:define-class tile-test-hq (nt:hq) ())

(ncom:define-class tile-test-warrior (nt:warrior) ())

(ncom:define-class tile-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :designators '(tile-test-hq (tile-test-warrior 34))))

(define-test tile-instantiation
  (let ((tile (make-instance 'tile-test-tile)))
    (is eq nil (nr:owner tile)))
  (let* ((army (make-instance 'tile-test-army))
         (tile (make-instance 'tile-test-tile :owner army)))
    (is eq army (nr:owner tile)))
  (fail (make-instance 'tile-test-tile :owner 42) type-error))

(ncom:define-class tile-test-skill (nsk:skill) ())

(ncom:define-class tile-test-skill-having (nt:skill-having) ())

(define-test tile-skill-having
  (fail (make-instance 'tile-test-skill-having :skills 42) type-error)
  (fail (make-instance 'tile-test-skill-having :skills '(42)) type-error)
  (true (make-instance 'tile-test-skill-having))
  (true (make-instance 'tile-test-skill-having :skills '()))
  (let* ((skill (make-instance 'tile-test-skill))
         (skill-having (make-instance 'tile-test-skill-having
                                      :skills (list skill))))
    (is = 1 (length (nt:skills skill-having)))
    (is eq skill (first (nt:skills skill-having)))))

(ncom:define-class tile-test-hq (nt:hq) ())

(define-test tile-hq
  (fail (make-instance 'tile-test-hq :starting-hit-points nil) type-error)
  (true (make-instance 'tile-test-hq))
  (true (make-instance 'tile-test-hq :starting-hit-points 42)))

;; TODO initiative tests
