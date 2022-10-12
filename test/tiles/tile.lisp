;;;; test/tiles/tile.lisp

(in-package #:nervous-island.test)

(define-class tile-test-tile (nt:tile) ())

(define-class tile-test-hq (nt:hq) ())

(define-class tile-test-warrior (nt:warrior) ())

(define-class tile-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :designators '(tile-test-hq (tile-test-warrior 34))))

(define-test tile-instantiation
  (let ((tile (make-instance 'tile-test-tile)))
    (is eqv nil (nel:owner tile)))
  (let* ((army (make-instance 'tile-test-army))
         (tile (make-instance 'tile-test-tile :owner army))
         (unowned-tile (make-instance 'tile-test-tile)))
    (is eqv army (nel:owner tile))
    (isnt eqv tile unowned-tile))
  (fail (make-instance 'tile-test-tile :owner 42) type-error))

(define-class tile-test-skill-1 (nsk:skill) ())

(define-class tile-test-skill-2 (nsk:skill) ())

(define-class tile-test-skill-having (nt:tile nsk:skill-having) ())

(define-test tile-skill-having
  (fail (make-instance 'tile-test-skill-having :skills 42) type-error)
  (fail (make-instance 'tile-test-skill-having :skills '(42)) type-error)
  (true (make-instance 'tile-test-skill-having))
  (true (make-instance 'tile-test-skill-having :skills (set)))
  (let* ((skill (make-instance 'tile-test-skill-1))
         (skill-having (make-instance 'tile-test-skill-having
                                      :skills (set skill))))
    (is = 1 (set-count (nsk:skills skill-having)))
    (is eqv skill (first (set-contents (nsk:skills skill-having))))))

(define-class tile-test-hq (nt:hq) ())

(define-test tile-hq
  (fail (make-instance 'tile-test-hq :starting-hit-points nil) type-error)
  (true (make-instance 'tile-test-hq))
  (true (make-instance 'tile-test-hq :starting-hit-points 42)))

;; TODO initiative tests

(define-test tile-skill-difference
  (let* ((skill-1 (make-instance 'tile-test-skill-1))
         (tile-1 (make-instance 'tile-test-warrior :skills (set skill-1)))
         (skill-2 (make-instance 'tile-test-skill-2))
         (tile-2 (make-instance 'tile-test-warrior :skills (set skill-2))))
    (isnt eqv tile-1 tile-2)))

(define-test tile-skill-order
  (let* ((skill-1 (make-instance 'tile-test-skill-1))
         (skill-2 (make-instance 'tile-test-skill-2))
         (tile-1 (make-instance 'tile-test-warrior
                                :skills (set skill-1 skill-2)))
         (tile-2 (make-instance 'tile-test-warrior
                                :skills (set skill-2 skill-1))))
    (is eqv tile-1 tile-2)))
