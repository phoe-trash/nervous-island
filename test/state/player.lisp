;;;; test/state/player.lisp

(in-package #:nervous-island.test)

(define-class player-test-hq (nt:hq) ()
  (:default-initargs :starting-hit-points 42))

(define-class player-test-warrior (nt:warrior) ())

(define-class player-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :designators '(player-test-hq (player-test-warrior 34))))

(define-test player-instantiation
  (flet ((test (army-designator)
           (let* ((player (make-instance 'np:player :army army-designator))
                  (army (np:army player))
                  (hq (first (nr:hq-elements army)))
                  (hit-points (np:hit-points player))
                  (draw-pile (np:draw-pile player)))
             (is eq 42 (gethash hq hit-points))
             (is eq '() (np:hand player))
             (is a:set-equal (nr:elements army) draw-pile)
             (is eq '() (np:discard-pile player)))))
    (test (make-instance 'player-test-army))
    (test 'player-test-army)))

(define-test player-instantiation-negative
  ;; Type errors
  (let* ((army (make-instance 'player-test-army)))
    (fail (make-instance 'np:player :army 42) 'type-error)
    (fail (make-instance 'np:player :army army :hit-points '()) 'type-error)
    (fail (make-instance 'np:player :army army :hit-points :zero) 'type-error)
    (fail (make-instance 'np:player :army army :hand 42) 'type-error)
    (fail (make-instance 'np:player :army army :hand '(42)) 'type-error)
    (fail (make-instance 'np:player :army army :draw-pile 42) 'type-error)
    (fail (make-instance 'np:player :army army :draw-pile '(42)) 'type-error)
    (fail (make-instance 'np:player :army army :discard-pile 42) 'type-error)
    (fail (make-instance 'np:player :army army :discard-pile '(42))
        'type-error))
  ;; HQ hit points over limit
  (let* ((army (make-instance 'player-test-army))
         (hq (first (nr:hq-elements army)))
         (hit-points (a:plist-hash-table (list hq 43) :test #'eq)))
    (fail (make-instance 'np:player :army army :hit-points hit-points)
        'np:hq-hit-points-over-limit))
  ;; Mismatched HQ tiles
  (let* ((army (make-instance 'player-test-army))
         (hq-1 (first (nr:hq-elements army)))
         (hq-2 (make-instance 'player-test-hq))
         (hit-points (a:plist-hash-table (list hq-1 42 hq-2 42) :test #'eq)))
    (fail (make-instance 'np:player :army army :hit-points hit-points)
        'np:mismatched-hq-tiles))
  ;; Too many tiles in hand
  (let* ((army (make-instance 'player-test-army))
         (tiles (nr:elements army))
         (discard-pile (subseq tiles 0 1))
         (hand (subseq tiles 1 4))
         (draw-pile (subseq tiles 3)))
    (fail (make-instance 'np:player :army army :hand hand :draw-pile draw-pile
                                    :discard-pile discard-pile)
        np:too-many-tiles)))

(define-test player-edit-player
  (let* ((army (make-instance 'player-test-army))
         (hq (first (nr:hq-elements army)))
         (tiles (nr:elements army))
         (hit-points-1 (a:plist-hash-table (list hq 20)))
         (discard-pile-1 (subseq tiles 0 1))
         (hand-1 (subseq tiles 1 4))
         (draw-pile-1 (subseq tiles 4))
         (hit-points-2 (a:plist-hash-table (list hq 10)))
         (discard-pile-2 (subseq tiles 0 4))
         (hand-2 (subseq tiles 8 9))
         (draw-pile-2 (subseq tiles 9))
         (player-1 (make-instance 'np:player :army army :hand hand-1
                                             :hit-points hit-points-1
                                             :draw-pile draw-pile-1
                                             :discard-pile discard-pile-1)))
    (is eql hit-points-1 (np:hit-points player-1))
    (is eql hand-1 (np:hand player-1))
    (is eql draw-pile-1 (np:draw-pile player-1))
    (is eql discard-pile-1 (np:discard-pile player-1))
    (let ((player-2 (np:edit-player player-1
                                    :hit-points hit-points-2 :hand hand-2
                                    :draw-pile draw-pile-2
                                    :discard-pile discard-pile-2)))
      (isnt eql player-1 player-2)
      (is eq (np:army player-1) (np:army player-2))
      (is eql hit-points-2 (np:hit-points player-2))
      (is eql hand-2 (np:hand player-2))
      (is eql draw-pile-2 (np:draw-pile player-2))
      (is eql discard-pile-2 (np:discard-pile player-2)))))

(define-test player-edit-player-negative
  (let* ((army-1 (make-instance 'player-test-army))
         (army-2 (make-instance 'player-test-army))
         (player (make-instance 'np:player :army army-1)))
    (fail (np:edit-player player :army army-2) np:cannot-edit-army)))

;;; TODO test remaining-tiles
