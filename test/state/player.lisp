;;;; test/state/player.lisp

(in-package #:nervous-island.test)

(defclass player-test-hq (nt:hq) ()
  (:default-initargs :starting-hit-points 42))

(defclass player-test-warrior (nt:warrior) ())

(defclass player-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :hq-tiles '(player-test-hq)
   :tiles '((player-test-warrior 34))))

(define-test player-instantiation
  (let* ((army (make-instance 'player-test-army))
         (hq (first (nr:hq-tiles army)))
         (player (make-instance 'np:player :army army)))
    (is eq army (np:army player))
    (let ((hit-points (np:hit-points player)))
      (is eq 42 (gethash hq hit-points)))
    (is eq '() (np:hand player))
    (let ((draw-pile (np:draw-pile player)))
      (is = 34 (length draw-pile))
      (true (every (a:rcurry #'typep 'player-test-warrior) draw-pile)))
    (is eq '() (np:discard-pile player))))
