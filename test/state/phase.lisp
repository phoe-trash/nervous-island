;;;; test/state/phase.lisp

(in-package #:nervous-island.test)

(defclass phase-test-hq (nt:hq) ())

(defclass phase-test-warrior (nt:warrior) ())

(defclass phase-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :designators '(phase-test-hq (phase-test-warrior 34))))

(defclass phase-test-player-phase (nph:player-phase) ())

(defclass phase-test-with-initiatives (nph:with-initiatives) ())

(define-test phase-instantiation
  (let* ((army (make-instance 'phase-test-army))
         (player (make-instance 'np:player :army army)))
    (flet ((make (class)
             (true (make-instance class :player player :number 1))))
      (mapc #'make '(nph:start nph:draw-tiles nph:discard-tile nph:turn
                     nph:before-battle nph:after-battle
                     nph:final-draw-tiles nph:final-discard-tile nph:final-turn
                     nph:before-final-full-board-battle
                     nph:after-final-full-board-battle
                     nph:before-final-battle nph:after-final-battle)))
    (dotimes (n 10)
      (flet ((make (class)
               (let ((initiative (make-instance 'nsk:initiative :value 1)))
                 (true (make-instance class :player player :number 1
                                            :initiative initiative)))))
        (mapc #'make
              '(nph:battle nph:final-full-board-battle nph:final-battle))))
    (true (make-instance 'nph:end))
    (true (make-instance 'phase-test-with-initiatives
                         :initiative (make-instance 'nsk:initiative :value 1)))
    (true (make-instance 'phase-test-player-phase :player player :number 1))))

(define-test phase-instantiation-negative
  (fail (make-instance 'nph:phase) p:protocol-object-instantiation)
  (fail (make-instance 'nph:battle-part) p:protocol-object-instantiation)
  (fail (make-instance 'nph:final) p:protocol-object-instantiation)
  (fail (make-instance 'nph:final-full-board) p:protocol-object-instantiation)
  (let* ((army (make-instance 'phase-test-army))
         (player (make-instance 'np:player :army army)))
    (fail (make-instance 'nph:player-phase :player player
                                           :number 1)
        p:protocol-object-instantiation))
  (let* ((army (make-instance 'phase-test-army))
         (player (make-instance 'np:player :army army)))
    (fail (make-instance 'phase-test-player-phase))
    (fail (make-instance 'phase-test-player-phase :number 1))
    (fail (make-instance 'phase-test-player-phase :player 42 :number 1)
        type-error)
    (fail (make-instance 'phase-test-player-phase :player player :number '())
        type-error))
  (fail (make-instance 'phase-test-with-initiatives))
  (fail (make-instance 'phase-test-with-initiatives :initiative '())
      type-error))
