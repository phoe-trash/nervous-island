;;;; test/state/phase.lisp

(in-package #:nervous-island.test)

(defclass phase-test-hq (nt:hq) ())

(defclass phase-test-warrior (nt:warrior) ())

(defclass phase-test-army (nr:army) ()
  (:default-initargs
   :name :test-army
   :hq-tiles '(phase-test-hq)
   :tiles '((phase-test-warrior 34))))

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
               (make-instance class :player player :number 1 :initiative n)))
        (true (make 'nph:battle))
        (true (make 'nph:final-full-board-battle))
        (true (make 'nph:final-battle))))
    (true (make-instance 'nph:end))))

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
  ;; TODO typechecks for PLAYER-PHASE and BATTLE
  )
