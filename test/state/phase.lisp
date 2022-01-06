;;;; test/state/phase.lisp

(in-package #:nervous-island/test)

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
                     nph:before-final-battle nph:after-final-battle)))
    (dotimes (n 10)
      (flet ((make (class)
               (let ((initiative (make-instance 'nsk:initiative :value 1)))
                 (true (make-instance class :player player :number 1
                                            :initiative initiative)))))
        (mapc #'make '(nph:battle nph:final-battle))))
    (true (make-instance 'nph:end :number 42))
    (true (make-instance 'phase-test-with-initiatives
                         :number 42
                         :initiative (make-instance 'nsk:initiative :value 1)))
    (true (make-instance 'phase-test-player-phase :player player :number 1))))

(define-test phase-instantiation-negative
  (fail (make-instance 'nph:phase :number 1)
      p:protocol-object-instantiation)
  (fail (make-instance 'nph:battle-part :number 1)
      p:protocol-object-instantiation)
  (fail (make-instance 'nph:final :number 1)
      p:protocol-object-instantiation)
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
  (fail (make-instance 'phase-test-with-initiatives :number 1 :initiative '())
      type-error))
