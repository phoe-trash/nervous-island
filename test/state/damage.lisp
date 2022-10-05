;;;; test/state/damage.lisp

(in-package #:nervous-island/test)

;;; TODO

(define-class damage-test-tile (nt:tile) ())

(define-class damage-test-warrior (nt:warrior) ())

(define-class damage-test-attack (na:attack) ())

(define-class damage-test-instant (nt:instant) ())

(define-class damage-test-foundation (nt:foundation) ())

(define-class damage-test-token (nto:token) ())

(define-test damage-instantiation
  ;; Damage protocol class
  (let ((tile (make-instance 'damage-test-tile)))
    (fail (make-instance 'nd:damage))
    (fail (make-instance 'nd:damage :target tile))
    (fail (make-instance 'nd:damage :value 1))
    (fail (make-instance 'nd:damage :target tile :value tile))
    (fail (make-instance 'nd:damage :target 1 :value 1))
    (fail (make-instance 'nd:damage :target tile :value 1)
        p:protocol-object-instantiation))
  ;; Attack damage
  (let ((tile (make-instance 'damage-test-tile))
        (warrior (make-instance 'damage-test-warrior))
        (attack (make-instance 'damage-test-attack :direction :w))
        (instant (make-instance 'damage-test-instant))
        (foundation (make-instance 'damage-test-foundation))
        (token (make-instance 'damage-test-token)))
    (fail (make-instance 'nd:attack-damage :target tile :value 1
                                           :source tile :attack attack
                                           :direction :w)
        type-error)
    (fail (make-instance 'nd:attack-damage :target tile :value 1
                                           :source warrior :attack warrior
                                           :direction :w)
        type-error)
    (fail (make-instance 'nd:attack-damage :target tile :value 1
                                           :source warrior :attack attack
                                           :direction 42)
        type-error)
    (fail (make-instance 'nd:attack-damage :target tile :value 1
                                           :source instant :attack attack
                                           :direction :w)
        type-error)
    (fail (make-instance 'nd:attack-damage :target tile :value 1
                                           :source foundation :attack attack
                                           :direction :w)
        type-error)
    (fail (make-instance 'nd:attack-damage :target tile :value 1
                                           :source token :attack attack
                                           :direction :w)
        type-error)
    (true (make-instance 'nd:attack-damage :target tile :value 1
                                           :source warrior :attack attack
                                           :direction :w)))
  ;; Non-attack damage
  (let ((tile (make-instance 'damage-test-tile))
        (warrior (make-instance 'damage-test-warrior))
        (instant (make-instance 'damage-test-instant))
        (foundation (make-instance 'damage-test-foundation))
        (token (make-instance 'damage-test-token)))
    (fail (make-instance 'nd:non-attack-damage :target tile :value 1
                                               :source 42))
    (fail (make-instance 'nd:non-attack-damage :target tile :value 1
                                               :source warrior)
        type-error)
    (true (make-instance 'nd:non-attack-damage :target tile :value 1
                                               :source instant))
    (true (make-instance 'nd:non-attack-damage :target tile :value 1
                                               :source foundation))
    (true (make-instance 'nd:non-attack-damage :target tile :value 1
                                               :source token))))
