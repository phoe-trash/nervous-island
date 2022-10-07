;;;; src/armies/death-breath.lisp

(uiop:define-package #:nervous-island.armies.death-breath
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:battle #:move #:reappearance #:castling-with-the-opponent)
  (:export
   #:army #:hq
   #:infected #:gripper #:zombie #:anomaly #:mutant #:corpse #:devourer #:beast
   #:medic #:officer #:scout
   #:battle #:move #:reappearance #:castling-with-the-opponent))

(in-package #:nervous-island.armies.death-breath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :death-breath
   :token-count 7
   :token-designators '((nto:zombie 6) nto:tentacles)
   :designators '(hq
                  (infected 3) (gripper 3) zombie (anomaly 2) mutant corpse
                  devourer beast
                  (medic 2) (officer 2) (scout 3)
                  (battle 8) (move 3) (reappearance 2)
                  castling-with-the-opponent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0) (nsk:revival))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit infected (nt:warrior)
  (na:melee :w) (na:melee :e) (nsk:initiative 2) (nsk:initiative 0)
  (nsk:charge))

(nt:define-unit gripper (nt:warrior)
  (na:melee :w) (nsk:initiative 2) (nsk:tentacles :e) (nsk:rotation))

(nt:define-unit zombie (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (na:melee :a) (na:melee :d)
  (nsk:initiative 3))

(nt:define-unit anomaly (nt:warrior)
  (na:melee :d) (na:zombie-melee :d 2) (nsk:initiative 0))

(nt:define-unit mutant (nt:warrior)
  (na:melee :w) (na:zombie-melee :w) (nsk:tentacles :q) (nsk:tentacles :e)
  (nsk:initiative 2))

(nt:define-unit corpse (nt:warrior)
  (na:melee :w) (nsk:zombie-initiative 3) (nsk:initiative 2) (nsk:initiative 1)
  (nsk:charge))

(nt:define-unit devourer (nt:warrior)
  (na:zombie-melee :q) (na:melee :w) (na:zombie-melee :e)
  (nsk:initiative 1) (nsk:devouring))

(nt:define-unit beast (nt:warrior)
  (na:zombie-melee :q) (na:melee :q) (na:zombie-melee :w) (na:melee :w)
  (na:zombie-melee :e) (na:melee :e)
  (nsk:initiative 1) (nsk:charge) (nsk:toughness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit medic (nt:module)
  (ne:directed-medic :w) (ne:directed-medic :e))

(nt:define-unit officer (nt:module)
  (ne:directed-melee-officer :w) (ne:directed-melee-officer :e))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :a) (ne:directed-scout :w) (ne:directed-scout :d))
