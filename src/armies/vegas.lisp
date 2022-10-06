;;;; src/armies/vegas.lisp

(uiop:define-package #:nervous-island.armies.vegas
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:mine
                #:battle #:rotation #:push-back #:castling #:move #:sniper)
  (:export
   #:army #:hq
   #:mercenary #:bodyguard #:guard #:marksman
   #:medic #:scout #:agitator #:saboteur
   #:mine
   #:battle #:rotation #:push-back #:castling #:move #:sniper))

(in-package #:nervous-island.armies.vegas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :vegas
   :token-count 4
   :token-designators '((nto:takeover 4))
   :designators '(hq
                  (mercenary 2) (bodyguard 2) (guard 2) (marksman 2)
                  medic (scout 2) (agitator 3) saboteur
                  (mine 2)
                  (battle 5) (rotation 3) (push-back 3) (castling 2) (move 3)
                  sniper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (ne:directed-takeover :d)
  (nsk:initiative 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit mercenary (nt:warrior)
  (na:ranged :e 2) (nsk:initiative 1))

(nt:define-unit bodyguard (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (nsk:initiative 2))

(nt:define-unit guard (nt:warrior)
  (nsk:armor :q) (nsk:armor :w) (nsk:armor :e) (nsk:toughness))

(nt:define-unit marksman (nt:warrior)
  (na:ranged :e) (nsk:initiative 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit saboteur (nt:module)
  (ne:directed-saboteur :q) (ne:directed-saboteur :w) (ne:directed-saboteur :e)
  (ne:directed-saboteur :a) (ne:directed-saboteur :s) (ne:directed-saboteur :d))

(nt:define-unit agitator (nt:module)
  (ne:directed-takeover :w))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :w))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :w))
