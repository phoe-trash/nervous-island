;;;; src/armies/mississippi.lisp

(uiop:define-package #:nervous-island.armies.mississippi
  (:use #:nervous-island.cl)
  (:shadow #:shadow)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:toxic-bomb
                #:battle #:move #:smokescreen #:push-back #:transposition)
  (:export
   #:army #:hq
   #:shadow #:mutant #:poisoner #:guard #:hitman #:net-fighter
   #:paralysis #:mutation #:medic #:zone #:boiler
   #:toxic-bomb
   #:battle #:move #:smokescreen #:push-back #:transposition))

(in-package #:nervous-island.armies.mississippi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :mississippi
   :token-count 6
   :token-designators '((nto:venom 5) nto:toxic-bomb)
   :designators '(hq
                  (shadow 2) (mutant 3) (poisoner 2) (guard 4) hitman
                  net-fighter
                  (paralysis 2) (mutation 2) (medic 2) zone (boiler 3)
                  toxic-bomb
                  (battle 4) (move 3) smokescreen push-back transposition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0) (nsk:push-back))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit shadow (nt:warrior)
  (na:melee :anywhere) (nsk:initiative 1))

(nt:define-unit mutant (nt:warrior)
  (na:melee :q 2) (na:melee :e 2) (na:melee :s 2) (nsk:initiative 3))

(nt:define-unit monster (nt:warrior)
  (na:melee :a) (na:melee :q 2) (na:melee :w 3) (na:melee :e 2) (na:melee :d)
  (nsk:initiative 0) (nsk:toughness))

(nt:define-unit poisoner (nt:warrior)
  (na:poisoning :q) (na:poisoning :w) (nsk:initiative 3))

(nt:define-unit guard (nt:warrior)
  (na:ranged :q) (na:ranged :e) (nsk:initiative 2))

(nt:define-unit hitman (nt:warrior)
  (na:ranged :d) (nsk:initiative 2) (nsk:sharpshooter))

(nt:define-unit net-fighter (nt:warrior)
  (na:melee :w) (nsk:net :w) (nsk:initiative 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit paralysis (nt:module)
  (ne:directed-paralysis :w) (ne:directed-paralysis :e))

(nt:define-unit mutation (nt:module)
  (ne:directed-toughness :w 2) (ne:directed-toughness :e 2) (nsk:toughness))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :w) (ne:directed-medic :d))

(nt:define-unit zone (nt:module)
  (ne:directed-zone :q) (ne:directed-zone :w) (ne:directed-zone :e)
  (nsk:toughness))

(nt:define-unit boiler (nt:module)
  (ne:directed-venom :q) (ne:directed-venom :w) (ne:directed-venom :e))
