;;;; src/armies/iron-gang.lisp

(uiop:define-package #:nervous-island.armies.iron-gang
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:order #:doubled-move)
  (:export
   #:army #:hq
   #:fanatic #:ranged-net-fighter #:lumberjack #:mountain #:biker #:berserker
   #:officer #:boss
   #:order #:doubled-move))

(in-package #:nervous-island.armies.iron-gang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :iron-gang
   :token-count 2
   :token-designators '((nto:ranged-net 2))
   :designators '(hq
                  (fanatic 3) (ranged-net-fighter 3) (lumberjack 5) (mountain 3)
                  (biker 4) berserker
                  (officer 4) boss
                  (order 9) (doubled-move 2))
   :discard '(berserker)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0) (nsk:chain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit fanatic (nt:warrior)
  (na:melee :q) (nsk:initiative 3) (nsk:chain) (nsk:explosives))

(nt:define-unit ranged-net-fighter (nt:warrior)
  (nsk:ranged-net :a) (nsk:toughness) (nsk:rotation))

(nt:define-unit lumberjack (nt:warrior)
  (na:melee :a) (na:melee :s) (nsk:armor :w) (nsk:armor :e)
  (nsk:initiative 2) (nsk:initiative 1) (nsk:chain))

(nt:define-unit mountain (nt:warrior)
  (na:melee :q) (na:melee :w 2) (na:melee :e)
  (nsk:armor :q) (nsk:armor :e) (nsk:armor :s)
  (nsk:toughness) (nsk:chain))

(nt:define-unit biker (nt:warrior)
  (nsk:armor :w) (nsk:chain) (nsk:mobility 2) (ne:directed-speed :s))

(nt:define-unit berserker (nt:warrior)
  (na:melee :q 2) (na:melee :e 2) (nsk:initiative 2) (nsk:sacrifice))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit officer (nt:module)
  (ne:directed-melee-officer :q)
  (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e))

(nt:define-unit boss (nt:module)
  (ne:undirected-hidden-activation))
