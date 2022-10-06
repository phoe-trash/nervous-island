;;;; src/armies/borgo.lisp

(uiop:define-package #:nervous-island.armies.doomsday-machine
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:push-back #:small-bomb)
  (:export
   #:army #:hq
   #:alpha-shooter #:gamma-shooter #:delta-shooter #:omega-shooter
   #:gauss-cannon
   #:tripler #:doom-net-fighter #:fireblast
   #:trap #:medic #:officer #:scout #:main-war-processor
   #:battle #:push-back #:small-bomb))

(in-package #:nervous-island.armies.doomsday-machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :doomsday-machine
   :designators '(hq
                  (alpha-shooter 2) gamma-shooter delta-shooter
                  (omega-shooter 4) (gauss-cannon 2)
                  (tripler 2) (doom-net-fighter 2) (fireblast 2)
                  trap (medic 5) (officer 2) (scout 2) (main-war-processor 2)
                  (battle 4) push-back small-bomb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0) (ne:undirected-rotation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit alpha-shooter (nt:warrior)
  (na:ranged :q) (na:ranged :e) (nsk:initiative 3))

(nt:define-unit gamma-shooter (nt:warrior)
  (na:ranged :w)
  (nsk:initiative 2) (nsk:initiative 1) (nsk:toughness)
  (nsk:armor :a) (nsk:armor :d))

(nt:define-unit delta-shooter (nt:warrior)
  (na:ranged :w 2) (nsk:initiative 1) (nsk:toughness))

(nt:define-unit omega-shooter (nt:warrior)
  (na:ranged :q) (na:ranged :w) (na:ranged :e)
  (nsk:initiative 2) (nsk:armor :s))

(nt:define-unit gauss-cannon (nt:warrior)
  (na:gauss-cannon :w) (nsk:initiative 1) (nsk:toughness))

(nt:define-unit tripler (nt:warrior)
  (nsk:redirection-input :a)
  (nsk:redirection-input :s)
  (nsk:redirection-input :d)
  (nsk:redirection-output :q)
  (nsk:redirection-output :w)
  (nsk:redirection-output :e)
  (nsk:toughness))

(nt:define-unit doom-net-fighter (nt:warrior)
  (nsk:redirection-input :a)
  (nsk:redirection-input :s)
  (nsk:redirection-input :d)
  (nsk:redirection-input :q)
  (nsk:redirection-output :w)
  (nsk:redirection-input :e)
  (nsk:net :w))

(nt:define-unit fireblast (nt:warrior)
  (nsk:redirection-input :a)
  (nsk:redirection-input :s)
  (nsk:redirection-input :d)
  (nsk:redirection-input :q)
  (nsk:redirection-output :w)
  (nsk:redirection-input :e)
  (nsk:armor :w)
  (nsk:toughness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit trap (nt:module)
  (ne:directed-trap :w))

(nt:define-unit medic (nt:module)
  (ne:long-range-directed-medic :w))

(nt:define-unit officer (nt:module)
  (ne:long-range-directed-ranged-officer :w))

(nt:define-unit main-war-processor (nt:module)
  (ne:long-range-directed-mother :w))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :w) (ne:directed-scout :d))
