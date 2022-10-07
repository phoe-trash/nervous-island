;;;; src/armies/neojungle.lisp

(uiop:define-package #:nervous-island.armies.neojungle
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:roots #:battle #:castling #:move #:small-bomb)
  (:export
   #:army #:hq
   #:wall-of-trees
   #:crusher #:monster #:net-fighter #:nightshade #:swarm #:slicer
   #:medic #:vines #:symbiont-alpha #:symbiont-beta #:symbiont-gamma
   #:roots
   #:castling #:battle #:move #:small-bomb))

(in-package #:nervous-island.armies.neojungle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :neojungle
   :token-count 4
   :token-designators '((nto:venom 2) (nto:roots 2))
   :designators '(hq
                  wall-of-trees (crusher 3) monster (net-fighter 3)
                  (nightshade 2) (swarm 2) (slicer 4)
                  (medic 3) vines
                  symbiont-alpha symbiont-beta (symbiont-gamma 2)
                  (roots 2)
                  (castling 2) (battle 4) move small-bomb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-motherland :q) (ne:directed-motherland :w)
  (ne:directed-motherland :e) (ne:directed-motherland :a)
  (ne:directed-motherland :s) (ne:directed-motherland :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit wall-of-trees (nt:warrior)
  (nsk:armor :q) (nsk:armor :w) (nsk:armor :e) (nsk:toughness 2))

(nt:define-unit crusher (nt:warrior)
  (na:melee :w 2) (nsk:initiative 1))

(nt:define-unit monster (nt:warrior)
  (na:melee :a) (na:melee :q 2) (na:melee :w 3) (na:melee :e 2) (na:melee :d)
  (nsk:initiative 0) (nsk:toughness))

(nt:define-unit net-fighter (nt:warrior)
  (nsk:net :w))

(nt:define-unit nightshade (nt:warrior)
  (na:melee :q) (na:melee :w) (nsk:initiative 2) (nsk:venom))

(nt:define-unit swarm (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 1))

(nt:define-unit slicer (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (nsk:initiative 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit medic (nt:module)
  (ne:directed-medic :w))

(nt:define-unit vines (nt:module)
  (ne:directed-saboteur :q) (ne:directed-saboteur :w) (ne:directed-saboteur :e)
  (ne:directed-saboteur :a) (ne:directed-saboteur :s) (ne:directed-saboteur :d))

(nt:define-unit symbiont-alpha (nt:module)
  (ne:directed-melee-officer :w) (nsk:toughness) (nsk:armor :s))

(nt:define-unit symbiont-beta (nt:module)
  (ne:directed-scout :w) (nsk:toughness) (nsk:armor :s))

(nt:define-unit symbiont-gamma (nt:module)
  (ne:directed-scout :w) (ne:directed-melee-officer :w) (nsk:armor :s))
