;;;; src/armies/sand-runners.lisp

(uiop:define-package #:nervous-island.armies.sand-runners
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:quicksands
                #:sandstorm #:move #:push-back)
  (:export
   #:army #:hq #:adapted-hq
   #:trigger #:optimist #:vulture #:hammer #:secateur
   #:field-medic-santa #:field-medic-lu
   #:mirage #:officer-1 #:officer-2 #:chieftain #:caravan
   #:quicksands
   #:sandstorm #:move #:push-back))

(in-package #:nervous-island.armies.sand-runners)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :sand-runners
   :token-count 2
   :token-designators '((nto:quicksands 2))
   :designators '(hq adapted-hq
                  trigger (optimist 3) (vulture 2) (hammer 3) (secateur 2)
                  field-medic-santa field-medic-lu
                  (mirage 2) (officer-1 2) officer-2 (chieftain 2) (caravan 2)
                  (quicksands 2)
                  (sandstorm 5) (move 3) (push-back 2))
   :discard '(adapted-hq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (nsk:adaptation 'adapted-hq))

(nt:define-unit adapted-hq (nt:hq)
  (nsk:armor :w) (na:melee :s 2) (nsk:initiative 2)
  (nsk:adaptation 'hq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit trigger (nt:warrior)
  (na:melee :q) (na:melee :s) (na:melee :e) (na:ranged :a 2)
  (nsk:initiative 3))

(nt:define-unit optimist (nt:warrior)
  (ne:directed-speed :q) (na:melee :a 3) (ne:directed-speed :s)
  (nsk:initiative 1) (nsk:mobility))

(nt:define-unit vulture (nt:warrior)
  (na:melee :q) (na:ranged :q) (na:melee :a) (na:ranged :a) (nsk:initiative 2))

(nt:define-unit hammer (nt:warrior)
  (na:ranged :a 2) (nsk:initiative 2) (nsk:toughness))

(nt:define-unit secateur (nt:warrior)
  (na:ranged :w 2) (na:ranged :d 2) (nsk:initiative 1))

(nt:define-unit field-medic (nt:warrior)
  (na:melee :a) (na:melee :q) (na:melee :w)
  (ne:directed-medic :s) (ne:directed-medic :e)
  (nsk:initiative 0))
(define-class field-medic-santa (field-medic) ())
(define-class field-medic-lu (field-medic) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit mirage (nt:module)
  (ne:directed-saboteur :q) (ne:directed-saboteur :w) (ne:directed-saboteur :e))

(nt:define-unit officer-1 (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w))

(nt:define-unit officer-2 (nt:module)
  (ne:directed-ranged-officer :q)
  (ne:directed-ranged-officer :e)
  (ne:directed-ranged-officer :s))

(nt:define-unit chieftain (nt:module)
  (ne:directed-new-initiative :q) (ne:directed-new-initiative :w))

(nt:define-unit caravan (nt:module)
  (ne:directed-mobility :q) (nsk:mobility))
