;;;; src/armies/pirates.lisp

(uiop:define-package #:nervous-island.armies.pirates
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:battle #:move #:wave)
  (:export
   #:army #:hq
   #:motorboat #:fisherman #:harpoon
   #:crocodile #:helmsman #:smuggler #:bossman #:water-gun
   #:mother #:officer #:saboteur #:medic
   #:tavern
   #:battle #:move #:wave))

(in-package #:nervous-island.armies.pirates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :pirates
   :token-count 6
   :token-designators '((nto:water-adjacency 6))
   :designators '(hq
                  (motorboat 2) fisherman (harpoon 2)
                  (crocodile 2) (helmsman 3) (smuggler 3) bossman water-gun
                  (mother 2) (officer 2) (saboteur 3) (medic 2)
                  tavern
                  (battle 5) (move 3) wave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:ranged :e) (nsk:initiative 0) (nsk:drift))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit motorboat (nt:warrior)
  (na:ranged :q 2) (nsk:initiative 1) (nsk:armor :q) (nsk:drift))

(nt:define-unit fisherman (nt:warrior)
  (nsk:net :w) (nsk:drift))

(nt:define-unit harpoon (nt:warrior)
  (na:ranged :a) (na:melee :d 2) (nsk:initiative 2) (nsk:grab) (nsk:drift))

(nt:define-unit crocodile (nt:warrior)
  (na:melee :q) (nsk:initiative 1) (nsk:initiative 0) (nsk:charge))

(nt:define-unit helmsman (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :d) (nsk:initiative 1))

(nt:define-unit smuggler (nt:warrior)
  (na:melee :w 2) (na:ranged :e) (na:melee :w 2)
  (nsk:initiative 2))

(nt:define-unit bossman (nt:warrior)
  (na:melee :q) (nsk:initiative 0) (nsk:boarding))

(nt:define-unit water-gun (nt:warrior)
  (na:water-gun :w 3) (nsk:initiative 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules


(nt:define-unit mother (nt:module)
  (ne:directed-mother :q) (ne:directed-mother :w) (ne:directed-mother :e))

(nt:define-unit officer (nt:module)
  (ne:directed-melee-officer :q)
  (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e))

(nt:define-unit saboteur (nt:module)
  (ne:directed-saboteur :q) (ne:directed-saboteur :w) (ne:directed-saboteur :e))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :q) (ne:directed-medic :w) (ne:directed-medic :e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implants

(nt:define-implant tavern (nt:battle))
