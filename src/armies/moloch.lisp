;;;; src/armies/moloch.lisp

(uiop:define-package #:nervous-island.armies.moloch
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:move #:air-strike #:push-back)
  (:export
   #:army #:hq
   #:blocker #:hybrid #:gauss-cannon #:juggernaut #:hunter-killer #:clown
   #:armored-hunter #:armored-guard #:guard #:protector #:hornet #:net-fighter
   #:stormtrooper
   #:mother #:medic #:brain #:officer #:scout
   #:air-strike #:battle #:move #:push-back))

(in-package #:nervous-island.armies.moloch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :moloch
   :designators '(hq
                  (blocker 2) (hybrid 2) gauss-cannon juggernaut
                  (hunter-killer 2) clown (armored-hunter 2) armored-guard guard
                  protector hornet net-fighter stormtrooper
                  mother (medic 2) brain officer scout
                  air-strike (battle 4) move (push-back 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-ranged-officer :q) (ne:directed-ranged-officer :w)
  (ne:directed-ranged-officer :e) (ne:directed-ranged-officer :a)
  (ne:directed-ranged-officer :s) (ne:directed-ranged-officer :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit blocker (nt:warrior)
  (nsk:armor :w) (nsk:toughness 2))

(nt:define-unit hybrid (nt:warrior)
  (na:ranged :w) (nsk:initiative 3))

(nt:define-unit gauss-cannon (nt:warrior)
  (na:gauss-cannon :a) (nsk:initiative 1) (nsk:toughness))

(nt:define-unit juggernaut (nt:warrior)
  (na:melee :w 2) (na:ranged :e) (nsk:toughness)
  (nsk:initiative 1) (nsk:armor :w) (nsk:armor :a) (nsk:armor :d))

(nt:define-unit hunter-killer (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (na:melee :s) (nsk:initiative 3))

(nt:define-unit clown (nt:warrior)
  (na:melee :q) (na:melee :w) (nsk:initiative 2) (nsk:explosion) (nsk:toughness))

(nt:define-unit armored-hunter (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 2) (nsk:armor :q) (nsk:armor :w))

(nt:define-unit armored-guard (nt:warrior)
  (na:ranged :q) (na:ranged :e) (nsk:initiative 2) (nsk:armor :w))

(nt:define-unit guard (nt:warrior)
  (na:ranged :q) (na:ranged :w) (nsk:initiative 2))

(nt:define-unit protector (nt:warrior)
  (na:ranged :q) (na:ranged :w) (na:ranged :e) (nsk:initiative 1) (nsk:toughness))

(nt:define-unit hornet (nt:warrior)
  (na:melee :w 2) (nsk:initiative 2))

(nt:define-unit net-fighter (nt:warrior)
  (nsk:net :q) (nsk:net :w))

(nt:define-unit stormtrooper (nt:warrior)
  (na:ranged :w) (nsk:initiative 2) (nsk:initiative 1) (nsk:toughness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit mother (nt:module)
  (ne:directed-additional-initiative :w))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :w) (ne:directed-medic :a) (ne:directed-medic :d))

(nt:define-unit brain (nt:module)
  (ne:directed-melee-officer :w) (ne:directed-ranged-officer :w)
  (ne:directed-melee-officer :a) (ne:directed-ranged-officer :a)
  (ne:directed-melee-officer :d) (ne:directed-ranged-officer :d))

(nt:define-unit officer (nt:module)
  (ne:directed-ranged-officer :q) (ne:directed-ranged-officer :e)
  (ne:directed-ranged-officer :s))

(nt:define-unit scout (nt:module)
  (ne:directed-speed :w) (ne:directed-speed :a) (ne:directed-speed :d))
