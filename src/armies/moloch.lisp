;;;; src/armies/moloch.lisp

(uiop:define-package #:nervous-island.armies.moloch
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:ns #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.instant
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

(defclass army (nr:army) ()
  (:default-initargs
   :name :moloch
   :hq (make-instance 'hq)
   :tiles '((blocker 2) (hybrid 2) gauss-cannon juggernaut (hunter-killer 2)
            clown (armored-hunter 2) armored-guard guard protector hornet
            net-fighter stormtrooper
            mother (medic 2) brain officer scout
            air-strike (battle 4) move (push-back 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (ns:initiative 0)
  (ne:directed-ranged-officer :q) (ne:directed-ranged-officer :w)
  (ne:directed-ranged-officer :e) (ne:directed-ranged-officer :a)
  (ne:directed-ranged-officer :s) (ne:directed-ranged-officer :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit blocker (nt:warrior)
  (ns:armor :w) (ns:toughness) (ns:toughness))

(nt:define-unit hybrid (nt:warrior)
  (na:ranged :w) (ns:initiative 3))

(nt:define-unit gauss-cannon (nt:warrior)
  (na:gauss-cannon :a) (ns:initiative 1) (ns:toughness))

(nt:define-unit juggernaut (nt:warrior)
  (na:melee :w 2) (na:ranged :e) (ns:toughness)
  (ns:armor :w) (ns:armor :a) (ns:armor :d))

(nt:define-unit hunter-killer (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (na:melee :s) (ns:initiative 3))

(nt:define-unit clown (nt:warrior)
  (na:melee :q) (na:melee :w) (ns:initiative 2) (ns:explosion) (ns:toughness))

(nt:define-unit armored-hunter (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (ns:initiative 2) (ns:armor :q) (ns:armor :w))

(nt:define-unit armored-guard (nt:warrior)
  (na:ranged :q) (na:ranged :e) (ns:initiative 2) (ns:armor :w))

(nt:define-unit guard (nt:warrior)
  (na:ranged :q) (na:ranged :w) (ns:initiative 2))

(nt:define-unit protector (nt:warrior)
  (na:ranged :q) (na:ranged :w) (na:ranged :e) (ns:initiative 1) (ns:toughness))

(nt:define-unit hornet (nt:warrior)
  (na:melee :w 2) (ns:initiative 2))

(nt:define-unit net-fighter (nt:warrior)
  (ns:net :q) (ns:net :w))

(nt:define-unit stormtrooper (nt:warrior)
  (na:ranged :w) (ns:initiative 2) (ns:initiative 1) (ns:toughness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit mother (nt:module)
  (ne:directed-mother :w))

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
  (ne:directed-scout :w) (ne:directed-scout :a) (ne:directed-scout :d))
