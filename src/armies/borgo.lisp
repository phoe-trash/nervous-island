;;;; src/armies/borgo.lisp

(uiop:define-package #:nervous-island.armies.borgo
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.instant
                #:battle #:move #:grenade)
  (:export
   #:army #:hq
   #:mutant #:claws #:super-mutant #:net-fighter #:brawler #:assassin
   #:medic #:officer #:super-officer #:scout
   #:battle #:move #:grenade))

(in-package #:nervous-island.armies.borgo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(ncom:define-class army (nr:army) ()
  (:default-initargs
   :name :borgo
   :designators '(hq
                  (mutant 6) (claws 4) super-mutant (net-fighter 2) (brawler 2)
                  (assassin 2)
                  medic (officer 2) super-officer (scout 2)
                  (battle 6) (move 4) grenade)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-scout :q) (ne:directed-scout :w)
  (ne:directed-scout :e) (ne:directed-scout :a)
  (ne:directed-scout :s) (ne:directed-scout :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit mutant (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (nsk:initiative 2))

(nt:define-unit claws (nt:warrior)
  (na:melee :q) (na:melee :a) (nsk:initiative 3))

(nt:define-unit super-mutant (nt:warrior)
  (na:melee :q) (na:melee :w 2) (na:melee :e)
  (nsk:initiative 2) (nsk:toughness))

(nt:define-unit net-fighter (nt:warrior)
  (na:melee :d 3) (nsk:net :d) (nsk:initiative 1))

(nt:define-unit brawler (nt:warrior)
  (na:melee :w 2) (nsk:initiative 2))

(nt:define-unit assassin (nt:warrior)
  (na:ranged :q) (nsk:initiative 3) (nsk:mobility))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit medic (nt:module)
  (ne:directed-medic :q) (ne:directed-medic :w) (ne:directed-medic :e))

(nt:define-unit officer (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e))

(nt:define-unit super-officer (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e) (nsk:toughness))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :q) (ne:directed-scout :w) (ne:directed-scout :e))
