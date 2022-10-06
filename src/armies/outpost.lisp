;;;; src/armies/outpost.lisp

(uiop:define-package #:nervous-island.armies.outpost
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:move #:sniper)
  (:export
   #:army #:hq
   #:runner #:hmg #:commando #:annihilator #:mobile-armor #:brawler
   #:saboteur #:recon-center #:medic #:officer #:scoper #:scout
   #:battle #:move #:sniper))

(in-package #:nervous-island.armies.outpost)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :outpost
   :designators '(hq
                  (runner 2) hmg (commando 5) (annihilator 2) mobile-armor
                  brawler
                  saboteur recon-center (medic 2) officer scoper (scout 2)
                  (battle 6) (move 7) sniper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-mother :q) (ne:directed-mother :w)
  (ne:directed-mother :e) (ne:directed-mother :a)
  (ne:directed-mother :s) (ne:directed-mother :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit runner (nt:warrior)
  (na:melee :w) (nsk:initiative 2) (nsk:mobility))

(nt:define-unit hmg (nt:warrior)
  (na:ranged :w) (nsk:initiative 2) (nsk:initiative 1))

(nt:define-unit commando (nt:warrior)
  (na:ranged :d) (nsk:initiative 3))

(nt:define-unit annihilator (nt:warrior)
  (na:ranged :a 2) (nsk:initiative 2))

(nt:define-unit mobile-armor (nt:warrior)
  (na:ranged :q) (na:melee :w 2) (nsk:initiative 3) (nsk:initiative 2)
  (nsk:mobility))

(nt:define-unit brawler (nt:warrior)
  (na:melee :w 2) (nsk:initiative 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit saboteur (nt:module)
  (ne:directed-saboteur :q) (ne:directed-saboteur :w) (ne:directed-saboteur :e)
  (ne:directed-saboteur :a) (ne:directed-saboteur :s) (ne:directed-saboteur :d))

(nt:define-unit recon-center (nt:module)
  (ne:undirected-recon-center))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :q) (ne:directed-medic :w) (ne:directed-medic :e))

(nt:define-unit officer (nt:module)
  (ne:directed-ranged-officer :q) (ne:directed-ranged-officer :w)
  (ne:directed-ranged-officer :e) (ne:directed-ranged-officer :a)
  (ne:directed-ranged-officer :s) (ne:directed-ranged-officer :d))

(nt:define-unit scoper (nt:module)
  (ne:directed-scoper :q) (ne:directed-scoper :w) (ne:directed-scoper :e)
  (ne:directed-scoper :a) (ne:directed-scoper :s) (ne:directed-scoper :d))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :a) (ne:directed-scout :w) (ne:directed-scout :d))
