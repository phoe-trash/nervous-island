;;;; src/armies/sharrash.lisp

(uiop:define-package #:nervous-island.armies.sharrash
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:move #:paralysis #:hole)
  (:export
   #:army #:hq
   #:plague #:beast #:mutant #:mortar #:rats #:explosive
   #:underworlds #:landfill #:officer #:medic #:transport #:scout #:mother
   #:hole
   #:battle #:move #:paralysis))

(in-package #:nervous-island.armies.sharrash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :sharrash
   :designators '(hq
                  (plague 2) beast (mutant 3) (mortar 3) (rats 3) (explosive 4)
                  underworlds landfill officer (medic 2) (transport 2) scout mother
                  hole
                  (battle 5) (move 2) paralysis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (nsk:underground-castling))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit plague (nt:warrior)
  (na:melee :q) (na:melee :e) (nsk:initiative 1) (nsk:initiative 0)
  (nsk:mobility) (nsk:paralysis))

(nt:define-unit beast (nt:warrior)
  (na:melee :w 3) (nsk:initiative 2))

(nt:define-unit mutant (nt:warrior)
  (na:melee :w) (nsk:initiative 3) (nsk:paralysis))

(nt:define-unit mortar (nt:warrior)
  (na:ranged :w) (nsk:initiative 2) (nsk:initiative 1) (nsk:mortar))

(nt:define-unit rats (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (nsk:initiative 2))

(nt:define-unit explosive (nt:warrior)
  (na:explosive :w t) (nsk:initiative :before))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit underworlds (nt:module)
  (ne:undirected-underground) (nsk:underground))

(nt:define-unit landfill (nt:module)
  (ne:directed-paralysis :q) (ne:directed-paralysis :w) (nsk:underground))

(nt:define-unit officer (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w)
  (nsk:underground))

(nt:define-unit transport (nt:module)
  (ne:directed-recon-center :q) (ne:directed-recon-center :w) (nsk:underground))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :q) (ne:directed-medic :w) (nsk:underground))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :q) (ne:directed-scout :w) (nsk:underground))

(nt:define-unit mother (nt:module)
  (ne:directed-mother :q) (ne:directed-mother :w) (nsk:underground))
