;;;; src/armies/smart.lisp

(uiop:define-package #:nervous-island.armies.smart
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:move #:push-back #:sniper #:terror)
  (:export
   #:army #:hq
   #:twister #:ripper #:gauss-cannon #:cyborg #:golem-mk3 #:bio-droid
   #:transporter #:net-fighter
   #:mother #:officer #:scout
   #:battle #:move #:push-back #:sniper #:terror))

(in-package #:nervous-island.armies.smart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :smart
   :designators '(hq
                  twister ripper (gauss-cannon 3) (cyborg 2) (golem-mk3 2)
                  bio-droid (transporter 3) (net-fighter 2)
                  (mother 2) (officer 4) (scout 2)
                  (battle 4) move (push-back 3) sniper (terror 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-mobility :q) (ne:directed-mobility :w)
  (ne:directed-mobility :e) (ne:directed-mobility :a)
  (ne:directed-mobility :s) (ne:directed-mobility :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit twister (nt:warrior)
  (na:melee :q) (na:melee :w 2) (na:melee :e) (na:melee :s)
  (nsk:initiative 2) (nsk:initiative 1))

(nt:define-unit ripper (nt:warrior)
  (na:melee :w 2) (nsk:initiative 2))

(nt:define-unit gauss-cannon (nt:warrior)
  (na:gauss-cannon :e) (nsk:initiative 2))

(nt:define-unit cyborg (nt:warrior)
  (na:ranged :e) (nsk:initiative 3))

(nt:define-unit golem-mk3 (nt:warrior)
  (na:ranged :d) (nsk:initiative 2))

(nt:define-unit bio-droid (nt:warrior)
  (na:ranged :e) (na:ranged :s) (nsk:initiative 3) (nsk:return))

(nt:define-unit transporter (nt:warrior)
  (nsk:armor :q) (nsk:armor :w) (nsk:toughness) (nsk:mobility) (nsk:open))

(nt:define-unit net-fighter (nt:warrior)
  (nsk:net :s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit mother (nt:module)
  (ne:directed-additional-initiative :w))

(nt:define-unit officer (nt:module)
  (ne:directed-ranged-officer :w))

(nt:define-unit scout (nt:module)
  (ne:directed-speed :w))
