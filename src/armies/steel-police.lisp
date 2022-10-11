;;;; src/armies/steel-police.lisp

(uiop:define-package #:nervous-island.armies.steel-police
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:mine #:battle #:move #:push-back #:terror)
  (:export
   #:army #:hq
   #:predator #:judge #:pacifier #:riot-policeman #:executioner #:bandog
   #:wardog
   #:net-of-steel-launcher #:officer #:sergeant #:scout #:saboteur #:medic
   #:steroids-dispender
   #:battle #:move #:push-back #:terror))

(in-package #:nervous-island.armies.steel-police)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :steel-police
   :token-count 1
   :token-designators '(nto:steel-net)
   :designators '(hq
                  predator (judge 3) (pacifier 2) (riot-policeman 2) executioner
                  bandog (wardog 2)
                  net-of-steel-launcher (officer 3) (sergeant 3) (scout 2)
                  (saboteur 2) (medic 2) steroids-dispender
                  (battle 5) move push-back terror)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (nsk:net-of-steel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit predator (nt:warrior)
  (na:melee :a) (na:ranged :q 2) (na:melee :w)
  (nsk:initiative 2))

(nt:define-unit judge (nt:warrior)
  (na:ranged :w) (nsk:mobility) (nsk:reflection :s)
  (nsk:initiative 1) (nsk:initiative 0))

(nt:define-unit riot-policeman (nt:warrior)
  (na:melee :q) (na:melee :w) (nsk:armor :w)
  (nsk:initiative 2) (nsk:push-back))

(nt:define-unit executioner (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e) (na:melee :s)
  (nsk:initiative 1) (nsk:initiative 1) (nsk:execution))

(nt:define-unit bandog (nt:warrior)
  (na:ranged :w) (nsk:armor :w) (nsk:initiative 3))

(nt:define-unit wardog (nt:warrior)
  (na:ranged :w) (na:ranged :e) (nsk:initiative 2))

(nt:define-unit pacifier (nt:warrior)
  (nsk:net :q) (nsk:open)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit net-of-steel-launcher (nt:module)
  (ne:undirected-net-of-steel-launcher))

(nt:define-unit officer (nt:module)
  (ne:directed-ranged-officer :q)
  (ne:directed-ranged-officer :w)
  (ne:directed-ranged-officer :e))

(nt:define-unit sergeant (nt:module)
  (ne:directed-additional-initiative :q)
  (ne:directed-additional-initiative :e))

(nt:define-unit scout (nt:module)
  (ne:directed-speed :a) (ne:directed-speed :w) (ne:directed-speed :d))

(nt:define-unit saboteur (nt:module)
  (ne:directed-saboteur :q) (ne:directed-saboteur :w) (ne:directed-saboteur :e)
  (ne:directed-saboteur :a) (ne:directed-saboteur :s) (ne:directed-saboteur :d))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :q) (ne:directed-medic :e))

(nt:define-unit steroids-dispender (nt:module)
  (ne:directed-melee-officer :q 2) (ne:directed-melee-officer :e 2))
