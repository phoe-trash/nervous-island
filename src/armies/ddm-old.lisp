;;;; src/armies/borgo.lisp

(uiop:define-package #:nervous-island.armies.ddm-old
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:push-back #:grab #:reposition)
  (:export
   #:army #:hq
   #:alpha-shooter #:beta-shooter #:gamma-shooter #:delta-shooter
   #:omega-shooter
   #:tripler #:doom-net-fighter #:pusher
   #:medic #:officer #:scout #:main-war-processor
   #:battle #:push-back #:grab #:reposition))

(in-package #:nervous-island.armies.ddm-old)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :ddm-old
   :designators '(hq
                  (alpha-shooter 2) (beta-shooter 2) gamma-shooter delta-shooter
                  (omega-shooter 2)
                  (tripler 1) (doom-net-fighter 2) (pusher 2)
                  (medic 4) (officer 7) (scout 2) (main-war-processor 2)
                  (battle 3) push-back grab reposition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0) (ne:undirected-rotation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit alpha-shooter (nt:warrior)
  (na:ranged :q) (na:ranged :e) (nsk:initiative 3))

(nt:define-unit beta-shooter (nt:warrior)
  (na:ranged :w) (nsk:initiative 2) (nsk:toughness))

(nt:define-unit gamma-shooter (nt:warrior)
  (na:ranged :w) (nsk:initiative 2) (nsk:initiative 1))

(nt:define-unit delta-shooter (nt:warrior)
  (na:ranged :w 2) (nsk:initiative 1) (nsk:toughness))

(nt:define-unit omega-shooter (nt:warrior)
  (na:ranged :q) (na:ranged :w) (na:ranged :e)
  (nsk:initiative 2))

(nt:define-unit tripler (nt:warrior)
  (nsk:redirection-input :a)
  (nsk:redirection-input :s)
  (nsk:redirection-input :d)
  (nsk:redirection-output :q)
  (nsk:redirection-output :w)
  (nsk:redirection-output :e)
  (nsk:toughness))

(nt:define-unit doom-net-fighter (nt:warrior)
  (nsk:redirection-input :a)
  (nsk:redirection-input :s)
  (nsk:redirection-input :d)
  (nsk:redirection-input :q)
  (nsk:redirection-output :w)
  (nsk:redirection-input :e)
  (nsk:net :w)
  (nsk:grab))

(nt:define-unit pusher (nt:warrior)
  (nsk:redirection-input :a)
  (nsk:redirection-input :s)
  (nsk:redirection-input :d)
  (nsk:redirection-input :q)
  (nsk:redirection-output :w)
  (nsk:redirection-input :e)
  (nsk:armor :w)
  (nsk:toughness)
  (nsk:push-back))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit medic (nt:module)
  (ne:long-range-directed-medic :w))

(nt:define-unit officer (nt:module)
  (ne:long-range-directed-ranged-officer :w))

(nt:define-unit main-war-processor (nt:module)
  (ne:long-range-directed-additional-initiative :w))

(nt:define-unit scout (nt:module)
  (ne:long-range-directed-speed :w))
