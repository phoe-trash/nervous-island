;;;; src/armies/troglodytes.lisp

(uiop:define-package #:nervous-island.armies.troglodytes
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:battle #:move #:avalanche #:cannibalize-enemy)
  (:export
   #:army #:hq
   #:frost #:archer #:tundra #:icicle #:bear #:icy-mistress #:ice-monkey #:kids
   #:battle #:move #:avalanche #:cannibalize-enemy))

(in-package #:nervous-island.armies.troglodytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :troglodytes
   :token-count 14
   :token-designators '((nto:lungs 3) (nto:claws 3)
                        nto:eyes nto:fangs nto:muscles nto:heart
                        (nto:freezing 4))
   :designators '(hq
                  (frost 2) (archer 2) (tundra 4) (icicle 5)
                  (bear 3) (icy-mistress 2) (ice-monkey 2) (kids 2)
                  (battle 7) (move 3) avalanche cannibalize-enemy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-gourmet :q) (ne:directed-gourmet :w) (ne:directed-gourmet :e)
  (ne:directed-gourmet :a) (ne:directed-gourmet :s) (ne:directed-gourmet :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit frost (nt:warrior)
  (na:melee :q) (na:melee :d)
  (ne:directed-freezing :s) (ne:directed-freezing :e)
  (nsk:initiative 3) (nsk:cannibalism))

(nt:define-unit archer (nt:warrior)
  (na:ranged :aq) (nsk:initiative 3) (nsk:initiative 1)
  (nsk:thrower) (nsk:rotation) (nsk:cannibalism))

(nt:define-unit tundra (nt:warrior)
  (na:melee :a) (na:melee :q) (nsk:initiative 2) (nsk:toughness)
  (nsk:cannibalism))

(nt:define-unit icicle (nt:warrior)
  (na:melee :e) (na:melee :d) (na:melee :s) (nsk:initiative 2)
  (nsk:cannibalism))

(nt:define-unit bear (nt:warrior)
  (na:melee :q) (na:melee :s) (na:melee :e) (nsk:initiative 1)
  (nsk:bloodlust) (nsk:cannibalism))

(nt:define-unit icy-mistress (nt:warrior)
  (na:melee :e) (nsk:net :d) (na:melee :s) (nsk:initiative 1) (nsk:cannibalism))

(nt:define-unit ice-monkey (nt:warrior)
  (na:melee :q) (na:melee :w) (nsk:armor :e)
  (na:melee :a) (nsk:armor :s) (na:melee :d)
  (nsk:initiative 1) (nsk:cannibalism))

(nt:define-unit kids (nt:warrior)
  (nsk:armor :q) (na:ranged :aq) (nsk:armor :a) (nsk:initiative 1)
  (nsk:thrower) (nsk:push-back) (nsk:cannibalism))
