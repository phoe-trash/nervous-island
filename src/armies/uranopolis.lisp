;;;; src/armies/uranopolis.lisp

(uiop:define-package #:nervous-island.armies.uranopolis
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:battle #:move #:push-back #:ray)
  (:export
   #:army #:hq
   #:mechanic #:mercenary
   #:electro-net-fighter #:inferno #:bulldozer #:guard #:drill
   #:hammerhead #:ravager
   #:acceleration-generator #:combat-generator #:medic #:doubler
   #:gauss-transformer #:transport #:wastes
   #:battle #:move #:push-back #:ray))

(in-package #:nervous-island.armies.uranopolis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :uranopolis
   :token-count 12
   :token-designators '((nto:no-power 12))
   :designators '(hq
                  (mechanic 3) mercenary
                  (electro-net-fighter 2) (inferno 4) bulldozer (guard 2) drill
                  hammerhead ravager
                  acceleration-generator (combat-generator 2) medic doubler
                  gauss-transformer (transport 2) wastes
                  (battle 4) move (push-back 3) ray)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-power-supply :q) (ne:directed-power-supply :w)
  (ne:directed-power-supply :e) (ne:directed-power-supply :a)
  (ne:directed-power-supply :s) (ne:directed-power-supply :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit mechanic (nt:warrior)
  (ne:directed-power-supply :q) (na:melee :w) (ne:directed-power-supply :e)
  (nsk:initiative 2) (nsk:mobility))

(nt:define-unit mercenary (nt:warrior)
  (na:melee :e) (nsk:initiative 3))

(nt:define-unit electro-net-fighter (nt:warrior)
  (nsk:net :q) (nsk:net :e) (nsk:powered))

(nt:define-unit inferno (nt:warrior)
  (na:ranged :a 2) (nsk:initiative 2) (nsk:toughness) (nsk:powered))

(nt:define-unit bulldozer (nt:warrior)
  (na:melee :w 2) (nsk:initiative 2) (nsk:push-back) (nsk:toughness)
  (nsk:powered))

(nt:define-unit guard (nt:warrior)
  (na:ranged :w) (na:ranged :e) (nsk:initiative 3) (nsk:powered))

(nt:define-unit drill (nt:warrior)
  (na:ranged :e) (nsk:initiative 3) (nsk:initiative 2) (nsk:initiative 1)
  (nsk:armor :q) (nsk:armor :s) (nsk:powered))

(nt:define-unit hammerhead (nt:warrior)
  (na:ranged :w 2) (nsk:initiative 1) (nsk:initiative 0) (nsk:toughness)
  (nsk:powered))

(nt:define-unit ravager (nt:warrior)
  (na:demolition :e) (na:demolition :d) (na:demolition :ed)
  (nsk:initiative 3) (nsk:powered))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit acceleration-generator (nt:module)
  (ne:directed-scout :q) (ne:directed-power-supply :q)
  (ne:directed-scout :e) (ne:directed-power-supply :e))

(nt:define-unit combat-generator (nt:module)
  (ne:directed-ranged-officer :q) (ne:directed-power-supply :q)
  (ne:directed-ranged-officer :w) (ne:directed-power-supply :w)
  (nsk:toughness))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :w) (ne:directed-medic :e) (ne:directed-medic :d))

(nt:define-unit gauss-transformer (nt:module)
  (ne:directed-quartermaster :w 'na:ranged 'na:gauss-cannon)
  (ne:directed-quartermaster :d 'na:ranged 'na:gauss-cannon))

(nt:define-unit transport (nt:module)
  (ne:directed-transport :w) (ne:directed-power-supply :w) (nsk:rotation))

(nt:define-unit doubler (nt:module)
  (ne:directed-mother :q) (ne:directed-mother :w))

(nt:define-unit wastes (nt:module)
  (ne:directed-wastes :q) (ne:directed-wastes :w) (ne:directed-wastes :e)
  (ne:directed-wastes :a) (ne:directed-wastes :s) (ne:directed-wastes :d)
  (nsk:toughness))
