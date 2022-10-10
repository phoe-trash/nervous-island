;;;; src/armies/beasts.lisp

(uiop:define-package #:nervous-island.armies.beasts
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:battle #:move #:grab #:hunt)
  (:export
   #:army #:hq #:cerberus
   #:alpha #:hive #:the-ram #:acid-spit #:bug #:urchin #:vulture #:arachnid
   #:ranger #:officer #:scrabbler
   #:battle #:move #:grab #:hunt))

(in-package #:nervous-island.armies.beasts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :beasts
   :total-element-count 36
   :element-count 35
   :designators '(hq cerberus
                  alpha (hive 2) (the-ram 4) (acid-spit 2) (bug 2) (urchin 2)
                  (vulture 2) arachnid
                  (ranger 3) (officer 2) scrabbler
                  (battle 6) (move 2) (grab 3) hunt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ and Cerberus

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 1) (nsk:lair))

(nt:define-unit cerberus (nt:warrior)
  (na:friendly-fire-melee :w 2) (na:friendly-fire-melee :s 2)
  (nsk:initiative 2) (nsk:mobility) (nsk:toughness) (nsk:lair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit alpha (nt:warrior)
  (na:friendly-fire-melee :w) (nsk:initiative 4) (nsk:rotation))

(nt:define-unit hive (nt:warrior)
  (na:friendly-fire-melee :q) (na:friendly-fire-melee :w)
  (na:friendly-fire-melee :e)
  (na:friendly-fire-melee :s) (na:friendly-fire-melee :d)
  (nsk:initiative 2))

(nt:define-unit the-ram (nt:warrior)
  (na:friendly-fire-melee :q) (na:friendly-fire-melee :w 2)
  (na:friendly-fire-melee :e)
  (na:friendly-fire-melee :s) (nsk:initiative 2) (nsk:toughness))

(nt:define-unit acid-spit (nt:warrior)
  (na:ranged :q) (nsk:initiative 2))

(nt:define-unit bug (nt:warrior)
  (na:friendly-fire-melee :e) (na:friendly-fire-melee :d)
  (na:friendly-fire-melee :s)
  (nsk:initiative 1) (nsk:initiative :agony))

(nt:define-unit urchin (nt:warrior)
  (na:melee :q) (na:melee :a) (nsk:initiative 1) (nsk:initiative :agony))

(nt:define-unit vulture (nt:warrior)
  (na:melee :s) (nsk:initiative 1) (nsk:flying) (nsk:scavenger))

(nt:define-unit arachnid (nt:warrior)
  (nsk:friendly-fire-net :q) (nsk:friendly-fire-net :w)
  (nsk:friendly-fire-net :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit ranger (nt:module)
  (ne:directed-speed :w) (nsk:rotation) (nsk:toughness))

(nt:define-unit officer (nt:module)
  (ne:directed-melee-officer :w) (nsk:rotation))

(nt:define-unit scrabbler (nt:module)
  (ne:directed-grab :w) (nsk:rotation))
