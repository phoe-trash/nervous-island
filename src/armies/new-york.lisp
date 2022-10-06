;;;; src/armies/new-york.lisp

(uiop:define-package #:nervous-island.armies.new-york
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:mine #:battle #:move #:push-back #:sniper)
  (:export
   #:army #:hq
   #:runner #:hmg #:commando #:annihilator #:mobile-armor #:brawler
   #:saboteur #:recon-center #:medic #:officer #:scoper #:scout
   #:battle #:move #:sniper))

(in-package #:nervous-island.armies.new-york)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :new-york
   :designators '(hq
                  spy-cleaner (spy-shooter 2) rocket-launcher shotgun
                  (hammer 2) (steel-boxer 2) (sharpshooter 2) (cop 2)
                  shooter pusher net-fighter
                  sergeant (officer-1 2) (officer-2 2) (scout 2)
                  (mine 2)
                  (battle 5) (move 2) push-back sniper)))

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

(nt:define-unit spy-cleaner (nt:warrior)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 2) (nsk:spy))

(nt:define-unit spy-shooter (nt:warrior)
  (na:ranged :q) (nsk:armor :w) (na:ranged :e)
  (nsk:initiative 2) (nsk:initiative 1))

(nt:define-unit rocket-launcher (nt:warrior)
  (na:rocket-launcher :w 3) (nsk:initiative 2))

(nt:define-unit shotgun (nt:warrior)
  (na:shotgun :w 3) (nsk:initiative 2))

(nt:define-unit hammer (nt:warrior)
  (na:melee :w 2) (nsk:initiative 2))

(nt:define-unit steel-boxer (nt:warrior)
  (na:melee :w) (na:melee :e) (nsk:initiative 2) (nsk:initiative 1))

(nt:define-unit sharpshooter (nt:warrior)
  (na:ranged :w) (nsk:initiative 2) (nsk:sharpshooter))

(nt:define-unit cop (nt:warrior)
  (na:melee :w) (nsk:initiative 3))

(nt:define-unit shooter (nt:warrior)
  (na:ranged :w) (nsk:initiative 3))

(nt:define-unit pusher (nt:warrior)
  (na:ranged :q) (na:ranged :w) (na:ranged :e)
  (nsk:armor :w) (nsk:initiative 3) (nsk:push-back) (nsk:toughness))

(nt:define-unit net-fighter (nt:warrior)
  (nsk:net :q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit sergeant (nt:module)
  (ne:directed-mother :a) (ne:directed-mother :w) (ne:directed-mother :d))

(nt:define-unit officer-1 (nt:module)
  (ne:directed-melee-officer :q)
  (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e))

(nt:define-unit officer-2 (nt:module)
  (ne:directed-ranged-officer :q)
  (ne:directed-ranged-officer :w)
  (ne:directed-ranged-officer :e))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :a) (ne:directed-scout :w)  (ne:directed-scout :d))
