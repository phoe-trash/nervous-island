;;;; src/armies/hegemony.lisp

(uiop:define-package #:nervous-island.armies.hegemony
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.instant
                #:battle #:move #:push-back #:sniper)
  (:export
   #:army #:hq
   #:runner #:thug #:ganger #:gladiator #:net-fighter #:net-master #:guard
   #:universal-soldier
   #:boss #:officer-1 #:officer-2 #:scout #:transport #:quartermaster
   #:battle #:move #:sniper #:push-back))

(in-package #:nervous-island.armies.hegemony)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(defclass army (nr:army) ()
  (:default-initargs
   :name :hegemony
   :designators '(hq
                  (runner 3) thug (ganger 4) gladiator (net-fighter 2)
                  net-master guard (universal-soldier 3)
                  boss (officer-1 2) officer-2 scout transport quartermaster
                  (battle 5) (move 3) sniper (push-back 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (na:melee :a) (na:melee :s) (na:melee :d)
  (nsk:initiative 0)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e) (ne:directed-melee-officer :a)
  (ne:directed-melee-officer :s) (ne:directed-melee-officer :d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Warriors

(nt:define-unit runner (nt:warrior)
  (na:melee :w) (nsk:initiative 2) (nsk:mobility))

(nt:define-unit thug (nt:warrior)
  (na:melee :q) (na:melee :w 2) (na:melee :e) (nsk:initiative 2))

(nt:define-unit ganger (nt:warrior)
  (na:melee :q) (nsk:initiative 3))

(nt:define-unit gladiator (nt:warrior)
  (na:melee :q 2) (na:melee :w 2) (na:melee :e 2)
  (nsk:armor :q) (nsk:armor :w) (nsk:armor :e)
  (nsk:initiative 1) (nsk:toughness))

(nt:define-unit net-fighter (nt:warrior)
  (nsk:net :e))

(nt:define-unit net-master (nt:warrior)
  (nsk:net :a) (nsk:net :d) (na:melee :s) (nsk:initiative 2))

(nt:define-unit guard (nt:warrior)
  (na:melee :a) (na:melee :q) (na:melee :w) (nsk:initiative 2) (nsk:toughness))

(nt:define-unit universal-soldier (nt:warrior)
  (na:ranged :q) (na:melee :q) (nsk:initiative 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit boss (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-scout :q)
  (ne:directed-melee-officer :w) (ne:directed-scout :w))

(nt:define-unit officer-1 (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w))

(nt:define-unit officer-2 (nt:module)
  (ne:directed-melee-officer :q) (ne:directed-melee-officer :w)
  (ne:directed-melee-officer :e))

(nt:define-unit scout (nt:module)
  (ne:directed-scout :q) (ne:directed-scout :w) (ne:directed-scout :e))

(nt:define-unit transport (nt:module)
  (ne:directed-transport :q) (ne:directed-transport :w)
  (ne:directed-transport :e) (ne:directed-transport :q)
  (ne:directed-transport :w) (ne:directed-transport :e))

(nt:define-unit quartermaster (nt:module)
  (ne:directed-quartermaster :w))
