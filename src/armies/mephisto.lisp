;;;; src/armies/mephisto.lisp

(uiop:define-package #:nervous-island.armies.mephisto
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:import-from #:nervous-island.tile
                #:battle #:move #:castling)
  (:export
   #:army #:hq
   #:probe #:muzzle #:claw #:accelerator #:toughener
   #:jaws #:incubator #:transmitter #:tail #:tentacles
   #:left-quill #:right-quill #:limbs #:drill
   #:battle #:move #:castling))

(in-package #:nervous-island.armies.mephisto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :mephisto
   :token-count 6
   :token-designators '(nto:accelerator nto:claw nto:attack-net nto:acid-thrower
                        nto:left-quill nto:right-quill)
   :designators '(hq
                  (probe 2) muzzle (claw 4) (accelerator 3) (toughener 2)
                  (jaws 2) (incubator 3) transmitter (tail 2) (tentacles 2)
                  left-quill right-quill (limbs 2) drill
                  (battle 3) (move 2) (castling 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HQ

(nt:define-unit hq (nt:hq)
  (na:melee :q) (na:melee :w) (na:melee :e)
  (nsk:initiative 1) (nsk:armor :s) (ne:undirected-rotation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules

(nt:define-unit probe (nt:module)
  (ne:undirected-implant-activation))

(nt:define-unit muzzle (nt:module)
  (ne:undirected-muzzle))

(nt:define-unit medic (nt:module)
  (ne:directed-medic :q) (ne:directed-medic :w) (ne:directed-medic :e))

(nt:define-unit claw (nt:module)
  (ne:undirected-melee-officer) (nsk:armor :s))

(nt:define-unit accelerator (nt:module)
  (ne:undirected-scout) (nsk:armor :s))

(nt:define-unit toughener (nt:module)
  (ne:directed-toughness :q) (ne:directed-toughness :w)
  (ne:directed-toughness :e) (ne:directed-toughness :a)
  (ne:directed-toughness :s) (ne:directed-toughness :d)
  (nsk:toughness))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implants

(nt:define-implant jaws (nt:battle))
(nt:define-implant incubator (nt:incubation))
(nt:define-implant transmitter (nt:castling))
(nt:define-implant tail (nt:push-back))
(nt:define-implant tentacles (nt:grab))
(nt:define-implant left-quill (nt:left-quill))
(nt:define-implant right-quill (nt:right-quill))
(nt:define-implant limbs (nt:move))
(nt:define-implant drill (nt:drill))
