;;;; src/armies/dancer.lisp

(uiop:define-package #:nervous-island.armies.dancer
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.attack)
                    (#:ne #:nervous-island.effect)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile))
  (:import-from #:nervous-island.tile
                #:mine #:battle #:move #:push-back #:action)
  (:export
   #:army #:object-blue #:object-red #:object-yellow
   #:battle #:move #:push-back #:action))

(in-package #:nervous-island.armies.dancer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Army

(define-class army (nr:army) ()
  (:default-initargs
   :name :dancer
   :hq-element-count 3
   :element-count 32
   :designators '(object-blue object-red object-yellow
                  (battle 8) (move 10) (push-back 7) (action 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Objects

(nt:define-unit object-blue (nt:object)
  (na:melee :q) (na:melee :w 2) (na:melee :e)
  (na:melee :a 2) (na:melee :d 2)
  (nsk:initiative 2)
  (ne:directed-ranged-officer :s))

(nt:define-unit object-red (nt:object)
  (na:ranged :q) (na:ranged :w 2) (na:ranged :e)
  (na:ranged :a 2) (na:ranged :d 2)
  (nsk:initiative 1)
  (ne:directed-melee-officer :s))

(nt:define-unit object-yellow (nt:object)
  (nsk:net :q) (na:ranged :w 2) (nsk:net :e)
  (ne:directed-scout :a) (ne:directed-scout :d)
  (nsk:initiative 0)
  (ne:directed-healing :s 2))
