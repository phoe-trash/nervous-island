;;;; test/package.lisp

(uiop:define-package #:nervous-island.test
  (:use #:nervous-island.cl
        #:parachute)
  (:local-nicknames
   ;; External packages
   (#:a #:alexandria)
   (#:p #:protest/base)
   ;; common/
   (#:ncom #:nervous-island.common)
   ;; tiles/
   (#:nel #:nervous-island.element)
   (#:nr #:nervous-island.army)
   (#:nsk #:nervous-island.skill)
   (#:na #:nervous-island.attack)
   (#:ne #:nervous-island.effect)
   (#:nt #:nervous-island.tile)
   (#:nto #:nervous-island.token)
   (#:nu #:nervous-island.user)
   (#:nsp #:nervous-island.space))
  ;; state/
  (:local-nicknames
   (#:nc #:nervous-island.coord)
   (#:nb #:nervous-island.board)
   ;; (#:nd #:nervous-island.damage)
   ;; (#:nch #:nervous-island.choice)
   ;; (#:np #:nervous-island.player)
   ;; (#:nph #:nervous-island.phase)
   ;; (#:ns #:nervous-island.step)
   ;; (#:nst #:nervous-island.state)
   )
  ;; Nervous Island armies
  (:local-nicknames
   ;; (#:moloch #:nervous-island.armies.moloch)
   ;; (#:outpost #:nervous-island.armies.outpost)
   ;; (#:borgo #:nervous-island.armies.borgo)
   ;; (#:hegemony #:nervous-island.armies.hegemony)
   ))

(in-package #:nervous-island.test)

(defvar *center-axial* (nc:ensure-axial '(0 0)))

(defvar *center-cube* (nc:ensure-cube '(0 0 0)))

;; (defun make-standard-board ()
;;   (apply #'nb:make-board '((-2 2) (-2 1) (-2 0)
;;                            (-1 2) (-1 1) (-1 0) (-1 -1)
;;                            (0 2) (0 1) (0 0) (0 -1) (0 -2)
;;                            (1 1) (1 0) (1 -1) (1 -2)
;;                            (2 0) (2 -1) (2 -2))))
