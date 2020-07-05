;;;; test/package.lisp

(defpackage #:nervous-island.test
  (:use #:cl
        #:parachute)
  ;; External packages
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  ;; Nervous Island packages
  (:local-nicknames (#:na #:nervous-island.attack)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:nch #:nervous-island.choice)
                    (#:ncom #:nervous-island.common)
                    (#:ne #:nervous-island.effect)
                    (#:ni #:nervous-island.instant)
                    (#:np #:nervous-island.player)
                    (#:nph #:nervous-island.phase)
                    (#:nr #:nervous-island.army)
                    (#:ns #:nervous-island.skill)
                    (#:nsp #:nervous-island.space)
                    (#:nst #:nervous-island.state)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  ;; Nervous Island armies
  (:local-nicknames (#:moloch #:nervous-island.armies.moloch)
                    (#:outpost #:nervous-island.armies.outpost)
                    (#:borgo #:nervous-island.armies.borgo)
                    (#:hegemony #:nervous-island.armies.hegemony)))

(in-package #:nervous-island.test)

(defvar *center-axial* (nc:ensure-axial '(0 0)))

(defvar *center-cube* (nc:ensure-cube '(0 0 0)))

(defun make-standard-board ()
  (apply #'nb:make-board '((-2 2) (-2 1) (-2 0)
                           (-1 2) (-1 1) (-1 0) (-1 -1)
                           (0 2) (0 1) (0 0) (0 -1) (0 -2)
                           (1 1) (1 0) (1 -1) (1 -2)
                           (2 0) (2 -1) (2 -2))))
