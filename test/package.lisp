;;;; test/package.lisp

(defpackage #:nervous-island.test
  (:use #:cl
        #:parachute)
  (:local-nicknames (#:a #:alexandria)
                    (#:ncom #:nervous-island.common)
                    (#:nc #:nervous-island.coord)
                    (#:nb #:nervous-island.board)))

(in-package #:nervous-island.test)

(defvar *center* (nc:ensure-axial '(0 0)))

(defun make-standard-board ()
  (apply #'nb:make-board '((-2 2) (-2 1) (-2 0)
                           (-1 2) (-1 1) (-1 0) (-1 -1)
                           (0 2) (0 1) (0 0) (0 -1) (0 -2)
                           (1 1) (1 0) (1 -1) (1 -2)
                           (2 0) (2 -1) (2 -2))))
