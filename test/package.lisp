;;;; test/package.lisp

(defpackage #:nervous-island.test
  (:use #:cl
        #:parachute)
  (:local-nicknames (#:g #:nervous-island.grid)
                    (#:a #:alexandria)))

(defvar *center* (g:ensure-space '(0 0)))

(defun make-standard-board ()
  (apply #'g:make-board '((-2 2) (-2 1) (-2 0)
                          (-1 2) (-1 1) (-1 0) (-1 -1)
                          (0 2) (0 1) (0 0) (0 -1) (0 -2)
                          (1 1) (1 0) (1 -1) (1 -2)
                          (2 0) (2 -1) (2 -2))))
