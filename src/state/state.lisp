;;;; src/state/state.lisp

(uiop:define-package #:nervous-island.state
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)))

(in-package #:nervous-island.state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

(defclass state ()
  )
