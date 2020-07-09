;;;; src/state/state.lisp

(uiop:define-package #:nervous-island.state
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:na #:nervous-island.army)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:nch #:nervous-island.choice)
                    (#:nsp #:nervous-island.space)
                    (#:np #:nervous-island.player)
                    (#:nph #:nervous-island.phase)
                    (#:ncom #:nervous-island.common))
  (:export #:state #:board #:players #:alliances #:spaces #:current-phase))

(in-package #:nervous-island.state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

(ncom:define-typechecked-class state ()
  ((board :type nb:board)
   (players :type (φ:list-of np:player))
   (alliances :type (φ:list-of list) :initform '())
   (spaces :type hash-table :requiredp nil)
   (current-phase :type nph:phase :initarg (make-instance 'nph:start)))
  (:after (lambda (state &key (spaces nil spacesp) &allow-other-keys)
            (declare (ignore spaces))
            (when (and (not spacesp) (not (spaces state)))
              (setf (slot-value state '%spaces)
                    (apply #'nsp:make-spaces (nb:axials (board state))))))))
