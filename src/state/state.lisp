;;;; src/state/state.lisp

(uiop:define-package #:nervous-island.state
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:φ #:phoe-toolbox)
                    (#:na #:nervous-island.army)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)
                    (#:nch #:nervous-island.choice)
                    (#:np #:nervous-island.player)
                    (#:nph #:nervous-island.phase)
                    (#:ns #:nervous-island.step)
                    (#:nsp #:nervous-island.space)
                    (#:nt #:nervous-island.tile))
  (:export #:state #:board #:players #:remaining-tiles #:alliances #:spaces
           #:current-phase))

(in-package #:nervous-island.state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

(deftype remaining-tiles-list ()
  '(cons np:player (cons (φ:list-of nt:tile) null)))

(deftype player-list ()
  '(φ:list-of np:player))

(ncom:define-typechecked-class state ()
  ((board :type nb:board)
   (players :type (φ:list-of np:player))
   (alliances :type (φ:list-of player-list) :initform '())
   (spaces :type hash-table :requiredp nil)
   (current-phase :type nph:phase))
  (:after #'make-state-after))

(defun make-state-after (state &key
                                 (spaces nil spacesp)
                         &allow-other-keys)
  (declare (ignore spaces))
  (when (and (not spacesp) (not (slot-boundp state '%spaces)))
    (let ((axials (a:hash-table-keys (nb:axials (board state)))))
      (setf (slot-value state '%spaces)
            (apply #'nsp:make-spaces axials)))))
