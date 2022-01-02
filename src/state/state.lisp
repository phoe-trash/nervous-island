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
           #:current-phase #:previous-steps))

(in-package #:nervous-island.state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

(defun transform-board (thing)
  (etypecase thing
    (nb:board thing)
    ((eql :standard) (nb:make-standard-board))))

(defun transform-player (thing)
  (etypecase thing
    (np:player thing)
    (keyword (let* ((name (symbol-name thing))
                    (army (find-symbol "ARMY" name)))
               (make-instance 'np:player :army army)))))

(deftype player-list ()
  '(φ:list-of np:player))

(ncom:define-typechecked-class state ()
  ((board :type nb:board :initform (nb:make-standard-board)
          :transform #'transform-board)
   (players :type (φ:list-of np:player) :transform #'transform-player)
   (alliances :type (φ:list-of player-list) :initform '())
   (spaces :type hash-table :requiredp nil)
   (current-phase :type nph:phase)
   (previous-steps :type (φ:list-of ns:step) :initform '()))
  (:after #'make-state-after))

(define-condition duplicated-player (ncom:nervous-island-error)
  ((%player :reader duplicated-player-player :initarg :player))
  (:default-initargs :player (a:required-argument :player))
  (:report (lambda (condition stream)
             (format stream "Duplicated player ~S"
                     (duplicated-player-player condition)))))

(defun check-no-duplicated-players (state)
  (let ((hash-table (make-hash-table)))
    (dolist (player (players state))
      (if (gethash player hash-table)
          (error 'duplicated-player :player player)
          (setf (gethash player hash-table) player)))))

(defun make-state-after (state &key (spaces nil spacesp) &allow-other-keys)
  (declare (ignore spaces))
  (check-no-duplicated-players state)
  (when (and (not spacesp) (not (slot-boundp state '%spaces)))
    (let ((axials (a:hash-table-keys (nb:axials (board state)))))
      (setf (slot-value state '%spaces)
            (apply #'nsp:make-spaces axials)))))
