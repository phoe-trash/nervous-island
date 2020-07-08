;;;; src/state/state.lisp

(uiop:define-package #:nervous-island.state
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:na #:nervous-island.army)
                    (#:nb #:nervous-island.board)
                    (#:nc #:nervous-island.coord)
                    (#:nch #:nervous-island.choice)
                    (#:nsp #:nervous-island.space)
                    (#:np #:nervous-island.player)
                    (#:nph #:nervous-island.phase)
                    (#:ncom #:nervous-island.common))
  (:export
   #:state #:board #:players #:current-player #:spaces #:current-phase
   #:choices #:used-choices))

(in-package #:nervous-island.state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State

;;; TODO alliances

(defclass state ()
  ((%board :reader board :initarg :board)
   (%players :reader players :initarg :players)
   (%alliances :reader alliances :initarg :alliances)
   (%current-player :reader current-player :initarg :current-player)
   (%spaces :reader spaces :initarg :spaces)
   (%current-phase :reader current-phase :initarg :current-phase)
   ;; (%choices :reader choices :initarg :choices)
   ;; (%used-choices :reader used-choices :initarg used-choices)
   )
  (:default-initargs
   :board (a:required-argument :board)
   :players (a:required-argument :players)
   :current-phase (make-instance 'nph:start)
   ;; :used-choices '()
   ))

(defmethod initialize-instance :after
    ((state state) &key
                     (alliances nil alliances-p)
                     (current-player nil current-player-p)
                     (spaces nil spacesp)
                     ;; (choices nil choicesp)
                     )
  (declare (ignore spaces ;; choices
                   current-player))
  (unless alliancesp
    (setf (slot-value state '%alliances)
          (mapcar #'list alliances)))
  (unless current-player-p
    (setf (slot-value state '%current-player)
          (first (players state))))
  (unless spacesp
    (setf (slot-value state '%spaces)
          (apply #'nsp:make-spaces (nb:axials (board state)))))
  ;; (unless choicesp
  ;;   (let* ((player (first (players state)))
  ;;          (hq-tiles (na:hq-tiles (np:army player))))
  ;;     (setf (slot-value state '%choices)
  ;;           (make-instance 'nch:place-hq-tiles :player player
  ;;                                              :hq-tiles hq-tiles))))
  )
