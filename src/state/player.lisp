;;;; src/state/player.lisp

(uiop:define-package #:nervous-island.player
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:nr #:nervous-island.army)
                    (#:nt #:nervous-island.tile))
  (:export #:player #:army #:hit-points #:hand #:draw-pile #:discard-pile))

(in-package #:nervous-island.player)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Player

(defclass player ()
  ((%army :reader army :initarg :army)
   (%hit-points :reader hit-points :initarg :hit-points)
   (%hand :reader hand :initarg :hand)
   (%draw-pile :reader draw-pile :initarg :draw-pile)
   (%discard-pile :reader discard-pile :initarg :discard-pile))
  (:default-initargs
   :army (a:required-argument :army) :hand '() :discard-pile '()))

(defmethod initialize-instance :after
    ((player player) &key (hit-points nil hit-points-p)
                       (draw-pile nil draw-pile-p))
  (declare (ignore hit-points draw-pile))
  (unless hit-points-p
    (let ((hit-points (make-hash-table :test #'eq)))
      (dolist (hq (nr:hq-tiles (army player)))
        (setf (gethash hq hit-points) (nt:starting-hp hq)))
      (setf (slot-value player '%hit-points) hit-points)))
  (unless draw-pile-p
    (let ((draw-pile (a:shuffle (copy-list (nr:tiles (army player))))))
      (setf (slot-value player '%draw-pile) draw-pile))))

(defgeneric edit-player (player &rest initargs)
  (:method ((player player) &rest initargs)
    (apply #'φ:shallow-copy-object player initargs)))
