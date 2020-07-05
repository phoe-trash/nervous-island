;;;; src/state/player.lisp

(uiop:define-package #:nervous-island.player
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
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

(define-condition hq-hit-points-over-limit (ncom:nervous-island-error)
  ((%current-hit-points :reader current-hit-points :initarg
                        :current-hit-points)
   (%starting-hit-points :reader starting-hit-points
                         :initarg :starting-hit-points)
   (%hq :reader hq :initarg :hq))
  (:default-initargs
   :current-hit-points (a:required-argument :current-hit-points)
   :starting-hit-points (a:required-argument :starting-hit-points)
   :hq (a:required-argument :hq))
  (:report (lambda (condition stream)
             (format stream "The HP value for ~S (~D) is over its maximum (~D)."
                     (current-hit-points condition) (hq condition)
                     (starting-hit-points condition)))))

(defmethod shared-initialize :around
    ((player player) slots &rest args &key
                                        (army nil armyp)
                                        (hit-points nil hit-points-p)
                                        (hand nil handp)
                                        (draw-pile nil draw-pile-p)
                                        (discard-pile nil discard-pile-p))
  (when armyp
    (check-type army nr:army)
    (nconc (list :army army) args))
  (when hit-points-p
    (check-type hit-points hash-table)
    (let ((hq-tiles (nr:hq-tiles army)))
      (dolist (hq hq-tiles)
        (let ((starting (nt:starting-hit-points hq))
              (current (gethash hq hit-points)))
          (unless (<= current starting)
            (error 'hq-hit-points-over-limit :hq hq
                                             :starting-hit-points starting
                                             :current-hit-points current)))))
    (nconc (list :hit-points hit-points) args))
  (when handp
    (nconc (list :hand hand) args))
  (when draw-pile-p
    (nconc (list :draw-pile draw-pile) args))
  (when discard-pile-p
    (nconc (list :discard-pile discard-pile) args))
  ;; TODO check that the number of all tiles does not exceed the total army size
  (apply #'call-next-method player slots args))

(defmethod initialize-instance :after
    ((player player) &key (hit-points nil hit-points-p)
                       (draw-pile nil draw-pile-p))
  ;; TODO typechecks
  (declare (ignore hit-points draw-pile))
  (unless hit-points-p
    (let ((hit-points (make-hash-table :test #'eq)))
      (dolist (hq (nr:hq-tiles (army player)))
        (setf (gethash hq hit-points) (nt:starting-hit-points hq)))
      (setf (slot-value player '%hit-points) hit-points)))
  (unless draw-pile-p
    (let ((draw-pile (a:shuffle (copy-list (nr:tiles (army player))))))
      (setf (slot-value player '%draw-pile) draw-pile))))

(defgeneric edit-player (player &rest initargs)
  (:method ((player player) &rest initargs)
    (apply #'φ:shallow-copy-object player initargs)))
