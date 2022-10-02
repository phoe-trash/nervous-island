;;;; src/state/player.lisp

(uiop:define-package #:nervous-island.player
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:Φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:nr #:nervous-island.army)
                    (#:nt #:nervous-island.tile))
  (:export
   #:player #:army #:hit-points #:hand #:draw-pile #:discard-pile
   #:hq-hit-points-over-limit #:mismatched-hq-tiles #:cannot-edit-army
   #:too-many-tiles #:edit-player))

;;; TODO unify condition accessor names - shorten them and such

(in-package #:nervous-island.player)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Player

(ncom:define-class player ()
  ((army :type nr:army :transform #'nr:ensure-army)
   (hit-points :type hash-table :requiredp nil)
   ;; TODO check all other classes for REQUIREDP
   (hand :type (φ:list-of nt:tile) :initform '())
   (draw-pile :type (φ:list-of nt:tile) :requiredp nil)
   (discard-pile :type (φ:list-of nt:tile) :initform '()))
  (:after #'make-player-after))

(defmethod print-object ((object player) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (nr:name (army object)))))

(define-condition mismatched-hq-tiles (ncom:nervous-island-error)
  ((%expected :reader expected :initarg :expected)
   (%actual :reader actual :initarg :actual))
  (:default-initargs :expected (a:required-argument :expected)
                     :actual (a:required-argument :actual))
  (:report (lambda (condition stream)
             (format stream "Mismatched HQs: expected ~S, actual ~S."
                     (expected condition) (actual condition)))))

(define-condition hq-hit-points-over-limit (ncom:nervous-island-error)
  ((%current-hit-points :reader current-hit-points
                        :initarg :current-hit-points)
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

(defun check-no-mismatched-hq-tiles (player)
  (let* ((actual (a:hash-table-keys (hit-points player)))
         (expected (nr:hq-elements (army player))))
    (unless (a:set-equal actual expected)
      (error 'mismatched-hq-tiles :actual actual :expected expected))))

(defun check-hit-points (player)
  (let* ((hit-points (hit-points player))
         (hqs (a:hash-table-keys hit-points)))
    (dolist (hq hqs)
      (check-type (gethash hq hit-points) unsigned-byte)
      (let ((starting (nt:starting-hit-points hq))
            (current (gethash hq hit-points)))
        (unless (<= current starting)
          (error 'hq-hit-points-over-limit
                 :hq hq :starting-hit-points starting
                 :current-hit-points current))))))

(define-condition too-many-tiles (ncom:nervous-island-error)
  ((%expected :reader expected :initarg :expected)
   (%actual :reader actual :initarg :actual))
  (:default-initargs :expected (a:required-argument :expected)
                     :actual (a:required-argument :actual))
  (:report (lambda (condition stream)
             (format stream "Too many tiles: expected up to ~S, but got ~S."
                     (expected condition) (actual condition)))))

(defun check-total-army-size (player)
  ;; TODO rewrite into CHECK-FOREIGN-TILES that errors whenever a tile is
  ;; detected that is not a part of the game state
  (let* ((hand-size (length (hand player)))
         (draw-pile-size (length (draw-pile player)))
         (discard-pile-size (length (discard-pile player)))
         (army (army player))
         (hq-tiles-size (length (nr:hq-elements army)))
         (tiles-size (length (nr:elements army))))
    (let ((expected (+ hq-tiles-size tiles-size))
          (actual (+ hq-tiles-size hand-size
                     draw-pile-size discard-pile-size)))
      (unless (<= actual expected)
        (error 'too-many-tiles :actual actual :expected expected)))))

(defun make-player-after (player &key
                                   (hit-points nil hit-points-p)
                                   (draw-pile nil draw-pile-p)
                          &allow-other-keys)
  (declare (ignore hit-points draw-pile))
  (unless (or hit-points-p (slot-boundp player '%hit-points))
    (let ((hit-points (make-hash-table :test #'eq)))
      (dolist (hq (nr:hq-elements (army player)))
        (setf (gethash hq hit-points) (nt:starting-hit-points hq)))
      (setf (slot-value player '%hit-points) hit-points)))
  (check-no-mismatched-hq-tiles player)
  (check-hit-points player)
  (unless (or draw-pile-p (slot-boundp player '%draw-pile))
    (setf (slot-value player '%draw-pile) (nr:elements (army player))))
  (check-total-army-size player))

(define-condition cannot-edit-army (ncom:nervous-island-error)
  ((%player :reader cannot-edit-army-player :initarg :player))
  (:default-initargs :player (a:required-argument :player))
  (:report
   (lambda (condition stream)
     (format stream "Attempted to edit the army of player ~S."
             (cannot-edit-army-player condition)))))

(defgeneric edit-player (player &rest initargs)
  (:method ((player player) &rest initargs
            &key (army nil armyp) &allow-other-keys)
    (declare (ignore army))
    (when armyp (error 'cannot-edit-army :player player))
    (apply #'φ:copy-object player initargs)))
