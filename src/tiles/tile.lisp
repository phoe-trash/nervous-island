;;;; src/tiles/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Tiles - protocol
   #:tile #:tile= #:instant #:target #:board-tile #:skill-having
   #:hq #:starting-hit-points #:unit #:skills #:foundation #:warrior #:module
   #:implant
   ;; Macros
   #:define-unit))

(in-package #:nervous-island.tile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - protocol

(ncom:define-typechecked-class tile (nr:element) ()
  (:protocolp t))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (let* ((owner (nr:owner object))
           (name (if owner (nr:name owner) 'unowned)))
      (format stream "~A ~A" name (type-of object)))))

(defgeneric tile= (tile-1 tile-2)
  (:method (tile-1 tile-2)
    (and (eq (class-name tile-1) (class-name tile-2))
         (eq (nr:owner tile-1) (nr:owner tile-2)))))

(p:define-protocol-class instant (tile) ())

(p:define-protocol-class board-tile (tile) ())

(p:define-protocol-class foundation (board-tile) ())

(p:define-protocol-class skill-having (tile)
  ((%skills :reader skills :initarg :skills))
  (:default-initargs :skills '()))

(defmethod shared-initialize :around
    ((tile skill-having) slots &rest args &key (skills nil skillsp))
  (when skillsp
    (check-type skills list)
    (loop for cons on skills do (check-type (car cons) nsk:skill))
    (nconc (list :skills skills) args))
  (apply #'call-next-method tile slots args))

(p:define-protocol-class hq (nr:hq-element skill-having board-tile)
  ((%starting-hit-points :reader starting-hit-points
                         :initarg :starting-hit-points))
  (:default-initargs :starting-hit-points 20))

(defmethod shared-initialize :around
    ((tile hq) slots &rest args
     &key (starting-hit-points nil starting-hit-points-p))
  (when starting-hit-points-p
    (check-type starting-hit-points (integer 1))
    (nconc (list :starting-hit-points starting-hit-points) args))
  (apply #'call-next-method tile slots args))

(p:define-protocol-class unit (skill-having board-tile) ())

(defclass warrior (unit) ())
(defclass module (unit) ())
(defclass implant (unit) ())

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(defclass ,name ,superclasses ()
     (:default-initargs :skills (list ,@skills))))
