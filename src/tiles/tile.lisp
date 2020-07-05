;;;; src/tiles/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:nr #:nervous-island.army)
                    (#:ns #:nervous-island.skill))
  (:export
   ;; Tiles - protocol
   #:tile #:owner #:tile= #:instant #:target #:board-tile #:skill-having
   #:hq #:starting-hp #:unit #:skills #:foundation #:warrior #:module #:implant
   ;; Macros
   #:define-unit))

(in-package #:nervous-island.tile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - protocol

(p:define-protocol-class tile ()
  ((%owner :reader owner :initarg :owner))
  (:default-initargs :owner nil))

(defmethod shared-initialize :around
    ((tile tile) slots &rest args &key (owner nil ownerp))
  (when ownerp
    (check-type owner (or null nr:army))
    (nconc (list :owner owner) args))
  (apply #'call-next-method tile slots args))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (let ((owner (if (owner object) (nr:name (owner object)) 'unowned)))
      (format stream "~A ~A" owner (type-of object)))))

(defgeneric tile= (tile-1 tile-2)
  (:method (tile-1 tile-2)
    (and (eq (class-name tile-1) (class-name tile-2))
         (eq (owner tile-1) (owner tile-2)))))

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
    (loop for cons on skills do (check-type (car cons) ns:skill))
    (nconc (list :skills skills) args))
  (apply #'call-next-method tile slots args))

(p:define-protocol-class hq (skill-having board-tile)
  ((%starting-hp :reader starting-hp :initarg :starting-hp))
  (:default-initargs :starting-hp 20))

(defmethod shared-initialize :around
    ((tile hq) slots &rest args &key (starting-hp nil starting-hp-p))
  (when starting-hp-p
    (check-type starting-hp (integer 1))
    (nconc (list :starting-hp starting-hp) args))
  (apply #'call-next-method tile slots args))

(p:define-protocol-class unit (skill-having board-tile) ())

(defclass warrior (unit) ())
(defclass module (unit) ())
(defclass implant (unit) ())

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(defclass ,name ,superclasses ()
     (:default-initargs :skills (list ,@skills))))
