;;;; src/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ng #:nervous-island.grid)
                    (#:nr #:nervous-island.army))
  (:export
   ;; Tiles - protocol
   #:tile #:owner #:tile= #:instant #:target #:board-tile #:direction
   #:hq #:hit-points #:unit #:skills #:foundation #:warrior #:module #:implant
   ;; Macros
   #:define-unit))

(in-package #:nervous-island.tile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - protocol

;; TODO negative tests for initial arguments everywhere
;; TODO print-object for all those

(p:define-protocol-class tile ()
  ((%owner :reader owner :initarg :owner))
  (:default-initargs :owner nil))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (let ((owner (if (owner object) (nr:name (owner object)) 'unowned)))
      (format stream "~A ~A" owner (type-of object)))))

(defgeneric tile= (tile-1 tile-2)
  (:method (tile-1 tile-2)
    (and (eq (class-name tile-1) (class-name tile-2))
         (eq (owner tile-1) (owner tile-2)))))

(p:define-protocol-class instant (tile)
  ((%target :reader target :initarg :target))
  (:default-initargs :target nil))

(p:define-protocol-class board-tile (tile)
  ((%direction :reader direction :initarg :direction))
  (:default-initargs :direction :w))

(p:define-protocol-class foundation (board-tile) ())

(p:define-protocol-class skill-having ()
  ((%skills :reader skills :initarg :skills))
  (:default-initargs :skills '()))

(p:define-protocol-class hq (skill-having board-tile) ())

(p:define-protocol-class unit (skill-having board-tile) ())

(defclass warrior (unit) ())
(defclass module (unit) ())
(defclass implant (unit) ())

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(defclass ,name ,superclasses ()
     (:default-initargs :skills (list ,@skills))))
