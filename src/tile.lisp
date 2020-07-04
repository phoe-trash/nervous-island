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

(defgeneric tile= (tile-1 tile-2)
  (:method (tile-1 tile-2)
    (and (eq (class-name tile-1) (class-name tile-2))
         (eq (owner tile-1) (owner tile-2)))))

(defun print-tile (object stream type)
  (print-unreadable-object (object stream :type nil :identity t)
    (let ((owner (if (owner object) (nr:name (owner object)) 'unowned)))
      (format stream "~A ~A - ~S" owner type (type-of object)))))

(p:define-protocol-class instant (tile)
  ((%target :reader target :initarg :target))
  (:default-initargs :target nil))
(defmethod print-object ((object instant) stream)
  (print-tile object stream 'instant))

(p:define-protocol-class board-tile (tile)
  ((%direction :reader direction :initarg :direction))
  (:default-initargs :direction :w))

(p:define-protocol-class foundation (board-tile) ())

(p:define-protocol-class skill-having ()
  ((%skills :reader skills :initarg :skills))
  (:default-initargs :skills '()))

(p:define-protocol-class hq (skill-having board-tile) ())
(p:define-protocol-class unit (skill-having board-tile) ())

(p:define-protocol-class warrior (unit) ())
(defmethod print-object ((object warrior) stream)
  (print-tile object stream 'warrior))

(p:define-protocol-class module (unit) ())
(defmethod print-object ((object module) stream)
  (print-tile object stream 'module))

(p:define-protocol-class implant (unit) ())
(defmethod print-object ((object implant) stream)
  (print-tile object stream 'implant))

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(defclass ,name ,superclasses ()
     (:default-initargs :skills (list ,@skills))))
