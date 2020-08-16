;;;; src/tiles/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Tiles - protocol
   #:tile #:tile= #:instant #:target #:board-tile #:skill-having #:skills
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

(ncom:define-typechecked-class skill-having (tile)
  ((skills :type (φ:list-of nsk:skill) :initform '()))
  (:protocolp t))

(ncom:define-typechecked-class hq (nr:hq-element skill-having board-tile)
  ((starting-hit-points :type (integer 1) :initform 20))
  (:protocolp t))

(p:define-protocol-class unit (skill-having board-tile) ())

(defclass warrior (unit) ())
(defclass module (unit) ())
(defclass implant (unit) ())

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(defclass ,name ,superclasses ()
     (:default-initargs :skills (list ,@skills))))
