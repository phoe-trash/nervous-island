;;;; src/tiles/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:nel #:nervous-island.element)
                    (#:ncom #:nervous-island.common)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Tiles - protocol
   #:tile #:instant #:target #:board-tile #:skill-having #:skills
   #:hq #:starting-hit-points #:unit #:skills #:foundation #:warrior #:module
   ;; Macros
   #:define-unit))

(in-package #:nervous-island.tile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - protocol

(define-class tile (nel:element) ()
  (:protocolp t))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (let* ((owner (nel:owner object))
           (name (if owner (nr:name owner) 'unowned)))
      (format stream "~A ~A" name (type-of object)))))

(define-class instant (tile) ()
  (:protocolp t))
(define-class board-tile (tile) ()
  (:protocolp t))
(define-class foundation (board-tile) ()
  (:protocolp t))

(define-class skill-having (tile)
  ((skills :type (φ:list-of nsk:skill) :initform '()))
  (:protocolp t))

(define-class hq (nel:hq-element skill-having board-tile)
  ((starting-hit-points :type (integer 1) :initform 20))
  (:protocolp t))

(define-class unit (skill-having board-tile) ()
  (:protocolp t))

(define-class warrior (unit) () (:protocolp t))
(define-class module (unit) () (:protocolp t))

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(define-class ,name ,superclasses ()
     (:default-initargs :skills (list ,@skills))))
