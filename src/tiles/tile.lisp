;;;; src/tiles/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:φ #:phoe-toolbox)
                    (#:nel #:nervous-island.element)
                    (#:ncom #:nervous-island.common)
                    (#:nr #:nervous-island.army)
                    (#:nsk #:nervous-island.skill)
                    (#:nto #:nervous-island.token))
  (:export
   ;; Tiles - protocol
   #:tile #:instant #:target #:board-tile #:skill-having #:skills
   #:hq #:starting-hit-points #:object
   #:unit #:skills #:warrior #:module
   #:implant #:behavior
   #:foundation
   ;; Macros
   #:define-unit #:define-implant
   ;; Tiles - foundation
   #:roots #:mine #:hole
   ;; Tiles - instant
   #:battle
   #:move #:push-back #:grab #:reposition #:castling #:rotation #:drill
   #:sniper #:grenade #:air-strike #:small-bomb
   #:terror #:action #:paralysis
   #:incubation #:quill #:left-quill #:right-quill))

(in-package #:nervous-island.tile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - protocol

(define-class tile (nel:element) ()
  (:protocolp t))

(define-class instant (tile) ()
  (:protocolp t))
(define-class board-tile (tile) ()
  (:protocolp t))
(define-class foundation (board-tile) ()
  (:protocolp t))

(define-class skill-having (tile)
  ((skills :type set :initform (set)))
  (:protocolp t))

(define-class unit (skill-having board-tile) ()
  (:protocolp t))

(define-class hq (nel:hq-element unit)
  ((starting-hit-points :type (integer 1)))
  (:default-initargs :starting-hit-points 20)
  (:protocolp t))
(define-class object (hq) () (:protocolp t)
  (:default-initargs :starting-hit-points 10))

(define-class warrior (unit) () (:protocolp t))
(define-class module (unit) () (:protocolp t))
(define-class implant (unit)
  ((behavior :type instant))
  (:protocolp t))

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(define-class ,name ,superclasses ()
     (:default-initargs :skills (set ,@skills))))

(defmacro define-implant (name behavior &body skills)
  `(define-class ,name (implant) ()
     (:default-initargs :behavior ,behavior
                        :skills (set ,@skills))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - foundation

(define-class roots (foundation) ())
(define-class mine (foundation) ())
(define-class hole (foundation) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - instant

(defmacro define-instant (name &optional ((&key (superclass 'instant))))
  `(progn
     (define-class ,name (,superclass) ())
     (defun ,name () (make-instance ',name))))

(define-instant battle)

(define-instant move)
(define-instant push-back)
(define-instant grab)
(define-instant reposition)
(define-instant castling)
(define-instant rotation)
(define-instant drill)

(define-instant sniper)
(define-instant grenade)
(define-instant air-strike)
(define-instant small-bomb)

(define-instant terror)
(define-instant action)
(define-instant paralysis)

(define-instant incubation)
(define-class quill (instant) ()
  (:protocolp t))
(define-instant left-quill (:superclass quill))
(define-instant right-quill (:superclass quill))
