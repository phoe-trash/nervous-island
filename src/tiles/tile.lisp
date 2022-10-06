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
   #:define-unit
   ;; Tiles - foundation
   #:roots #:mine
   ;; Tiles - instant
   #:battle
   #:move #:push-back #:grab #:reposition #:castling
   #:sniper #:grenade #:air-strike #:small-bomb
   #:terror #:action))

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

(define-class hq (nel:hq-element skill-having board-tile)
  ((starting-hit-points :type (integer 1) :initform 20))
  (:protocolp t))

(define-class unit (skill-having board-tile) ()
  (:protocolp t))

(define-class warrior (unit) () (:protocolp t))
(define-class module (unit) () (:protocolp t))

(defmacro define-unit (name (&rest superclasses) &body skills)
  `(define-class ,name ,superclasses ()
     (:default-initargs :skills (set ,@skills))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - foundation

(define-class roots (foundation) ())
(define-class mine (foundation) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - instant

(defmacro define-instant (name)
  `(progn
     (define-class ,name (instant) ())
     (defun ,name () (make-instance ',name))))

(define-instant battle)

(define-instant move)
(define-instant push-back)
(define-instant grab)
(define-instant reposition)
(define-instant castling)

(define-instant sniper)
(define-instant grenade)
(define-instant air-strike)
(define-instant small-bomb)

(define-instant terror)
(define-instant action)
