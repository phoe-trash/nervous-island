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
   #:tile #:instant #:target #:board-tile
   #:hq #:starting-hit-points #:object
   #:unit #:warrior #:module
   #:implant #:behavior
   #:foundation
   ;; Macros
   #:define-unit #:define-implant
   ;; Tiles - foundation
   #:roots #:mine #:hole #:toxic-bomb #:quicksands
   ;; Tiles - instant
   #:battle #:order #:sandstorm
   #:move #:doubled-move
   #:push-back #:grab #:reposition #:castling #:rotation #:drill
   #:transposition
   #:sniper #:grenade #:air-strike #:small-bomb #:ray #:avalanche #:hunt #:wave
   #:terror #:action #:paralysis #:smokescreen #:reappearance
   #:castling-with-the-opponent #:cannibalize-enemy
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

(define-class unit (board-tile nsk:skill-having) ()
  (:protocolp t))

(define-class hq (nel:hq-element unit)
  ((starting-hit-points :type (integer 1)))
  (:default-initargs :starting-hit-points 20)
  (:protocolp t))
(define-class object (hq) () (:protocolp t)
  (:default-initargs :starting-hit-points 10))

(define-class warrior (unit) ())
(define-class module (unit) ())
(define-class implant (unit)
  ((behavior :type instant)))

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
(define-class toxic-bomb (foundation nsk:skill-having) ()
  (:default-initargs :skills (set (nsk:explosion) (nsk:initiative 1))))
(define-class quicksands (foundation nsk:skill-having) ()
  (:default-initargs :skills (set (nsk:sandstorm-move))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - instant

(defmacro define-instant (name &optional ((&key (superclass 'instant))))
  `(progn
     (define-class ,name (,superclass) ())
     (defun ,name () (make-instance ',name))))

(define-instant battle)
(define-instant order (:superclass battle))
(define-instant sandstorm (:superclass battle))

(define-instant move)
(define-instant doubled-move (:superclass move))
(define-instant push-back)
(define-instant grab)
(define-instant reposition)
(define-instant castling)
(define-instant rotation)
(define-instant drill)
(define-instant transposition)
(define-instant reappearance)

(define-instant sniper)
(define-instant grenade)
(define-instant air-strike)
(define-instant small-bomb)
(define-instant ray)
(define-instant avalanche)
(define-instant hunt)
(define-instant wave)

(define-instant terror)
(define-instant action)
(define-instant paralysis)
(define-instant smokescreen)
(define-instant castling-with-the-opponent)
(define-instant cannibalize-enemy)

(define-instant incubation)
(define-class quill (instant) ()
  (:protocolp t))
(define-instant left-quill (:superclass quill))
(define-instant right-quill (:superclass quill))
