;;;; src/tile.lisp

(uiop:define-package #:nervous-island.tile
  (:use #:cl)
  (:shadow #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ng #:nervous-island.grid)))

(in-package #:nervous-island.tile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tiles - protocol

(p:define-protocol-class tile ()
  ((%owner :reader owner :initarg :owner))
  (:default-initargs :owner nil))

(defgeneric tile= (tile-1 tile-2)
  (:method (tile-1 tile-2) nil))

(p:define-protocol-class instant-tile (tile)
  ((%target :reader target :initarg :target))
  (:default-initargs :target nil))

(p:define-protocol-class board-tile (tile)
  ((%direction :accessor direction :initarg :direction))
  (:default-initargs :direction :w))

(p:define-protocol-class hq (board-tile)
  ((%hit-points :accessor hit-points :initarg :hit-points))
  (:default-initargs :hit-points 20))
(p:define-protocol-class unit (board-tile)
  ((%skills :accessor skills :initarg :skills))
  (:default-initargs :skills '()))
(p:define-protocol-class foundation (board-tile) ())

(p:define-protocol-class warrior (unit) ())
(p:define-protocol-class module (unit) ())
(p:define-protocol-class implant (unit) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - protocol

(p:define-protocol-class skill () ())

(p:define-protocol-class directed (skill)
  ((%direction :reader direction :initarg :direction))
  (:default-initargs :direction (a:required-argument :direction)))
(p:define-protocol-class undirected (skill) ())

(p:define-protocol-class active (skill)
  ((%activation-time :reader activation-time :initarg :activation-time))
  (:default-initargs :activation-time :initiative))
(p:define-protocol-class passive (skill) ())

(p:define-protocol-class active-directed (active directed) ())
(p:define-protocol-class active-undirected (active undirected) ())
(p:define-protocol-class passive-directed (passive directed) ())
(p:define-protocol-class passive-undirected (passive undirected) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - concrete classes

(defclass armor (passive-directed) ())
(defclass net (passive-directed) ())

(defclass mobility (active-undirected) ()
  (:default-initargs :activation-time :player-turn))
(defclass explosion (active-undirected) ()
  (:default-initargs :activation-time :initiative-player-choice))

(defclass toughness (passive-undirected) ())
(defclass initiative (passive-undirected)
  ((%value :reader value :initarg :value))
  (:default-initargs :value 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - attacks

(p:define-protocol-class attack (active-directed)
  ((%strength :reader strength :initarg :strength))
  (:default-initargs :strength 1))

(defclass melee (attack) ())
(defclass ranged (attack) ())
(defclass gauss-cannon (attack) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects

(p:define-protocol-class module-effect (passive)
  ((%targets :reader targets :initarg :targets))
  (:default-initargs :targets '(:friendly)))

(p:define-protocol-class directed-module-effect (directed module-effect) ())
(p:define-protocol-class undirected-module-effect (undirected module-effect) ())

(p:define-protocol-class numeric-effect (module-effect)
  ((%strength :reader strength :initarg :strength))
  (:default-initargs :strength 1))

(p:define-protocol-class melee-officer (numeric-effect) ())
(p:define-protocol-class ranged-officer (numeric-effect) ())
(p:define-protocol-class scout (numeric-effect) ())
(p:define-protocol-class mother (numeric-effect) ())
(p:define-protocol-class medic (module-effect) ())
(p:define-protocol-class transport (module-effect) ())
(p:define-protocol-class quartermaster (module-effect) ())
(p:define-protocol-class recon-center (module-effect) ())

(p:define-protocol-class scoper (module-effect) ()
  (:default-initargs :targets '(:enemy)))
(p:define-protocol-class saboteur (module-effect) ()
  (:default-initargs :targets '(:enemy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instant tiles

(defclass battle (instant-tile) ())

(defclass move (instant-tile) ()
  (:default-initargs :target '(:friendly)))
(defclass push-back (instant-tile) ()
  (:default-initargs :target '(:friendly)))

(defclass sniper (instant-tile) ()
  (:default-initargs :target '(:enemy)))
(defclass grenade (instant-tile) ()
  (:default-initargs :target '(:enemy)))
(defclass bomb (instant-tile) ()
  (:default-initargs :target '(:enemy)))
