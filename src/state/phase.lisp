;;;; src/state/phase.lisp

(uiop:define-package #:nervous-island.phase
  (:use #:cl)
  (:shadow #:phase #:number)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:np #:nervous-island.player))
  (:export #:phase #:player-phase #:player #:number
           #:with-initiatives #:battle-part #:final #:final-full-board
           #:start #:draw-tiles #:discard-tile #:turn
           #:before-battle #:battle #:after-battle
           #:final-draw-tiles #:final-discard-tile #:final-turn
           #:before-final-full-board-battle #:final-full-board-battle
           #:after-final-full-board-battle
           #:before-final-battle #:final-battle #:after-final-battle #:end
           #:possible-phases))

(in-package #:nervous-island.phase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - protocol

(p:define-protocol-class phase () ())

(p:define-protocol-class player-phase (phase)
  ((%player :reader player :initarg :player)
   (%number :reader number :initarg :number))
  (:default-initargs :player (a:required-argument :player)
                     :number (a:required-argument :number)))

(defmethod shared-initialize :around
    ((phase player-phase) slots &rest args
     &key (player nil playerp) (number nil numberp))
  (when playerp
    (check-type player np:player)
    (nconc (list :player player) args))
  (when numberp
    (check-type number (integer 1))
    (nconc (list :number number) args))
  (apply #'call-next-method phase slots args))

(p:define-protocol-class with-initiatives (phase)
  ((%initiative :reader initiative :initarg :initiative))
  (:default-initargs :initiative (a:required-argument :initiative)))

(defmethod shared-initialize :around
    ((phase with-initiatives) slots &rest args
     &key (initiative nil initiativep))
  (when initiativep
    (check-type initiative unsigned-byte)
    (nconc (list :initiative initiative) args))
  (apply #'call-next-method phase slots args))

(p:define-protocol-class battle-part (phase) ())

(p:define-protocol-class final (phase) ())

(p:define-protocol-class final-full-board (final battle-part) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - concrete classes

(defclass start (player-phase) ())

(defclass draw-tiles (player-phase) ())
(defclass discard-tile (player-phase) ())
(defclass turn (player-phase) ())

(defclass before-battle (battle-part player-phase) ())
(defclass battle (battle-part player-phase with-initiatives) ())
(defclass after-battle (battle-part player-phase) ())

(defclass final-draw-tiles (final player-phase) ())
(defclass final-discard-tile (final player-phase) ())
(defclass final-turn (final player-phase) ())

(defclass before-final-full-board-battle (final-full-board before-battle) ())
(defclass final-full-board-battle (final-full-board battle) ())
(defclass after-final-full-board-battle (final-full-board after-battle) ())

(defclass before-final-battle (final before-battle) ())
(defclass final-battle (final battle) ())
(defclass after-final-battle (final after-battle) ())

(defclass end (final) ())
