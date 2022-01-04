;;;; src/state/phase.lisp

(uiop:define-package #:nervous-island.phase
  (:use #:cl)
  (:shadow #:phase #:number)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill)
                    (#:np #:nervous-island.player))
  (:export #:phase #:number #:player-phase #:player
           #:with-initiatives #:battle-part #:final
           #:start #:place-hq-tiles #:draw-tiles #:discard-tile #:turn
           #:before-battle #:battle #:after-battle
           #:final-draw-tiles #:final-discard-tile #:final-turn
           #:before-final-battle #:final-battle #:after-final-battle #:end))

(in-package #:nervous-island.phase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - protocol

(ncom:define-typechecked-class phase ()
  ((number :type (integer 1)))
  (:protocolp t))

(ncom:define-typechecked-class player-phase (phase)
  ((player :type np:player))
  (:protocolp t))

(ncom:define-typechecked-class with-initiatives (phase)
  ((initiative :type nsk:initiative)))

(p:define-protocol-class battle-part (phase) ())

(p:define-protocol-class final (phase) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - concrete classes

(defclass start (player-phase) ())

(defclass place-hq-tiles (player-phase) ())

(defclass draw-tiles (player-phase) ())
(defclass turn (player-phase) ())
(defclass discard-tile (player-phase) ())

(defclass before-battle (battle-part player-phase) ())
(defclass battle (battle-part player-phase with-initiatives) ())
(defclass after-battle (battle-part player-phase) ())

(defclass final-draw-tiles (final player-phase) ())
(defclass final-discard-tile (final player-phase) ())
(defclass final-turn (final player-phase) ())

(defclass before-final-battle (final before-battle) ())
(defclass final-battle (final battle) ())
(defclass after-final-battle (final after-battle) ())

(defclass end (final) ())
