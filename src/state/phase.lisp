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

(ncom:define-class phase ()
  ((number :type (integer 1)))
  (:protocolp t))

(ncom:define-class player-phase (phase)
  ((player :type np:player))
  (:protocolp t))

(ncom:define-class with-initiatives (phase)
  ((initiative :type nsk:initiative)))

(p:define-protocol-class battle-part (phase) ())

(p:define-protocol-class final (phase) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - concrete classes

(ncom:define-class start (player-phase) ())

(ncom:define-class place-hq-tiles (player-phase) ())

(ncom:define-class draw-tiles (player-phase) ())
(ncom:define-class turn (player-phase) ())
(ncom:define-class discard-tile (player-phase) ())

(ncom:define-class before-battle (battle-part player-phase) ())
(ncom:define-class battle (battle-part player-phase with-initiatives) ())
(ncom:define-class after-battle (battle-part player-phase) ())

(ncom:define-class final-draw-tiles (final player-phase) ())
(ncom:define-class final-discard-tile (final player-phase) ())
(ncom:define-class final-turn (final player-phase) ())

(ncom:define-class before-final-battle (final before-battle) ())
(ncom:define-class final-battle (final battle) ())
(ncom:define-class after-final-battle (final after-battle) ())

(ncom:define-class end (final) ())
