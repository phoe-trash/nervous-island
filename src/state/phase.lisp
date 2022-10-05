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

(define-class phase ()
  ((number :type (integer 1)))
  (:protocolp t))

(define-class player-phase (phase)
  ((player :type np:player))
  (:protocolp t))

(define-class with-initiatives (phase)
  ((initiative :type nsk:initiative)))

(p:define-protocol-class battle-part (phase) ())

(p:define-protocol-class final (phase) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - concrete classes

(define-class start (player-phase) ())

(define-class place-hq-tiles (player-phase) ())

(define-class draw-tiles (player-phase) ())
(define-class turn (player-phase) ())
(define-class discard-tile (player-phase) ())

(define-class before-battle (battle-part player-phase) ())
(define-class battle (battle-part player-phase with-initiatives) ())
(define-class after-battle (battle-part player-phase) ())

(define-class final-draw-tiles (final player-phase) ())
(define-class final-discard-tile (final player-phase) ())
(define-class final-turn (final player-phase) ())

(define-class before-final-battle (final before-battle) ())
(define-class final-battle (final battle) ())
(define-class after-final-battle (final after-battle) ())

(define-class end (final) ())
