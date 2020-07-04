;;;; src/state/phase.lisp

(uiop:define-package #:nervous-island.phase
  (:use #:cl)
  (:shadow #:phase)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export #:phase #:player-phase #:start #:turn #:battle #:end))

(in-package #:nervous-island.phase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phase - protocol

(p:define-protocol-class phase () ())

(p:define-protocol-class player-phase (phase)
  ((%player :reader player :initarg :player))
  (:default-initargs :player (a:required-argument :player)))

(defclass start (player-phase) ())

(defclass turn (player-phase) ())

(defclass before-battle (player-phase) ())

(defclass battle (player-phase)
  ((%initiative :reader initiative :initarg :initiative))
  (:default-initargs :initiative (a:required-argument :initiative)))

(defclass after-battle (player-phase) ())

(p:define-protocol-class final (phase) ())

(defclass before-final-battle (final before-battle) ())

(defclass final-battle (final battle)
  ((%initiative :reader initiative :initarg :initiative))
  (:default-initargs :initiative (a:required-argument :initiative)))

(defclass after-final-battle (final after-battle) ())

(defclass end (final) ())
