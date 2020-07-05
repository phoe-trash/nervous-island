;;;; src/tiles/instant.lisp

(uiop:define-package #:nervous-island.instant
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:nt #:nervous-island.tile))
  (:export #:battle #:move #:push-back #:sniper #:grenade #:air-strike))

(in-package #:nervous-island.instant)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instant tiles

(defclass battle (nt:instant) ())
(defun battle () (make-instance 'battle))

(defclass move (nt:instant) ())
(defun move () (make-instance 'move))

(defclass push-back (nt:instant) ())
(defun push-back () (make-instance 'push-back))

(defclass sniper (nt:instant) ())
(defun sniper () (make-instance 'sniper))

(defclass grenade (nt:instant) ())
(defun grenade () (make-instance 'grenade))

(defclass air-strike (nt:instant) ())
(defun air-strike () (make-instance 'air-strike))
