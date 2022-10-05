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

(define-class battle (nt:instant) ())
(defun battle () (make-instance 'battle))

(define-class move (nt:instant) ())
(defun move () (make-instance 'move))

(define-class push-back (nt:instant) ())
(defun push-back () (make-instance 'push-back))

(define-class sniper (nt:instant) ())
(defun sniper () (make-instance 'sniper))

(define-class grenade (nt:instant) ())
(defun grenade () (make-instance 'grenade))

(define-class air-strike (nt:instant) ())
(defun air-strike () (make-instance 'air-strike))
