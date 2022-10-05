;;;; src/tiles/instant.lisp

(uiop:define-package #:nervous-island.instant
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:nt #:nervous-island.tile))
  (:export #:battle #:move #:push-back #:sniper #:grenade #:air-strike))

(in-package #:nervous-island.instant)

;; TODO merge this into NI.TILE

(defmacro define-instant (name)
  `(progn
     (define-class ,name (nt:instant) ())
     (defun ,name () (make-instance ',name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Instant tiles - concrete classes

(define-instant battle)
(define-instant move)
(define-instant push-back)
(define-instant sniper)
(define-instant grenade)
(define-instant air-strike)
