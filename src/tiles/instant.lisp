;;;; src/tiles/instant.lisp

(uiop:define-package #:nervous-island.instant
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:nt #:nervous-island.tile))
  (:export
   #:battle
   #:move #:push-back #:grab #:reposition
   #:sniper #:grenade #:air-strike #:small-bomb))

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
(define-instant grab)
(define-instant reposition)
(define-instant sniper)
(define-instant grenade)
(define-instant air-strike)
(define-instant small-bomb)
