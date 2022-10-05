;;;; src/tiles/attack.lisp

(uiop:define-package #:nervous-island.attack
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export #:attack #:strength #:melee #:ranged #:gauss-cannon))

(in-package #:nervous-island.attack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - protocol

(define-class attack (nsk:active nsk:directed)
  ((strength :type (or (member t) (integer 1)) :initform 1))
  (:protocolp t))

(defmethod nsk:skill-printables append ((attack attack))
  (list (strength attack)))

(defmacro define-attack (name)
  `(progn
     (define-class ,name (attack) ())
     (defun ,name (direction &optional (strength 1))
       (make-instance ',name :direction direction :strength strength))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - concrete classes

(define-attack melee)
(define-attack ranged)
(define-attack gauss-cannon)
