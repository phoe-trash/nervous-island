;;;; src/tiles/attack.lisp

(uiop:define-package #:nervous-island.attack
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export #:attack #:strength
           #:melee #:zombie-melee #:friendly-fire-melee
           #:ranged #:gauss-cannon #:shotgun #:rocket-launcher
           #:explosive #:poisoning #:demolition #:water-gun))

(in-package #:nervous-island.attack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - protocol

(define-class attack (nsk:active nsk:directed)
  ((strength :type (or (member t) (integer 1)) :initform 1))
  (:protocolp t))

(defmethod print-object ((object attack) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A ~A" (type-of object) (nsk:direction object)
            (strength object))))

(defmacro define-attack (name (&key (superclasses '(attack))))
  `(progn
     (define-class ,name (,@superclasses) ())
     (defun ,name (direction &optional (strength 1))
       (make-instance ',name :direction direction :strength strength))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - concrete classes

(define-attack melee ())
(define-attack zombie-melee (:superclasses (attack nsk:zombie)))
(define-attack friendly-fire-melee (:superclasses (attack nsk:friendly-fire)))
(define-attack ranged ())
(define-attack gauss-cannon ())
(define-attack shotgun ())
(define-attack rocket-launcher ())
(define-attack explosive ())
(define-attack poisoning ())
(define-attack demolition ())
(define-attack water-gun ())
