;;;; src/tiles/attack.lisp

(uiop:define-package #:nervous-island.attack
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:nsk #:nervous-island.skill))
  (:export #:attack #:strength #:melee #:ranged #:gauss-cannon))

(in-package #:nervous-island.attack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - protocol

(p:define-protocol-class attack (nsk:active-directed)
  ((%strength :reader strength :initarg :strength))
  (:default-initargs :strength 1))

(defmethod shared-initialize :around
    ((attack attack) slots &rest args &key (strength nil strengthp))
  (when strengthp
    (check-type strength (or (eql t) (integer 1)))
    (nconc (list :strength strength) args))
  (apply #'call-next-method attack slots args))

(defmethod print-object ((object attack) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A ~S ~A" (type-of object) 'attack
            (nsk:direction object) (strength object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - concrete classes

(defclass melee (attack) ())
(defun melee (direction &optional (strength 1))
  (make-instance 'melee :strength strength :direction direction))

(defclass ranged (attack) ())
(defun ranged (direction &optional (strength 1))
  (make-instance 'ranged :strength strength :direction direction))

(defclass gauss-cannon (attack) ())
(defun gauss-cannon (direction &optional (strength 1))
  (make-instance 'gauss-cannon :strength strength :direction direction))
