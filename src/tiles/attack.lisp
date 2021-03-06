;;;; src/tiles/attack.lisp

(uiop:define-package #:nervous-island.attack
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export #:attack #:strength #:melee #:ranged #:gauss-cannon))

(in-package #:nervous-island.attack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks - protocol

(ncom:define-typechecked-class attack (nsk:active-directed)
  ((strength :type (or (eql t) (integer 1)) :initform 1))
  (:protocolp t))

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
