;;;; src/tiles/attack.lisp

(uiop:define-package #:nervous-island.attack
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ns #:nervous-island.skill))
  (:export
   ;; Attacks
   #:attack #:strength #:melee #:ranged #:gauss-cannon))

(in-package #:nervous-island.attack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attacks

(p:define-protocol-class attack (ns:active-directed)
  ((%strength :reader strength :initarg :strength))
  (:default-initargs :strength 1))

(defmethod print-object ((object attack) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A ~S ~A" (type-of object) 'attack
            (ns:direction object) (strength object))))

(defclass melee (attack) ())
(defun melee (direction &optional (strength 1))
  (make-instance 'melee :strength strength :direction direction))

(defclass ranged (attack) ())
(defun ranged (direction &optional (strength 1))
  (make-instance 'ranged :strength strength :direction direction))

(defclass gauss-cannon (attack) ())
(defun gauss-cannon (direction &optional (strength 1))
  (make-instance 'gauss-cannon :strength strength :direction direction))
