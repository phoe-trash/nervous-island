;;;; src/tiles/skill.lisp

(uiop:define-package #:nervous-island.skill
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common))
  (:export
   ;; Skills - protocol
   #:skill #:directed #:direction #:undirected
   #:active #:activation-time #:passive
   #:active-directed #:active-undirected #:passive-directed #:passive-undirected
   ;; Skills - concrete classes
   #:armor #:net #:mobility #:explosion #:toughness #:initiative #:value))

(in-package #:nervous-island.skill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - protocol

(p:define-protocol-class skill () ())

(deftype activation-time ()
  '(member :initiative :initiative-player-choice :turn))

(p:define-protocol-class active (skill)
  ((%activation-time :reader activation-time :initarg :activation-time))
  (:default-initargs :activation-time :initiative))

(defmethod shared-initialize :around
    ((skill active) slots &rest args
     &key (activation-time nil activation-time-p))
  (when activation-time-p
    (check-type activation-time activation-time)
    (nconc (list :activation-time activation-time) args))
  (apply #'call-next-method skill slots args))

(p:define-protocol-class passive (skill) ())

(p:define-protocol-class directed (skill)
  ((%direction :reader direction :initarg :direction))
  (:default-initargs :direction (a:required-argument :direction)))

(defmethod shared-initialize :around
    ((skill directed) slots &rest args
     &key (direction nil directionp))
  (when directionp
    (check-type direction (or ncom:direction ncom:diagonal))
    (nconc (list :direction direction) args))
  (apply #'call-next-method skill slots args))

(defmethod print-object ((object directed) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~S" (type-of object) (direction object))))

(p:define-protocol-class undirected (skill) ())

(defmethod print-object ((object undirected) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A" (type-of object))))

(p:define-protocol-class active-directed (active directed) ())
(p:define-protocol-class active-undirected (active undirected) ())
(p:define-protocol-class passive-directed (passive directed) ())
(p:define-protocol-class passive-undirected (passive undirected) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - concrete classes

(defclass armor (passive-directed) ())
(defun armor (direction) (make-instance 'armor :direction direction))

(defclass net (passive-directed) ())
(defun net (direction) (make-instance 'net :direction direction))

(defclass mobility (active-undirected) ()
  (:default-initargs :activation-time :turn))
(defun mobility () (make-instance 'mobility))

(defclass explosion (active-undirected) ()
  (:default-initargs :activation-time :initiative-player-choice))
(defun explosion () (make-instance 'explosion))

(defclass toughness (passive-undirected) ())
(defun toughness () (make-instance 'toughness))

(defclass initiative (passive-undirected)
  ((%value :reader value :initarg :value))
  (:default-initargs :value (a:required-argument :value)))
(defun initiative (value) (make-instance 'initiative :value value))

(defmethod print-object ((object initiative) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A" (type-of object) (value object))))
