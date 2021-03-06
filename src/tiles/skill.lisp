;;;; src/tiles/skill.lisp

(uiop:define-package #:nervous-island.skill
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common))
  (:export
   ;; Skills - protocol
   #:skill #:directed #:direction #:undirected
   #:*activation-times* #:activation-time #:active #:activation-time #:passive
   #:active-directed #:active-undirected #:passive-directed #:passive-undirected
   ;; Skills - concrete classes
   #:armor #:net #:mobility #:explosion #:toughness
   #:*special-initiative-values* #:initiative-value #:initiative #:value))

(in-package #:nervous-island.skill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skills - protocol

(ncom:define-typechecked-class skill () ()
  (:protocolp t))

(ncom:define-typechecked-class directed (skill)
  ((direction :type (or ncom:direction ncom:diagonal)))
  (:protocolp t))

(defmethod print-object ((object directed) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~S" (type-of object) (direction object))))

(ncom:define-typechecked-class undirected (skill) () (:protocolp t))

(defvar *activation-times* '(:initiative :initiative-player-choice :turn))
(deftype activation-time ()
  '(member :initiative :initiative-player-choice :turn))

(ncom:define-typechecked-class active (skill)
  ((activation-time :type activation-time :initform :initiative))
  (:protocolp t))

(ncom:define-typechecked-class passive (skill) () (:protocolp t))

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

(defvar *special-initiative-values* '(:before :after))
(deftype initiative-value () '(or (member :before :after) (integer 0)))

(ncom:define-typechecked-class initiative (passive-undirected)
  ((value :type initiative-value)))
(defun initiative (value) (make-instance 'initiative :value value))

(defmethod print-object ((object initiative) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~A ~A" (type-of object) (value object))))
