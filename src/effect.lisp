;;;; src/effect.lisp

(uiop:define-package #:nervous-island.effect
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ns #:nervous-island.skill))
  (:export
   ;; Module effects - protocol
   #:effect #:directed-effect #:undirected-effect #:numeric-effect
   #:melee-officer #:ranged-officer #:scout #:mother
   #:medic #:transport #:quartermaster #:recon-center #:scoper #:saboteur
   ;; Module effects - concrete classes
   #:directed-melee-officer #:directed-ranged-officer #:directed-scout
   #:directed-mother #:directed-medic #:directed-transport
   #:directed-quartermaster #:directed-recon-center #:directed-scoper
   #:directed-saboteur
   #:undirected-melee-officer #:undirected-ranged-officer #:undirected-scout
   #:undirected-mother #:undirected-medic #:undirected-transport
   #:undirected-quartermaster #:undirected-recon-center #:undirected-scoper
   #:undirected-saboteur))

(in-package #:nervous-island.effect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - protocol

(p:define-protocol-class effect (ns:passive)
  ((%targets :reader targets :initarg :targets))
  (:default-initargs :targets '(:friendly)))

(p:define-protocol-class directed-effect (ns:directed effect) ())
(p:define-protocol-class undirected-effect (ns:undirected effect) ())

(p:define-protocol-class numeric-effect (effect)
  ((%strength :reader strength :initarg :strength))
  (:default-initargs :strength 1))



(p:define-protocol-class melee-officer (numeric-effect) ())
(p:define-protocol-class ranged-officer (numeric-effect) ())
(p:define-protocol-class scout (numeric-effect) ())
(p:define-protocol-class mother (numeric-effect) ())

(p:define-protocol-class medic (effect) ())
(p:define-protocol-class transport (effect) ())
(p:define-protocol-class quartermaster (effect) ())
(p:define-protocol-class recon-center (effect) ())

(p:define-protocol-class scoper (effect) ()
  (:default-initargs :targets '(:enemy)))
(p:define-protocol-class saboteur (effect) ()
  (:default-initargs :targets '(:enemy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - concrete classes, directed

(defclass directed-melee-officer (melee-officer directed-effect) ())
(defun directed-melee-officer (direction &optional (strength 1))
  (make-instance 'directed-melee-officer :direction direction
                                         :strength strength))

(defclass directed-ranged-officer (ranged-officer directed-effect) ())
(defun directed-ranged-officer (direction &optional (strength 1))
  (make-instance 'directed-ranged-officer :direction direction
                                          :strength strength))

(defclass directed-scout (scout directed-effect) ())
(defun directed-scout (direction &optional (strength 1))
  (make-instance 'directed-scout :direction direction
                                 :strength strength))

(defclass directed-mother (mother directed-effect) ())
(defun directed-mother (direction &optional (strength 1))
  (make-instance 'directed-mother :direction direction
                                  :strength strength))

(defclass directed-medic (medic directed-effect) ())
(defun directed-medic (direction)
  (make-instance 'directed-medic :direction direction))

(defclass directed-transport (transport directed-effect) ())
(defun directed-transport (direction)
  (make-instance 'directed-transport :direction direction))

(defclass directed-quartermaster (directed-effect quartermaster) ())
(defun directed-quartermaster (direction)
  (make-instance 'directed-quartermaster :direction direction))

(defclass directed-recon-center (directed-effect recon-center) ())
(defun directed-recon-center (direction)
  (make-instance 'directed-recon-center :direction direction))

(defclass directed-scoper (directed-effect scoper) ())
(defun directed-scoper (direction)
  (make-instance 'directed-scoper :direction direction))

(defclass directed-saboteur (directed-effect saboteur) ())
(defun directed-saboteur (direction)
  (make-instance 'directed-saboteur :direction direction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - concrete classes, undirected

(defclass undirected-melee-officer (undirected-effect melee-officer) ())
(defun undirected-melee-officer (&optional (strength 1))
  (make-instance 'undirected-melee-officer :strength strength))

(defclass undirected-ranged-officer (undirected-effect ranged-officer) ())
(defun undirected-ranged-officer (&optional (strength 1))
  (make-instance 'undirected-ranged-officer :strength strength))

(defclass undirected-scout (undirected-effect scout) ())
(defun undirected-scout (&optional (strength 1))
  (make-instance 'undirected-scout :strength strength))

(defclass undirected-mother (undirected-effect mother) ())
(defun undirected-mother (&optional (strength 1))
  (make-instance 'undirected-mother :strength strength))

(defclass undirected-medic (undirected-effect medic) ())
(defun undirected-medic () (make-instance 'undirected-medic))

(defclass undirected-transport (undirected-effect transport) ())
(defun undirected-transport () (make-instance 'undirected-transport))

(defclass undirected-quartermaster (undirected-effect quartermaster) ())
(defun undirected-quartermaster () (make-instance 'undirected-quartermaster))

(defclass undirected-recon-center (undirected-effect recon-center) ())
(defun undirected-recon-center () (make-instance 'undirected-recon-center))

(defclass undirected-scoper (undirected-effect scoper) ())
(defun undirected-scoper () (make-instance 'undirected-scoper))

(defclass undirected-saboteur (undirected-effect saboteur) ())
(defun undirected-saboteur ()  (make-instance 'undirected-saboteur))
