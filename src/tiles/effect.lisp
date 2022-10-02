;;;; src/tiles/effect.lisp

(uiop:define-package #:nervous-island.effect
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Module effects - protocol
   #:effect #:directed-effect #:undirected-effect #:numeric-effect #:value
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

(p:define-protocol-class effect (nsk:passive) ())

(p:define-protocol-class directed-effect (nsk:directed effect) ())
(p:define-protocol-class undirected-effect (nsk:undirected effect) ())

(ncom:define-class numeric-effect (effect)
  ((strength :type (integer 1) :initform 1))
  (:protocolp t))

(p:define-protocol-class melee-officer (numeric-effect) ())
(p:define-protocol-class ranged-officer (numeric-effect) ())
(p:define-protocol-class scout (numeric-effect) ())
(p:define-protocol-class mother (numeric-effect) ())

(p:define-protocol-class medic (effect) ())
(p:define-protocol-class transport (effect) ())
(p:define-protocol-class quartermaster (effect) ())
(p:define-protocol-class recon-center (effect) ())

(p:define-protocol-class scoper (effect) ())
(p:define-protocol-class saboteur (effect) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - concrete classes, directed

(ncom:define-class directed-melee-officer (melee-officer directed-effect) ())
(defun directed-melee-officer (direction &optional (strength 1))
  (make-instance 'directed-melee-officer :direction direction
                                         :strength strength))

(ncom:define-class directed-ranged-officer (ranged-officer directed-effect) ())
(defun directed-ranged-officer (direction &optional (strength 1))
  (make-instance 'directed-ranged-officer :direction direction
                                          :strength strength))

(ncom:define-class directed-scout (scout directed-effect) ())
(defun directed-scout (direction &optional (strength 1))
  (make-instance 'directed-scout :direction direction
                                 :strength strength))

(ncom:define-class directed-mother (mother directed-effect) ())
(defun directed-mother (direction &optional (strength 1))
  (make-instance 'directed-mother :direction direction
                                  :strength strength))

(ncom:define-class directed-medic (medic directed-effect) ())
(defun directed-medic (direction)
  (make-instance 'directed-medic :direction direction))

(ncom:define-class directed-transport (transport directed-effect) ())
(defun directed-transport (direction)
  (make-instance 'directed-transport :direction direction))

(ncom:define-class directed-quartermaster (directed-effect quartermaster) ())
(defun directed-quartermaster (direction)
  (make-instance 'directed-quartermaster :direction direction))

(ncom:define-class directed-recon-center (directed-effect recon-center) ())
(defun directed-recon-center (direction)
  (make-instance 'directed-recon-center :direction direction))

(ncom:define-class directed-scoper (directed-effect scoper) ())
(defun directed-scoper (direction)
  (make-instance 'directed-scoper :direction direction))

(ncom:define-class directed-saboteur (directed-effect saboteur) ())
(defun directed-saboteur (direction)
  (make-instance 'directed-saboteur :direction direction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - concrete classes, undirected

(ncom:define-class undirected-melee-officer (undirected-effect melee-officer) ())
(defun undirected-melee-officer (&optional (strength 1))
  (make-instance 'undirected-melee-officer :strength strength))

(ncom:define-class undirected-ranged-officer (undirected-effect ranged-officer) ())
(defun undirected-ranged-officer (&optional (strength 1))
  (make-instance 'undirected-ranged-officer :strength strength))

(ncom:define-class undirected-scout (undirected-effect scout) ())
(defun undirected-scout (&optional (strength 1))
  (make-instance 'undirected-scout :strength strength))

(ncom:define-class undirected-mother (undirected-effect mother) ())
(defun undirected-mother (&optional (strength 1))
  (make-instance 'undirected-mother :strength strength))

(ncom:define-class undirected-medic (undirected-effect medic) ())
(defun undirected-medic () (make-instance 'undirected-medic))

(ncom:define-class undirected-transport (undirected-effect transport) ())
(defun undirected-transport () (make-instance 'undirected-transport))

(ncom:define-class undirected-quartermaster (undirected-effect quartermaster) ())
(defun undirected-quartermaster () (make-instance 'undirected-quartermaster))

(ncom:define-class undirected-recon-center (undirected-effect recon-center) ())
(defun undirected-recon-center () (make-instance 'undirected-recon-center))

(ncom:define-class undirected-scoper (undirected-effect scoper) ())
(defun undirected-scoper () (make-instance 'undirected-scoper))

(ncom:define-class undirected-saboteur (undirected-effect saboteur) ())
(defun undirected-saboteur ()  (make-instance 'undirected-saboteur))
