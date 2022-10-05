;;;; src/tiles/effect.lisp

(uiop:define-package #:nervous-island.effect
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Module effects - protocol
   #:effect #:directed-effect #:undirected-effect #:numeric-effect #:strength
   ;; Module effects - definitions
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

(define-class effect (nsk:passive) ()
  (:protocolp t))

(define-class directed-effect (nsk:directed effect) ()
  (:protocolp t))

(define-class undirected-effect (nsk:undirected effect) ()
  (:protocolp t))

(define-class numeric-effect (effect)
  ((strength :type (integer 1) :initform 1))
  (:protocolp t))

(defmacro define-effect (name (&key numericp))
  (let ((directed-name (a:symbolicate '#:directed- name))
        (undirected-name (a:symbolicate '#:undirected- name))
        (superclass (if numericp 'numeric-effect 'effect))
        (lambda-list (if numericp '(&optional (strength 1)) '()))
        (initargs (if numericp '(:strength strength) '())))
    `(progn
       (define-class ,name (,superclass) ()
         (:protocolp t))
       (define-class ,directed-name (,name directed-effect) ())
       (defun ,directed-name (direction ,@lambda-list)
         (make-instance ',directed-name :direction direction ,@initargs))
       (define-class ,undirected-name (,name undirected-effect) ())
       (defun ,undirected-name (,@lambda-list)
         (make-instance ',undirected-name ,@initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - concrete classes

(define-effect melee-officer (:numericp t))
(define-effect ranged-officer (:numericp t))
(define-effect scout (:numericp t))
(define-effect mother (:numericp t))

(define-effect medic ())
(define-effect transport ())
(define-effect quartermaster ())
(define-effect recon-center ())

(define-effect scoper ())
(define-effect saboteur ())
