;;;; src/tiles/effect.lisp

(in-package #:nervous-island.common)

(defmacro define-effect-package (name &body clauses)
  (flet ((make-effect-names (effect)
           (loop for prefix in '(#:|| #:undirected- #:directed-
                                 #:long-range-directed-)
                 for name = (format nil "~A~A" prefix effect)
                 collect (make-symbol name))))
    (let* ((effects (a:assoc-value clauses :export-effects))
           (effect-names (mapcan #'make-effect-names effects)))
      `(uiop:define-package ,name
         ,@(remove :export-effects clauses :key #'car)
         (:export ,@effect-names)))))

(define-effect-package #:nervous-island.effect
  (:use #:nervous-island.cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Module effects - protocol
   #:effect #:undirected-effect #:directed-effect :long-range-directed-effect
   #:numeric-effect #:strength
   ;; Module effects - trap (DDM)
   #:trap #:directed-trap)
  (:export-effects
   #:melee-officer #:ranged-officer #:scout #:mother
   #:medic #:transport #:rotation #:quartermaster #:recon-center #:healing
   #:scoper #:saboteur
   #:motherland #:net-of-steel-launcher))

(in-package #:nervous-island.effect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - protocol

(define-class effect (nsk:passive) ()
  (:protocolp t))

(define-class undirected-effect (nsk:undirected effect) ()
  (:protocolp t))
(define-class directed-effect (nsk:directed effect) ()
  (:protocolp t))
(define-class long-range-directed-effect (nsk:directed effect) ()
  (:protocolp t))

;; TODO rename to just NUMERIC
(define-class numeric-effect (effect)
  ((strength :type (integer 1) :initform 1))
  (:protocolp t))

(defmacro define-effect (name (&key numericp))
  (let ((undirected-name (a:symbolicate '#:undirected- name))
        (directed-name (a:symbolicate '#:directed- name))
        (long-range-directed-name (a:symbolicate '#:long-range-directed- name))
        (superclass (if numericp 'numeric-effect 'effect))
        (lambda-list (if numericp '(&optional (strength 1)) '()))
        (initargs (if numericp '(:strength strength) '())))
    `(progn
       (define-class ,name (,superclass) ()
         (:protocolp t))
       (define-class ,undirected-name (,name undirected-effect) ())
       (defun ,undirected-name (,@lambda-list)
         (make-instance ',undirected-name ,@initargs))
       (define-class ,directed-name (,name directed-effect) ())
       (defun ,directed-name (direction ,@lambda-list)
         (make-instance ',directed-name :direction direction ,@initargs))
       (define-class ,long-range-directed-name
           (,name long-range-directed-effect) ())
       (defun ,long-range-directed-name (direction ,@lambda-list)
         (make-instance ',long-range-directed-name :direction direction
                        ,@initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - concrete classes

(define-effect melee-officer (:numericp t))
(define-effect ranged-officer (:numericp t))
(define-effect scout (:numericp t))
(define-effect mother (:numericp t))
(define-effect healing (:numericp t))

(define-effect medic ())
(define-effect transport ())
(define-effect rotation ())
(define-effect quartermaster ())
(define-effect recon-center ())

(define-effect scoper ())
(define-effect saboteur ())

(define-effect motherland ())
(define-effect net-of-steel-launcher ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - trap (DDM)

(define-class trap (nsk:active) ())
(define-class directed-trap (trap nsk:directed) ())
(defun directed-trap (direction)
  (make-instance 'directed-trap :direction direction))
