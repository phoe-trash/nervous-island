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
  (:shadow #:speed)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:na #:nervous-island.attack)
                    (#:ncom #:nervous-island.common)
                    (#:nsk #:nervous-island.skill))
  (:export
   ;; Module effects - protocol
   #:effect #:undirected-effect #:directed-effect :long-range-directed-effect
   #:numeric #:strength
   ;; Module effects - trap (DDM)
   #:trap #:directed-trap)
  (:export-effects
   #:melee-officer #:ranged-officer #:speed #:saboteur #:additional-initiative
   #:toughness #:healing #:quartermaster #:attack-types
   #:medic #:mobility #:grab #:rotation #:move-doubler
   #:underground #:paralysis #:venom
   #:scoper #:takeover #:zone #:wastes
   #:motherland #:net-of-steel-launcher
   #:implant-activation #:muzzle #:net-on-melees #:explosion
   #:power-supply
   #:hidden-activation
   #:gourmet #:freezing))

(in-package #:nervous-island.effect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - protocol

(define-class effect (nsk:passive) ()
  (:protocolp t))

(define-class undirected-effect (effect nsk:undirected) ()
  (:protocolp t))
(define-class directed-effect (effect nsk:directed) ()
  (:protocolp t))
(define-class long-range-directed-effect (effect nsk:directed) ()
  (:protocolp t))

(define-class numeric (effect)
  ((strength :type (integer 1) :initform 1))
  (:protocolp t))

(defmacro define-effect (name (&key numericp quartermasterp))
  (let ((undirected-name (a:symbolicate '#:undirected- name))
        (directed-name (a:symbolicate '#:directed- name))
        (long-range-directed-name (a:symbolicate '#:long-range-directed- name))
        (slots (cond (quartermasterp '((attack-types :type set
                                                     :initform (set))))
                     (t '())))
        (superclass (cond (numericp 'numeric)
                          (t 'effect)))
        (lambda-list (cond (numericp '(&optional (strength 1)))
                           (quartermasterp '(&rest attack-types))
                           (t '())))
        (initargs (cond (numericp '(:strength strength))
                        (quartermasterp '(:attack-types
                                          (apply #'set attack-types)))
                        (t '())))
        (options (cond (quartermasterp '((:default-initargs
                                          :attack-types
                                          (set 'na:melee 'na:ranged))))
                       (t '()))))
    `(progn
       (define-class ,name (,superclass) (,@slots)
         (:protocolp t)
         ,@options)
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
(define-effect speed (:numericp t))
(define-effect saboteur (:numericp t))
(define-effect additional-initiative (:numericp t))
(define-effect toughness (:numericp t))
(define-effect healing (:numericp t))
(define-effect mobility (:numericp t))

(define-effect quartermaster (:quartermasterp t))
(defmethod nsk:skill-printables append ((skill quartermaster))
  (list (set-contents (attack-types skill))))

(define-effect medic ())
(define-effect grab ())
(define-effect rotation ())
(define-effect move-doubler ())
(define-effect underground ())
(define-effect paralysis ())
(define-effect venom ())

(define-effect scoper ())
(define-effect takeover ())
(define-effect zone ())
(define-effect wastes ())

(define-effect motherland ())
(define-effect net-of-steel-launcher ())

(define-effect implant-activation ())
(define-effect muzzle ())
(define-effect net-on-melees ())
(define-effect explosion ())

(define-effect power-supply ())

(define-effect hidden-activation ())

(define-effect gourmet ())
(define-effect freezing ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module effects - trap (DDM)

(define-class trap (nsk:active) ())
(define-class directed-trap (trap nsk:directed) ())
(defun directed-trap (direction)
  (make-instance 'directed-trap :direction direction))
