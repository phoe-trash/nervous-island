;;;; src/state/step.lisp

(uiop:define-package #:nervous-island.step
  (:use #:cl)
  (:shadow #:step #:number #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:Φ #:phoe-toolbox)
                    (#:na #:nervous-island.attack)
                    (#:ncom #:nervous-island.common)
                    (#:nd #:nervous-island.damage)
                    (#:nsp #:nervous-island.space)
                    (#:nsk #:nervous-island.skill)
                    (#:nt #:nervous-island.tile)
                    (#:nto #:nervous-island.token))
  (:export #:define-step #:step #:step-with-tile
           #:draw-tile #:start-full-board-battle))

(in-package #:nervous-island.step)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-step
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(define-class ,name ,superclasses
     ,slot-definitions (:protocolp nil) ,@options))

(setf (trivial-indent:indentation 'define-step) '(4 &lambda &body))

(defmacro define-protocol-step
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(define-class ,name ,superclasses
     ,slot-definitions (:protocolp t) ,@options))

(setf (trivial-indent:indentation 'define-protocol-step) '(4 &lambda &body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Steps - protocol

(define-protocol-step step ()
  ((dependent-steps :initform '())))

(define-protocol-step step-with-tile (step)
  ((tile :type nt:tile)))

(define-protocol-step step-with-target-tile (step)
  ((target-tile :type nt:tile)))

(define-protocol-step step-with-damage (step)
  ((damage :type nd:damage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Steps - concrete classes

(define-step draw-tile (step) ())

(define-step create-damage (step-with-damage) ())

(define-step heal-damage (step-with-tile step-with-damage) ())

(define-step apply-damage (step-with-damage) ())

(define-step discard-tile (step-with-tile) ())

(define-step start-battle (step) ())

(define-step start-final-battle (start-battle) ())

(define-step start-full-board-battle (start-battle) ())
