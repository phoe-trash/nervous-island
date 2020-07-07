;;;; src/state/step.lisp

(uiop:define-package #:nervous-island.step
  (:use #:cl)
  (:shadow #:step #:number #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:Φ #:phoe-toolbox)
                    (#:na #:nervous-island.attack)
                    (#:ncom #:nervous-island.common)
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
  `(ncom:define-typechecked-object ,name ,superclasses
     ,slot-definitions ,@options :protocolp nil))

(setf (trivial-indent:indentation 'define-step) '(4 &lambda &body))

(defmacro define-protocol-step
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(ncom:define-typechecked-object ,name ,superclasses
     ,slot-definitions ,@options :protocolp t))

(setf (trivial-indent:indentation 'define-protocol-step) '(4 &lambda &body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Steps - protocol

(define-protocol-step step () ())

(define-protocol-step step-with-tile (step)
  ((tile :type nt:tile)))

(define-protocol-step step-with-target-tile (step)
  ((target-tile :type nt:tile)))

(define-protocol-step step-with-damage (step)
  ((source :type (or nt:warrior nt:instant nt:foundation nto:token))
   (attack :type (or null na:attack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Steps - concrete classes

(define-step draw-tile (step-with-tile) ())

(define-step deal-damage (step-with-tile step-with-target-tile
                          step-with-damage) ())

(define-step heal-target (step-with-tile step-with-target-tile)
  ((damage :type t))) ;; TODO nd:damage

(define-step apply-damage (step-with-tile step-with-target-tile
                           step-with-damage) ())

(define-step discard-tile-from-space (step-with-tile)
  ((space :type nsp:space)))

(define-step start-full-board-battle (step) ())
