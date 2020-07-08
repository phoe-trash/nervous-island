;;;; src/state/choice.lisp

(uiop:define-package #:nervous-island.choice
  (:use #:cl)
  (:shadow #:number #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:Φ #:phoe-toolbox)
                    (#:ncom #:nervous-island.common)
                    (#:np #:nervous-island.player)
                    (#:ns #:nervous-island.step)
                    (#:nsk #:nervous-island.skill)
                    (#:nsp #:nervous-island.space)
                    (#:nt #:nervous-island.tile))
  (:export
   #:define-choice #:choice #:choose-player-order
   #:choice-with-tile #:place-tile #:discard-tile #:end-turn
   #:use-instant-tile))

(in-package #:nervous-island.choice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-choice
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(ncom:define-typechecked-class ,name ,superclasses
     ,slot-definitions :protocolp nil ,@options))

(setf (trivial-indent:indentation 'define-choice) '(4 &lambda &body))

(defmacro define-protocol-choice
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(ncom:define-typechecked-class ,name ,superclasses
     ,slot-definitions :protocolp t ,@options))

(setf (trivial-indent:indentation 'define-protocol-choice) '(4 &lambda &body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choices - protocol

(define-protocol-choice choice (ns:step)
  ((player :type np:player)))

(define-protocol-choice choice-with-tile (choice)
  ((tile :type nt:tile)))

(define-protocol-choice use-instant-tile (choice) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choices - concrete classes

(define-choice choose-player-order (choice)
  ((players :type (Φ:list-of np:player))))

(define-choice place-tile (choice-with-tile)
  ((space :type nsp:space)))

(define-choice discard-tile (choice-with-tile) ())

(define-choice end-turn (choice) ())

(define-choice use-mobility (choice-with-tile)
  ((skill-source :type nsk:skill)
   (new-space :type nsp:space)))

(define-choice use-double-mobility (use-mobility)
  ((intermediate-space :type nsp:space)))

(define-choice perform-normal-attack (choice-with-tile) ())

(define-choice perform-explosion (choice-with-tile) ())

(define-choice heal-target (choice-with-tile)
  ((target-tile :type nt:tile)))
