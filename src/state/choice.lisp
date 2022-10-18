;;;; src/state/choice.lisp

(uiop:define-package #:nervous-island.choice
  (:use #:cl)
  (:shadow #:number #:space)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base)
                    (#:Φ #:phoe-toolbox)
                    (#:nc #:nervous-island.coord)
                    (#:ncom #:nervous-island.common)
                    (#:np #:nervous-island.player)
                    (#:ns #:nervous-island.step)
                    (#:nsk #:nervous-island.skill)
                    (#:nsp #:nervous-island.space)
                    (#:nt #:nervous-island.tile))
  (:export
   #:define-choice #:define-protocol-choice
   #:choice #:choice-with-tile #:use-instant-tile
   #:choose-player-order #:randomize-player-order
   #:place-tile #:discard-tile #:end-turn
   #:use-mobility #:use-double-mobility #:perform-normal-attack
   #:perform-explosion #:heal-target))

(in-package #:nervous-island.choice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro define-choice
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(define-class ,name ,superclasses
     ,slot-definitions (:protocolp nil) ,@options))

(setf (trivial-indent:indentation 'define-choice) '(4 &lambda &body))

(defmacro define-protocol-choice
    (name (&rest superclasses) (&rest slot-definitions) &body options)
  `(define-class ,name ,superclasses
     ,slot-definitions (:protocolp t) ,@options))

(setf (trivial-indent:indentation 'define-protocol-choice) '(4 &lambda &body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choices - protocol

(define-protocol-choice choice (ns:step) ())

(define-protocol-choice player-choice (choice)
  ((player :type np:player)))

(define-protocol-choice choice-with-tile (player-choice)
  ((tile :type nt:tile)))

(define-protocol-choice use-instant-tile (choice-with-tile) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choices - concrete classes

(define-choice choose-player-order (choice)
  ((players :type (Φ:list-of np:player)))) ;; TODO Φ:list-set-of? TODO validate

(define-choice randomize-player-order (choice) ())

(define-choice place-tile (choice-with-tile)
  ((space :type nsp:space)))

(define-choice discard-tile (choice-with-tile) ())

(define-choice end-turn (player-choice) ())

(define-choice use-mobility (choice-with-tile)
  ((skill-source :type nsk:skill)
   (final-space :type nsp:space)
   (final-rotation :type ncom:direction)))

(define-choice use-double-mobility (use-mobility)
  ((intermediate-space :type nsp:space)
   (intermediate-rotation :type ncom:direction)))

(define-choice perform-normal-attack (choice-with-tile) ())

(define-choice perform-explosion (choice-with-tile) ())

(define-choice heal-target (choice-with-tile)
  ((target-tile :type nt:tile)))
