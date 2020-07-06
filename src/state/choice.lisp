;;;; src/state/choice.lisp

(uiop:define-package #:nervous-island.choice
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export #:choice #:place-hq-tile))

(in-package #:nervous-island.choice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choice - protocol

(p:define-protocol-class choice () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choice - concrete classes

(defclass place-hq-tile (choice)
  ((%hq-tile :reader hq-tile :initarg :hq-tile))
  (:default-initargs :hq-tile (a:required-argument :hq-tile)))
