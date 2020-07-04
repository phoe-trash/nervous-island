;;;; src/state/choice.lisp

(uiop:define-package #:nervous-island.choice
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:protest/base))
  (:export #:choice #:place-hq-tiles))

(in-package #:nervous-island.choice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choice - protocol

(p:define-protocol-class choice ()
  ((%player :reader player :initarg :player))
  (:default-initargs :player (a:required-argument :player)))

(defclass place-hq-tiles (choice)
  ((%hq-tiles :reader hq-tiles :initarg :hq-tiles))
  (:default-initargs :hq-tiles (a:required-argument :hq-tiles)))
