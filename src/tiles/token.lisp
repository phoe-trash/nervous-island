;;;; src/tokens/token.lisp

(uiop:define-package #:nervous-island.token
  (:use #:cl)
  (:local-nicknames (#:p #:protest/base))
  (:export #:token #:damage #:net))

(in-package #:nervous-island.token)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens - protocol

(p:define-protocol-class token () ())

(ncom:define-class damage (token) ())

(ncom:define-class net (token) ())
