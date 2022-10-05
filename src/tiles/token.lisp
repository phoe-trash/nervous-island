;;;; src/tokens/token.lisp

(uiop:define-package #:nervous-island.token
  (:use #:nervous-island.cl)
  (:local-nicknames (#:p #:protest/base))
  (:export #:token #:damage #:net))

(in-package #:nervous-island.token)

(define-class token () () (:protocolp t))

(defmacro define-token (name)
  `(define-class ,name (token) ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens - concrete classes

(define-token damage)
(define-token net)
