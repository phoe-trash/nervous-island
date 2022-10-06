;;;; src/tokens/token.lisp

(uiop:define-package #:nervous-island.token
  (:use #:nervous-island.cl)
  (:local-nicknames (#:p #:protest/base)
                    (#:nel #:nervous-island.element))
  (:export #:token
           #:damage #:net #:venom))

(in-package #:nervous-island.token)

(define-class token (nel:element) ()
  (:protocolp t))

(defmacro define-token (name (&key owned))
  (let ((lambda-list (if owned '(owner) '()))
        (initargs (if owned '(:owner owner) '())))
    `(progn
       (define-class ,name (token) ())
       (defun ,name (,@lambda-list)
         (make-instance ',name ,@initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens - concrete classes

(define-token damage ())
(define-token net ())
(define-token venom (:owned t))
