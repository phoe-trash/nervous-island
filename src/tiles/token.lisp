;;;; src/tokens/token.lisp

(uiop:define-package #:nervous-island.token
  (:use #:nervous-island.cl)
  (:local-nicknames (#:p #:protest/base)
                    (#:nel #:nervous-island.element))
  (:export #:token
           #:damage #:net #:venom #:takeover #:steel-net #:paralysis #:hole
           #:incubator-token #:accelerator #:claw #:attack-net #:acid-thrower
           #:quill #:left-quill #:right-quill))

(in-package #:nervous-island.token)

(define-class token (nel:element) ()
  (:protocolp t))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (let* ((owner (nel:owner object))
           (name (if owner (nel:name owner) 'unowned)))
      (format stream "~A ~A ~A" name (type-of object) 'token))))

(defmacro define-token (name (&key owned (superclass 'token)))
  (let ((lambda-list (if owned '(owner) '()))
        (initargs (if owned '(:owner owner) '())))
    `(progn
       (define-class ,name (,superclass) ())
       (defun ,name (,@lambda-list)
         (make-instance ',name ,@initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens - concrete classes

(define-token damage ())
(define-token net ())

(define-token venom (:owned t))
(define-token takeover (:owned t))
(define-token steel-net (:owned t))
(define-token paralysis (:owned t))
(define-token hole (:owned t))

(define-class incubator-token (token) ()
  (:protocolp t))

(define-token accelerator (:owned t :superclass incubator-token))
(define-token claw (:owned t :superclass incubator-token))
(define-token attack-net (:owned t :superclass incubator-token))
(define-token acid-thrower (:owned t :superclass incubator-token))

(define-class quill (token) ()
  (:protocolp t))
(define-token left-quill (:owned t :superclass quill))
(define-token right-quill (:owned t :superclass quill))
